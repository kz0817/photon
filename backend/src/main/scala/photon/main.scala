package photon

import BufferStore.toByteBuffer
import scala.concurrent._
import java.nio.file.Paths
import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption
import java.net.InetSocketAddress
import java.net.StandardSocketOptions
import java.nio.channels.AsynchronousServerSocketChannel
import java.nio.channels.AsynchronousSocketChannel
import java.nio.channels.AsynchronousFileChannel
import java.nio.channels.CompletionHandler

object logger {

  def log(level: String, msg: String) {
    val tid = Thread.currentThread().getId();
    println(f"[T:$tid%05d] $level $msg")
  }

  def debug(msg: String) = log("DEBUG", msg)
  def info(msg: String) = log("INFO", msg)
  def error(msg: String) = log("ERROR", msg)
  def bug(msg: String) = log("BUG", msg)
}

case class HttpResponse(val code: Int, val header: String = "",
    val body: String = "") {

  val statusMap = Map(
    200 -> "OK",
    400 -> "Bad Request",
    403 -> "Forbidden",
    404 -> "Not found",
    500 -> "Internal Server Error",
    501 -> "Not Implemented",
  )

  def get = {
    if (statusMap contains code) {
      val status = statusMap(code)
      List(s"HTTP/1.1 $code $status", "", body) mkString("\r\n")
    } else {
      val errStatus = statusMap(500)
      logger.bug(s"Unknown code: $code")
      List(s"HTTP/1.1 500 $errStatus", "") mkString("\r\n")
    }
  }
}


sealed trait HttpProcessorState
case object HttpProcStart  extends HttpProcessorState
case object HttpProcHeader extends HttpProcessorState
case object HttpProcRoute extends HttpProcessorState
case object HttpProcBody   extends HttpProcessorState
case object HttpProcHold   extends HttpProcessorState
case object HttpProcDone   extends HttpProcessorState

case class HttpProcResult(
  nextState: HttpProcessorState,
  response: Option[HttpResponse])

sealed trait HttpMethod
case object GET    extends HttpMethod
case object POST   extends HttpMethod
case object PUT    extends HttpMethod
case object DELETE extends HttpMethod
case object HEAD   extends HttpMethod

case class ServerOperations(
  write: PhotonBuffer => Unit,
  close: () => Unit)

sealed trait ProtocolProcResult
case object ProtocolProcCompleted extends ProtocolProcResult
case object ProtocolProcContinue  extends ProtocolProcResult
case object ProtocolProcHold      extends ProtocolProcResult

case class Request(
  method: HttpMethod,
  url: String, 
  queryParameters: Map[String, String],
  version: String)

class HttpConnectionContext(
  var state: HttpProcessorState,
  var req: Option[Request],
  val headers: scala.collection.mutable.Map[String, String],
  var prevByte: Byte,
  val lineBuilder: StringBuilder,
  val srvOps: ServerOperations) {

  def write(buff: PhotonBuffer) = srvOps.write(buff)
  def close() = srvOps.close()
}

abstract class ProtocolProcessor[T] {
  def createContext(srvOps: ServerOperations): T
  def parseRawInput(ctx: T, buff: ByteBuffer): ProtocolProcResult
}

abstract class HttpProcessor extends ProtocolProcessor[HttpConnectionContext] {

  type HeaderMap = Map[String, String]
  type RouteHandler = HttpConnectionContext => HttpProcResult

  def responseOk(body: String = ""): HttpResponse = {
    HttpResponse(200)
  }

  val LF = 0x0a
  val CR = 0x0d

  val badRequest = HttpResponse(400)
  val notFound = HttpResponse(404)
  val notImplemented = HttpResponse(501)
  val routes: Map[HttpMethod, Map[String, RouteHandler]]

  private def buildLine(ctx: HttpConnectionContext, buff: ByteBuffer):
    Either[HttpResponse, Option[String]] = {
    while (buff.hasRemaining) {
      val currByte = buff.get
      if (ctx.prevByte == CR && currByte == LF) {
        val line = ctx.lineBuilder.toString
        ctx.lineBuilder.setLength(0)
        return Right(Some(line))
      } else if (!Util.isAscii(currByte)) {
        // TODO: consider UTF-8 string
        if (currByte != CR)
          logger.info(f"Non-ascii character: $currByte%02x")
      } else {
        ctx.lineBuilder.append(currByte.toChar)
      }
      ctx.prevByte = currByte
    }
    Right(None)
  }

  def send(ctx: HttpConnectionContext, res: HttpResponse): Unit = {
    val content = res.get
    logger.debug(s"send !! size: ${content.size}")
    val buff = BufferStore.get()
    //Util.dump(buff)
    buff.put(content.getBytes())
    ctx.write(buff)
    buff.recycle()
  }

  def appendBody(
    ctx: HttpConnectionContext,
    buff: PhotonBuffer, completed: Boolean = false) {

    logger.debug(s"appendBody, q.numOut: ${BufferStore.getNumOut}")
    ctx.write(buff)
    if (completed)
      ctx.close()
  }

  def abortResponse() {
    assert(false, "Not implemented")
  }

  implicit def toProcResult(res: HttpResponse): HttpProcResult
    = HttpProcResult(HttpProcDone, Some(res))

  implicit def stateToProcResult(state: HttpProcessorState): HttpProcResult
    = HttpProcResult(state, None)

  private def procRequestLine(
    ctx: HttpConnectionContext, buff: ByteBuffer): HttpProcResult = {

    buildLine(ctx, buff) match {
      case Left(res) => res
      case Right(optLine) => optLine match {
        case None => HttpProcDone
        case Some(line) => parseRequestLine(line) match {
          case Left(res) => res
          case Right(req) =>
            ctx.req = Some(req)
            HttpProcHeader
        }
      }
    }
  }

  private def procHeader(
    ctx: HttpConnectionContext, buff: ByteBuffer): HttpProcResult = {

    buildLine(ctx, buff) match {
      case Left(res) => res
      case Right(optLine) => optLine match {
        case None => HttpProcHeader
        case Some(line) if (line == "") => HttpProcRoute
        case Some(line) => parseHeader(line) match {
          case Left(res) => res
          case Right((key, value)) =>
            ctx.headers += (key -> value)
            HttpProcHeader
        }
      }
    }
  }

  override def createContext(
    srvOps: ServerOperations): HttpConnectionContext = {

    new HttpConnectionContext(HttpProcStart, None,
      scala.collection.mutable.Map[String, String](),
      0, new StringBuilder, srvOps)
  }

  override def parseRawInput(ctx: HttpConnectionContext, buff: ByteBuffer):
    ProtocolProcResult = {

    val result = ctx.state match {
      case HttpProcStart => procRequestLine(ctx, buff)
      case HttpProcHeader => procHeader(ctx, buff)
      case HttpProcRoute => routeRequest(ctx)
      case _ => HttpProcResult(HttpProcDone, Some(notImplemented))
    }
    logger.debug(s"${ctx.state}")
    result.response.map(res => send(ctx, res))
    ctx.state = result.nextState
    result.nextState match {
      case HttpProcDone => ProtocolProcCompleted
      case HttpProcHold => ProtocolProcHold
      case HttpProcRoute => parseRawInput(ctx, buff)
      case _ =>
        if (buff.hasRemaining)
          parseRawInput(ctx, buff)
        else
          ProtocolProcContinue
    }
  }

  private def collectGetQueryParamters(url: String): Map[String, String] = {
    val posQuestion = url.indexOf('?')
    if (posQuestion == -1)
      return Map()
    val queries = url.slice(posQuestion + 1, url.length) split '&'
    queries.map(_.split("=", 2)).map(_ match {
      case arr if (arr.length == 2) => arr(0) -> arr(1)
      case arr => arr(0) -> ""
    }).toMap
  }

  private def parseRequestLine(line: String): Either[HttpResponse, Request] = {
    val sepIdx1 = line.indexOf(' ')
    if (sepIdx1 == -1)
      return Left(badRequest)
    val sepIdx2 = line.indexOf(' ', sepIdx1 + 1)
    if (sepIdx2 == -1)
      return Left(badRequest)

    val methodName = line.slice(0, sepIdx1)
    val url = line.slice(sepIdx1+1, sepIdx2)
    val queryParameters: HeaderMap = methodName match {
      case "GET" => collectGetQueryParamters(url)
      case _ => Map()
    }
    val version = line.slice(sepIdx2+1, line.length)

    (methodName match {
      case "GET"    => Some(GET)
      case "POST"   => Some(POST)
      case "PUT"    => Some(PUT)
      case "DELETE" => Some(DELETE)
      case "HEAD"   => Some(HEAD)
      case _ => None
    })
    .map { method =>
      logger.debug(f"method: $method, $url")
      Right(Request(method, url, queryParameters, version))
    }
    .getOrElse {
      logger.info(s"Unknown HTTP method: $methodName")
      Left(badRequest)
    }
  }

  private def routeRequest(ctx: HttpConnectionContext): HttpProcResult = {

    val req = ctx.req.get
    if (routes contains req.method) {
      routes(req.method).get(req.url)
      .map(_(ctx))
      .getOrElse(notFound)
    } else {
      notFound
    }
  }

  private def parseHeader(line: String): Either[HttpResponse, (String, String)] = {
    val sepIdx = line indexOf ": "
    if (sepIdx == -1) {
      logger.info(s"No colon: $line")
      Left(badRequest)
    } else {
      val key = line.slice(0, sepIdx)
      val headerVal = line.slice(sepIdx+1, line.length)
      Right(key -> headerVal)
    }
  }

  def parseBody(list: String): Option[HttpResponse] = {
    None
  }
}

class Server[T](processor: ProtocolProcessor[T]) {
  val DEFAULT_PORT = 10318

  // TODO: custom thread group if needed
  val server = AsynchronousServerSocketChannel.open()
  server.setOption(StandardSocketOptions.SO_REUSEADDR, Boolean.box(true))

  private def close(channel: AsynchronousSocketChannel): Unit = {
    channel.close()
    logger.info("channel: closed")
  }

  private def setReadHandler(
    channel: AsynchronousSocketChannel, connCtx: T): Unit = {

    logger.debug("setReadHandler")
    val buff = BufferStore.get()
    logger.debug(s"0: position: ${buff.position()}, limit: ${buff.limit()}")
    channel.read(buff, null, new CompletionHandler[Integer, Null] {
      def completed(readBytes: Integer, arg: Null): Unit = {
        logger.debug(s"read completed: $readBytes")
        if (readBytes < 0) {
          close(channel)
          buff.recycle()
          return
        }
        //Util.dump(buff)
        buff.flip()
        val next = processor.parseRawInput(connCtx, buff)
        buff.recycle()
        next match {
          case ProtocolProcCompleted => close(channel)
          case ProtocolProcContinue => setReadHandler(channel, connCtx)
          case ProtocolProcHold => {}
        }
      }

      def failed(t: Throwable, arg: Null) = {
        logger.error("read: Failed")
        close(channel)
      }
    })
  }

  private def setAcceptHandler(): Unit = {
    val accept =
      server.accept(
        null, new CompletionHandler[AsynchronousSocketChannel, Null] {
      def completed(channel: AsynchronousSocketChannel, arg: Null) = {
        logger.info("Accepted")
        setAcceptHandler()
        val srvOps = ServerOperations(
          buff => write(channel, buff),
          () => close(channel))
        setReadHandler(channel, processor.createContext(srvOps))
      }
      def failed(t: Throwable, arg: Null) = {
        logger.error("accept: Failed")
      }
    })
  }

  def write(channel: AsynchronousSocketChannel, buff: PhotonBuffer) {
    // TODO: Be async
    logger.debug(s"Server#write: ${buff.limit()}")
    buff.flip()
    //Util.dump(buff)
    channel.write(buff).get
  }

  def start() {
    val port = DEFAULT_PORT
    logger.info(s"Started service. port: $port")
    server.bind(new InetSocketAddress(port))

    logger.info("Wait for connection.")
    setAcceptHandler()
  }

  def shutdown() {
    logger.info(s"Shutdown service.")
    server.close()
  }
}

class HttpFileProvider(httpProc: HttpProcessor) {

  val distDir = "../frontend/dist"

  def apply(ctx: HttpConnectionContext, filePath: String) = {
    // TODO: Catch exception
    val path = Paths.get(distDir, filePath)
    val channel = AsynchronousFileChannel.open(path, StandardOpenOption.READ)
    val fileSize = channel.size()
    httpProc.send(ctx, httpProc.responseOk())
    setFileReadHandler(ctx, channel, 0, fileSize)
    HttpProcResult(HttpProcHold, None)
  }

  def setFileReadHandler(
    ctx: HttpConnectionContext,
    channel: AsynchronousFileChannel, position: Long, fileSize: Long) {

    val buff = BufferStore.get()
    def completedHandler(readBytes: Integer): Unit = {
        logger.debug(s"FILE read: $readBytes, position: $position, fileSize: $fileSize")
        if (readBytes < 0) {
          logger.error("READ BYTES: 0")
          return
        }

        val totalRead = position + readBytes
        if (totalRead > fileSize) {
          httpProc.abortResponse()
          return
        }

        val completed = (totalRead == fileSize)
        logger.debug(s"totalRead: $totalRead, completed: $completed")
        httpProc.appendBody(ctx, buff, completed)
        if (!completed)
          setFileReadHandler(ctx, channel, totalRead, fileSize)
    }

    channel.read(buff, position, null, new CompletionHandler[Integer, Null] {
      def completed(readBytes: Integer, arg: Null): Unit = {
        completedHandler(readBytes)
        buff.recycle()
      }

      def failed(t: Throwable, arg: Null) = {
        logger.error("accept: Failed")
        buff.recycle()
      }
    })
  }
}

class Photon extends HttpProcessor {

  val fileProvider = new HttpFileProvider(this)

  override val routes = Map(
    GET -> Map(
      "/" -> (fileProvider(_, "index.html")),
      "/main.js" -> (fileProvider(_, "main.js"))
    ),
  )
}

object Main {

  def run() {
    logger.info(s"Running... Hit enter to exit")
    io.StdIn.readLine()
  }

  def main(args: Array[String]) {
    val server = new Server[HttpConnectionContext](new Photon)
    try {
      server.start()
      run()
    } finally {
      server.shutdown()
    }
  }
}
