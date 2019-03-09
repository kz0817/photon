package photon

import java.util.concurrent.atomic.AtomicInteger
import java.nio.ByteBuffer
import java.util.NoSuchElementException

class PhotonBuffer {
  val BUFFER_SIZE = 32
  val buff = ByteBuffer.allocate(BUFFER_SIZE)
  var recycled = false

  def recycle() {
    assert(recycled == false)
    recycled = true
    BufferStore.add(this)
  }
}

object BufferStore {

  implicit def toByteBuffer(b: PhotonBuffer): ByteBuffer = b.buff
  private val q = new scala.collection.mutable.SynchronizedQueue[PhotonBuffer]
  private val numOut = new AtomicInteger(0)

  def get(): PhotonBuffer = {
    try {
      val elem = q.dequeue()
      elem.recycled = false
      elem
    } catch {
      case e: NoSuchElementException => new PhotonBuffer()
    } finally {
      numOut.incrementAndGet()
    }
  }

  def add(buff: PhotonBuffer) {
    buff.clear()
    q += buff
    numOut.decrementAndGet()
  }

  def getNumOut = numOut.get
}
