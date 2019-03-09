package photon.frontend

import org.scalajs.dom
import dom.document
import dom.raw.HTMLElement

object FrontendApp {

  def createElement(elemType: String): HTMLElement =
    dom.document.createElement(elemType).asInstanceOf[HTMLElement]

  def createThumbnail(): HTMLElement = {
    println("createThumbnail()")
    val nail = createElement("div")
    val img = createElement("img")
    val title = createElement("p")
    nail.className = "thumbnail"
    title.innerHTML = "2018/03 park"
    nail.appendChild(img)
    nail.appendChild(title)
    nail
  }

  def showThumbnails(stage: dom.raw.Element) {
    println("showThumbnails()")
    for (i <- 1 to 5) {
      val nail = createThumbnail()
      stage.appendChild(nail)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Photon started !!")
    dom.document.addEventListener("DOMContentLoaded", { e: dom.Event =>
      println("DOMContentLoaded")
      showThumbnails(dom.document.getElementById("main-stage"))
    })
  }
}

