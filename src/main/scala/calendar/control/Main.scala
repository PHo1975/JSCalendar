package calendar.control

import clientbase.connection.WebSocketConnector
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement

/**
  * Created by Peter on 06.02.2016.
  */
object Main {
  def main(args: Array[String]): Unit = {
    dom.document.getElementById("maindiv").asInstanceOf[HTMLElement].innerHTML="1"
    WebSocketConnector.start("WebCal",GridController.start _)
  }



}
