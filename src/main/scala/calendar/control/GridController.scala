package calendar.control

import java.net.URLDecoder

import calendar.model.{AllProjects, ReportFrame, WeekEventModel}
import calendar.ui.{CalendarGrid, DialogFrame, EventFrame, WeekEventGrid}
import clientbase.connection.UserSettings
import definition.expression.DateConstant
import org.scalajs.dom
import org.scalajs.dom.html.{Div, Select}
import org.scalajs.dom.raw.{Event, HTMLElement, PopStateEvent}
import org.scalajs.dom.window
import scalatags.JsDom.all._
import util.Log

import scala.util.control.NonFatal

/**
  * Created by Peter on 08.02.2016.
  */
object GridController {
  val mainDiv: HTMLElement =dom.document.getElementById("maindiv").asInstanceOf[HTMLElement]
  val firstOptionStyle="margin-left:27px;"
  val comboBox: Select =select(`class`:="project-combo")(option(style:=firstOptionStyle)(CalendarGrid.allProjectName),option(style:=firstOptionStyle)(CalendarGrid.ownName)).render
  val calendarDiv: Div =div().render
  val grid=new CalendarGrid()
  lazy val model=new WeekEventModel
  var weekGrid:WeekEventGrid=_
  var visibleDialog:Option[DialogFrame]=None

  /*def applyDefined(obj: js.Dynamic, methods: Iterable[String], args: js.Any*): Unit =
    methods.find(m => !js.isUndefined(obj.selectDynamic(m))) match {
      case Some(m) => println("found:"+m); obj.applyDynamic(m)(args: _*)
      case None => js.undefined.asInstanceOf[js.Dynamic]
    }*/

  def start():Unit= try {
    mainDiv.innerHTML="2"
    /*mainDiv.onclick=(e:Event)=>{
      if(firstTouch) {
        firstTouch=false
        val el=dom.document.documentElement
        applyDefined(  el.asInstanceOf[js.Dynamic], Seq("requestFullscreen", "msRequestFullscreen", "mozRequestFullScreen","webkitRequestFullscreen" ))
      }
    }*/
    val today=DateConstant()
    UserSettings.basicFolders.get("Calendar") match {
      case Some(calRef) =>
        AllProjects.createOwnProject(calRef)
        AllProjects.model = Some(model)
        grid.model = model
        weekGrid = new WeekEventGrid(model)
        model.weekGrid = weekGrid
        AllProjects.loadMonthTables((prNames) => {
          mainDiv.innerHTML="3"
          for (i <- prNames.indices; n = prNames(i))
            comboBox.appendChild(option(div(div(style := "width:20px;height:20px;float:left;background-color:#" + CalendarGrid.projectColors(i % 14) + ";"),
              span(style := "margin-left:7px;")(n))).render)
          grid.changeMonth(today.month, today.year)
          mainDiv.innerHTML=""
          mainDiv.appendChild(calendarDiv)
          calendarDiv.appendChild(comboBox)
          grid.appendToParent(calendarDiv)
          calendarDiv.appendChild(weekGrid.gridTable)
          comboBox.onchange = (e: Event) => {
            AllProjects.selectProject(comboBox.value)
          }
          window.onpopstate= (event:PopStateEvent) => {onPopState(event) }
          val splitted=window.location.href.split("\\?")
          if(splitted.size< 2) grid.selectWeek(today.julian,doLoad = true,doPushState=false) // no params, start today
          else {
            Array(scala.scalajs.js.URIUtils.decodeURI(splitted(1))) ++ splitted.drop(2)  match {
              case EventFrame(frame)=> showDialog(frame)
              case ReportFrame(rframe)=> showDialog(rframe)
              case Array(prName,DateConstant(d))=>
                hideDialog()
                //println("Start pr:"+prName+"date "+d)
                setPrjAndDate(prName,d)
              case Array()=>Log.e("too short")
              case o=>
                Log.e("location:"+o.mkString(";"))
            }
          }
        })
      case None => Log.e("Cant find basicFolder Calendar in User Data");mainDiv.innerHTML="Cant find basicFolder Calendar in User Data"
    }
  } catch {
    case NonFatal(e)=> Log.e("startup ",e);mainDiv.innerHTML=e.getMessage+"<br>"+e.getStackTrace.mkString("<br>")
  }

  def onPopState(event:PopStateEvent):Unit= {
    //println("On Pop "+(if(event==null) "null" else event.state.toString))
    if(event!=null) {
      event.state.toString.split('|') match {
        case Array(prjName,DateConstant(monday))=>
          hideDialog()
          //println("pr:"+prjName+" mondayDate:"+monday)
          setPrjAndDate(prjName,monday)
        case EventFrame(frame)=>showDialog(frame)
      }

    }
  }

  def setPrjAndDate(prjName:String,date:DateConstant):Unit= {
    grid.changeMonth(date.month,date.year)
    //println("set prranddate "+prjName+" "+date)
    grid.selectWeek(date.julian,doLoad = false)
    val decName=URLDecoder.decode(prjName,"UTF-8")
    comboBox.value=if(decName=="")CalendarGrid.allProjectName else decName
    AllProjects.selectProject(decName)
  }

  def hideDialog(): Unit = for(d<-visibleDialog){
      //println("hide dialog "+d)
      d.shutDown()
      mainDiv.removeChild(mainDiv.firstChild)
      mainDiv.appendChild(calendarDiv)
      visibleDialog=None

      model.pushState()
    }


  def showDialog(frame:DialogFrame): Unit ={
    for(d<-visibleDialog) d.shutDown()
    mainDiv.removeChild(mainDiv.firstChild)
    mainDiv.appendChild(frame.frameDiv)
    visibleDialog=Some(frame)
  }
}
