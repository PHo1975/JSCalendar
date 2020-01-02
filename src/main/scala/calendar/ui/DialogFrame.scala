package calendar.ui

import calendar.control.GridController
import calendar.model.{AllProjects, CalEvent, DataSource}
import clientbase.connection.WebSocketConnector
import definition.data.{OwnerReference, Reference}
import definition.expression._
import org.scalajs.dom
import org.scalajs.dom.html.{Button, Div, Input, Paragraph, TextArea}
import org.scalajs.dom.raw.HTMLElement
import scalatags.JsDom.all._
import util.{Log, StrToInt}

/**
  * Created by Kathi on 24.07.2016.
  */
trait DialogFrame {
  def frameDiv:HTMLElement
  def shutDown():Unit

  def close():Unit={
    //println("close")
    GridController.hideDialog()
  }
}

class TestFrame extends DialogFrame {
  val frameDiv: Div =div(div(style:="height:60px;")("Test-Frame"),button(`type`:="button",onclick:={ ()=>close()})("Zurück")).render
  def shutDown(): Unit ={
    println("shut down test frame")
  }
}



object EventFrame extends DialogFrame {
  val labelClass="form-label"
  val dayStrings=Array("Mo","Di","Mi","Do","Fr","Sa","So")

  var currentEvent:Option[CalEvent]=None
  var currentProject:DataSource=_
  var currentDay:DateConstant=_
  var currentHour:Int=0
  var doneClicked:Boolean=false

  val nameEdit: TextArea =textarea(`class`:="form-label-high",`type`:="text",name:="eventName").render
  val placeEdit: Input =input(`class`:=labelClass,`type`:="text",name:="eventPlace").render
  val doneCheckBox: Input =input(`type`:="checkbox",name:="erl",value:="erledigt",onclick:={ ()=>doneClicked=true}).render
  val infoLabel: Paragraph =p().render

  val deleteEventBut: Button =button(`class`:="form-button",`type`:="button",onclick:={ ()=>deleteEvent()})("Ereignis Löschen").render

  val frameDiv: Div =div(`class`:="dialog-frame")(
    div(`class`:=labelClass)(infoLabel),
    div(`class`:="form-label-wrap")(
      div(`class`:="form-label-left")("Ereignis:"),div(`class`:="form-label-right")(doneCheckBox,label("erledigt"))
    ),
    nameEdit,
    div(`class`:=labelClass)("Ort:"),
    placeEdit,
    div(`class`:=labelClass)(p()),
    div(`class`:="button-line") (
      button(`class`:="form-button",`type`:="button",onclick:={()=>save()})("Speichern"),
      deleteEventBut,
      button(`class`:="form-button",`type`:="button",onclick:={()=>dom.window.history.back()})("Abbruch")
    )
  ).render

  def unapply(par:Array[String]):Option[DialogFrame] = {
    par match {
      case Array(prjName,"N",DateConstant(day), StrToInt(hour)) =>
        //println("Create " + prjName + " date:" + day + " hour:" + hour)
        AllProjects.findProject(prjName) match {
          case Some(pr)=> openForCreate(pr,day,hour);Some(this)
          case None => Log.e("Could not find pr:"+prjName);None
        }

      case Array(prjName,"E", DateConstant(day), Reference(ref)) =>
        //println("Edit " + prjName + " day:" + day + " ref:" + ref)
        AllProjects.findProject(prjName) match {
          case Some(pr)=> pr.findEvent(ref) match {
            case Some(event)=>openForEdit(day,event);Some(this)
            case None => Log.e("Could not find Event ref:"+ref);None
          }
          case None => Log.e("Could not find pr:"+prjName);None
        }
      case _=> None
    }

  }

  def openForCreate(prj:DataSource,day:DateConstant,hour:Int): Unit ={
    //println("open for create "+prj.name+" "+day+" "+hour)
    currentProject=prj
    currentDay=day
    currentHour=hour
    infoLabel.innerHTML=dayStrings(currentDay.weekDay)+" "+currentDay+" | "+currentProject.name+" | "+(if(currentHour==0)"Aufgabe" else
      currentHour.toString+" Uhr")
    nameEdit.value=""
    placeEdit.value=""
    doneCheckBox.checked=false
    doneClicked=false
    deleteEventBut.style.visibility="hidden"
    nameEdit.focus()
    dom.document.title="Neuer Termin:"+AllProjects.currentSourceName+" "+day+" "+hour
    dom.window.history.pushState(currentProject.name+"|N|"+currentDay+"|"+hour,
      currentProject.name+" "+currentDay+" "+(if(hour==0)"Aufgabe" else hour.toString+" Uhr"),
      "?"+scala.scalajs.js.URIUtils.encodeURI(currentProject.name)+"?N?"+currentDay+"?"+hour)
  }

  def openForEdit(day:DateConstant,event:CalEvent): Unit ={
    //println("Open for edit "+day+" "+event.name)
    currentProject=null
    currentDay=day
    currentEvent=Some(event)
    infoLabel.innerHTML=dayStrings(currentDay.weekDay)+" "+currentDay+" | "+event.project.name+" | "+
      (if(event.time==0)"Aufgabe" else event.time.toString+" Uhr")
    nameEdit.value=event.name
    placeEdit.value=event.place
    doneCheckBox.checked=event.done
    doneClicked=false
    deleteEventBut.style.visibility="visible"
    nameEdit.focus()
    //val monday=currentDay.addDays(1-currentDay.weekDay)
    dom.document.title="Bearbeite "+event.name+" "+event.time.toString+" Uhr"
    dom.window.history.pushState(event.project.name+"|E|"+day.toString()+"|"+event.ref.bToString(),
      event.name+" "+currentDay+" "+(if(event.time==0)"Aufgabe" else event.time.toString+" Uhr"),
      "?"+scala.scalajs.js.URIUtils.encodeURI(event.project.name)+"?E?"+day.toString+"?"+event.ref.sToString())
  }


  def shutDown(): Unit = {
    nameEdit.value=""
    placeEdit.value=""
    currentEvent=None
    currentProject=null
    currentDay=null
    currentHour=0
    doneCheckBox.checked=false
    doneClicked=false
  }

  def save():Unit= if(currentProject!=null ) {
      currentProject.getOrCreateMonthRoot(currentDay.month+currentDay.year*100, (rootRef:Reference)=>{
        val nameText=nameEdit.value.trim
        //println("rootRef:"+rootRef+" nameText:"+nameText)
        if(nameText.length>0) WebSocketConnector.createInstance(151,Array(OwnerReference(0.toByte,rootRef)), (inst:Constant)=>{
          //println("create, inst "+inst)
          val nref=new Reference(151,inst.toInt)
          WebSocketConnector.writeInstanceField(nref,0,IntConstant(currentDay.day))
          WebSocketConnector.writeInstanceField(nref,1,StringConstant(nameText))
          WebSocketConnector.writeInstanceField(nref,2,IntConstant(currentHour))
          WebSocketConnector.writeInstanceField(nref,3,IntConstant(WebSocketConnector.userID))
          val placeText=placeEdit.value.trim
          //println("place: "+placeText)
          if(placeText.length>0) WebSocketConnector.writeInstanceField(nref,4,StringConstant(placeText))
          if(doneClicked && doneCheckBox.checked)  WebSocketConnector.writeInstanceField(nref,5,BoolConstant(true))
          close()
        })
        else close()
      })
    } else {
      currentEvent match {
        case Some(event) =>
          val nameText = nameEdit.value.trim
          if (nameText != event.name) WebSocketConnector.writeInstanceField(event.ref,1,StringConstant(nameText))
          val placeText= placeEdit.value.trim
          if(placeText != event.place) WebSocketConnector.writeInstanceField(event.ref,4,StringConstant(placeText))
          val doneChecked=doneCheckBox.checked
          if(doneChecked!=event.done) WebSocketConnector.writeInstanceField(event.ref,5,BoolConstant(doneChecked))
        case None => println("no currentEvent")
      }
      close()
    }

  def deleteEvent(): Unit = currentEvent match {
    case Some(event)=>
      WebSocketConnector.deleteInstance(event.ref)
      close()
    case _ =>

  }
}


