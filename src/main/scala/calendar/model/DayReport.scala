package calendar.model

import calendar.ui.{CalendarGrid, DialogFrame}
import clientbase.connection.WebSocketConnector
import definition.data.{InstanceData, OwnerReference, Referencable, Reference}
import definition.expression.{Constant, DateConstant, IntConstant, StringConstant}
import org.scalajs.dom
import org.scalajs.dom.html.{Div, Paragraph, TextArea}
import scalatags.JsDom.all._
import util.Log

/**
  * Created by Kathi on 07.01.2017.
  */
case class DayReport(data:InstanceData,project:DataSource) extends Referencable {
  def ref: Reference =data.ref
  def day: Int =data.fieldValue(0).toInt
  def weather: Int =data.fieldValue(1).toInt
  def works: String =data.fieldValue(2).toString
  def additWorks: String =data.fieldValue(3).toString
  def faults: String =data.fieldValue(4).toString
  def documents: String =data.fieldValue(5).toString
  def notes: String =data.fieldValue(6).toString
}

object ReportFrame extends DialogFrame {
  val labelClass="form-label"
  val wbuttonPressedClass="weather-button-pressed"
  val images=List(("sonne.gif",0,"Sonnig"),("wolke.gif",1,"Bewölkt"),("sturm.gif",2,"Stürmisch"),("Frost.gif",3,"Frost"),
    ("regen.gif",4,"Regen"),("mann.gif",5,"Bauleitung war vor Ort"))
  val wbuttons=images.map {case(file,index,text)=> button(title:=text,`class`:="weather-button",onclick :={()=>toggleButton(index)})(img(src:=file)).render}
  val wbuttonsPressed=new Array[Boolean](6)
  var currentReport:Option[DayReport]=None
  var currentProject:DataSource=null
  var currentDay:DateConstant=null
  val worksEdit: TextArea =textarea(`class`:="form-label-high",`type`:="text",name:="works").render
  val addWorksEdit: TextArea =textarea(`class`:="form-label-high",`type`:="text",name:="add works").render
  val faultsEdit: TextArea =textarea(`class`:="form-label-high",`type`:="text",name:="faults").render
  val documentsEdit: TextArea =textarea(`class`:="form-label-high",`type`:="text",name:="documents").render
  val notesEdit: TextArea =textarea(`class`:="form-label-high",`type`:="text",name:="works").render
  val infoLabel: Paragraph =p().render


  val frameDiv: Div =div(`class`:="dialog-frame") (
    div(`class`:=labelClass)(infoLabel),
    div(`class`:="button-line")(wbuttons),
    div(`class`:=labelClass)("Arbeiten:"),
    worksEdit,
    div(`class`:=labelClass)("Zusätzliche Arbeiten:"),
    addWorksEdit,
    div(`class`:=labelClass)("Mängel:"),
    faultsEdit,
    div(`class`:=labelClass)("Übergebene Dokumente:"),
    documentsEdit,
    div(`class`:=labelClass)("Notizen:"),
    notesEdit,
    div(`class`:="button-line") (
      button(`class`:="form-button",`type`:="button",onclick:={()=>save()})("Speichern"),
      button(`class`:="form-button",`type`:="button",onclick:={()=>dom.window.history.back()})("Abbruch")
    )
  ).render


  def shutDown():Unit= {
    currentReport=None
    currentProject=null
    currentDay=null
    worksEdit.value=""
    addWorksEdit.value=""
    faultsEdit.value=""
    documentsEdit.value=""
    notesEdit.value=""
    for(b<-0 until 6){
      wbuttonsPressed(b)=false
      wbuttons(b).classList.remove(wbuttonPressedClass)
    }
  }

  def save():Unit= if(currentProject!=null ) {
      currentProject.getOrCreateMonthRoot(currentDay.month+currentDay.year*100, (rootRef:Reference)=> {
        WebSocketConnector.createInstance(153, Array(OwnerReference(1.toByte, rootRef)), (inst: Constant) => {
          //println("create, inst "+inst)
          val nref = new Reference(153, inst.toInt)
          WebSocketConnector.writeInstanceField(nref, 0, IntConstant(currentDay.day))
          val weatherValue=weatherValueFromButtons
          if(weatherValue>0) WebSocketConnector.writeInstanceField(nref,1,IntConstant(weatherValue))
          if(worksEdit.value.trim.nonEmpty) WebSocketConnector.writeInstanceField(nref, 2, StringConstant(worksEdit.value))
          if(addWorksEdit.value.trim.nonEmpty) WebSocketConnector.writeInstanceField(nref, 3, StringConstant(addWorksEdit.value))
          if(faultsEdit.value.trim.nonEmpty) WebSocketConnector.writeInstanceField(nref, 4, StringConstant(faultsEdit.value))
          if(documentsEdit.value.trim.nonEmpty) WebSocketConnector.writeInstanceField(nref, 5, StringConstant(documentsEdit.value))
          if(notesEdit.value.trim.nonEmpty) WebSocketConnector.writeInstanceField(nref, 6, StringConstant(notesEdit.value))
          close()
        })
      })

    } else currentReport match {
    case Some(report)=>
      val worksText=worksEdit.value
      val addWorksText=addWorksEdit.value
      val faultsText=faultsEdit.value
      val documentsText=documentsEdit.value
      val notesText=notesEdit.value
      val weatherValue=weatherValueFromButtons
      if(weatherValue!=report.weather) WebSocketConnector.writeInstanceField(report.ref,1,IntConstant(weatherValue))
      if(worksText!=report.works) WebSocketConnector.writeInstanceField(report.ref,2,StringConstant(worksText))
      if(addWorksText!=report.additWorks) WebSocketConnector.writeInstanceField(report.ref,3,StringConstant(addWorksText))
      if(faultsText!=report.faults) WebSocketConnector.writeInstanceField(report.ref,4,StringConstant(faultsText))
      if(documentsText!=report.documents) WebSocketConnector.writeInstanceField(report.ref,5,StringConstant(documentsText))
      if(notesText!=report.notes) WebSocketConnector.writeInstanceField(report.ref,6,StringConstant(notesText))
      close()
    case None => close()
  }

  def openForEdit(day:DateConstant,report:DayReport): Unit = {
    currentProject=null
    currentDay=day
    infoLabel.innerHTML="<br>"+report.project.name+" | "+CalendarGrid.dayNames(day.weekDay)+" "+day+"<br"
    currentReport=Some(report)
    worksEdit.value=report.works
    addWorksEdit.value=report.additWorks
    faultsEdit.value=report.faults
    documentsEdit.value=report.documents
    notesEdit.value=report.notes
    for(i<-0 until 6) if(( (1<<i) & report.weather)>0){
      toggleButton(i)
    }
    worksEdit.focus()
    dom.document.title="Bearbeite Tagesbericht:"+AllProjects.currentSourceName+" "+day
    dom.window.history.pushState(report.project.name+"|ER|"+currentDay+"|"+report.ref.bToString(),
      report.project.name+" "+currentDay,
      "?"+scala.scalajs.js.URIUtils.encodeURI(report.project.name)+"?ER?"+currentDay+"?"+report.ref.bToString())
  }

  def openForCreate(project:DataSource,day:DateConstant): Unit = {
    currentProject=project
    currentDay=day
    infoLabel.innerHTML="<br>"+project.name+" | "+CalendarGrid.dayNames(day.weekDay)+" "+day+"<br"
    worksEdit.focus()
    dom.document.title="Neuer Tagesbericht:"+AllProjects.currentSourceName+" "+day
    dom.window.history.pushState(currentProject.name+"|NR|"+currentDay,
      currentProject.name+" "+currentDay,
      "?"+scala.scalajs.js.URIUtils.encodeURI(currentProject.name)+"?NR?"+currentDay)
  }

  def unapply(par:Array[String]):Option[DialogFrame] = {
    par match {
      case Array(prjName,"NR",DateConstant(day)) => {
        AllProjects.findProject(prjName) match {
          case Some(pr)=> openForCreate(pr,day);Some(this)
          case None => Log.e("Could not find pr:"+prjName);None
        }
      }
      case Array(prjName,"ER",DateConstant(day),Reference(ref)) => {
        AllProjects.findProject(prjName) match {
          case Some(pr)=> pr.findReport(ref) match {
            case Some(report)=>openForEdit(day,report);Some(this)
            case None => Log.e("Could not find Report ref:"+ref);None
          }
          case None => Log.e("Could not find pr:"+prjName);None
        }
      }
      case _=> None
    }
  }

  def weatherValueFromButtons:Int= {
    var result=0
    for(i<-0 until 6) if(wbuttonsPressed(i))result += 1 << i
    result
  }

  def toggleButton(index:Int):Unit= {
    wbuttonsPressed(index)= !wbuttonsPressed(index)
    if(wbuttonsPressed(index)) wbuttons(index).classList.add(wbuttonPressedClass)
    else wbuttons(index).classList.remove(wbuttonPressedClass)
  }

}
