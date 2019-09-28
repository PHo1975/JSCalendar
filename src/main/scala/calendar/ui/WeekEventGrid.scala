package calendar.ui

import calendar.control.GridController
import calendar.model._
import org.scalajs.dom
import org.scalajs.dom.html.{Button, Table}
import scalatags.JsDom.all._
import util.Log

import scala.collection.immutable.IndexedSeq

/**
  * Created by Kathi on 08.02.2016.
  */
class WeekEventGrid(eventModel:WeekEventModel) {
  val startTime=7
  val endTime=19
  def workRow: Int =endTime-startTime+1
  val reports= new Array[DayReport](6)
  val cellclass="week-table-cell"
  val cellReportClass="header-with-report"
  val cellNoReportClass= "report-button"
  val reportButtons: IndexedSeq[Button] =for(i<-0 until 6) yield button(`class`:="report-button",onclick:={ ()=>reportButtonClicked(i) })("B").render
  val gridTable: Table =table(`class`:="week-table")(tr(th("Berichte:"),for(i<-0 until 6; d=CalendarGrid.dayNames(i))yield th(`class`:="week-table-header")(span(d),reportButtons(i)))).render
  for(i<-startTime to endTime) gridTable.appendChild(tr(td(`class`:="week-table-hours")(i.toString+":00"),
    for (col <- 0 until 6) yield td(`class` := cellclass, onclick := { () => onEmptyTimeClick(col, i) })).render)
  gridTable.appendChild(tr(td(`class`:="week-table-hours")("Aufgaben:"),
    for (col <- 0 until 6) yield td(`class` := cellclass, onclick := { () => onEmptyTodoClick(col) })).render)


  def addWorkLine():Unit={
    gridTable.appendChild(tr(td(`class`:="week-table-hours")(""),
      for (col <- 0 until 6) yield td(`class` := cellclass, onclick := { () => onEmptyTodoClick(col) })).render)
  }

  def createEventCell(col:Int, event:CalEvent):dom.Element={
    val eventCell=div(CalendarGrid.refKey:=event.ref.bToString,CalendarGrid.langKey:="de",style:="background-color:#"+event.project.color)(event.name).render
    eventCell.addEventListener("click",(e:dom.Event)=>{onEventClick(col,event);e.stopPropagation()},useCapture = true)
    eventCell
  }

  def addEvent(col:Int, row:Int, event:CalEvent):Unit={
    val tableRow=gridTable.rows(row+1)
    tableRow.children(col+1).appendChild(createEventCell(col,event))
  }

  def addWork(col:Int, event:CalEvent,maxWork:Int):Unit= {
    val numWorkLines = gridTable.rows.length-workRow-1
    //println("add Work col:"+col+" event:"+event+" maxWork:"+maxWork+" numWorkLines:"+numWorkLines)
    var found=false
    for(rowIx<- 0 until numWorkLines;if !found ) {
      //if(rowIx+workRow+1>= gridTable.rows.length ) {println("rowIX to high "+rowIx+" wl: "+numWorkLines+" "+gridTable.rows.length);addWorkLine() }
      val row=gridTable.rows(rowIx+workRow+1)
      if(row==null) println("Row == null")
      val rootCell=row.children(col+1)
      if(rootCell.childElementCount==0){ // empty cell
        rootCell.appendChild(createEventCell(col,event))
        if(numWorkLines<maxWork+1) addWorkLine()
        found=true
      }
    }
  }

  def onEmptyTimeClick(col:Int,time:Int):Unit= {
    //println("Empty Time Click "+eventModel.monday.addDays(col)+" time:"+time)
    //val row=time-startTime
    AllProjects.currentSource match {
      case Some(prj)=>
        GridController.showDialog(EventFrame)
        EventFrame.openForCreate(prj,eventModel.monday.addDays(col),time)
      case _=>
    }
  }

  def onEmptyTodoClick(col:Int):Unit = {
   //println("empty todo click "+eventModel.monday.addDays(col))
    AllProjects.currentSource match {
      case Some(prj)=>
        GridController.showDialog(EventFrame)
        EventFrame.openForCreate(prj,eventModel.monday.addDays(col),0)
      case _=>
    }
  }

  def onEventClick(col:Int,event:CalEvent):Unit={
    //println("Event "+eventModel.monday.addDays(col)+ (if(row<12) " time:"+(startTime-1+row) else "todo ")+event.name+" pr:"+event.project)
    GridController.showDialog(EventFrame)
    EventFrame.openForEdit(eventModel.monday.addDays(col),event)
  }


  def removeEventFromCell(col:Int, row:Int, event:CalEvent):Unit={
    //println("removeFromCell "+col+" "+row+" "+event)
    //if(row>=workRow|| row<0) println("remove cell r:"+row+" col:"+col+" event:"+event+" row to large")
    val rootCell=gridTable.rows(row+1).children(col+1)
    val size=rootCell.childElementCount
    val refText=event.ref.bToString()
    //println("removeFromCell c:"+col+" r:"+row+" e:"+event+" size:"+size)
    for(i<-0 until size;el = rootCell.children(i))
      if (el.hasAttribute("ref") && el.getAttribute("ref") == refText){
        rootCell.removeChild(el)
      }

  }


  def removeWork(col:Int, event:CalEvent,maxWork:Int):Unit = {
    val numWorkLines = gridTable.rows.length-workRow-1
    var found=false
    //println("removeWork col:"+col+" event:"+event+" maxWork:"+maxWork+" nwl:"+numWorkLines)
    for(rowIx<- 0 until numWorkLines;if !found) {
      //if(rowIx + workRow>=gridTable.rows.length) println("wrong row ix "+(rowIx + workRow)+" size:"+gridTable.rows.length)
      val row = gridTable.rows(rowIx + workRow+1)
      if(row==null) println("Row == null  rowIx:"+rowIx+" numWorkLines:"+numWorkLines)
      val rootCell = row.children(col + 1)
      val refText=event.ref.bToString()
      if(rootCell.childElementCount>0 ) {
        val el=rootCell.children(0)
        if (el.hasAttribute("ref") && el.getAttribute("ref") == refText) {
          rootCell.removeChild(el)
          found=true
          if(numWorkLines>maxWork+1) gridTable.removeChild(gridTable.lastChild)
        }
      }
    }
  }

  def setReport(report:DayReport,col:Int,on:Boolean):Unit = {
    val cell=gridTable.rows(0).children(col+1).children(1)
    if(cell==null) Log.e("set Report cell=null col:"+col+" on:"+on)
    //cell.innerHTML=dayNames(col)+(if(on)" (B)" else "")

    val classes=cell.classList
    if(on) {
      if(classes.contains(cellNoReportClass)) classes.remove (cellNoReportClass)
      if(!classes.contains(cellReportClass)) classes.add(cellReportClass)
      reports(col)=report
    }
    else {
      if(classes.contains(cellReportClass)) classes.remove(cellReportClass)
      if(! classes.contains(cellNoReportClass)) classes.add(cellNoReportClass)
      reports(col)=null
    }
  }

  def removeReports(): Unit ={
    //println("remove Reports")
    for(i<-0 to 5) {
      setReport(null,i,on=false)
      reports(i)=null
    }
  }


  def reportButtonClicked(col:Int):Unit= {
    //println("reportButtonClicked col:"+col)
    val day=eventModel.monday.addDays(col)
    AllProjects.currentSource match {
      case Some(prj)=>
        GridController.showDialog(ReportFrame)
        reports(col) match {
          case null => ReportFrame.openForCreate(prj,day)
          case report=> ReportFrame.openForEdit(day,report)
        }
      case _=>
    }

  }


}
