package calendar.model

import calendar.ui.WeekEventGrid
import definition.data.Reference
import definition.expression.DateConstant
import org.scalajs.dom

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Created by Kathi on 08.02.2016.
  */
trait EventModel {
  def addEvent(yearMonth:Int, event:CalEvent):Unit
  def removeEvent(yearMonth:Int, event:CalEvent):Unit
  def addReport(yearMonth:Int,report:DayReport):Unit
  def removeReport(yearMonth:Int,report:DayReport):Unit
  def clear():Unit
  def setDate(monday:DateConstant,sunday:DateConstant,doLoad:Boolean,pushState:Boolean):Unit
  def pushState():Unit
}


class WeekEventModel extends EventModel{

  var monday:DateConstant=DateConstant.NULL_DATE
  var sunday:DateConstant=DateConstant.NULL_DATE
  val dayEventList: Array[ArrayBuffer[CalEvent]] =Array.fill[ArrayBuffer[CalEvent]](6)(new ArrayBuffer[CalEvent])
  val dayWorkList: Array[ArrayBuffer[CalEvent]] =Array.fill[ArrayBuffer[CalEvent]](6)(new ArrayBuffer[CalEvent])
  val eventMap: mutable.HashMap[Reference, Int] =collection.mutable.HashMap[Reference,Int]()
  var weekGrid:WeekEventGrid=_

  def setDate(nmonday:DateConstant,nsunday:DateConstant,doLoad:Boolean,doPushState:Boolean=true):Unit={
    clear()
    monday=nmonday
    sunday=nsunday
    //println("set Date "+monday+" "+sunday+" p:"+doPushState)
    AllProjects.loadTime(monday.year*100+monday.month,if(sunday.month!=monday.month) Some(sunday.year*100+sunday.month) else None,doLoad)
    if(doPushState) pushState()
  }


  def toJulian(yearMonth:Int,day:Int):Long= DateConstant.julian(day,yearMonth%100,yearMonth/100)


  def fitsSpan(yearMonth:Int,event:CalEvent):Boolean={
    val julian=toJulian(yearMonth,event.day)
    julian>=monday.julian&& julian<=monday.julian+6
  }

  def maxWorkNum:Int=dayWorkList.maxBy(_.length).length

  def addEvent(yearMonth: Int, event: CalEvent): Unit = if(fitsSpan(yearMonth,event)){
    intRemoveEvent(event)
    val day=toJulian(yearMonth,event.day)
    val col=(day-monday.julian).toInt
    if(col<0||col>5)println("add data ym:"+yearMonth+" e:"+event.day+" julian:"+day+"mondayjul:"+monday.julian+" col:"+col)
    else {
      eventMap(event.ref) = col
      if (event.time == 0) {
        dayWorkList(col) += event
        weekGrid.addWork(col, event, maxWorkNum)
      } else {
        weekGrid.addEvent(col, event.time - weekGrid.startTime, event)
        dayEventList(col) += event
      }
    }
  }

  private def intRemoveEvent(event:CalEvent):Unit=
    for( oldCol<-eventMap.get(event.ref)){
      eventMap.remove(event.ref)
      if (event.time==0) {
        dayWorkList(oldCol).indexWhere(_.ref==event.ref) match {
          case -1 => println("Remove work, not found:"+event)
          case ix => dayWorkList(oldCol).remove(ix)
        }
        weekGrid.removeWork(oldCol,event,maxWorkNum)
      }
      else dayEventList(oldCol).indexWhere(_.ref==event.ref) match {
        case -1 => println("Remove event, not found:" + event)
        case ix => weekGrid.removeEventFromCell(oldCol, event.time - weekGrid.startTime, event)
            dayEventList(oldCol).remove(ix)
      }
    }


  def removeEvent(yearMonth: Int, event: CalEvent): Unit = if(fitsSpan(yearMonth,event)) intRemoveEvent(event)


  def clear(): Unit = {
    for(col<-dayEventList.indices; dayBuffer=dayEventList(col)) {
      for(ev<-dayBuffer) weekGrid.removeEventFromCell(col,ev.time-weekGrid.startTime,ev)
      dayBuffer.clear()
    }
    for(col<-dayWorkList.indices; dayBuffer=dayWorkList(col)){
      for(ev<-dayBuffer) weekGrid.removeWork(col,ev,0)
      dayBuffer.clear()
    }
    eventMap.clear()
    weekGrid.removeReports()
  }

  def addReport(yearMonth:Int,report:DayReport):Unit = {
    val day=toJulian(yearMonth,report.day)
    val col=(day-monday.julian).toInt
    if(col>=0 && col <6) weekGrid.setReport(report,col,on=true)
  }

  def removeReport(yearMonth:Int,report:DayReport):Unit = {
    val day=toJulian(yearMonth,report.day)
    val col=(day-monday.julian).toInt
    if(col>=0 && col <6) weekGrid.setReport(null,col,on=false)
  }

  override def pushState():Unit= {
    //println("Model pushState")
    dom.document.title=AllProjects.currentSourceName+" "+monday.toString
    dom.window.history.pushState(AllProjects.currentSourceName+"|"+monday.toString(),AllProjects.currentSourceName+" "+monday.toString,
      "?"+scala.scalajs.js.URIUtils.encodeURI(AllProjects.currentSourceName)+"?"+monday.toString)
  }
}
