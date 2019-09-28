package calendar.ui

import calendar.model.EventModel
import definition.expression.DateConstant
import definition.typ.SystemSettings
import org.scalajs.dom.html.{Button, Div, Paragraph, Table, TableRow}
import org.scalajs.dom.raw.{HTMLElement, Node}
import scalatags.JsDom.all._
import util.Log

/**
  * Created by Kathi on 06.02.2016.
  */
class CalendarGrid  {
  val monthNames=Array("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")
  val headerNames=Array("KW.","Mo","Di","Mi","Do","Fr","Sa","So")
  var currentMonth:Int=1
  var currentYear:Int=2010
  val monthLabel: Paragraph =p(`class`:="month-label").render
  val leftButton: Button =button(`class`:="month-back",onclick:={ ()=>decreaseMonth()})("<").render
  val rightButton: Button =button(`class`:="month-next",onclick:={ ()=>increaseMonth()})(">").render
  val headerRow: Div =div(`class`:="grid-header")(leftButton,rightButton,monthLabel).render
  val thclass="cal-grid-header"
  val rowclass="cal-grid-row"
  val cellclass="cal-grid-cell"
  val sundayclass=" cal-grid-sunday"
  val todayclass=" cal-grid-today"
  val selRowclass="cal-grid-sel-row"
  val tabHeaders: TableRow =tr(for(h<-headerNames)yield td(`class`:=thclass)(h)).render
  val gridTable: Table =table(`class`:="cal-grid-table")(tr(for(t<-headerNames) yield th(`class`:=thclass)(t))).render
  var holidays:collection.immutable.Map[Int,String]=Map.empty

  var selectedWeek:Option[Int]=None
  var firstDayColumn:Int=_
  var firstJulian:Long=_
  var model:EventModel=_


  def appendToParent(parent:HTMLElement): Node ={
    parent.appendChild(headerRow)
    parent.appendChild(gridTable)
  }

  def changeMonth(newMonth:Int,newYear:Int): Unit = {
    holidays=SystemSettings().getHolidays(newYear).
      filter{case (dateConst,_)=>dateConst.month==newMonth}.
      map{case (dateConst,hname)=>dateConst.day->hname}
    val today=DateConstant()
    val firstDay=new DateConstant(1,newMonth,newYear)
    val todayDay=if(today.month==newMonth&&today.year==newYear) today.day else -1
    firstDayColumn=firstDay.weekDay
    firstJulian=firstDay.julian
    val numDays=DateConstant.getDaysInMonth(newMonth,newYear)
    val firstWeek=firstDay.weekOfYear
    val lastDay=firstDay.addDays(numDays-1)
    val lastDayColumn=lastDay.weekDay
    val lastWeek=lastDay.weekOfYear
    val betweenWeek= if(firstWeek>lastWeek) {
      newMonth match {
        case 1=> firstDay.addDays(0).weekOfYear
        case 12=>firstDay.addDays(numDays-8).weekOfYear
        case _=> Log.e("Error in Date Calculation fd:"+firstDay+" m:"+newMonth+" y:"+newYear  );0
      }
    } else 0
    val zw=betweenWeek>0
    val lw=if(zw) betweenWeek+lastWeek else lastWeek
    while(gridTable.children.length>1)
      gridTable.removeChild(gridTable.lastChild)

    def createRow(week:Int,days:Array[Int])={
      val firstDay=days(0)
      gridTable.appendChild(
        tr(`class`:=rowclass,onclick:={()=>selectWeek(firstJulian+firstDay-1,doLoad = true)} ) (td(`class`:=thclass)(week.toString),
          for(i<-days.indices;t=days(i)) yield
            td(`class`:=(if(t!=0)cellclass else "")+(if(t==todayDay) todayclass else if(i==6|| holidays.contains(t))sundayclass else ""),`title`:=(if(holidays.contains(t))holidays(t) else ""),
              CalendarGrid.dayKey:=(if(t!=0)(firstJulian+t-1).toString else ""))
            (if(t==0)"" else t.toString)).render)
    }
    val daysArray=Array.fill[Int](7)(0)

    for(week<-firstWeek to lw) {
      val weekNumber = if (zw && week > betweenWeek) week - betweenWeek else week
      if(weekNumber==firstWeek) {
        for(col<-firstDayColumn to 6)
          daysArray(col)=col-firstDayColumn+1
      } else if(weekNumber == lw) {
        for(col<-0 to lastDayColumn)
          daysArray(col)= numDays-lastDayColumn+col
        for(col <-lastDayColumn+1 to 6)
          daysArray(col)=0
      } else {
        val fd=(week-firstWeek)*7+1-firstDayColumn
        for(col<-0 to 6)
          daysArray(col)=fd+col
      }
      createRow(weekNumber,daysArray)
    }
    currentMonth=newMonth
    currentYear=newYear
    monthLabel.innerHTML=monthNames(currentMonth-1)+" "+currentYear
    model.clear()
  }

  def increaseMonth(): Unit = {
    selectedWeek=None
    if (currentMonth == 12) changeMonth(1, currentYear + 1)
    else changeMonth(currentMonth + 1, currentYear)
  }

  def decreaseMonth(): Unit = {
    selectedWeek=None
    if (currentMonth == 1) changeMonth(12, currentYear - 1)
    else changeMonth(currentMonth - 1, currentYear)
  }

  def selectWeek(julian:Long,doLoad:Boolean,doPushState:Boolean=true): Unit ={
    val firstMonday=firstJulian-firstDayColumn
    val weekNr=(julian-firstMonday)/7
    val monday=DateConstant.asJulian(firstMonday+weekNr*7)
    val sunday=DateConstant.asJulian(firstMonday+weekNr*7+6)
    for(s<-selectedWeek) gridTable.rows.item(s+1).classList.remove("cal-grid-sel-row")
    selectedWeek=Some(weekNr.toInt)
    gridTable.rows.item(weekNr.toInt+1).classList.add("cal-grid-sel-row")
    if(model!=null) model.setDate(monday,sunday,doLoad,doPushState)
  }
}

object CalendarGrid {
  val dayKey: Attr = attr("day")
  val refKey: Attr = attr("ref")
  val langKey: Attr = attr("lang")
  //val titleKey:Attr = attr("title")
  val projectColors=Array("FfeaEd","FFDbE7","EBDFFF","EfDFF2","DFE2FF","DFE7F2","DFEEF2","DFFFF2","DFF2EA","ECFFDF","F0F2DF","FFF0DF","F2EBDF","F4F6F8")
  val ownName="Eigene Termine"
  val allProjectName="Alle Projekte"
  val dayNames=Array("Mo","Di","Mi","Do","Fr","Sa")
}
