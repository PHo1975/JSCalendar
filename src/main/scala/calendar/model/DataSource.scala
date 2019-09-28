package calendar.model

import java.io.DataInput

import calendar.ui.CalendarGrid
import clientbase.connection.{InstSubscriber, Subscriber, WebSocketConnector}
import clientbase.localstore.{Loader, SubsArrayList, SubsMap}
import definition.data.{InstanceData, OwnerReference, Referencable, Reference}
import definition.expression.IntConstant
import util.Log


/**
  * Created by Peter on 07.02.2016.
  */

case class CalEvent(data:InstanceData,project:DataSource) extends Referencable {
  def ref: Reference =data.ref
  def day: Int =data.fieldValue(0).toInt
  def name: String =data.fieldValue(1).toString
  def time: Int =data.fieldValue(2).toInt
  def user: Int =data.fieldValue(3).toInt
  def place: String =data.fieldValue(4).toString
  def done: Boolean =data.fieldValue(5).toBoolean
  override def toString: String ="Event day:"+day+" name:"+name+" time:"+time+" place:"+place
}

class MonthCalList(source:DataSource,isFirstMonth:Boolean) extends InstSubscriber with SubsArrayList[CalEvent]  {
  def dataFactory(data: InstanceData): CalEvent = CalEvent(data,source)
  override def updateElement(el:CalEvent): Unit = for(m<-AllProjects.month(isFirstMonth)) AllProjects.addEvent(m,el)
  override def destructor(el:CalEvent): Unit = for(m<-AllProjects.month(isFirstMonth)) AllProjects.removeEvent(m,el)
  override def update(list:Iterable[CalEvent]): Unit = for(m<-AllProjects.month(isFirstMonth); el<-list) AllProjects.addEvent(m,el)
}

class MonthReportList(source:DataSource,isFirstMonth:Boolean) extends InstSubscriber with SubsArrayList[DayReport]   {
  def dataFactory(data: InstanceData): DayReport = DayReport(data,source)
  override def updateElement(el:DayReport): Unit = for(m<-AllProjects.month(isFirstMonth)) AllProjects.addReport(m,el)
  override def destructor(el:DayReport): Unit = for(m<-AllProjects.month(isFirstMonth)) AllProjects.removeReport(m,el)
  override def update(list:Iterable[DayReport]): Unit = for(m<-AllProjects.month(isFirstMonth); el<-list) AllProjects.addReport(m,el)
}

case class MonthRef(ref:Reference, month:Int) extends Referencable

trait DataSource {
  protected var newMonthData:Option[Reference]=None

  val monthRoots=new SubsMap[MonthRef] {
    override def factory(in:DataInput): MonthRef ={
      val inst=InstanceData.readWithChildInfo(Reference(in),in)
      MonthRef(inst.ref,inst.fieldValue.head.toInt)
    }
    override def  onChildAdded(data: MonthRef): Unit ={
      super.onChildAdded(data)
      if(data.month==0)
      //println("new MonthRoot "+name+" "+AllProjects.firstMonth)
      newMonthData=Some(data.ref)
    }

    def update(it:Iterator[MonthRef]):Unit={}

    override def onChange(data:MonthRef):Unit = {
      super.onChange(data)
      if(firstMonthEvents.subsID== -1 && // not loaded
        newMonthData.isDefined && data.ref==newMonthData.get){ // new Data is set from 0 to monthCount
          newMonthData=None
          val month=data.month
          //println("reload after new MonthRoot, month:"+ month)
          if(month==AllProjects.firstMonth||(AllProjects.secondMonth.isDefined&&month==AllProjects.secondMonth.get))
            loadMonths(AllProjects.firstMonth,AllProjects.secondMonth,()=>{})
      }
    }
  }
  val firstMonthEvents=new MonthCalList(DataSource.this,isFirstMonth=true)
  val secondMonthEvents=new MonthCalList(DataSource.this,isFirstMonth=false)
  val firstMonthReports=new MonthReportList(DataSource.this,isFirstMonth=true)
  val secondMonthReports=new MonthReportList(DataSource.this,isFirstMonth=false)

  def name:String
  def eventRef:Reference
  def nr:Int
  def color:String
  def propertyField:Byte=0

  def loadMonths(firstMonth:Int,secondMonth:Option[Int],callBack:()=>Unit):Unit={
    shutDown()
    //println("Load Months "+name+" fm:"+firstMonth+" sm:"+secondMonth+" all:"+AllProjects.currentSource.isDefined)

    def loadSecond():Unit={
        //println("load second "+name+" fm:"+firstMonth+" sm:"+secondMonth)
        secondMonth match {
          case Some(month)=> monthRoots.map.values.find(_.month==month) match {
            case Some(data)=>secondMonthEvents.load(data.ref,0,()=>if(AllProjects.currentSource.isDefined) secondMonthReports.load(data.ref,1,callBack) else callBack())
            case None=> callBack()
          }
          case None =>callBack()
        }
    }

    monthRoots.map.values.find(_.month==firstMonth) match {
      case Some(data)=>firstMonthEvents.load(data.ref,0,()=>if(AllProjects.currentSource.isDefined) firstMonthReports.load(data.ref,1,()=>loadSecond()) else loadSecond())
      case None => loadSecond()
    }

  }


  def shutDown(): Unit ={
    firstMonthEvents.unsubscribe()
    secondMonthEvents.unsubscribe()
    firstMonthReports.unsubscribe()
    secondMonthReports.unsubscribe()
  }

  def getOrCreateMonthRoot(monthYear:Int, callback:Reference=>Unit): Unit = if(!monthRoots.map.values.exists(_.month==monthYear)){
    //println("load Month Root my:"+monthYear+" name:"+name)
    WebSocketConnector.createInstance(150,Array(new OwnerReference(propertyField,eventRef)),(inst)=>{
      val nref=new Reference(150,inst.toInt)
      WebSocketConnector.writeInstanceField(nref,0,IntConstant(monthYear))
      callback(nref)
    })
  } else monthRoots.map.values.find(_.month==monthYear) match{
    case Some(resultMonth)=>callback(resultMonth.ref)
    case None=> Log.e("month not found "+monthYear)
  }

  override def toString: String =nr.toString+" "+name+" "+eventRef

  def load(callBack:()=>Unit): Unit = monthRoots.load(eventRef,propertyField,callBack)

  def findEvent(evRef:Reference):Option[CalEvent]=
    firstMonthEvents.list.find(_.ref == evRef) match {
      case ev: Some[CalEvent] => ev
      case None => secondMonthEvents.list.find(_.ref == evRef)
    }

  def findReport(evRef:Reference):Option[DayReport]=
    firstMonthReports.list.find(_.ref == evRef) match {
      case ev: Some[DayReport] => ev
      case None => secondMonthReports.list.find(_.ref == evRef)
    }


}



class Project(val nr:Int,val ref:Reference,val name:String,addressFolder:Reference,val eventRef:Reference) extends DataSource with Loader {
  def color: String =CalendarGrid.projectColors(nr % 14)
}



class OwnProject(val nr:Int,val eventRef:Reference) extends DataSource{
  val name: String =CalendarGrid.ownName
  val color="ffffff"
  override def propertyField: Byte =1.toByte
}



object AllProjects extends {
  var ownProject:OwnProject= _
  var currentSource:Option[DataSource]=None
  var projectList:Seq[Project]=Nil
  var model:Option[EventModel]=None
  protected[model] var firstMonth:Int=0
  protected[model] var someFirstMonth:Some[Int]=Some(0)
  protected[model] var secondMonth:Option[Int]=None

  def createOwnProject(ownRootRef:Reference):Unit=
    ownProject=new OwnProject(0,ownRootRef)

  def month(isFirstMonth:Boolean):Option[Int]=if(isFirstMonth)someFirstMonth else secondMonth

  def loadMonthTables(readyCallBack:(Seq[String])=>Unit):Unit={
    //println("Load MonthTables ")
    WebSocketConnector.registerCalendarReceiver(in=>{
      //println("Calreceiver callback")
      val numProjects=in.readInt
      projectList=for(i<-0 until numProjects) yield {
        val inst=InstanceData.readWithChildInfo(Reference(in),in)
        new Project(i+1,inst.ref,inst.fieldValue.head.toString.trim,Reference(in),Reference(in))
      }
      //println("ProjectList loaded. Now loading Projects")
      Loader.cascadedLoad(projectList,()=> ownProject.load(()=>  {
        //println("cascadedLoad callback")
        readyCallBack( projectList.map(_.name) )} ) )
    })
  }

  def shutDown(): Unit = {
    //println("allprojects shutdown currentSource:"+currentSource)
    currentSource match {
      case Some(dataSource) => dataSource.shutDown()
      case None =>
        ownProject.shutDown()
        for (p <- projectList) p.shutDown()
    }
  }

  def loadTime(nfirstMonth:Int,nsecondMonth:Option[Int],doLoad:Boolean): Unit = /*if(firstMonth!=nfirstMonth && secondMonth!=nsecondMonth)*/ {
    shutDown()
    //println("Load time "+nfirstMonth+" "+doLoad)
    firstMonth=nfirstMonth
    someFirstMonth=Some(firstMonth)
    secondMonth=nsecondMonth
    if(doLoad)currentSource match {
      case Some(dataSource)=> dataSource.loadMonths(firstMonth,secondMonth,Subscriber.doNothing)
      case None => loadAllProjects(Subscriber.doNothing)
    }
  }

  protected def loadAllProjects(doneListener:()=>Unit): Unit = {
    val numProjects=projectList.size
    //println("loadAllProjects "+numProjects)
    var current= -1

    def doNext():Unit={
      current+=1
      if(current<numProjects)
        projectList(current).loadMonths(firstMonth,secondMonth,doNext _)
      else doneListener()
    }

    ownProject.loadMonths(firstMonth,secondMonth,doNext _)
  }

  def setCurrentSource(newSource:Option[DataSource]):Unit={
    shutDown()
    //println("setCurrentSource:"+newSource+" size:"+projectList.size)
    currentSource=newSource
    for(m<-model){
      m.clear()
      m.pushState()
    }
    newSource match {
      case Some(dataSource)=>dataSource.loadMonths(firstMonth,secondMonth,Subscriber.doNothing)
      case None => loadAllProjects(Subscriber.doNothing)
    }

  }

  def currentSourceName: String =currentSource match {
    case Some(source) => source.name
    case None => ""
  }

  def addEvent(month:Int,event:CalEvent): Unit =
    for (m <- model) m.addEvent(month, event)

  def removeEvent(month:Int,event:CalEvent): Unit =
    for(m<-model)m.removeEvent(month,event)

  def addReport(month:Int,Report:DayReport): Unit =
    for(m<-model)m.addReport(month,Report)

  def removeReport(month:Int,Report:DayReport): Unit =
    for(m<-model)m.removeReport(month,Report)
  

  def selectProject(name:String):Unit= {
    //println("select Project "+name)
    if (name == CalendarGrid.allProjectName|| name.length==0) setCurrentSource(None)
    else if (name == CalendarGrid.ownName) setCurrentSource(Some(ownProject))
    else
      projectList.find(_.name == name) match {
      case pr:Some[Project]=> setCurrentSource(pr)
      case None => Log.e("Unknown project name:"+name)
    }
  }

  def getSelectNr(name:String):Int= {
    if (name == CalendarGrid.allProjectName|| name.length==0) return 0
    else if (name == CalendarGrid.ownName) return 1
    else
      for (p <- projectList.find(_.name == name)) return p.nr
    -1
  }

  def findProject(prName:String):Option[DataSource]={
    //println("Find project "+prName)
    if(prName==CalendarGrid.ownName) Some(ownProject)
    else if(prName==CalendarGrid.allProjectName) None
    else projectList.find(_.name== prName)
  }


}


