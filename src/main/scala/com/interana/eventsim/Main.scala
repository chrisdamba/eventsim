package com.interana.eventsim

import java.io.FileOutputStream

import com.interana.eventsim.Utilities.trackListenCount
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.rogach.scallop.{ScallopConf, ScallopOption}

import scala.collection.mutable

object Main extends App {
  private val sqrtE = Math.exp(0.5)

  def logNormalRandomValue = Math.exp(TimeUtilities.rng.nextGaussian()) / sqrtE

  val users = new mutable.PriorityQueue[User]()

  object Conf extends ScallopConf(args) {
    val nUsers: ScallopOption[Int] =
      opt[Int]("nusers", descr = "initial number of users",
        required = false, default = Option(1))

    val growthRate: ScallopOption[Double] =
      opt[Double]("growth-rate", descr = "annual user growth rate (as a fraction of current, so 1% => 0.01)",
        required = false, default = Option(0.0))

    val attritionRate: ScallopOption[Double] =
      opt[Double]("attrition-rate", descr = "annual user attrition rate (as a fraction of current, so 1% => 0.01)",
        required = false, default = Option(0.0))

    val startTimeArg: ScallopOption[String] =
      opt[String]("start-time", descr = "start time for data",
        required = false, default = Option(new DateTime().minusDays(14).toString(ISODateTimeFormat.dateTime()))
      )
    val endTimeArg: ScallopOption[String] =
      opt[String]("end-time", descr = "end time for data",
        required = false, default = Option(new DateTime().minusDays(7).toString(ISODateTimeFormat.dateTime()))
      )

    val from: ScallopOption[Int] =
      opt[Int]("from", descr = "from x days ago", required = false, default = Option(15))

    val to: ScallopOption[Int] =
      opt[Int]("to", descr = "to y days ago", required = false, default = Option(1))

    val firstUserId: ScallopOption[Int] =
      opt[Int]("userid", descr = "first user id", required = false, default = Option(1))

    val randomSeed: ScallopOption[Int] =
      opt[Int]("randomseed", descr = "random seed", required = false)

    val configFile: ScallopOption[String] =
      opt[String]("config", descr = "config file", required = true)

    val tag: ScallopOption[String] =
      opt[String]("tag", descr = "tag applied to each line", required = false)

    val verbose = toggle("verbose", default = Some(false), descrYes = "verbose output (not implemented yet)", descrNo = "silent mode")
    val outputFile: ScallopOption[String] = trailArg[String]("output-file", required = false, descr = "File name")

    val compute = toggle("compute", default = Some(false), descrYes = "compute listen counts then stop", descrNo = "run normally")

  }

  val startTime = if (Conf.startTimeArg.isSupplied) {
    new DateTime(Conf.startTimeArg())
  } else {
    new DateTime().minusDays(Conf.from())
  }

  val endTime = if (Conf.endTimeArg.isSupplied) {
    new DateTime(Conf.endTimeArg())
  } else {
    new DateTime().minusDays(Conf.to())
    new DateTime().minusDays(Conf.to())
  }

  SiteConfig.configFileLoader(Conf.configFile())

  var nUsers = Conf.nUsers()

  var seed = if (Conf.randomSeed.isSupplied) {
    Conf.randomSeed.get.get.toLong
  } else {
    SiteConfig.seed
  }

  def doStuff = {

    val out = if (Conf.outputFile.isSupplied) {
      new FileOutputStream(Conf.outputFile())
      //new PrintWriter(Conf.outputFile())
    } else {
      System.out
    }

    (0 until nUsers).foreach((_) =>
      users += new User(
        SiteConfig.alpha * logNormalRandomValue, // alpha = expected request inter-arrival time
        SiteConfig.beta * logNormalRandomValue, // beta = expected session inter-arrival time
        startTime, // start time
        SiteConfig.initialStates, // initial session states
        SiteConfig.authGenerator.randomThing,
        UserProperties.randomProps,
        DeviceProperties.randomProps,
        out
      ))

    if (Conf.growthRate() > 0) {
      var current = startTime
      while (current.isBefore(endTime)) {
        val mu = Constants.SECONDS_PER_YEAR / (nUsers * Conf.growthRate())
        current = current.plusSeconds(TimeUtilities.exponentialRandomValue(mu).toInt)
        users += new User(
          SiteConfig.alpha * logNormalRandomValue, // alpha = expected request inter-arrival time
          SiteConfig.beta * logNormalRandomValue, // beta = expected session inter-arrival time
          current, // start time
          SiteConfig.initialStates, // initial session states
          SiteConfig.newUserAuth,
          UserProperties.randomNewProps,
          DeviceProperties.randomProps,
          out
        )
        nUsers += 1
      }
    }
    System.err.println("Initial number of users: " + Conf.nUsers() + ", Final number of users: " + nUsers)



    val startTimeString = startTime.toString(ISODateTimeFormat.dateHourMinuteSecond())
    val endTimeString = endTime.toString(ISODateTimeFormat.dateHourMinuteSecond())

    var lastTimeStamp = System.currentTimeMillis()
    def showProgress(n: DateTime, users: Int, e: Int): Unit = {
      if ((e % 10000) == 0) {
        // val elapsed = new Interval(actualStartTime, new DateTime())
        val now = System.currentTimeMillis()
        val rate = 10000000 / (now - lastTimeStamp)
        lastTimeStamp = now
        var message = "Start: " + startTimeString + ", End: " + endTimeString +
          ", Now: " + n.toString(ISODateTimeFormat.dateHourMinuteSecond()) + ", Events:" + e + ", Rate: " + rate + " eps"
        System.err.write("\r".getBytes)
        System.err.write(message.getBytes)
      }
    }
    System.err.println("Starting to generate events.")
    System.err.println("Damping=" + SiteConfig.damping + ", Weekend-Damping=" + SiteConfig.weekendDamping)

    var clock = startTime
    var events = 1
    //val bins = scala.collection.mutable.HashMap[Long, Int]()

    while (clock.isBefore(endTime)) {
      //val bin = (clock.getMillis / 3600000L) * 3600L
      //if (clock.isAfter(startTime)) bins.put(bin, if (bins.contains(bin)) bins(bin) + 1 else 1)

      showProgress(clock, users.length, events)
      val u = users.dequeue()
      val prAttrition = nUsers * Conf.attritionRate() * (endTime.getMillis - startTime.getMillis / Constants.SECONDS_PER_YEAR)
      clock = u.session.nextEventTimeStamp.get

      //if (clock.isAfter(startTime)) out.println(u.eventString)
      if (clock.isAfter(startTime)) u.writeEvent
      u.nextEvent(prAttrition)
      users += u
      events += 1
    }
    System.err.println("")

    //bins.foreach((p: (Long, Int)) => println(p._1 + "," + p._2))
    System.err.println()

    out.flush()
    out.close()

  }

  if (Conf.compute())
    trackListenCount.compute
  else
    this.doStuff

}

