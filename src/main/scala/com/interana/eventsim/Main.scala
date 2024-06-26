package com.interana.eventsim

import java.io.{FileOutputStream,OutputStream}
import java.time.temporal.ChronoUnit
import java.time.{Duration, LocalDateTime, ZoneOffset}
import java.util.Properties

import com.interana.eventsim.Utilities.{SimilarSongParser, TrackListenCount}
import com.interana.eventsim.buildin.{DeviceProperties, UserProperties, RandomVideoContentGenerator}
import com.interana.eventsim.config.ConfigFromFile
import kafka.producer.{Producer, ProducerConfig}
import org.rogach.scallop.{ScallopOption, ScallopConf}

import scala.collection.mutable

object Main extends App {
  private val sqrtE = Math.exp(0.5)

  def logNormalRandomValue = Math.exp(TimeUtilities.rng.nextGaussian()) / sqrtE

  val users = new mutable.PriorityQueue[User]()

  object ConfFromOptions extends ScallopConf(args) {
    val simulateVideo = toggle("simulate-video", default = Some(false),
      descrYes = "simulate video streaming events", descrNo = "simulate music streaming events")

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
        required = false, default = Option(LocalDateTime.now().minus(14, ChronoUnit.DAYS).toString))

    val endTimeArg: ScallopOption[String] =
      opt[String]("end-time", descr = "end time for data",
        required = false, default = Option(LocalDateTime.now().minus(7, ChronoUnit.DAYS).toString))

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
      opt[String]("tag", descr = "tag applied to each line (for example, A/B test group)", required = false)

    val verbose = toggle("verbose", default = Some(false),
      descrYes = "verbose output (not implemented yet)", descrNo = "silent mode")
    val outputFile: ScallopOption[String] = trailArg[String]("output-file", required = false, descr = "File name")

    val kafkaTopic: ScallopOption[String] =
      opt[String]("kafkaTopic", descr = "kafka topic", required = false)

    val kafkaBrokerList: ScallopOption[String] =
      opt[String]("kafkaBrokerList", descr = "kafka broker list", required = false)

    val generateCounts = toggle("generate-counts", default = Some(false),
      descrYes = "generate listen counts file then stop", descrNo = "run normally")

    val generateSimilarSongs = toggle("generate-similars", default = Some(false),
      descrYes = "generate similar song file then stop", descrNo = "run normally")

    val realTime = toggle("continuous", default = Some(false),
      descrYes = "continuous output", descrNo = "run all at once")

  }

  val startTime = if (ConfFromOptions.startTimeArg.isSupplied) {
    LocalDateTime.parse(ConfFromOptions.startTimeArg())
  } else if (ConfigFromFile.startDate.nonEmpty) {
    LocalDateTime.parse(ConfigFromFile.startDate.get)
  } else {
    LocalDateTime.now().minus(ConfFromOptions.from(), ChronoUnit.DAYS)
  }

  val endTime = if (ConfFromOptions.endTimeArg.isSupplied) {
    LocalDateTime.parse(ConfFromOptions.endTimeArg())
  } else if (ConfigFromFile.endDate.nonEmpty) {
    LocalDateTime.parse(ConfigFromFile.endDate.get)
  } else {
    LocalDateTime.now().minus(ConfFromOptions.to(), ChronoUnit.DAYS)
  }

  ConfigFromFile.configFileLoader(ConfFromOptions.configFile())

  val simulateVideo = ConfFromOptions.simulateVideo()
  var nUsers = ConfigFromFile.nUsers.getOrElse(ConfFromOptions.nUsers())

  val seed = if (ConfFromOptions.randomSeed.isSupplied)
    ConfFromOptions.randomSeed.get.get.toLong
   else
    ConfigFromFile.seed


  val tag = if (ConfFromOptions.tag.isSupplied)
    ConfFromOptions.tag.get
  else
    ConfigFromFile.tag

  val growthRate = if (ConfFromOptions.growthRate.isSupplied)
    ConfFromOptions.growthRate.get
  else
    ConfigFromFile.growthRate

  val kafkaProducer = if (ConfFromOptions.kafkaBrokerList.isDefined) {
    val kafkaProperties = new Properties()
    kafkaProperties.setProperty("metadata.broker.list", ConfFromOptions.kafkaBrokerList.get.get)
    val producerConfig = new ProducerConfig(kafkaProperties)
    new Some(new Producer[Array[Byte],Array[Byte]](producerConfig))
  } else None

  val realTime = ConfFromOptions.realTime.get.get

  def determineOutputDestination(conf: ConfFromOptions.type, kafkaProducer: Option[Producer[Array[Byte], Array[Byte]]]) = {
    if (kafkaProducer.nonEmpty) {
      new KafkaOutputStream(kafkaProducer.get, conf.kafkaTopic.get.get)
    } else if (conf.outputFile.isSupplied) {
      new FileOutputStream(conf.outputFile())
    } else {
      System.out
    }
  }

  // Define a random generator for viewing hours, assuming 0-40 hours per week
  def randomViewingHours: Int = scala.util.Random.nextInt(41)

  // Randomly select a subscription type if it's an enum with values like Basic or Premium
  def randomSubscriptionType: SubscriptionType.SubscriptionType = {
    if (scala.util.Random.nextBoolean()) SubscriptionType.Basic else SubscriptionType.Premium
  }

  def initializeUsers(simulateVideo: Boolean, out: OutputStream, nUsers: Int, startTime: LocalDateTime) {
    val userGenerator = if (simulateVideo) {
      // Fetch random genres and shows
      val preferredGenres = RandomVideoContentGenerator.randomGenres(3)  // Generate a list of 3 random genres
      val favoriteShows = RandomVideoContentGenerator.randomShows(3)  // Generate a list of 3 random TV shows
      () => new User(
        ConfigFromFile.alpha * logNormalRandomValue,
        ConfigFromFile.beta * logNormalRandomValue,
        startTime,
        ConfigFromFile.initialStates,
        ConfigFromFile.authGenerator.randomThing,
        UserProperties.randomProps,
        DeviceProperties.randomProps,
        ConfigFromFile.levelGenerator.randomThing,
        out,
        preferredGenres,
        favoriteShows,
        randomViewingHours,                           // Generated viewing hours
        randomSubscriptionType                        // Generated subscription type
      )
    } else {
      () => new User(
        ConfigFromFile.alpha * logNormalRandomValue,
        ConfigFromFile.beta * logNormalRandomValue,
        startTime,
        ConfigFromFile.initialStates,
        ConfigFromFile.authGenerator.randomThing,
        UserProperties.randomProps,
        DeviceProperties.randomProps,
        ConfigFromFile.levelGenerator.randomThing,
        out,
        List.empty[String],                          // No preferred genres for music mode
        List.empty[String],                          // No favorite shows for music mode
        randomViewingHours,                          // Generated viewing hours
        randomSubscriptionType                       // Generated subscription type
      )
    }

    (0 until nUsers).foreach((_) => users += userGenerator())
  }

  def generateEvents() = {

    val out = determineOutputDestination(ConfFromOptions, kafkaProducer)

    initializeUsers(simulateVideo, out, nUsers, startTime)

    val growthRate = ConfigFromFile.growthRate.getOrElse(ConfFromOptions.growthRate.get.get)
    if (growthRate > 0) {
      var current = startTime
      while (current.isBefore(endTime)) {
        val mu = Constants.SECONDS_PER_YEAR / (nUsers * growthRate)
        val preferredGenres = RandomVideoContentGenerator.randomGenres(3)  // Generate a list of 3 random genres
        val favoriteShows = RandomVideoContentGenerator.randomShows(3)  // Generate a list of 3 random TV shows
        current = current.plusSeconds(TimeUtilities.exponentialRandomValue(mu).toInt)
        users += new User(
          ConfigFromFile.alpha * logNormalRandomValue,
          ConfigFromFile.beta * logNormalRandomValue,
          current,
          ConfigFromFile.initialStates,
          ConfigFromFile.newUserAuth,
          UserProperties.randomNewProps(current),
          DeviceProperties.randomProps,
          ConfigFromFile.newUserLevel,
          out,
          List.empty[String],                          // No preferred genres for music mode
          List.empty[String],                          // No favorite shows for music mode
          randomViewingHours,                          // Generated viewing hours
          randomSubscriptionType                       // Generated subscription type
        )
        nUsers += 1
      }
    }
    System.err.println("Initial number of users: " + ConfFromOptions.nUsers() + ", Final number of users: " + nUsers)

    val startTimeString =  startTime.toString
    val endTimeString = endTime.toString
    System.err.println("Start: " + startTimeString + ", End: " + endTimeString)

    var lastTimeStamp = System.currentTimeMillis()
    def showProgress(n: LocalDateTime, users: Int, e: Int): Unit = {
      if ((e % 10000) == 0) {
        val now = System.currentTimeMillis()
        val rate = 10000000 / (now - lastTimeStamp)
        lastTimeStamp = now
        val message = // "Start: " + startTimeString + ", End: " + endTimeString + ", " +
          "Now: " + n.toString + ", Events:" + e + ", Rate: " + rate + " eps"
        System.err.write("\r".getBytes)
        System.err.write(message.getBytes)
      }
    }
    System.err.println("Starting to generate events.")
    System.err.println("Damping=" + ConfigFromFile.damping + ", Weekend-Damping=" + ConfigFromFile.weekendDamping)

    var clock = startTime
    var events = 1

    while (clock.isBefore(endTime)) {

      if (realTime) {
        val now = LocalDateTime.now()
        val dif = Duration.between(now, clock)
        if (dif.isNegative)
          Thread.sleep(-dif.getSeconds)
      }

      showProgress(clock, users.length, events)
      val u = users.dequeue()
      val prAttrition = nUsers * ConfFromOptions.attritionRate() *
        (endTime.toEpochSecond(ZoneOffset.UTC) - startTime.toEpochSecond(ZoneOffset.UTC) / Constants.SECONDS_PER_YEAR)
      clock = u.session.nextEventTimeStamp.get

      if (clock.isAfter(startTime)) u.writeEvent()
      u.nextEvent(prAttrition)
      users += u
      events += 1
    }

    System.err.println("")
    System.err.println()

    out.flush()
    out.close()

  }

  if (ConfFromOptions.generateCounts())
    TrackListenCount.compute()
  else if (ConfFromOptions.generateSimilarSongs())
    SimilarSongParser.compute()
  else
    this.generateEvents()
}
