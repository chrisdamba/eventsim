package com.interana.eventsim

import java.io.{OutputStream, Serializable}
import java.time.{ZoneOffset, LocalDateTime}

import com.fasterxml.jackson.core.{JsonEncoding, JsonFactory}
import com.interana.eventsim.config.ConfigFromFile
import com.interana.eventsim.SubscriptionType.SubscriptionType
import play.api.libs.json._


class User(val alpha: Double,
           val beta: Double,
           val startTime: LocalDateTime,
           val initialSessionStates: scala.collection.Map[(String,String),WeightedRandomThingGenerator[State]],
           val auth: String,
           val props: Map[String,Any],
           var device: scala.collection.immutable.Map[String,Any],
           val initialLevel: String,
           val stream: OutputStream,
           val preferredGenres: List[String],           // new attribute for preferred genres
           val favoriteShows: List[String],             // new attribute for favorite shows
           val viewingHours: Int,                       // new attribute for typical viewing hours per week
           val subscriptionType: SubscriptionType       // new attribute for subscription type (e.g., 'basic', 'premium')
          ) extends Serializable with Ordered[User] {

  val userId = Counters.nextUserId
  var session = new Session(
    Some(Session.pickFirstTimeStamp(startTime, alpha, beta)),
      alpha, beta, initialSessionStates, auth, initialLevel)

  override def compare(that: User) =
    (that.session.nextEventTimeStamp, this.session.nextEventTimeStamp) match {
      case (None, None) => 0
      case (_: Some[LocalDateTime], None) => -1
      case (None, _: Some[LocalDateTime]) => 1
      case (thatValue: Some[LocalDateTime], thisValue: Some[LocalDateTime]) =>
        thatValue.get.compareTo(thisValue.get)
    }

    implicit val anyJsonWrites: Writes[Any] = new Writes[Any] {
      def writes(any: Any): JsValue = any match {
        case n: Int => JsNumber(n)
        case s: String => JsString(s)
        case b: Boolean => JsBoolean(b)
        case _ => JsString(any.toString)  // Fallback for unknown types
      }
    }

    implicit val mapWrites: Writes[Map[String, Any]] = new Writes[Map[String, Any]] {
      def writes(map: Map[String, Any]): JsValue = JsObject(map.map {
        case (key, value) => key -> Json.toJson(value)(anyJsonWrites)
      })
    }

  def nextEvent(): Unit = nextEvent(0.0)

  def nextEvent(prAttrition: Double) = {
    session.incrementEvent()
    if (session.done) {
      if (TimeUtilities.rng.nextDouble() < prAttrition ||
          session.currentState.auth == ConfigFromFile.churnedState.getOrElse("")) {
        session.nextEventTimeStamp = None
        // TODO: mark as churned
      }
      else {
        // Adjust the session logic based on user preferences and subscription type
        if (subscriptionType == SubscriptionType.Premium && TimeUtilities.rng.nextDouble() < 0.5) {
          // Simulate fewer ad interactions for premium users
          session = session.nextSession.copy(adInteraction = false)
        } else {
          session = session.nextSession
        }
      }
    }
  }

  private val EMPTY_MAP = Map()

  def eventString: String = {
    val showUserDetails = ConfigFromFile.showUserWithState(session.currentState.auth)
    var m: Map[String, Any] = Map(
      "ts" -> session.nextEventTimeStamp.get.toInstant(ZoneOffset.UTC).toEpochMilli,
      "userId" -> (if (showUserDetails) userId else ""),
      "sessionId" -> session.sessionId,
      "page" -> session.currentState.page,
      "auth" -> session.currentState.auth,
      "method" -> session.currentState.method,
      "status" -> session.currentState.status,
      "itemInSession" -> session.itemInSession,
      "preferredGenres" -> preferredGenres.mkString(","),
      "favoriteShows" -> favoriteShows.mkString(","),
      "viewingHours" -> viewingHours,
      "subscriptionType" -> subscriptionType.toString
    )

    // Add properties dynamically based on user details
    if (showUserDetails) {
      m = m ++ props.mapValues {
        case v: Long => v
        case v: Int => v
        case v: Double => v
        case v: Float => v.toFloat
        case v: String => v
        case other => other.toString
      }
    }

    // Update or handle additional attributes based on current session state
    val additionalDetails = session.currentState.page match {
      case "PlayVideo" | "PauseVideo" | "AdStart" | "AdEnd" =>
        Map(
          "contentId" -> session.currentContent.map(_._1),
          "title" -> session.currentContent.map(_._2),
          "duration" -> session.currentContent.map(_._3),
          "pausedAt" -> session.currentContent.map(_._3),
          "adId" -> session.currentAd.map(_._1),
          "adType" -> session.currentAd.map(_._2),
          "videoResolution" -> session.currentAd.map(_._3),
          "watchedDuration" -> session.currentAd.map(_._3)
        ).collect { case (k, Some(v)) => k -> v }  // Filter out None values
      case _ => Map.empty[String, Any]
    }

    m = m ++ additionalDetails

    Json.stringify(Json.toJson(m)(mapWrites))
  }


  val writer = User.jsonFactory.createGenerator(stream, JsonEncoding.UTF8)

  def writeEvent() = {
    // use Jackson streaming to maximize efficiency
    // (earlier versions used Scala's std JSON generators, but they were slow)
    val showUserDetails = ConfigFromFile.showUserWithState(session.currentState.auth)
    writer.writeStartObject()
    writer.writeNumberField("ts", session.nextEventTimeStamp.get.toInstant(ZoneOffset.UTC)toEpochMilli())
    writer.writeStringField("userId", if (showUserDetails) userId.toString else "")
    writer.writeNumberField("sessionId", session.sessionId)
    writer.writeStringField("page", session.currentState.page)
    writer.writeStringField("auth", session.currentState.auth)
    writer.writeStringField("method", session.currentState.method)
    writer.writeNumberField("status", session.currentState.status)
    writer.writeStringField("level", session.currentState.level)
    writer.writeNumberField("itemInSession", session.itemInSession)

    if (showUserDetails) {
      props.foreach((p: (String, Any)) => {
        p._2 match {
          case _: Long => writer.writeNumberField(p._1, p._2.asInstanceOf[Long])
          case _: Int => writer.writeNumberField(p._1, p._2.asInstanceOf[Int])
          case _: Double => writer.writeNumberField(p._1, p._2.asInstanceOf[Double])
          case _: Float => writer.writeNumberField(p._1, p._2.asInstanceOf[Float])
          case _: String => writer.writeStringField(p._1, p._2.asInstanceOf[String])
        }})
    }
    if (Main.tag.isDefined) {
        writer.writeStringField("tag", Main.tag.get)
    }
    if (session.currentState.page=="NextSong") {
      writer.writeStringField("artist", session.currentSong.get._2)
      writer.writeStringField("song",  session.currentSong.get._3)
      writer.writeNumberField("length", session.currentSong.get._4)
    }

    // Write additional details for new events
    session.currentState.page match {
      case "PlayVideo" | "PauseVideo" =>
        writer.writeStringField("contentId", session.currentContent.get._1)
        writer.writeStringField("title", session.currentContent.get._2)
        if (session.currentState.page == "PlayVideo") {
          writer.writeNumberField("duration", session.currentContent.get._3)
        } else {  // PauseVideo
          writer.writeNumberField("pausedAt", session.currentContent.get._3)
        }

      case "AdStart" | "AdEnd" =>
        writer.writeStringField("adId", session.currentAd.get._1)
        if (session.currentState.page == "AdStart") {
          writer.writeStringField("adType", session.currentAd.get._2)
          writer.writeStringField("videoResolution", session.currentAd.get._3.toString)
        } else {  // AdEnd
          writer.writeNumberField("watchedDuration", session.currentAd.get._3)
        }
    }
    
    writer.writeEndObject()
    writer.writeRaw('\n')
    writer.flush()
  }

  def tsToString(ts: LocalDateTime) = ts.toString()

  def nextEventTimeStampString =
    tsToString(this.session.nextEventTimeStamp.get)

  def mkString = props.+(
    "alpha" -> alpha,
    "beta" -> beta,
    "startTime" -> tsToString(startTime),
    "initialSessionStates" -> initialSessionStates,
    "nextEventTimeStamp" -> tsToString(session.nextEventTimeStamp.get) ,
    "sessionId" -> session.sessionId ,
    "userId" -> userId ,
    "currentState" -> session.currentState)
}

object User {
  protected val jsonFactory = new JsonFactory()
  jsonFactory.setRootValueSeparator("")
}
