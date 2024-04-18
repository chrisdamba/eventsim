package com.interana.eventsim

import java.time.LocalDateTime

import com.interana.eventsim.TimeUtilities._
import com.interana.eventsim.buildin.RandomSongGenerator
import com.interana.eventsim.buildin.RandomVideoContentGenerator
import com.interana.eventsim.config.ConfigFromFile

/**
 * Object to capture session related calculations and properties
 */
case class Session(var nextEventTimeStamp: Option[LocalDateTime],
              val alpha: Double, // expected request inter-arrival time
              val beta: Double,  // expected session inter-arrival time
              val initialStates: scala.collection.Map[(String,String),WeightedRandomThingGenerator[State]],
              val auth: String,
              val level: String,
              adInteraction: Boolean = true) { // Assuming you need to manage ad interactions

  val sessionId = Counters.nextSessionId
  var itemInSession = 0
  var done = false
  var currentState:State = initialStates((auth, level)).randomThing
  var currentSong:Option[(String,String,String,Double)] =
    if (currentState.page=="NextSong") Some(RandomSongGenerator.nextSong()) else None
  var currentSongEnd:Option[LocalDateTime] =
    if (currentState.page=="NextSong") Some(nextEventTimeStamp.get.plusSeconds(currentSong.get._4.toInt)) else None
  var currentContent: Option[(String, String, Double)] = None // Content ID, Title, Duration
  var currentAd: Option[(String, String, Double)] = None // Ad ID, Ad Type, Duration

  def incrementEvent() = {
    val nextState = currentState.nextState(rng)
    nextState match {
      case None =>
        done = true
      case Some(state) =>
        currentState = state
        itemInSession += 1
        nextEventTimeStamp = Some(nextEventTimeStamp.get.plusSeconds(exponentialRandomValue(alpha).toInt))

        // Simulate content playback and ad insertion logic
        state.page match {
          case "PlayVideo" =>
            currentContent = Some(RandomVideoContentGenerator.nextContent())
            currentAd = None // Reset ad status when new content starts

          case "PauseVideo" =>
            // Optionally log pause events, currentContent remains unchanged

          case "AdStart" =>
            // Simulate an ad being started if conditions met, such as natural breaks
            if (shouldStartAd()) {
              currentAd = Some(RandomVideoContentGenerator.nextAd())
              // Extend event time to include ad duration
              nextEventTimeStamp = Some(nextEventTimeStamp.get.plusSeconds(currentAd.get._3.toInt))
            }

          case "AdEnd" =>
            // Ad ends, possibly reset ad information
            currentAd = None

          case _ =>
            // Other event types can be added with their own logic
        }
  
      case x if 300 until 399 contains x.get.status =>
        nextEventTimeStamp=Some(nextEventTimeStamp.get.plusSeconds(1))
        currentState = nextState.get
        itemInSession += 1

      case x if x.get.page=="NextSong" =>
        if (currentSong.isEmpty) {
          nextEventTimeStamp=Some(nextEventTimeStamp.get.plusSeconds(exponentialRandomValue(alpha).toInt))
          currentSong = Some(RandomSongGenerator.nextSong())
        } else if (nextEventTimeStamp.get.isBefore(currentSongEnd.get)) {
          nextEventTimeStamp = currentSongEnd
          currentSong = Some(RandomSongGenerator.nextSong(currentSong.get._1))
        } else {
          nextEventTimeStamp=Some(nextEventTimeStamp.get.plusSeconds(exponentialRandomValue(alpha).toInt))
          currentSong = Some(RandomSongGenerator.nextSong(currentSong.get._1))
        }
        currentSongEnd = Some(nextEventTimeStamp.get.plusSeconds(currentSong.get._4.toInt))
        currentState = nextState.get
        itemInSession += 1

      case _ =>
        nextEventTimeStamp=Some(nextEventTimeStamp.get.plusSeconds(exponentialRandomValue(alpha).toInt))
        currentState = nextState.get
        itemInSession += 1

    }
  }
  def shouldStartAd(): Boolean = {
    // Logic to determine if an ad should start, e.g., between episodes or during a natural break in a movie
    // This is a placeholder function and should be adapted to actual logic based on content type, user behavior, etc.
    rng.nextDouble() < 0.3 // 30% chance to start an ad for demo purposes
  }
  def nextSession =
    new Session(Some(Session.pickNextSessionStartTime(nextEventTimeStamp.get, beta)),
      alpha, beta, initialStates, currentState.auth, currentState.level)

}

object Session {

  def pickFirstTimeStamp(st: LocalDateTime,
    alpha: Double, // expected request inter-arrival time
    beta: Double   // expected session inter-arrival time
   ): LocalDateTime = {
    // pick random start point, iterate to steady state
    val startPoint = st.minusSeconds(beta.toInt * 2)
    var candidate = pickNextSessionStartTime(startPoint, beta)
    while (candidate.isBefore(st.minusSeconds(beta.toInt))) {
      candidate = pickNextSessionStartTime(candidate, beta)
    }
    candidate
  }

  def pickNextSessionStartTime(lastTimeStamp: LocalDateTime, beta: Double): LocalDateTime = {
    val randomGap = exponentialRandomValue(beta).toInt + ConfigFromFile.sessionGap
    val nextTimestamp: LocalDateTime = TimeUtilities.standardWarp(lastTimeStamp.plusSeconds(randomGap))
    assert(randomGap > 0)

    if (nextTimestamp.isBefore(lastTimeStamp)) {
      // force forward progress
      pickNextSessionStartTime(lastTimeStamp.plusSeconds(ConfigFromFile.sessionGap), beta)
    } else if (keepThisDate(lastTimeStamp, nextTimestamp)) {
      nextTimestamp
    } else
      pickNextSessionStartTime(nextTimestamp, beta)
  }
}
