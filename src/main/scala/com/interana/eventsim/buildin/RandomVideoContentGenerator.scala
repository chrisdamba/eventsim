
package com.interana.eventsim.buildin

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import com.interana.eventsim.WeightedRandomThingGenerator

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object RandomVideoContentGenerator extends WeightedRandomThingGenerator[String] {
  System.err.println("Loading video content file...")
  val fis = new FileInputStream("data/video_content.txt.gz")
  val gis = new GZIPInputStream(fis)
  val s = Source.fromInputStream(gis, "ISO-8859-1")

  val videoLines = s.getLines()
  private val random = new Random

  private val genres = List(
    "Action comedy", "Adult chat", "Animated series", "Animated sitcom", "Anthology series", 
    "Apocalyptic and post-apocalyptic fiction", "Archive television program", "Brazilian telenovela", 
    "Breakfast television", "Casting television", "Children's television series", "Chopsocky", 
    "Television comedy", "Comedy drama", "Cooking show", "Court show", "Daytime television", 
    "Daytime television in the United States", "Debate show", "Dialect comedy", "Docudrama", 
    "Docufiction", "Documentary film", "Drama (film and television)", "Educational television", 
    "Ethnofiction", "Factual television", "Fantaserye and telefantasya", "Fantasy television", 
    "Fishing television series", "Food reality television", "Game show", "Generalist channel", 
    "Gods and demons fiction", "Hanukkah in television", "Hidden camera", "Historical drama", 
    "Hong Kong television drama", "Interview (journalism)", "Japanese variety show", "Jiangshi fiction", 
    "Kung fu film", "Late-night talk show", "Late-night television", "Legal drama", "Medical drama", 
    "Micro-series", "Mockumentary", "Mystery box show", "Neo-noir", "News magazine", 
    "Philippine noontime variety television shows", "Overnight television", "Paranormal television", 
    "Political drama", "Prime time", "Procedural drama", "Psychological drama", "Public affairs (broadcasting)", 
    "Quality television", "Real time (media)", "Reality television", "Satire", "Science fiction on television", 
    "Serial (radio and television)", "Shiny-floor show", "Sitcom", "Sketch comedy", "Soap opera", 
    "Space Western", "Specialty channel", "Stoner TV", "Student quiz show", "Tabloid talk show", 
    "Tabloid television", "Talk show", "Tamil television drama", "Teen sitcom", "Telenovela", "Téléroman", 
    "Television documentary", "Television play", "Thai television soap opera", "Thriller (genre)", 
    "Variety show", "Westerns on television", "Wuxia"
  )

  private val shows = List(
    "Stranger Things", "The Witcher", "Ozark", "The Crown", 
    "Black Mirror", "Narcos", "BoJack Horseman", "Bridgerton",
    "The Umbrella Academy", "The Queen's Gambit", "Mindhunter",
    "Dark", "Orange Is the New Black", "Money Heist", "Elite"
  )


  val contentIdMap = new mutable.HashMap[String, (String, Double, Int)]()
  var i = 0
  for (vl <- videoLines) {
    if ((i % 1000) == 0)
      System.err.print("\r" + i)
    i += 1
    try {
      val fields = vl.split("\t")
      val contentId = fields(0)
      val title = fields(1)
      val duration = {
        val d = fields(2)
        if (d != "") d.toDouble else 90.0 // Default to 90 minutes if not specified
      }
      val viewCount = fields(3).toInt
      contentIdMap.put(contentId, (title, duration, viewCount))  
      this.add(contentId, viewCount)
    } catch {
      case e: NumberFormatException =>
        println("\n" + vl + "\n")
        throw e
    }
  }
  System.err.println("\t...done loading video content file. " + contentIdMap.size + " contents loaded.")
  s.close()

  def nextContent(): (String, String, Double) = {
    val nextContentId = this.randomThing
    val content = contentIdMap(nextContentId)
    (nextContentId, content._1, content._2)
  }

  def nextAd(): (String, String, Double) = {
    // Assuming ads are also loaded similarly with content ID, type, and duration
    val nextAdId = this.randomThing
    val ad = contentIdMap(nextAdId)
    (nextAdId, "Commercial", ad._2) // Assuming all ads are tagged as "Commercial"
  }

  def randomGenres(count: Int): List[String] = {
    Random.shuffle(genres).take(count)
  }

  def randomShows(count: Int): List[String] = {
    Random.shuffle(shows).take(count)
  }
}
