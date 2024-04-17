
package com.interana.eventsim.buildin

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import com.interana.eventsim.WeightedRandomThingGenerator

import scala.collection.mutable
import scala.io.Source

object RandomVideoContentGenerator extends WeightedRandomThingGenerator[String] {
  System.err.println("Loading video content file...")
  val fis = new FileInputStream("data/video_content.txt.gz")
  val gis = new GZIPInputStream(fis)
  val s = Source.fromInputStream(gis, "ISO-8859-1")

  val videoLines = s.getLines()

  val contentIdMap = new mutable.HashMap[String, (String, String, Double)]()
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
}
