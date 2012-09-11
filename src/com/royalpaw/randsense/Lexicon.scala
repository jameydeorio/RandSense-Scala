package com.royalpaw.randsense

import java.io.{File, FileNotFoundException}
import xml.{NodeSeq, Elem}
import util.Random

/**
 * Created with IntelliJ IDEA.
 * User: jamey
 * Date: 8/27/12
 * Time: 6:19 PM
 */
object Lexicon {
  private val xmlTree = xml.XML.loadFile(Constants.LEXICON_LOCATION)
  private val words = xmlTree \ "word"

  /**
   * Searches the lexicon for a specified word and returns a NodeSeq of those words.
   *
   * @param properties is a Map of properties, like ("base" -> "jump", "transitive" -> "")
   * @return NodeSeq
   */
  def search(properties: Map[String, String]) = words filter ( word => (
      properties.forall(property => {
        val (key, value) = property
        !(word \ key).isEmpty && (word \ key).text == value
      })
    ))

  /**
   * Grabs a random word node matching the given properties.
   *
   * @param properties is a Map of properties, like ("base" -> "jump", "transitive" -> "")
   * @return NodeSeq is a single word randomly picked from the possibilities
   */
  def random(properties: Map[String, String]) = {
    val possibilities = search(properties)
    possibilities(Random.nextInt(possibilities.length))
  }
}
