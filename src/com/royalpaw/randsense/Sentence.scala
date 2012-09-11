package com.royalpaw.randsense

import util.Random

/**
 * User: jamey
 * Date: 8/30/12
 * Time: 6:23 PM
 */
class Sentence() {
  private val _posSentence = makePosSentence()
  private val _technicalSentence = makeTechnicalSentence()
  private val _baseSentence = _technicalSentence.map(word => (word \ "base").text)
  private val _finalSentence = makeSentence()

  def posSentence = _posSentence
  def technicalSentence = _technicalSentence
  def baseSentence = _baseSentence
  def finalSentence = _finalSentence

  /**
   * Traverses the grammar map to construct a sentence diagram.
   *
   * @param level the highest level from which to construct the result
   * @return a list of strings, each a part of speech
   */
  private def makePosSentence(level: String = "S"): List[String] = {
    def go(level: String, result: String): String = {
      if (!Grammar.rules.contains(level)) {
        result + level
      }
      else {
        val next = Grammar.rules(level)(Random.nextInt(Grammar.rules(level).size))
        next.map(go(_, result)).mkString(" ")
      }
    }
    go(level, "").split(" ").toList
  }

  /**
   * Creates a list of XML nodes, each describing all
   * the technical aspects we need for the word in question.
   *
   * @return a list of XML nodes
   */
  private def makeTechnicalSentence(): List[xml.Node] = {
    _posSentence map { partOfSpeech =>
      if (partOfSpeech.startsWith("verb"))
        Lexicon.random(Map("category" -> "verb", partOfSpeech.substring(5) -> ""))
      else if (partOfSpeech.startsWith("adverb"))
        Lexicon.random(Map("category" -> "adverb", partOfSpeech.substring(7) -> ""))
      else
        Lexicon.random(Map("category" -> partOfSpeech))
    }
  }

  /**
   * Gets the necessary word replacements from the Inflector and
   * plugs them into a new and now-finished sentence
   * @return a fully realized sentence
   */
  def makeSentence(): String = {
    val replacements = Inflector.inflect(_baseSentence, _posSentence, _technicalSentence)
    val pendingSentence = baseSentence.zipWithIndex.map{ case(word, index) =>
      val replacement = replacements.find(_._2 == index)
      if (replacement != None)
        replacement.toList(0)._1
      else
        word
    }
    Inflector.finish(_posSentence, pendingSentence)
  }
}
