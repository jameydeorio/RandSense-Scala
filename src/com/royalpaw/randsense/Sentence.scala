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
   * @param level the highest level from which to construct the result
   * @return a list of strings, each a part of speech
   */
  private def makePosSentence(level: String = "S"): List[String] = {
    def go(level: String, result: String): String = {
      if (!Grammar.rules.contains(level)) {
        result + level
      }
      else {
        val weights = Grammar.rules(level).map(choice => choice.filter(isDouble(_)))
          .map(element => if (element.isEmpty) 1.0 else element(0).toDouble)
        val choices = reduceChoices(
          Grammar.rules(level).map(choice => choice.filter(!isDouble(_))),
          weights
        )
        val next = choices(Random.nextInt(choices.size))
        next.map(go(_, result)).mkString(" ")
      }
    }
    go(level, "").split(" ").toList
  }

  /**
   * Creates a list of XML nodes, each describing all
   * the technical aspects we need for the word in question.
   * @return a list of XML nodes
   */
  private def makeTechnicalSentence(): List[xml.Node] = {
    _posSentence map { partOfSpeech =>
      if (partOfSpeech.contains("_"))
        Lexicon.random((Map(
          "category" -> partOfSpeech.substring(0, partOfSpeech.indexOf("_")),
          partOfSpeech.substring(partOfSpeech.indexOf("_") + 1) -> ""
        )))
      else
        Lexicon.random(Map("category" -> partOfSpeech))
    }
  }

  /**
   * Gets the necessary word replacements from the Inflector and
   * plugs them into a new and now-finished sentence.
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

  /**
   * Tests whether the string passed in can be converted into a Double.
   * @param string
   * @return
   */
  private def isDouble(string: String): Boolean = {
    try {
      string.toDouble
      true
    } catch {
      case e: NumberFormatException => false
    }
  }

  /**
   * Uses a list of weights that correspond to a list of List[String]s,
   * whittling the options down depending on the weight of the item.
   * @param choices
   * @param weights
   * @return
   */
  private def reduceChoices(choices: List[List[String]], weights: List[Double]): List[List[String]] = {
    choices.zipWithIndex.filter{case(choice, index) => weights(index) > Random.nextDouble}
      .map(_._1)
  }
}
