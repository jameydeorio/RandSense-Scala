package com.royalpaw.randsense

import util.Random

/**
 * User: jamey
 * Date: 8/30/12
 * Time: 6:29 PM
 */
object Inflector {
  val be = Map(
    "present" -> Map(
      "i" -> "am",
      "you" -> "are",
      "it" -> "is",
      "plural" -> "are"
    ),
    "simple_past" -> Map(
      "i" -> "was",
      "you" -> "were",
      "it" -> "was",
      "plural" -> "were"
    )
  )
  val vowels = List("a", "e", "i", "o", "u")

  val possibleDeterminersForVerb = IndexedSeq("determiner", "indefinite-article", "nominative-pronoun", "possessive-pronoun")
  val possibleSubjectsForVerb = IndexedSeq("noun", "nominative-pronoun")
  val possibleTenses = IndexedSeq("present", "simple_past")

  /**
   * High level entry point for an uninflected sentence, where its
   * replacement nouns and verbs are returned as list to the sentence.
   *
   * @param baseSentence
   * @param posSentence
   * @param technicalSentence
   * @return
   */
  def inflect(baseSentence: List[String], posSentence: List[String], technicalSentence: List[xml.Node]): List[(String, Int)] = {
    val replacementNouns = inflectNouns(posSentence, technicalSentence)
    val replacementVerbs = inflectVerbs(posSentence, technicalSentence)

    replacementNouns ::: replacementVerbs
  }

  /**
   * The companion noun to each determiner in the
   * sentence is found and inflected if need be.
   *
   * @param posSentence
   * @param technicalSentence
   * @return a list of tuple2 in the format of (inflected-noun, index for replacement)
   */
  private def inflectNouns(posSentence: List[String],  technicalSentence: List[xml.Node]): List[(String, Int)] = {
    val determinerIndices = posSentence.zipWithIndex.map { pos =>
      if (pos._1 == "determiner") pos._2 else -1
    }.filter(_ != -1)

    determinerIndices map { determinerIndex =>
      val nounIndex = posSentence.indexWhere(_ == "noun", determinerIndex)
      if (!(technicalSentence(determinerIndex) \ "plural").isEmpty)
        (pluralizeNoun(technicalSentence(nounIndex)), nounIndex)
      else
        ("", -1)
    }
  }

  /**
   * Pluralizes a noun according to its irregular plural rule
   * in the lexicon or by its base rule if no irregular exists.
   *
   * @param oldNoun xml node of noun to pluralize
   * @return the pluralized noun as a string
   */
  private def pluralizeNoun(oldNoun: xml.Node): String = {
    if ((oldNoun \ "plural").isEmpty) {
      val noun = (oldNoun \ "base").text
      if (List("ey", "ay", "oy").contains(noun.substring(noun.size - 2)))
        noun + "s"
      else if (noun(noun.size - 1) == 'y')
        noun.substring(0, noun.size - 1) + "ies"
      else if (List("ss", "ch").contains(noun.substring(noun.size - 2)))
        noun + "es"
      else if (List("s").contains(noun.substring(noun.size - 1)))
        noun + "ses"
      else
        noun + "s"
    } else {
      (oldNoun \ "plural").text
    }
  }

  /**
   * Finds all verbs in the sentence and sends them
   * to the conjugate method.
   *
   * @param posSentence
   * @param technicalSentence
   * @return
   */
  private def inflectVerbs(posSentence: List[String], technicalSentence: List[xml.Node]): List[(String, Int)] = {
    val verbIndices = posSentence.zipWithIndex.map { pos =>
      if (pos._1.startsWith("verb")) pos._2 else -1
    }.filter(_ != -1)

    val verbsToInflect = verbIndices map { verbIndex =>
      val reversedVerbIndex = reverseIndex(posSentence, verbIndex)
      val determinerIndex = reverseIndex(
        posSentence, findNextPossibilityInList(possibleDeterminersForVerb, posSentence.reverse, reversedVerbIndex)
      )
      val subjectIndex = reverseIndex(
        posSentence, findNextPossibilityInList(possibleSubjectsForVerb, posSentence.reverse, reversedVerbIndex)
      )
      Map("verb_index" -> verbIndex, "determiner_index" -> determinerIndex, "subject_index" -> subjectIndex)
    }

    verbsToInflect.map{verbToInflect =>
      (conjugateVerb(
        technicalSentence(verbToInflect("subject_index")),
        technicalSentence(verbToInflect("verb_index")),
        !(technicalSentence(verbToInflect("determiner_index")) \ "plural").isEmpty
      ),
        verbToInflect("verb_index")
      )
    }
  }

  /**
   * More of a router than a conjugator, conjugateVerb sends the necessary
   * data to the real conjugators depending on tense and other variables.
   *
   * @param subject xml node
   * @param verb xml node
   * @param isPlural is the determiner of the verb plural
   * @return
   */
  private def conjugateVerb(subject: xml.Node, verb: xml.Node, isPlural: Boolean): String = {
    val tense = possibleTenses(Random.nextInt(possibleTenses.size))

    if ((verb \ "base").text == "be")
      conjugateForBe(subject, tense, isPlural)
    else {
      tense match {
        case "present" => conjugateForPresent(subject, verb, isPlural)
        case "simple_past" => conjugateForSimplePast(verb)
      }
    }
  }

  /**
   * Conjugates a "be" verb.
   *
   * @param subject
   * @param tense
   * @param isPlural
   * @return
   */
  private def conjugateForBe(subject: xml.Node, tense: String, isPlural: Boolean): String = {
    if (isPlural) {
      be(tense)("plural")
    }
    else
      if (!List("i", "you").contains((subject \ "base").text))
        be(tense)("it")
      else
        be(tense)((subject \ "base").text)
  }

  /**
   * Conjugates a verb, depending on its determiner
   * and subject, for the present tense.
   *
   * @param subject
   * @param verb
   * @param isPlural
   * @return
   */
  private def conjugateForPresent(subject: xml.Node, verb: xml.Node, isPlural: Boolean): String = {
    val verbBase = (verb \ "base").text
    if (isPlural)
      verbBase
    else
      if (!List("i", "you", "we", "they").contains((subject \ "base").text))
        if (!(verb \ "present3s").isEmpty)
          (verb \ "present3s").text
        else
          if (List("ey", "ay", "oy").contains(verbBase.substring(verbBase.size - 2)))
            verbBase + "s"
          else if (verbBase(verbBase.size - 1) == 'y')
            verbBase.substring(0, verbBase.size - 1) + "ies"
          else if (List("sh", "ch", "ss").contains(verbBase.substring(verbBase.size - 2)))
            verbBase + "es"
          else
            verbBase + "s"
      else
        verbBase
  }

  /**
   * Conjugates a verb, depending on its determiner
   * and subject, for the simple past tense.
   *
   * @param verb
   * @return
   */
  private def conjugateForSimplePast(verb: xml.Node): String = {
    val verbBase = (verb \ "base").text
    if (!(verb \ "past").isEmpty)
      (verb \ "past").text
    else
      if (verbBase(verbBase.size - 1) == 'e')
        verbBase + "d"
      else if (List("ey", "ay", "oy").contains(verbBase.substring(verbBase.size - 2)))
        verbBase + "ed"
      else if (verbBase(verbBase.size - 1) == 'y')
        verbBase.substring(0, verbBase.size - 1) + "ied"
      else
        verbBase + "ed"
  }

  /**
   * Inflects all words following an indefinite article
   * (e.g., "an" apple, "a" cantaloupe
   *
   * @param posSentence
   * @param pendingSentence
   * @return
   */
  private def inflectForArticles(posSentence: List[String], pendingSentence: List[String]) = {
    val indefiniteArticleIndices = posSentence.zipWithIndex.map { pos =>
      if (pos._1 == "indefinite-article") pos._2 else -1
    }.filter(_ != -1)

    val replacementArticles = indefiniteArticleIndices map { articleIndex =>
      val nextWord = pendingSentence(articleIndex + 1)
      if (vowels.contains(nextWord.substring(0, 1)))
        if (!nextWord.startsWith("uni")) ("an", articleIndex) else ("a", articleIndex)
      else
        ("a", articleIndex)
    }

    pendingSentence.zipWithIndex.map { case(word, index) =>
      val replacement = replacementArticles.find(_._2 == index)
      if (replacement != None)
        replacement.toList(0)._1
      else
        word
    }
  }

  /**
   * Puts the final touches on a sentence, like indefinite article/noun
   * agreements and forming questions or statements.
   *
   * @param posSentence
   * @param pendingSentence
   * @return
   */
  def finish(posSentence: List[String], pendingSentence: List[String]): String = {
    val sentence = inflectForArticles(posSentence, pendingSentence)
      .mkString(" ").replaceAll("i ", "I ")
        .replaceAll(" ,", ",")
        .replaceAll(" ;", ";")
        .replaceAll(" :", ":")
          .capitalize
    if (sentence.contains("whose") || sentence.contains("whom"))
      sentence + "?"
    else
      sentence + "."
  }

  /**
   * Calculates the index of a position in a list if that list is reversed.
   * @param list
   * @param index
   * @return
   */
  private def reverseIndex(list: List[Any], index: Int): Int = list.size - (index + 1)

  /**
   * Finds the next item in a list that matches any
   * element in some list.
   *
   * @param possibilities
   * @param list
   * @param start
   * @return
   */
  private def findNextPossibilityInList(possibilities: IndexedSeq[String], list: List[String], start: Int): Int =
    list.indexWhere(item => possibilities.contains(item), start)
}
