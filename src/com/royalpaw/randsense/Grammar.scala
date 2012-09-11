package com.royalpaw.randsense

import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: jamey
 * Date: 9/9/12
 * Time: 4:26 PM
 */
object Grammar {
  private val _rules = parseGrammar(new File(Constants.GRAMMAR_LOCATION))

  def rules = _rules

  /**
   * Parses the grammar file into an internal Map.
   * @param file is a raw text file of the context-free grammar
   */
  private def parseGrammar(file: File): Map[String, List[List[String]]] = {
    val data = io.Source.fromFile(file).getLines().toList.map(_.trim).filter(e => !e.isEmpty && !e.startsWith("#"))
    val grammar = Map[String, List[List[String]]]()

    data.map(_.split(" -> ")).map(e => (e(0), e(1).split(" \\| ").map(_.split(" ").toList).toList)).toMap
  }
}
