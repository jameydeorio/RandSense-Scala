import com.royalpaw.randsense.Sentence

/**
 * Created with IntelliJ IDEA.
 * User: jamey
 * Date: 9/11/12
 * Time: 12:57 PM
 */
object Main {
  def main(args: Array[String]) {
    for (i <- Range(0, 10)) {
      val sentence = new Sentence
      println("> %d: %s".format(i + 1, sentence.finalSentence))
      println("pos: %s".format(sentence.posSentence))
      println("base: %s".format(sentence.baseSentence))
      println("technical: %s\n".format(sentence.technicalSentence))
    }
  }
}
