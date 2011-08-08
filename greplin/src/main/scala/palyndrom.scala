package greplin

import scala.io.Source

object Palyndrom {
  val processor = Pal2
  val iterations = 10

  def run(args: Array[String]) {
    val file = args match {
      case Array() => sys.error("No file given")
      case a => a(0)
    }
    val text = Source.fromFile(file).mkString
      .replaceAll("\n", "").replaceAll(" ", "").toLowerCase
    val t1 = time()
    for (i <- 1 to iterations) print(i + ":" + process(text) + " ")
    println()
    benchmark(text, iterations, (time()-t1))
  }

  def benchmark(text: String, iterations: Int, millis: Long) {
    val kbs = iterations * text.length / 1024
    println((kbs/(millis/1000.0)) + " kb/s")
  }

  def process(text: String): String = processor.longestPalyndrom(text)

  def time() = System.currentTimeMillis()
}
