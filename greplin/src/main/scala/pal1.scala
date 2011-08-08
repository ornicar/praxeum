package greplin

object Pal1 {
  type Words = List[String]

  def longestPalyndrom(text: String): String =
    analyse(text).foldLeft("")((a, b) =>
      if (b.length > a.length) b else a
    )

  def analyse(text: String): Words =
    if (text.length < 3) emptyWords()
    else palyndroms(text) ::: analyse(text.tail)

  def palyndroms(text: String): Words =
    wordsEndingWith(text, text.head.toString) filter (w => w == w.reverse)

  def wordsEndingWith(text: String, end: String): Words =
    allWordsIn(text) filter (_.endsWith(end))

  def allWordsIn(text: String): Words =
    text.foldLeft(emptyWords())((a,b) =>
      if (a.isEmpty) b.toString :: a
      else a.head + b.toString :: a
    ) filter (_.length > 2)

  def emptyWords(): Words = List[String]()
}
