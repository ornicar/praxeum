package greplin

object Pal2 {
  def longestPalyndrom(text: String): String = {
    val chars = text.toArray
    var longest = ""

    def expand(pos: Int, dist: Int): String =
      if (chars(pos-dist) == chars(pos+dist)) expand(pos, dist + 1)
      else chars.slice(pos-dist+1, pos+dist).mkString

    for (pos <- 2 to text.length - 3; pal = expand(pos, 0))
      if (pal.length > longest.length) longest = pal

    longest
  }
}
