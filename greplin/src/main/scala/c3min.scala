object C3min extends App {
  val ns = List(3,4,9,14,15,19,28,37,47,50,54,56,59,61,70,73,78,81,92,95,97,99)
  def º(o: Int) = ns filter (o<)
  val buffer = new collection.mutable.ListBuffer[List[Int]]
  for (a <- ns; b <- º(a); c <- º(b); d <- º(c); e <- º(d); f <- º(e))
    (List(b, a) /: List(c, d, e, f, 0))((x, y) =>
      if (ns contains x.sum) { buffer += x; y :: x } else y :: x)
  println((buffer.toSet + (ns take 7)).size + " solutions")
}
