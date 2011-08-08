import sbt._

object PraxeumBuild extends Build
{
	lazy val greplin = Project("greplin", file("greplin"))

	lazy val nqueens = Project("nqueens", file("nqueens"))

	lazy val config_reader = Project("config_reader", file("config_reader"))

	lazy val tictactoe = Project("tictactoe", file("tictactoe"))

	lazy val tictactoe_solution = Project("tictactoe_solution", file("tictactoe_solution"))

	lazy val sandbox = Project("sandbox", file("sandbox"))
}
