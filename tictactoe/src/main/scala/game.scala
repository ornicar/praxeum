sealed abstract class Game(val board: Game.Board) {

  def playerAt(pos: Int): Option[Player] = board(pos)

  def turnNumber: Int = board.count(_ != None)

  override def toString: String =
    board.grouped(3).map(line =>
      line.map(square =>
        square match { case Some(p) => p.symbol case None => "." }
      ).mkString(" ")
    ).mkString("\n") + "\n"
}

// a game that can receive a move
sealed trait PlayableGame extends Game {
  def player: Player = if (0 == turnNumber % 2) O else X

  def move(pos: Int): Result = new Result(pos match {
    case p if (board.indices contains p) =>
      playerAt(pos) match {
        case Some(_) => None
        case None => Some(Game(board.updated(pos, Some(player))))
      }
    case _ => None
  })
}

// at least one move has been played
sealed trait StartedGame

// a game with no move played yet
case class NewGame(b: Game.Board)
  extends Game(b) with PlayableGame

// in-play game
case class MiddleGame(b: Game.Board)
  extends Game(b) with PlayableGame with StartedGame

// all positions filled
case class FinishedGame(b: Game.Board)
  extends Game(b) with StartedGame

object Game {
  type Board = IndexedSeq[Option[Player]]

  val wins: List[String] =
    List("012", "345", "678", "036", "147", "258", "048", "246")

  def apply(): NewGame = new NewGame(IndexedSeq.fill(9)(None))

  def apply(board: Game.Board) =
    if (isFinished(board)) new FinishedGame(board)
    else new MiddleGame(board)

  def isFinished(board: Game.Board): Boolean =
    (board forall (_.isDefined)) ||
    (List(O, X) exists (p => wins contains {
      val indexedBoard = board.indices zip board
      (indexedBoard filter (_._2 == Some(p))).indices mkString ("")
    }))
}

final class Result(game: Option[StartedGame]) {
  def then(
    onInvalid: => Unit,
    onPlay: MiddleGame => Unit,
    onEnd: FinishedGame => Unit
  ): Unit = game match {
    case None => onInvalid
    case Some(fg @ FinishedGame(_)) => onEnd(fg)
    case Some(pg @ MiddleGame(_)) => onPlay(pg)
  }
}

sealed trait Player { val symbol: String }
object O extends Player { val symbol = "O" }
object X extends Player { val symbol = "X" }
