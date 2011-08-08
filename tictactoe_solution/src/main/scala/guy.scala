package ttt

sealed trait Mark{
  override def toString = getClass.getSimpleName
}
class Nought extends Mark
class Cross extends Mark

sealed trait Result
case class Won[M <: Mark : Manifest]() extends Result
case object Draw extends Result

sealed trait GameState
trait NotFinished extends GameState
trait Started extends NotFinished
trait NotStarted extends NotFinished

trait Finished extends GameState
trait Failed extends Finished
trait WinState extends Finished
trait DrawState extends Finished

case class Position(x: Int, y: Int)

sealed trait NextTurn[Last <: Mark, Next <: Mark]
object NextTurn {
  implicit val crossesTurn = new NextTurn[Nought, Cross] {}
  implicit val noughtsTurn = new NextTurn[Cross, Nought] {}
}

trait Board[+S <: GameState, M <: Mark]{
  val grid: List[List[Option[Mark]]]
  val previous: Option[Board[_, _]]

  def takeMoveBack[Previous <: Mark]
  (implicit manifest: Manifest[Previous],
   previousTurn: NextTurn[Previous, M],
   isStarted: S <:< Started): Board[NotFinished, Previous]

  def move[Next <: Mark](position: Position)
  (implicit manifest: Manifest[Next],
   nextTurn: NextTurn[M, Next],
   isNotFinished: S <:< NotFinished): Either[Board[Finished, Next], Board[Started, Next]]

  def whoWonOrDraw(implicit isFinished: S <:< Finished): Result
}

import scala.annotation.tailrec

object Board {
  def start = apply[NotStarted, Nought]()

  private[ttt] def apply[S <: GameState, M <: Mark]()
  (implicit manifestM: Manifest[M], manifestS: Manifest[S]) =
    new BoardImpl[S, M](List.fill[Option[Mark]](3, 3)(None))

  private[ttt] def hasFreeCells(grid: List[List[Option[Mark]]]) =
    !grid.forall(_.forall(_.isDefined))
}

class BoardImpl[S <: GameState, M <: Mark](val grid: List[List[Option[Mark]]], val previous: Option[Board[_, _]] = None)
(implicit mManifest: Manifest[M], sManifest: Manifest[S]) extends Board[S, M]{

  import Board._

  private[ttt] def isInBounds(position: Position) =
    position.y >= 0 && position.y < grid.size &&
  position.x >= 0 && position.x < grid.head.size

  private[ttt] def isEmpty(position: Position) =
    isInBounds(position) &&
  grid(position.x)(position.y).isEmpty

  def takeMoveBack[Previous <: Mark]
  (implicit manifest: Manifest[Previous], previousTurn: NextTurn[Previous, M],
   isStarted: S <:< Started): Board[NotFinished, Previous] =
    previous.map(board => new BoardImpl[NotFinished, Previous](board.grid))
  .getOrElse(Board[NotFinished, Previous]())

  def move[Next <: Mark](position: Position)
  (implicit manifest: Manifest[Next], nextTurn: NextTurn[M, Next], isNotFinished: S <:< NotFinished):
  Either[Board[Finished, Next], Board[Started, Next]] =
    if (isEmpty(position)){
      val updatedGrid = grid.updated(position.x,
                                     grid(position.x)
                                     .updated(position.y,
                                              Some(manifest.erasure
                                                   .newInstance.asInstanceOf[Next])))

      def isWin = {
        def checkLine(lineIndices: Iterable[(Int, Int)]) =
          lineIndices.forall(pair => updatedGrid(pair._1)(pair._2)
                             .map(mark => manifest.erasure.isAssignableFrom(mark.getClass))
                             .getOrElse(false))

        val vertical = (0 to grid.size - 1).zip(List.fill(grid.size)(position.y))
        val diagonal1 = (0 to grid.size - 1).zip((0 to grid.size - 1).reverse)
        val diagonal2 = (0 to grid.size - 1).zip((0 to grid.size - 1))

        List(diagonal1, diagonal2, vertical, vertical.map(_.swap))
        .map(checkLine)
        .contains(true)
      }

      (isWin, !hasFreeCells(updatedGrid)) match {
        case (false, false) => Right(new BoardImpl[Started, Next](updatedGrid, Option(this)))
        case (true, _) => Left(new BoardImpl[WinState, Next](updatedGrid, Option(this)))
        case (_, true) => Left(new BoardImpl[DrawState, Next](updatedGrid, Option(this)))
      }

    }
  else Left(new BoardImpl[Failed, Next](grid))

  def whoWonOrDraw(implicit isFinished: S <:< Finished): Result =
    this match {
      case board: Board[WinState, _] => Won[M]
      case _ => Draw
    }

  def history: List[Board[_, _]] = history(Option(this), Nil)

  @tailrec
  private def history(board: Option[Board[_, _]], boards: List[Board[_, _]]): List[Board[_, _]] =
    board match {
      case Some(board: Board[GameState, Mark]) => history(board.previous, board :: boards)
      case None => boards
    }

  override def toString = "Game %s, last move by %s: \r\n%s"
  .format(sManifest.erasure.getSimpleName,
          mManifest.erasure.getSimpleName,
          grid.map(_.mkString(" ")).mkString("\r\n"))

}
