package ttt

import org.specs2.mutable._

class GameSpecs extends Specification{

  implicit def toOption[A, B](either: Either[A, B]): Option[B] = either.right.toOption

  "Play a winning game" in {
    Board.start.move[Cross](Position(1, 1))
    .flatMap(_.move[Nought](Position(0, 0)))
    //.flatMap(_.move[Nought](Position(2, 2))) <- not compiles, noughts can't move twice in a row
    .flatMap(_.move[Cross](Position(0, 2)))
    .flatMap(_.move[Nought](Position(1, 0)))
    .map(_.takeMoveBack[Cross])
    .flatMap(_.move[Nought](Position(2, 2)))
    //.flatMap(_.whoWonOrDraw) <- cannot define game's result on the game in progress
    .map(_.move[Cross](Position(2, 0)))
    .map{board =>
      board.fold(classOf[Board[WinState, Cross]].isInstance(_),_ must beNull)
      //println(board)
    } must beSome
  }

  "Play a winning game then play again" in {
    Board.start.move[Cross](Position(1, 1))
    .flatMap(_.move[Nought](Position(0, 0)))
    //.flatMap(_.move[Nought](Position(2, 2))) <- not compiles, noughts can't move twice in a row
    .flatMap(_.move[Cross](Position(0, 2)))
    .flatMap(_.move[Nought](Position(1, 0)))
    .map(_.takeMoveBack[Cross])
    .flatMap(_.move[Nought](Position(2, 2)))
    //.flatMap(_.whoWonOrDraw) <- cannot define game's result on the game in progress
    .map(_.move[Cross](Position(2, 0)))
    //.flatMap(_.move[Cross](Position(2, 0)))
    //.map(_.move[Cross](Position(2, 1))) <- cannot play on finished game
    .map{board =>
      board.fold(classOf[Board[WinState, Cross]].isInstance(_),_ must beNull)
      //println(board)
    } must beSome
  }

  "Make a wrong move" in {
    Board.start.move[Cross](Position(-1, 1))
    .fold({board =>
        classOf[Board[Failed, Cross]].isInstance(board)
        //println(board)
      }, _ must beNull)
    true
  }

  "Play a draw" in {
    Board.start.move[Cross](Position(0, 0))
    .flatMap(_.move[Nought](Position(0, 1)))
    .flatMap(_.move[Cross](Position(0, 2)))
    .flatMap(_.move[Nought](Position(1, 0)))
    .flatMap(_.move[Cross](Position(1, 1)))
    .flatMap(_.move[Nought](Position(2, 0)))
    .flatMap(_.move[Cross](Position(1, 2)))
    .flatMap(_.move[Nought](Position(2, 2)))
    .map(_.move[Cross](Position(2, 1)))
    .map{board =>
      board.fold(classOf[Board[DrawState, Cross]].isInstance(_),_ must beNull)
      println(board)
    } must beSome
  }

}
