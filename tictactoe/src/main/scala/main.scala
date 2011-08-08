object Main {
  def main(args: Array[String]) {

    def move(
      game: PlayableGame,
      pos: Int,
      onPlay: PlayableGame => Unit
    ): Unit = game move pos then (
      { println("Invalid") },
      { g => println(g); onPlay(g) },
      { g => println(g); println("End") }
    )

    val g1 = Game()
    move(g1, 0, { g2 =>
      move(g2, 8, { g3 =>
        move(g3, 1, { g4 =>
          move(g4, 7, { g5 =>
            move(g5, 2, { g6 =>
              g6 move 6
            })
          })
        })
      })
    })
  }
}
