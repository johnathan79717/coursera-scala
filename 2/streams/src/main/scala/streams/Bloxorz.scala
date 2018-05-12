package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terrain to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1,3)
    val goal = Pos(5,8)
  }

  println(InfiniteLevel.solution)

  /**
   * A simple level constructed using the StringParserTerrain
   */
  abstract class Level extends Solver with StringParserTerrain

  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println(Level0.solution)

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends Level {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  println("Level 1: " + Level1.solution)

  object Level2 extends Level {
    val level =
    """------oooo--ooo
      |oooo--ooxo--oTo
      |ooOo--oooo--ooo
      |oooo--oooo--ooo
      |oSoo--oooo--ooo
      |oooo--oooo-----""".stripMargin

    override val switches = Map(
      Pos(2, 2) -> Toggle(List(Pos(4, 4), Pos(4, 5)), false),
      Pos(1, 8) -> Toggle(List(Pos(4, 10), Pos(4, 11)), true)
      )
  }

  println("Level 2: " + Level2.solution)

  object Level3 extends Level {
    val level =
    """------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo
      |oooo-------oooo
      |------------ooo""".stripMargin
  }

  println("Level 3: " + Level3.solution)

  object Level4 extends Level {
    val level =
    """---ooooooo----
      |---ooooooo----
      |oooo-----ooo--
      |ooo-------oo--
      |ooo-------oo--
      |oSo--ooooooooo
      |ooo--ooooooooo
      |-----oTo--oooo
      |-----ooo--oooo""".stripMargin
  }

  println("Level 4: " + Level4.solution)

  object Level5 extends Level {
    val level =
    """-----------oooo
      |-ooooooo1ooooSo
      |-oooo-------ooo
      |-oo2o----------
      |-oooo----------
      |---ooo3oooooo--
      |----------oooo4
      |ooo-------ooooo
      |oTooo5ooooooo--
      |oooo-----------""".stripMargin
      override val switches = Map(
        Pos(1, 8) -> Toggle(List(Pos(1, 5), Pos(1, 6)), false),
        Pos(5, 6) -> Remove(List(Pos(8, 5), Pos(8, 6)), false),
        Pos(6, 14) -> Toggle(List(Pos(8, 5), Pos(8, 6)), false)
        )
  }

  println("Level 5: " + Level5.solution)

  object Level6 extends Level {
    val level =
    """-----oooooo----
      |-----o--ooo----
      |-----o--ooooo--
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo----
      |------ooooo----
      |------ooooo----
      |-------ooo-----""".stripMargin
  }

  println("Level 6: " + Level6.solution)


  object Level7 extends Level {
    val level =
    """--------oooo---
      |--------oooo---
      |ooo-----o--oooo
      |oSooooooo---oTo
      |ooo----ooX--ooo
      |ooo----ooo--ooo
      |-oo----o-------
      |--oooooo-------""".stripMargin
    val x = findChar('X', vector)
    //val q = findChar('?', vector)
    //println(q)
    //override lazy val goal = x
    override val switches = Map(
      x -> Toggle(List(Pos(6, 3)), true)
      )
  }

  println("Level 7: " + Level7.solution)

  // Level 8: Passcode: 499707

}
