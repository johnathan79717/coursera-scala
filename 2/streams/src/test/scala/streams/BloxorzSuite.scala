package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): BlockTilesState =
      ls.foldLeft(startState) { case (state, move) =>
        require(state.isLegal) // The solution must always lead to legal blocks
        val BlockTilesState(block, tiles) = state
        BlockTilesState(move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }, tiles)
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level3 extends SolutionChecker {
    val level =
    """------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo
      |oooo-------oooo
      |------------ooo""".stripMargin
  }

  trait Level2 extends SolutionChecker {
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

  test("terrain function level 1") {
    new Level1 {
      assert(startTerrain(Pos(0,0)), "0,0")
      assert(startTerrain(Pos(1,1)), "1,1") // start
      assert(startTerrain(Pos(4,7)), "4,7") // goal
      assert(startTerrain(Pos(5,8)), "5,8")
      assert(!startTerrain(Pos(5,9)), "5,9")
      assert(startTerrain(Pos(4,9)), "4,9")
      assert(!startTerrain(Pos(6,8)), "6,8")
      assert(!startTerrain(Pos(4,11)), "4,11")
      assert(!startTerrain(Pos(-1,0)), "-1,0")
      assert(!startTerrain(Pos(0,-1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val state = BlockTilesState(Block(Pos(1,1),Pos(1,1)), vector)
      assert(neighborsWithHistory(state, List(Left,Up)).toSet ===
        Set(
          (BlockTilesState(Block(Pos(1,2),Pos(1,3)), vector), List(Right,Left,Up)),
          (BlockTilesState(Block(Pos(2,1),Pos(3,1)), vector), List(Down,Left,Up))
        )
      )
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      assert(
        newNeighborsOnly(
          Set(
            (BlockTilesState(Block(Pos(1,2),Pos(1,3)), vector), List(Right,Left,Up)),
            (BlockTilesState(Block(Pos(2,1),Pos(3,1)), vector), List(Down,Left,Up))
          ).toStream,

          Set(BlockTilesState(Block(Pos(1,2),Pos(1,3)), vector), BlockTilesState(Block(Pos(1,1),Pos(1,1)), vector))
        ) === Set(
          (BlockTilesState(Block(Pos(2,1),Pos(3,1)), vector), List(Down,Left,Up))
        ).toStream
      )
    }
  }


  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution).block == Block(goal, goal))
      println("Level 1: " + solution)
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution for level 2") {
    new Level2 {
      //println(pathsFromStart.take(10).toList)
      println("Level 2: " + solution)
    }
  }

  test("optimal solution for level 3") {
    new Level3 {
      println("Level 3: " + solution)
    }
  }

}
