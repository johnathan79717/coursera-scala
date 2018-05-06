package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   *
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   *
   * is represented as
   *
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    case Pos(row, col) => row >= 0 && row < levelVector.size &&
                          col >= 0 && col < levelVector(row).size &&
                          levelVector(row)(col) != '-'
  }

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val row = levelVector indexWhere (_ contains c)
    val col = levelVector(row) indexOf c
    Pos(row, col)
  }

  lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  abstract class Switch {
    val targets: List[Pos]
    val isHard: Boolean
    def transformTile(c: Char): Char
    def activate(isStanding: Boolean, s: BlockTilesState): BlockTilesState = {
      if (isHard && !isStanding) s
      else {
        val newTiles = (targets foldLeft s.tiles) {
          case (t, Pos(row, col)) => {
            val rowTiles = t(row)
            t.updated(row, rowTiles.updated(col, transformTile(rowTiles(col))))
          }
        }
        BlockTilesState(s.block, newTiles)
      }
    }
  }
  case object NoOp extends Switch {
    val targets = List()
    val isHard = true
    def transformTile(c: Char) = c
  }
  case class Toggle(val targets: List[Pos], val isHard: Boolean) extends Switch {
    def transformTile(c: Char) = if (c == '-') 'o' else '-'
  }

  val switches: Map[Pos, Switch] = Map().withDefaultValue(NoOp);
  type Tiles = Vector[Vector[Char]]

  case class BlockTilesState(val block: Block, val tiles: Tiles) extends State {
    lazy val terrain = terrainFunction(tiles)
    def legalNeighbors = for {
      (newBlock, move) <- block.neighbors
      s = BlockTilesState(newBlock, tiles).activateSwitch if s.isLegal
    } yield (s, move)

    def activateSwitch: BlockTilesState = {
      if (block.isStanding) activateSwitch(block.b1, true)
      else activateSwitch(block.b1, false).activateSwitch(block.b2, false)
    }

    def activateSwitch(p: Pos, isStanding: Boolean): BlockTilesState =
      switches.getOrElse(p, NoOp).activate(isStanding, this)

    def isLegal: Boolean = terrain(block.b1) && terrain(block.b2)

    override def toString = {
      def updated(t: Tiles, p: Pos, c: Char) =
        t.updated(p.row, t(p.row).updated(p.col, c))
      val newTiles = updated(updated(tiles, block.b1, '#'), block.b2, '#')
      "\n" + (newTiles.map(_ mkString "") mkString "\n")
    }
  }

  lazy val startTerrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)
  def startState = BlockTilesState(startBlock, vector)
}
