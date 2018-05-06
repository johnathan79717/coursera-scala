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

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  abstract class Switch {
    val positions: List[Pos]
    val isHard: Boolean
    def transformSquare(c: Char): Char
    def activate(t: TerrainWithSwitches): TerrainWithSwitches = {
      val newVector = (positions foldLeft t.vector) {
        case (v, Pos(row, col)) => {
          val rowVector = v(row)
          v.updated(row, rowVector.updated(col, transformSquare(rowVector(col))))
        }
      }
      TerrainWithSwitches(newVector)
    }
  }
  case object NoOp extends Switch {
    val positions = List()
    val isHard = true
    def transformSquare(c: Char) = c
  }
  case class Toggle(val positions: List[Pos], val isHard: Boolean) extends Switch {
    def transformSquare(c: Char) = if (c == '-') 'o' else '-'
  }

  val switches: Map[Pos, Switch] = Map()
  type V2 = Vector[Vector[Char]]

  case class TerrainWithSwitches(val vector: V2) extends Terrain {
    def apply(pos: Pos) = terrainFunction(vector)(pos)
    def pressedBy(p: Pos, isStanding: Boolean): TerrainWithSwitches = {
      val switch = switches(p)
      if (switch.isHard && !isStanding) this
      else switch.activate(this)
    }
    def pressedBy(b: Block): Terrain =
      if (b.isStanding) pressedBy(b.b1, true)
      else pressedBy(b.b1, false).pressedBy(b.b2, false)
  }

  lazy val startTerrain: Terrain = TerrainWithSwitches(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
