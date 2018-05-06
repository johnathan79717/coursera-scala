package streams

/**
 * This trait defines an infinite terrain, where the block can
 * go on any position.
 *
 * It keeps the `startPos` and the `goal` positions abstract.
 *
 * Using this trait is useful for testing. It can be used to find
 * the shortest path between two positions without terrain
 * restrictions.
 */
trait InfiniteTerrain extends GameDef {
  val startTerrain: Terrain = (pos: Pos) => true
  case class BlockState(val block: Block) extends State {
    def legalNeighbors = for {
      (newBlock, move) <- block.neighbors
      s = BlockState(newBlock) if s.isLegal
    } yield (s, move)

    def isLegal: Boolean = startTerrain(block.b1) && startTerrain(block.b2)
  }
  def startState = BlockState(startBlock)
}
