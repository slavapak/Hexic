package hexic

import collection.mutable

/**
 * @author Slava Pak
 */
class Game(private[this] val board: Board) {

  val player = new Robot(this)
  private[this] var score = 0

  def run() {
    var nextMove = player.nextMove
    Hexic.printBoard("Start", board)
    while (nextMove != null) {
      println("Move " + nextMove)
      make(nextMove)
      Hexic.printBoard("After move", board)
      var clusters = board.enumerateClusters
      while (!clusters.isEmpty) {
        score = score + scoreClusters(clusters)
        board.swipe(clusters)
        clusters = board.enumerateClusters
      }
      nextMove = player.nextMove
      println("Score " + score)
      println()
    }
  }

  private def make(move: Move) {
    move match {
      case LeftClockwise(i, j) =>
        board.rotateLeftClockwise(i, j)
      case LeftCounterClockwise(i, j) =>
        board.rotateLeftCounterClockwise(i, j)
      case RightClockwise(i, j) =>
        board.rotateRightClockwise(i, j)
      case RightCounterClockwise(i, j) =>
        board.rotateRightCounterClockwise(i, j)
    }
  }

  def scoreClusters(clusters: mutable.Set[Cluster]) = {
    val scoreSmallClusters = clusters.filter(_.points.size == 3).size
    val scoreBigClusters = clusters.filter(_.points.size > 3).map(
      c => {
        var score = 3
        for (i <- 1 to (c.points.size - 3))
          score = score * 3
        score
      }).sum
    scoreSmallClusters + scoreBigClusters
  }

  def boardView =
    board.copy

  def getScore =
    score

  def dimensions =
    (board.w, board.h)

  def synchronize(b: Board) {
    for (i <- 0 until b.h;
         j <- 0 until b.w)
      b.set(i, j, board(i, j))
  }

}

sealed abstract class Move(i: Int, j: Int)
case class LeftClockwise(i: Int, j: Int) extends Move(i, j)
case class LeftCounterClockwise(i: Int, j: Int) extends Move(i, j)
case class RightClockwise(i: Int, j: Int) extends Move(i, j)
case class RightCounterClockwise(i: Int, j: Int) extends Move(i, j)