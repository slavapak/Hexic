package hexic

import util.Random
import collection.mutable
import Board.Black

/**
 * @author Slava Pak
 */
class Board(val w: Int, val h: Int, colNum: Int) {

  private[this] val random = new Random

  val board = Array.ofDim[Int](h, w)

  def apply(i: Int, j: Int) =
    board(i)(j)

  def set(i: Int, j: Int, c: Int) {
    board(i)(j) = c
  }

  def randomColour =
    random.nextInt(colNum) + 1

  def copy = {
    val b = new Board(w, h, colNum)
    for (i <- 0 until h;
         j <- 0 until w)
      b.board(i)(j) = board(i)(j)
    b
  }

  def rotateLeftClockwise(i: Int, j: Int) {
    require(i < h - 2)
    if (i % 2 != 0)
      require(j > 0)
    val t = board(i)(j)
    if (i % 2 == 0) {
      board(i)(j) = board(i + 1)(j)
      board(i + 1)(j) = board(i + 2)(j)
    } else {
      board(i)(j) = board(i + 1)(j - 1)
      board(i + 1)(j - 1) = board(i + 2)(j)
    }
    board(i + 2)(j) = t
  }

  def rotateLeftCounterClockwise(i: Int, j: Int) {
    require(i < h - 2)
    if (i % 2 != 0)
      require(j > 0)
    val t = board(i)(j)
    board(i)(j) = board(i + 2)(j)
    if (i % 2 == 0) {
      board(i + 2)(j) = board(i + 1)(j)
      board(i + 1)(j) = t
    } else {
      board(i + 2)(j) = board(i + 1)(j - 1)
      board(i + 1)(j - 1) = t
    }
  }

  def rotateRightClockwise(i: Int, j: Int) {
    require(i < h - 2)
    if (i % 2 == 0)
      require(j < w - 1)
    val t = board(i)(j)
    board(i)(j) = board(i + 2)(j)
    if (i % 2 == 0) {
      board(i + 2)(j) = board(i + 1)(j + 1)
      board(i + 1)(j + 1) = t
    } else {
      board(i + 2)(j) = board(i + 1)(j)
      board(i + 1)(j) = t
    }
  }

  def rotateRightCounterClockwise(i: Int, j: Int) {
    require(i < h - 2)
    if (i % 2 == 0)
      require(j < w - 1)
    val t = board(i)(j)
    if (i % 2 == 0) {
      board(i)(j) = board(i + 1)(j + 1)
      board(i + 1)(j + 1) = board(i + 2)(j)
    } else {
      board(i)(j) = board(i + 1)(j)
      board(i + 1)(j) = board(i + 2)(j)
    }
    board(i + 2)(j) = t
  }

  val used = Array.ofDim[Boolean](h, w)

  private def clear(used: Array[Array[Boolean]]) {
    for (i <- 0 until h;
         j <- 0 until w)
      used(i)(j) = false
  }
  //this could be improved counting clusters only in local area around some pivot points,
  // but for the sake of simplicity I just traverse the whole board
  def enumerateClusters = {
    clear(used)
    val clusters = mutable.Set.empty[Cluster]
    for (i <- 0 until h - 2;
    j <- 0 until w) {
     if (isTopInLeftCluster(i, j)) {
       val points = if (i % 2 == 0)
         mutable.Set((i, j), (i + 1, j), (i + 2, j))
       else
         mutable.Set((i, j), (i + 1, j - 1), (i + 2, j))
       addToClusters(points, clusters)
     }
     if (isTopInRightCluster(i, j)) {
       val points = if (i % 2 == 0)
         mutable.Set((i, j), (i + 1, j + 1), (i + 2, j))
       else
         mutable.Set((i, j), (i + 1, j), (i + 2, j))
       addToClusters(points, clusters)
     }
    }
    clusters
  }

  def addToClusters(points: mutable.Set[(Int, Int)], clusters: mutable.Set[Cluster]) {
    val usedPoints = points.filter{case (i, j) => used(i)(j)}
    if (usedPoints.size == 0) {
      clusters += new Cluster(points)
      points.foreach{case (i, j) => used(i)(j) = true}
    } else if (usedPoints.size == 1) {
      //if a cluster found, but only one of it's points is already used by other clusters, omit the new found one
    } else if (usedPoints.size == 2) {
      //select from clusters such that contains both used points
      //if such cluster exists than there is only one such cluster,
      //because we merge clusters if they have 2 common points on each iteration
      clusters.filter(_.points.intersect(usedPoints).size == 2).headOption.map(
        c => {
          //add to that cluster all points of new cluster,
          // since set used as a container, only one new point will be added to that cluster
          c.points ++= points
          points.foreach{case (i, j) => used(i)(j) = true}
        })
    }
  }

  def swipe(clusters: mutable.Set[Cluster]) {
    clusters.flatMap(_.points).foreach{case (i, j) => board(i)(j) = Black}
    Hexic.printBoard("Before remove empty cells", this)
    dropCellsDown()
    Hexic.printBoard("After remove empty cells", this)
    fillEmptyCells()
    Hexic.printBoard("After drop empty cells down", this)
  }

  def dropCellsDown() {
    for (i <- h - 1 to 1 by -1;
         j <- 0 until w)
      if (board(i)(j) == Black) {
        var k = i - 2
        while (k > - 1 && board(k)(j) == Black)
          k = k - 2
        if (k > - 1) {
          board(i)(j) = board(k)(j)
          board(k)(j) = Black
        }
      }
  }

  def fillEmptyCells() {
    for (i <- h - 1 to 0 by -1;
    j <- 0 until w)
      if (board(i)(j) == Board.Black)
        board(i)(j) = randomColour
  }

  def isTopInLeftCluster(i: Int, j: Int) = {
    if (i > h - 3 || i % 2 != 0 && j < 1 || board(i)(j) == Black)
      false
    else if (i % 2 == 0)
      board(i)(j) == board(i + 1)(j) && board(i)(j) == board(i + 2)(j)
    else
      board(i)(j) == board(i + 1)(j - 1) && board(i)(j) == board(i + 2)(j)
  }

  def isTopInRightCluster(i: Int, j: Int) = {
    if (i > h - 3  || i % 2 == 0 && j > w - 2 || board(i)(j) == Black)
      false
    else if (i % 2 == 0)
      board(i)(j) == board(i + 2)(j) && board(i)(j) == board(i + 1)(j + 1)
    else
      board(i)(j) == board(i + 2)(j) && board(i)(j) == board(i + 1)(j)
  }

}

object Board {

  val Black = 0

  def generate(w: Int, h: Int, colNum: Int) = {
    val board = new Board(w, h, colNum)
    for (i <- h - 1 to 0 by  -1;
         j <- 0 until w) {
      do {
        board.set(i, j, board.randomColour)
      } while (board.isTopInLeftCluster(i, j)  || board.isTopInRightCluster(i, j))
    }
    board
  }

}

class Cluster(val points: mutable.Set[(Int, Int)] = mutable.Set.empty[(Int, Int)])