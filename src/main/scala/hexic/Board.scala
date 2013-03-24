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

  //this could be improved counting clusters only in local area around some pivot points,
  // but for the sake of simplicity I just traverse the whole board
  def enumerateClusters = {
    val clusters = mutable.Set.empty[Cluster]
    val used = Array.ofDim[Boolean](h, w)

    def addToClusters(points: mutable.Set[(Int, Int)]) {
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

    for (i <- 0 until h - 2;
    j <- 0 until w) {
     if (isTopInLeftCluster(i, j)) {
       val points = if (i % 2 == 0)
         mutable.Set((i, j), (i + 1, j), (i + 2, j))
       else
         mutable.Set((i, j), (i + 1, j - 1), (i + 2, j))
       addToClusters(points)
     }
     if (isTopInRightCluster(i, j)) {
       val points = if (i % 2 == 0)
         mutable.Set((i, j), (i + 1, j + 1), (i + 2, j))
       else
         mutable.Set((i, j), (i + 1, j), (i + 2, j))
       addToClusters(points)
     }
    }
    clusters
  }

  def swipe(clusters: mutable.Set[Cluster]) {
    clusters.flatMap(_.points).foreach{case (i, j) => board(i)(j) = Black}
    dropCellsDown()
    Hexic.printBoard(this)
    fillEmptyCells()
    Hexic.printBoard(this)
  }

  def dropCellsDown() {
    for (i <- h - 1 to 1 by -1;
         j <- 0 until w)
      if (board(i)(j) == Black) {
        var k = i - 1
        while (k > - 1 && board(k)(j) == Black)
          k = k - 1
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

//  def isMiddleInLeftCluster(i: Int, j: Int) = {
//    if (i < 1 || i > h - 2  || i % 2 == 0 && j > w - 2 || board(i)(j) == Black)
//      false
//    else if (i % 2 == 0)
//      board(i)(j) == board(i - 1)(j + 1) && board(i)(j) == board(i + 1)(j + 1)
//    else
//      board(i)(j) == board(i - 1)(j) && board(i)(j) == board(i + 1)(j)
//  }
//
//  def isBottomInLeftCluster(i: Int, j: Int) = {
//    if (i < 2 || i % 2 != 0 && j == 0 || board(i)(j) == Black)
//      false
//    else if (i % 2 == 0)
//      board(i)(j) == board(i - 2)(j) && board(i)(j) == board(i - 1)(j)
//    else
//      board(i)(j) == board(i - 2)(j) && board(i)(j) == board(i - 1)(j - 1)
//  }
//
//  def isMiddleInRightCluster(i: Int, j: Int) = {
//    if (i < 1 || i > h - 2  || i % 2 != 0 && j < 1 || board(i)(j) == Black)
//      false
//    else if (i % 2 == 0)
//      board(i)(j) == board(i - 1)(j) && board(i)(j) == board(i + 1)(j)
//    else
//      board(i)(j) == board(i - 1)(j - 1) && board(i)(j) == board(i + 1)(j - 1)
//  }
//
//  def isBottomInRightCluster(i: Int, j: Int) = {
//    if (i < 2 || i % 2 == 0 && j > w - 2 || board(i)(j) == Black)
//      false
//    else if (i % 2 == 0)
//      board(i)(j) == board(i - 2)(j) && board(i)(j) == board(i - 1)(j + 1)
//    else
//      board(i)(j) == board(i - 2)(j) && board(i)(j) == board(i - 1)(j)
//  }
//
  //  def isInCluster(i: Int, j: Int) = {
  ////    !getClusterAroundPoint(i, j, Array.ofDim[Boolean](w, h)).isEmpty
  //    isTopInLeftCluster(i, j) ||
  //      isTopInRightCluster(i, j) ||
  //      isMiddleInLeftCluster(i, j) ||
  //      isMiddleInRightCluster(i, j) ||
  //      isBottomInLeftCluster(i, j) ||
  //      isBottomInRightCluster(i, j)
  //  }
//
  //  def getNeighbours(i: Int, j: Int) = {
  //    val neighbours =
  //      if (i % 2 ==0)
  //        (i - 2, j) :: (i - 1, j + 1) :: (i + 1, j + 1) :: (i + 2, j) :: (i + 1, j) :: (i - 1, j) :: Nil
  //      else
  //        (i - 2, j) :: (i - 1, j) :: (i + 1, j) :: (i + 2, j) :: (i + 1, j - 1) :: (i - 1, j - 1) :: Nil
  //    neighbours.filter(p => p._1 < 0 || p._1 > h - 1 || p._2 < 0  || p._2 > w - 1)
  //  }
  //
  //  def areConsequent(p1: (Int, Int), p2: (Int, Int)) = {
  //    val (i1, j1) = if (p1._1 < p2._1) p1 else p2
  //    val (i2, j2) = if (p1._1 > p2._1) p1 else p2
  //    if (i2 - i1 == 1) {
  //      if (i1 % 2 == 0)
  //        j2 == j1 || j2 == j1 + 1
  //      else
  //        j2 == j1 || j2 == j1 - 1
  //    } else
  //      i2 - i1 == 2 && j1 == j2
  //  }
//
  //  def getClusterAroundPoint(i: Int, j: Int, used: Array[Array[Boolean]]) = {
  //
  //    def canAddToCluster(p1: (Int, Int), p2: (Int, Int)) =
  //      !used(p1._1)(p1._2) && !used(p2._1)(p2._2) &&
  //        board(p1._1)(p1._2) == board(i)(j) && board(p2._1)(p2._2) == board(i)(j)
  //
  //    val cluster = new Cluster()
  //    if (used(i)(i)) {
  //      cluster
  //    } else {
  //      val neighbours = getNeighbours(i, j)
  //      var head = neighbours.head
  //      var tail = neighbours.tail
  //      while (tail != Nil) {
  //        if (canAddToCluster(head, tail.head))
  //          addPointsToCluster(cluster, used, head, tail.head)
  //        head = tail.head
  //        tail = tail.tail
  //      }
  //      if (neighbours.size == 6 && canAddToCluster(head, neighbours.head))
  //        addPointsToCluster(cluster, used, head /*which now is neighbours.last */, neighbours.head)
  //      if (!cluster.isEmpty)
  //        addPointsToCluster(cluster, used, (i, j))
  //      cluster
  //    }
  //  }
  //
  //  def expandCluster(c: Cluster, used: Array[Array[Boolean]]) {
  //    val border = c.points.flatMap(getNeighbours _).filterNot(p => used(p._1)(p._2))
  //    border.filter(canBeAddedToCluster(_, c))
  //  }
  //
  //  def canBeAddedToCluster(p: (Int, Int), c: Cluster)= {
  //    val clusterNeighbours = getNeighbours(p).toSet.intersect(c.points)
  //    //to be added to cluster a point must have at least two neighbours which are already in it
  //    if (clusterNeighbours.size < 2 || board(p._1)(p._2) != board(clusterNeighbours.head._1)(clusterNeighbours.head._2))
  //      false
  //    //those two neighbours must be consequent
  //    else if (clusterNeighbours.size == 2) {
  //      areConsequent(clusterNeighbours.head, clusterNeighbours.last)
  //    } else if (clusterNeighbours.size == 3) {
  //      val first = clusterNeighbours.head
  //      val second = clusterNeighbours.tail.head
  //      val third = clusterNeighbours.last
  //      areConsequent(first, second) || areConsequent(first, third) || areConsequent(third, second)
  //    } else
  //    //any cell can have no more than 6 neighbours, so if there are four neighbours which are already in cluster
  //    //at least two of them are consequent
  //      true
  //  }
  //
  //
  //  private def addPointsToCluster(c: Cluster, used: Array[Array[Boolean]], points: (Int, Int)*) {
  //    points.foreach(
  //      p => {
  //        c.points.add(p)
  //        used(p._1)(p._2) = true
  //      })
  //  }
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