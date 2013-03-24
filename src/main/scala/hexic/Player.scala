package hexic

/**
 * @author Slava Pak
 */
abstract class Player(game: Game) {

  def nextMove: Move

  def allowedMoves(i: Int, j: Int) = {
    val (w, h) = game.dimensions
    if (i > h - 3)
      Nil
    else if (j < 1)
      RightClockwise(i, j) :: RightCounterClockwise(i, j) :: Nil
    else if (j > w - 2)
      LeftClockwise(i, j) :: LeftCounterClockwise(i, j) :: Nil
    else
      LeftClockwise(i, j) :: LeftCounterClockwise(i, j) :: RightClockwise(i, j) :: RightCounterClockwise(i, j) :: Nil
  }

}

class Robot(game: Game) extends Player(game) {

  override def nextMove: Move = {
    var maxScore = 0
    var selectedMove: Move = null
    val (w, h) = game.dimensions
    for (i <- 0 until h - 2;
         j <- 0 until w) {
      val moves = allowedMoves(i, j)
      moves.map(scoreMove _).foreach {
        case (score, move) =>
          if (maxScore < score) {
            maxScore = score
            selectedMove = move
          }
      }
    }
    selectedMove
  }

  def scoreMove(move: Move): (Int, Move) = {
    val board = game.boardView //make a copy of board
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
    //this fragment may seem to be code duplication with logic from game.run, but actually it is not:
    //player must score it's moves independently to define the most effective strategy,
    //just in this case it is the same as game scoring
    var score = 0
    var clusters = board.enumerateClusters
    while (!clusters.isEmpty) {
      score = score + game.scoreClusters(clusters)
      clusters.flatMap(_.points).foreach{case (i, j) => board.set(i, j, Board.Black)}
      board.dropCellsDown()
      clusters = board.enumerateClusters
    }
    (score, move)
  }

}