package hexic

object Hexic {

  def main(args: Array[String]) {
    val w = 5
    val h = 15
    val colNum = 5 // purple, blue, orange, green, red (according to http://zone.msn.com/en/hexic/default.htm)
    val board = Board.generate(w, h, colNum)
    printBoard(board)
    new Game(board).run()
  }

  def printBoard(board: Board) {
    for (j <- 0 until board.w)
      print("   _")
    println()
    print(" ")
    for (j <- 0 until board.w)
      print("_/" + board(0, j) + "\\")
    println()
    for (i <- 1 until board.h) {
      if (i % 2 == 0)
        print("\\_")
      for (j <- 0 until board.w - 1)
        print("/" + board(i, j) + "\\_")
      print("/" + board(i, board.w - 1) + "\\")
      if (i % 2 != 0)
        print("_/")
      println()
    }
    if (board.h % 2 != 0)
      print("  ")
    for (j <- 0 until board.w)
      print("\\_/ ")
    println()
  }

}