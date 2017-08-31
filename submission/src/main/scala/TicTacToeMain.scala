import hw.tictactoe._

case class Game(turn: Player, dimension: Int, board: Map [( Int , Int ) , Player ]) extends GameLike[Game] {

  def isFinished(): Boolean = 
    getWinner() match {
      case None => false
      case _ => true
    }

  def notTurn(x: Player): Player =
    if(x==X) O
    else X

  def isFull(): Boolean = (board.size== (this.dimension*this.dimension))

  /* Assume that isFinished is true */
  def getWinner(): Option[Player] = { //.exists and .forAll
    val aD = getAntiDiagonal()
    val gD = getDiagonal()
    val gH = getHorizontal(dimension, board)
    val gV = getVertical(dimension, board)
    if((board.isEmpty)) None
    else if(aD==Some(X)) Some(X)
    else if(aD==Some(O)) Some(O)
    else if(gD==Some(X)) Some(X)
    else if(gD==Some(O)) Some(O)
    else if(gH.exists(lst=>lst.forall((el => el==Some(X))))) Some(X)
    else if(gH.exists(lst=>lst.forall((el => el==Some(O))))) Some(O)
    else if(gV.exists(lst=>lst.forall((el => el==Some(X))))) Some(X)
    else if(gV.exists(lst=>lst.forall((el => el==Some(O))))) Some(O)
    else None

  }

  def nextBoards(): List[Game] = startNB(0, 0)

  def startNB(xcoord: Int, ycoord: Int): List[Game] =
    (xcoord, ycoord, posOnBoard(xcoord, ycoord), isFinished())  match {
        case(_, _, _, true) => Nil
        case (x, y, 1, _) => //End of the board
          { if(isItNone(x,y)) List(Solution.createGame(turn, dimension, board + ((x, y) -> turn))) //add to list of itself
            else Nil  //No list
          }
        case (x, y, 0, false) => //end of row
          {
            if(isItNone(x, y)) Solution.createGame(turn, dimension, board + ((x, y) -> turn)) :: startNB(0, y+1) //get to last column, go to next row
            else startNB(0, y+1) 
          }
        case (x, y, -1, false) => { //in the last column, go to the next row
          if(isItNone(x, y)) Solution.createGame(turn, dimension, board + ((x, y) -> turn)) :: startNB(x+1, y)
          else startNB(x+1, y)
        }
        case (_, _, _, false) => Nil
      }


  def posOnBoard(xcoord: Int, ycoord: Int): Int =
    if(xcoord==ycoord && xcoord == dimension-1) 1
    else if(xcoord==dimension-1) 0
    else -1

  def isItNone(xcoord: Int, ycoord: Int): Boolean = board.getOrElse((xcoord, ycoord), None)==None
  
  def getHorizontal(dim: Int, board: Map [( Int , Int ) , Player ]): List[List[Option[(Player)]]] = {
      0.to(dim-1).toList.map({ y =>  
        0.to(dim-1).toList.map({ x => { 
          board.get(x,y)
          }
        })
      })
  }

  def getVertical(dim: Int, board: Map [( Int , Int ) , Player ]): List[List[Option[(Player)]]] = {
      0.to(dim-1).toList.map({ x =>              
        0.to(dim-1).toList.map({ y => { 
          board.get(x,y)
          }
        })
      })
  }

  def getAntiDiagonal(): Option[(Player)] = {
    val b = board.filterKeys((x => x._1+x._2 == dimension-1))
    val x = b.values.count(_ == X) 
    val o = b.values.count(_ == O)
    if(x>o && x==dimension) Some(X)
    else if(o>x && o==dimension)Some(O)
    else None
 }

  def getDiagonal(): Option[(Player)] = {
    val b = board.filterKeys((x => x._1==x._2))
    val x = b.values.count(_ == X)
    val o = b.values.count(_ == O)
    if(x>o && x==dimension){Some(X)}
    else if(o>x && o==dimension){Some(O)}
    else None
 }
}

object Solution extends MinimaxLike {

  type T = Game // T is an "abstract type member" of MinimaxLike

  def createGame ( turn : Player , dim : Int , board : Map [( Int , Int ) , Player ]): Game = new Game(turn, dim, board)  //player first move, map player positions  to player
      

  def minimaxHelp(boards: List[Game]): List[Option[Player]] = boards match{
        case Nil => Nil
        case h::t=> minimax(createGame(h.notTurn(h.turn), h.dimension, h.board))::minimaxHelp(t)
  }

  def minimax ( board : Game ): Option [ Player ] = {
      val b = board.nextBoards()
      val w = board.getWinner()
      val m = minimaxHelp(b)
      if(w==Some(board.turn)) Some(board.turn)
      else if(board.isFull() && w == None) None
      else{ //recurse
        if(m.exists(x => x==Some(board.turn))) Some(board.turn)
        else if(m.exists(x => x==None)) None 
        else Some(board.notTurn(board.turn))
      }
}
}




