type Pos = (Int, Int)
type Tile = Int

case class Board( size: Int, tiles: Map[Pos,Tile] ) {

  def swap(p1: Pos, p2: Pos) = {
    val t1 = (tiles get p1, tiles get p2) match {
      case (Some(v1), Some(v2)) => tiles + (p1 -> v2) + (p2 -> v1)
      case (Some(v1), None    ) => (tiles - p1) + (p2 -> v1) 
      case (None,     Some(v2)) => (tiles - p2) + (p1 -> v2) 
      case _                    => tiles
    }
    new Board(size, t1)
  } 

  override def toString = {
    var b = " +"
    for (i <- 1 to size) 
      b = b + "--"
    b = b + "-+\n"
  
    var s = b
    for (i <- 1 to size) {
      s = s + " |" 
      for (j <- 1 to size)
        tiles get (i,j) match {
          case None    => s = s + "  "
          case Some(v) => s = s + " " + v
        }
      s = s + " |\n"  
    }
    s + b
  } 
}



abstract class State() {
  var explored: Boolean = false
  var predecessor: State = null

  def setExplored() = {
    explored = !explored
  }

  def isExplored() = explored

  def setPredecessor (s: State) = {
    predecessor = s
  }

  def getPredecessor () = predecessor
}

case class BoardState (board: Board, emptyPos: Pos) extends State {
  override def toString = board.toString + " " + emptyPos + "\n"
}

abstract class Operator {
  def apply(s: State): Option[State]
}


case object Left extends Operator {
  override def apply (s: State): Option[State] =
  s match {
    case BoardState(_, (_, 1)) => None
    case BoardState(b, (r, c)) => {
      val ep = (r, c - 1)
      Some( BoardState(b.swap((r, c), ep), ep) )
    }
  }
}

case object Right extends Operator {
  override def apply (s: State): Option[State] =
  s match {
    case BoardState(b, (r, c)) => if (c == b.size) None else {
      val ep = (r, c + 1)
      Some( BoardState(b.swap((r, c), ep), ep) )
    }
  }
}

case object Up extends Operator {
  override def apply (s: State): Option[State] =
  s match {
    case BoardState(_, (1, _)) => None
    case BoardState(b, (r, c)) => {
      val ep = (r - 1, c)
      Some( BoardState(b.swap((r, c), ep), ep) )
    }
  }
}

case object Down extends Operator {
  override def apply (s: State): Option[State] =
  s match {
    case BoardState(b, (r, c)) => if (r == b.size) None else {
      val ep = (r + 1, c)
      Some( BoardState(b.swap((r, c), ep), ep) )
    }
  }
}



def goalState(n: Int) = {
  var m: Map[Pos,Tile] = Map()
  for (i <- 1 to n; j <- 1 to n) {
    println((i,j))
    m = m + ((i,j) -> (j + n * (i - 1)))
  }
  val lastPos = (n,n)
  BoardState(Board(n, m - lastPos), lastPos)
}

//((1, 2, 3), (4, 8, 5), (0, 7, 6))

val ts = Map((1, 1) -> 1, (1,2) -> 2, (1, 3) -> 3, (2, 1) -> 4, (2, 2) -> 8, (2, 3) -> 5, (3, 2) -> 7, (3, 3) -> 6)
// sample board
val b = Board(3, ts)
val start = BoardState(b, (3,1))

