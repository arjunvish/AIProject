import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set



object A_star {
  def Astar [P <: State] (startState: P, goal: P, Operators: List[Operator], h: (P, P)=> Double): List[P] = {
  val frontier = PriorityQueue.empty[(Double, Int, P)](
    Ordering.by((_: (Double, Int, P))._1).reverse
  )
  val explored = Set[P]()
  explored += startState
  frontier += ((h(startState, goal), 0, startState))
  while (!frontier.isEmpty) {
    val stateTuple = frontier.dequeue()
    val g = stateTuple._2
    var state = stateTuple._3
    if (state == goal) {
      var path = List[P]()
      path = goal :: path
      while (state != startState) {
        val prevState: P = state.getPredecessor().asInstanceOf[P]
        path = state :: path
        state = prevState
      }  
      path = startState :: path
      println("explored size s" + explored.size)
      return path
    }
    for (op <- Operators) {
        val output = op(state)
        //println(output)
        if (output != None && !explored.contains(output.get.asInstanceOf[P])){
          val o: P = output.get.asInstanceOf[P]
          o.setPredecessor(state)
          frontier += ((g+1+h(o, goal), g+1, o))
          explored += o
        } 
      }
  }
  return List[P]()
}

def manhattan(a: BoardState, b: BoardState): Double = {
  val amap = a.board.tiles.map(_.swap)
  val bmap = b.board.tiles.map(_.swap)
  var h = 0
  for (i <- amap) {
    var num = i._1
    var diff1 = amap(num)._1 - bmap(num)._1
    var diff2 = amap(num)._2 - bmap(num)._2
    h += (if (diff1 > 0) diff1 else -diff1)
    h += (if (diff2 > 0) diff2 else -diff2)
  }
  h
}

def misplaced(a: BoardState, b: BoardState): Double = {
  val amap = a.board.tiles.map(_.swap)
  val bmap = b.board.tiles.map(_.swap)
  var h = 0
  for (i <- amap) {
    var num = i._1
    if (amap(num) != bmap(num)) h = h+1
  }
  h
}

  def main(args: Array[String]) 
  {
    var x = Astar(start, goalState(3), List(Left, Right, Up, Down), misplaced: (BoardState, BoardState) => Double)
    println(x.length)
    var y = Astar(start, goalState(3), List(Left, Right, Up, Down), manhattan: (BoardState, BoardState) => Double)
    println(y.length)
    println(x==y)
  }
  
  
   object State {
   
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

val ts = Map(
    (1,1) -> 3, (1,2) -> 6, (1,3) -> 2,
    (2,1) -> 4, (2,2) -> 7, 
    (3,1) -> 1, (3,2) -> 5, (3,3) -> 8
    )

// sample board
val b = Board(3, ts)
val start = BoardState(b, (2,3))

}
  
}
