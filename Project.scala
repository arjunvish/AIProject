import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set

object Project {
	
	//A Pos represents the coordinates of a position on the board, Tile represents the value at a position on the board
	type Pos = (Int, Int)
	type Tile = Int

	//A (size x size) Board has a mapping 'tiles' from positions to tile values.
	case class Board( size: Int, tiles: Map[Pos,Tile] ) {
 	 //swap() swaps the values of positions p1 and p2 and returnsa a new board which is the result of the swapping
	  def swap(p1: Pos, p2: Pos) = {
	    val t1 = (tiles get p1, tiles get p2) match {
	      case (Some(v1), Some(v2)) => tiles + (p1 -> v2) + (p2 -> v1)
	      case (Some(v1), None    ) => (tiles - p1) + (p2 -> v1) 
	      case (None,     Some(v2)) => (tiles - p2) + (p1 -> v2) 
	      case _                    => tiles
	    }
	    new Board(size, t1)
	  } 
	  //toString overrides the default toString of the case class to print out a board in the required format
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


	//A state represents a particular configuration of the board
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

	//A BoardState inherits the State class - it represents the configuration of the board, and the empty position
	case class BoardState (board: Board, emptyPos: Pos) extends State {
	  override def toString = board.toString + " " + emptyPos + "\n"
	}

	//We have 4 operators - Left, Right, Up, and Down - each one moves the blank space in the obvious direction
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


	//Represents the goal state, with an empty tile on the bottom right corner
	def goalState(n: Int) = {
	  var m: Map[Pos,Tile] = Map()
	  for (i <- 1 to n; j <- 1 to n) {
	    println((i,j))
	    m = m + ((i,j) -> (j + n * (i - 1)))
	  }
	  val lastPos = (n,n)
	  BoardState(Board(n, m - lastPos), lastPos)
	}
	
	//Astar is a function that implements the A* algorithm. It requires a P types as input, which is a child of the State class.
	//It inputs the start state, the goal state, the list of operators, and a heuristic function. It returns a list of states.
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
	      //path = goal :: path
	      while (state != startState) {
	        val prevState: P = state.getPredecessor().asInstanceOf[P]
	        path = state :: path
	        state = prevState
	      }  
	      path = startState :: path
	      println("explored size " + explored.size)
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
	
	
	//manhattan() defines the Manhattan heuristic function
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
	
	//misplaced() defines the Misplaced-tiles heuristic function
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
	
	//maxSwap() defines the max-swap heuristic function
	def maxSwap(a: BoardState, b: BoardState): Double = {
	  var current = a
	  var goal = b
	  //amap and bmap are tile-value to position mappings (reverse of tile maps)
	  var swapCount = 0 //keeps track of the number of swaps which is the heuristic count
	  while(current != goal)
	  {
	    println(current)
	    var current_reverse = current.board.tiles.map(_.swap)
	      var goal_reverse = goal.board.tiles.map(_.swap)
	    var current_empty_pos = current.emptyPos //where the empty tile should be
	    //var value_in_current = goal.board.tiles(current_empty_pos) //value in the position where the empty tile should be
	    if(goal.emptyPos != current.emptyPos)
	    {
	      //if there is a non-empty tile in the location where the empty tile should be, swap that tile with the empty tile
	      var goal_current_empty = goal.board.tiles(current_empty_pos)
	      //swap and update current
	      var new_board = current.board.swap(current_reverse(goal_current_empty), current_empty_pos)
	      var new_emptyPos = current_reverse(goal_current_empty)
	      current = BoardState(new_board, new_emptyPos)
	      swapCount += 1 //increment count
	    }
	    else
	    {
	      var flag : Boolean = false 
	      //if the empty-tile is in its actual position, swap it with the first tile in the board which isn't in its actual position and then repeat
	      for(i <- 1 to current.board.size){
	        for(j <- 1 to current.board.size){
	          var posn : Pos = (i, j) //all positions in the board
	          if((posn != current_empty_pos) && (!flag)){
	            //check all positions except current empty, stop if a position to swap with is found
	            var current_pos = current.board.tiles(posn)
	            var goal_pos = goal.board.tiles(posn)
	            if(current_pos != goal_pos){
	              //Swap and change table
	              flag = true
	              var new_board = current.board.swap(posn, current_empty_pos)
	              var new_emptyPos = current_pos
	              current = BoardState(new_board, posn)
	              swapCount += 1 //increment count
	            }
	          }
	        }
	      }
	    }
	  }
	  swapCount//return swapCount
	}
	
	def main(args: Array[String]) {
		val ts = Map((1, 1) -> 2, (1, 2) -> 4, (1, 3) -> 3, (2, 1) -> 1, (2, 2) -> 5, (2, 3) -> 6, (3, 1) -> 7, (3, 2) -> 8) // sample board
		val b = Board(3, ts)
		val start = BoardState(b, (3,3))
		var x = Astar(start, goalState(3), List(Left, Right, Up, Down), misplaced: (BoardState, BoardState) => Double)
		println(x.length-1)
		var y = Astar(start, goalState(3), List(Left, Right, Up, Down), manhattan: (BoardState, BoardState) => Double)
		println(y.length-1)
		println(x==y)
	}
}
