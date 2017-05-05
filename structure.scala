package project

//Import modules for implementing priority queues and sets
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set

	//A (size x size) Board has a mapping 'tiles' from positions to tile values.
	case class Board( size: Int, tiles: Map[(Int, Int),Int] ) {
 	 //swap() swaps the values of positions p1 and p2 and returnsa a new board which is the result of the swapping
	  def swap(p1: (Int, Int), p2: (Int, Int)) = {
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
	    var b = "+"
	    for (i <- 1 to size) 
	      b = b + "---"

	    b = b + "---+\n"
  
	    var s = b
	    for (i <- 1 to size) {
	      s = s + " |" 
	      for (j <- 1 to size)
	        tiles get (i,j) match {
	          case None    => s = s + "    "
	          case Some(v) => s = s + "  " + String.format("%2s", Integer.toString(v))
        	}
	      s = s + " |\n"  
	    }
	    s + " " + b
	  } 
	}


	//A state represents a particular configuration of the board
	abstract class State() {
	  	var predecessor: State = null //Points to the previous configuration of the board

  		def setPredecessor (s: State) = {
	    	predecessor = s
	  	}

	  	def getPredecessor () = predecessor

	  	def cost[P <: State](s1: P, s2: P): Double
	}

	//A BoardState inherits the State class - it represents the 
	//configuration of the board, and the empty position
	case class BoardState (board: Board, emptyPos: (Int, Int)) extends State {
	  	override def toString = board.toString + " " + emptyPos + "\n"
	  	override def cost[P <: State] (s1: P, s2: P): Double = {
	  		if (s1 == s2) return 0
	  		else return 1
	  	}
	}

	//We have 4 operators - Left, Right, Up, and Down - 
	//each one moves the blank space in the corresponding direction
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
