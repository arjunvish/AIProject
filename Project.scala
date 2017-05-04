//Import modules for implementing priority queues and sets
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set

//Object project encapsulates the entire project
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
	case class BoardState (board: Board, emptyPos: Pos) extends State {
	  	override def toString = board.toString + " " + emptyPos + "\n"
	  	override def cost[P <: State] (s1: P, s2: P): Double = {
	  		if (s1 == s2) return 0
	  		if (manhattan(s1.asInstanceOf[BoardState], s2.asInstanceOf[BoardState]) == 1) return 1
	  		else return Double.PositiveInfinity
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


	//Represents the goal state, with all tiles in numerical order from top left to bottom right, 
	//and the empty tile on the bottom right corner
	def goalState(n: Int) = {
	  	var m: Map[Pos,Tile] = Map()
	  	for (i <- 1 to n; j <- 1 to n) {
	    	//println((i,j))
	    	m = m + ((i,j) -> (j + n * (i - 1)))
	  	}
	  	val lastPos = (n,n)
	  	BoardState(Board(n, m - lastPos), lastPos)
	}
	
	//Astar is a function that implements the A* algorithm. It requires a P types as input, which is a child of the State class.
	//It inputs the start state, the goal state, the list of operators, and a heuristic function. It returns a list of states.
	def Astar [P <: State] (startState: P, goal: P, Operators: List[Operator], h: (P, P)=> Double): List[P] = {
		//We represent the frontier as a priority queue, ordered by the minimum f value.
		val frontier = PriorityQueue.empty[(Double, Double, P)](
		    Ordering.by((_: (Double, Double, P))._1).reverse
		)
		//store the explored states in a set.
		val explored = Set[P]()
		//initialize the set of explored states and the frontier to the start state.
		explored += startState
		frontier += ((h(startState, goal), 0, startState))
		while (!frontier.isEmpty) {
		  	//extract the node with the minimum f value from the frontier.
		    val stateTuple = frontier.dequeue()
		    val g = stateTuple._2
		    var state = stateTuple._3
		    if (state == goal) {
		    	//If the goal state is found, then populate a list with the 
		    	//path from startState to goal, and return it.
				var path = List[P]()
				while (state != startState) {
					val prevState: P = state.getPredecessor().asInstanceOf[P]
					path = state :: path
					state = prevState
		      	}  
		    	path = startState :: path
		    	println("explored size " + explored.size)
		    	return path
		    }
		    //Otherwise, we apply each operator to the current state, and insert the outcome into the frontier,
		    //ordering by f value, only if the resultant state isn't already explored before.
		    for (op <- Operators) {
		        val output = op(state)
		        if (output != None && !explored.contains(output.get.asInstanceOf[P])){
		          	val o: P = output.get.asInstanceOf[P]
		          	o.setPredecessor(state)
		          	frontier += ((g+state.cost(state, goal)+h(o, goal), g+state.cost(state, goal), o))
		          	explored += o
		        } 
		    }
		}
		return List[P]()
	}
	
	//manhattan() defines the Manhattan heuristic function
	def manhattan(a: BoardState, b: BoardState): Double = {
	  	//create an inverse mappings of the tiles of a and b (tile values to positions)
	  	val amap = a.board.tiles.map(_.swap)
	  	val bmap = b.board.tiles.map(_.swap)
	  	var h = 0 //the heuristic value
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
	  	//create an inverse mappings of the tiles of a and b (tile values to positions)
	  	val amap = a.board.tiles.map(_.swap)
	  	val bmap = b.board.tiles.map(_.swap)
	  	var h = 0 //the heuristic value
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
	  	while(current != goal){
	    	//println(current)
	    	var current_reverse = current.board.tiles.map(_.swap)
	      	var goal_reverse = goal.board.tiles.map(_.swap)
	    	var current_empty_pos = current.emptyPos //where the empty tile should be
	    	//var value_in_current = goal.board.tiles(current_empty_pos) //value in the position where the empty tile should be
	    	if(goal.emptyPos != current.emptyPos){
	      		//if there is a non-empty tile in the location where the empty tile should be, swap that tile with the empty tile
	      		var goal_current_empty = goal.board.tiles(current_empty_pos)
	      		//swap and update current
	      		var new_board = current.board.swap(current_reverse(goal_current_empty), current_empty_pos)
	      		var new_emptyPos = current_reverse(goal_current_empty)
	      		current = BoardState(new_board, new_emptyPos)
	      		swapCount += 1 //increment count
	    	}
	    	else{
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
	
	//Function to calculate time, for test results
	def time[R](block: => R): R = {  
    	val t0 = System.nanoTime()
    	val result = block    // call-by-name
    	val t1 = System.nanoTime()
    	println("Elapsed time: " + (t1 - t0) + "ns")
    	result
	}

	//Recursive helper function for IDA* algorithm
	def Search [P <: State] (node: P, goal: P, Operators: List[Operator], h: (P, P) => Double, g: Double, threshold: Double, l: List[P]): (Option[Double], List[P]) = {
		//The search function performs a depth-first search starting from the input node
		//provided that the states being explored are within the threshold.
		var f = g + h(node, goal)
		if (f > threshold)
			return (Some(f), Nil) //If the f value goes above the threshold, we return f.
		if (node == goal) {
			//If we find the goal, add it to a list (which will eventually contain the path from root to goal)
			var newlist: List[P] = node :: l
			return (None, newlist)
		}
		var min : Double = Double.PositiveInfinity
		//We generate all successors of the current node, and perform depth-first search 
		for (op <- Operators) {
			val output = op(node)
			if (output != None && output.get != node.predecessor) {
				val o: P = output.get.asInstanceOf[P]
				o.setPredecessor(node)
				var temp = Search(o, goal, Operators, h, g + node.cost(node, o), threshold, l)
				if (temp._1 == None) {
					var newlist: List[P] = node :: temp._2
					return (None, newlist)
				}
				if (temp._1.get < min) {
					min = temp._1.get
				}
			}
		}
		return (Some(min), Nil)
	}  


	def IDAstar [P <: State] (startState: P, goal: P, Operators: List[Operator], h: (P, P)=> Double): List[P] = {
  		var threshold = h(startState, goal)
  		var l: List[P] = Nil
  		while(true){
   			var temp = Search(startState, goal, Operators, h, 0, threshold, l)
    		if(temp._1 == None) {
    			//If the search finds the goal state, we return the path from start to goal state.
    			return temp._2
    		}
    		if(temp._1.get == Double.PositiveInfinity) {
    			//If the search terminates without finding the goal state, or any node above the threshold, 
    			//we return the empty list denoting the failure.
    			return temp._2
    		}
    		//If we don't find the goal state, but find a node that is above the threshold,
    		//we update the threshold and perform the depth-first search again.
    		threshold = temp._1.get
  		}
  		return l
	}    
	
	//Main function, entry point to the program
	def main(args: Array[String]) = {
 
	  	val ts = Map((1, 1) -> 3, (1, 2) -> 6, (1, 3) -> 2, (2, 1) -> 4, (2, 2) -> 7, (3, 1) -> 1, (3, 2) -> 5, (3, 3) -> 8)
		val b = Board(3, ts)
		val start = BoardState(b, (2,3))
		//println(start)
		val ts2 = Map (
			(1, 1) -> 5 			, (1, 3) -> 1, (1, 4) -> 4,
			(2, 1) -> 9, (2, 2) -> 7, (2, 3) -> 3, (2, 4) -> 11,
			(3, 1) -> 6, (3, 2) -> 2, (3, 3) -> 14, (3, 4) -> 8,
			(4, 1) -> 13, (4, 2) -> 10, (4, 3) -> 15, (4, 4) -> 12
		)
		val b2 = Board(4, ts2)
		val start2 = BoardState(b2, (1, 2))
		//println(goalState(4))
		val y = IDAstar(start2, goalState(4), List(Left, Right, Up, Down), manhattan: (BoardState, BoardState) => Double)
		for (i <- y) println(i)
		//println(y.length)
		
		/*-> Display a menu for the user with the following options: 
		1. A* with Manhatten distance heuristics funciton on sample input 
		2. A* with misplaced tiles heuristics funciton on sample input 
		3. A* with max-swap heuristics funciton on sample input 
		4. IDA* with Manhatten distance heuristics funciton on sample input 
		5. IDA* with misplaced tiles heuristics funciton on sample input 
		6. IDA* with max-swap heuristics funciton on sample input 
		7. Statistics from part 1 of problem 
		8. Statistics from part 2 of problem. */
		
		/*-> Use the same start state and goal state for menu options 1-6 - display start 
		and goal state and the respective paths for options 1-5.*/
		
		/*-> Input lists (list8, list15)of start configurations for 8 puzzle and 15 puzzle 
		problems from new8.txt and new15.txt, respectively.*/
		
		/*-> Part 1: Run A* with all 3 heuristics functions on all start states in list8 
		and record number of nodes expanded, cost of solution, running time on a 
		specific machine, effective branching factor - possibly tabulate these. 
		Calculate and print the average value of metric for each solution depth.*/
		
		/*-> Part 2: Run IDA* on list15 for one of the heuristics functions - 
		not sure what we are supposed to report here, spec says "run a number of 
		test cases on the 15-puzzle", "fine tune your implementation so that it 
		is as fast possible on the Linux machines in our lab", and "report the 
		most difficult problem in terms of solution depth that your implementation 
		can solve in 20 minutes." - I suppose we want to tabulate all the solution 
		depths and their respective times and find the solution that takes 
		closest to 20 minutes to be solved, without crossing the 20 minute limit.
		*/
	}
}