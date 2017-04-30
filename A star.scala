import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set

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


    var x = Astar(start, goalState(3), List(Left, Right, Up, Down), misplaced: (BoardState, BoardState) => Double)
    println(x.length-1)
    var y = Astar(start, goalState(3), List(Left, Right, Up, Down), manhattan: (BoardState, BoardState) => Double)
    println(y.length-1)
    println(x==y)
