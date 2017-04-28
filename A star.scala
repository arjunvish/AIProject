import scala.collection.mutable.PriorityQueue

def Astar (startState: State, goal: State, Operators: List[Operator], h: (State, State)=> Double): List[State] = {
  val frontier = PriorityQueue.empty[(Double, Int, State)](
    Ordering.by((_: (Double, Int, State))._1).reverse
  )
  frontier += ((h(startState, goal), 0, startState))
  while (!frontier.isEmpty) {
    val stateTuple = frontier.dequeue()
    val g = stateTuple._2
    var state = stateTuple._3
    if (state == goal) {
      var path = List[State]()
      while (state != startState) {
        val prevState = state.getPredecessor()
        path = state :: path
        state = prevState
      }  
      return path
    }
    state.setExplored()
    for (op <- Operators) {
        val output = op(state)
        println(output)
        if (output != None && !output.get.isExplored()){
          output.get.setPredecessor(state)
          frontier += ((g+1+h(output.get, goal), g+1, output.get))
        } 
      }
  }
  return List[State]()
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

Astar(start, goalState(3), List(Left, Right, Up, Down), misplaced: (BoardState, BoardState) => Double)