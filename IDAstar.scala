import scala.collection.mutable.Queue
import scala.collection.mutable.Set

def IDAstar [P <: State] (startState: P, goal: P, Operators: List[Operator], h: (P, P)=> Double): List[P] = {
  val frontier = Queue.empty[(Double, Int, P)]
  val possibleThresholds = Queue.empty[(Double)]
  var threshold = h(startState, goal);
  possibleThresholds += threshold
  val explored = Set[P]()
  explored += startState
  frontier += ((h(startState, goal), 0, startState))
  while (!frontier.isEmpty) {
    possibleThresholds.clear()
    val stateTuple = frontier.dequeue()
    val g = stateTuple._2
    var state = stateTuple._3
    if (state == goal) {
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
    for (op <- Operators) {
        val output = op(state)
        //println(output)
        if (output != None && !explored.contains(output.get.asInstanceOf[P])){
          val o: P = output.get.asInstanceOf[P]
          if(h(o,goal) <= threshold)
          {
            o.setPredecessor(state)
            frontier += ((g+1+h(o, goal), g+1, o))
            println(frontier)
            explored += o
          }
          else
          {
            possibleThresholds += (g+1+h(o, goal))
            println(possibleThresholds)
          }
        } 
    }
      threshold = possibleThresholds.min()
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
    var x = IDAstar(start, goalState(3), List(Left, Right, Up, Down), misplaced: (BoardState, BoardState) => Double)
    println(x.length-1)
    var y = IDAstar(start, goalState(3), List(Left, Right, Up, Down), manhattan: (BoardState, BoardState) => Double)
    println(y.length-1)
    println(x==y)
