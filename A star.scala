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

var x = Astar(start, goalState(3), List(Left, Right, Up, Down), misplaced: (BoardState, BoardState) => Double)
println(x.length)
var y = Astar(start, goalState(3), List(Left, Right, Up, Down), manhattan: (BoardState, BoardState) => Double)
println(y.length)
println(x==y)