def Astar (startState: State, goal: State, Operators: List[Operator], h: (State, State)=> Double): List[State] = {
  val frontier = PriorityQueue.empty[(Double, Int, State)](
    Ordering.by((_: (Double, Int, State))._1).reverse
  )
  frontier += ((h(startState, goal), 0, startState))
  while (!frontier.isEmpty) {
    val stateTuple = frontier.dequeue()
    val g = stateTuple._2
    val state = stateTuple._3
    if (state == goal) {
      var path = List[State]()
      while (state != startState) {
        val prevState = state.predecessor()
        path = state :: path
        state = prevState
      }  
      return path
    }
    state.setExplored()
    for (op <- Operators) {
        val output = op(state)
        if (newState != None && !newState.get.isExplored()){
          newState.get.setPredecessor(state)
          frontier += ((g+1+h(newState.get, goalState), g+1, newState.get))
        } 
      }
  }
  return List[State]()
}

