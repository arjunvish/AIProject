/*==================================================
    CS:4420 Artificial Intelligence
    Spring 2017
    
    Homework 1
    
    Name: Sample solution

  ==================================================*/


//---------
// Part 1
//---------

/* Problem 1.1 */

def concatAll (l:List[String]) = {
  var s = ""
  for (e <- l)  s = s ++ e
  s
}

/*
This implementation will return the empty string if the input list is empty.
This is sensible because the concatenation of the strings in an empty
list can only be the empty string.
*/



/* Problem 1.2 */

def isIn[T](x:T, l:List[T]):Boolean = {
  for (e <- l)
    if (x == e) return true
  false
}

/*
This implementation returns false if the input list is empty.
This is consistent with the specification because, trivially,
x is not in l when l is empty.

Note the use of return which interrupts the for loop 
as soon as an element equal to x is found.
*/


/* Problem 1.3 */

def squareAll (l:List[Int]) = {
  var dl = List[Int]()
  for (e <- l)  dl = dl ++ List(e * e)
  dl
}

/*
Note how e * e is turned into a (on-element) list before being concatenated
with dl. This is necessary because ++ takes two lists as input.
*/




//---------
// Part 2
//---------

/* Problem 2.1 */

def concatAll (l:List[String]):String =
 l match {
  case Nil => ""
  case h :: t => h ++ concatAll(t)
 }


/* Problem 2.2 */

def isIn[T] (x:Int, l:List[T]):Boolean =
  l match {
    case Nil => false
    case h :: t => (h == x) || isIn(x, t)
  } 

/*
When the list l is non-empty x is not in it.
When l is non-empty x is in l iff x is l's head or in l's tail.
The second case of match captures this directly in Scala
with a Boolean expression.
*/

/* Problem 2.3 */

def squareAll (l:List[Int]):List[Int] =
  l match {
    case Nil => Nil
    case h :: t => (h * h) :: squareAll(t)
  } 


/* Problem 2.4 */

def remove (x:Int, l:List[Int]):List[Int] = 
 l match {
  case Nil => Nil
  case h :: t if x == h => remove(x, t)
  case h :: t =>  h :: remove(x, t)
  }

/* 
When l is empty, removing x from l results again in the empty list.
When l is non-empty,  if x equals the head of l removing x from l reduces 
to removing x from the tail of l; otherwise, it reduces to inserting 
the head of l into the result of removing x from the tail of l.
Note the use of the if guard in the second case statement.
*/


/* Problem 2.5 */

def replaceAll[T] (x:T, y:T, l:List[T]):List[T] =
 l match {
  case Nil => Nil
  case h :: t if h == x => y :: replaceAll(x, y, t)
  case h :: t =>  h :: replaceAll(x, y, t)
 }

/* Problem 2.6 */

def pair[A,B] (l1:List[A], l2:List[B]):List[(A, B)] =
  (l1, l2) match {
    case (Nil, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => (h1, h2) :: pair(t1, t2)
  }

/*
Note that match will fail at some point if the two input lists have 
different length. In that case, an exception will be raised by the 
system.
*/

/* Problem 2.7 */

sealed abstract class Tree[+X]
case object Empty extends Tree[Nothing]
case class Node[X](value:X, leftTree:Tree[X], rightTree:Tree[X]) extends Tree[X]

def occurs[X](x:X, t:Tree[X]): Boolean = 
  t match {
  	case Empty => false
  	case Node(v, t1, t2) => v == x || occurs(x, t1) || occurs(x, t2)
  }


/* Problem 2.8 */

def replaceLM[X](x:X, y:X, t:Tree[X]): Tree[X] = 
  t match {
  	case Empty => Empty
  	case Node(v, t1, t2) =>  {
  		if (occurs(x, t1))
  		  Node(v, replaceLM(x, y, t1), t2)
  		else if (x == v)
  		  Node(y, t1, t2)
  		else
  		  Node(v, t1, replaceLM(x, y, t2))
    }
  }


//---------
// Part 3
//---------

// A position on the puzzle board is encoded just as a pair of integers
// Each coordinate ranges from 1 to n, where n is the n of the puzzle
// position (1,1) is the top-left position in the board.
type Pos = (Int, Int)

// a puzzle tile is encoded just as an integers
type Tile = Int

      
// A board is encoded as a case class where the the cells are stored
// in an immutable map from position to tiles  
case class Board( size: Int, tiles: Map[Pos,Tile] ) {

  // swap returns a new board idential to this except that
  // the values at positioin p1 and p2 are swapped
  def swap(p1: Pos, p2: Pos) = {
    val t1 = (tiles get p1, tiles get p2) match {
      case (Some(v1), Some(v2)) => tiles + (p1 -> v2) + (p2 -> v1)
      case (Some(v1), None    ) => (tiles - p1) + (p2 -> v1) 
      case (None,     Some(v2)) => (tiles - p2) + (p1 -> v2) 
      case _                    => tiles
    }
    new Board(size, t1)
  } 
  /* Boards are converted to strings that print like this:
  +-------+
  | 1 3 2 |
  | 4   7 |
  | 6 8 5 |
  +-------+
  */
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
  /* Note: 
     An object of type Board represents a legal puzzle board in the problem 
     of size n iff 
     1) n = size > 0 
     2) tiles uniquely maps *each* non-empty tile in the n x n board 
        to a value in {1, ..., n}
     3) tiles(emptyPos) = 0 
  */
}

// sample map for a board
val ts = Map(
    (1,1) -> 2, (1,2) -> 8, (1,3) -> 3,
    (2,1) -> 1, (2,2) -> 6, (2,3) -> 4,
    (3,1) -> 7, (3,2) -> 5
    )

// sample board
val b = Board(3, ts)

// new board generated from b
val b2 = b.swap((2,3), (3,3))

// A state is encoded as a case class with a board and a the position
// of the empty cell in the board
case class State(board: Board, emptyPos: Pos) {
  /* States are converted to strings that pring like this:
     +-------+
     | 1 3 2 |
     | 4   7 |
     | 6 8 5 |
     +-------+
     (2,2)
  */
  override def toString = board.toString + " " + emptyPos + "\n"  
  /* Note: 
     An object of type State represents a legal problem state iff 
     board is a legal board and board.tiles(emptyPos) is undefined 
  */
}


// sample state using previous board b 
val s1 = State(b, (3,3))


// given n > 0, goalState generates a state with a board whose 
// cells are ordered increasingly left-to-right and top to bottom
def goalState(n: Int) = {
  var m: Map[Pos,Tile] = Map()
  for (i <- 1 to n; j <- 1 to n) {
    println((i,j))
    m = m + ((i,j) -> (j + n * (i - 1)))
  }
  val lastPos = (n,n)
  State(Board(n, m - lastPos), lastPos)
}


/* Problem 3.1 */

/*
Operators in for the n-puzzle problem are encoded as singleton subclasses (objects)
of the abstract class Operator.
*/
abstract class Operator {
  def apply(s: State): Option[State]
}

// Left operator
case object Left extends Operator {
  // the apply method returns 
  override def apply (s: State): Option[State] =
    s match {
      case State(_, (_, 1)) => None
      case State(b, (r, c)) => {
        val ep = (r, c - 1)
        Some( State(b.swap((r, c), ep), ep) )
      }
    }
  }

// sample application of Left to state s1
val Some(s2) = Left(s1)
val Some(s3) = Left(s2)

// returns None
Left(s3)


// Right operator
case object Right extends Operator {
  // the apply method returns 
  override def apply (s: State): Option[State] =
    s match {
      case State(_, (_, c)) if c == s.board.size => None
      case State(b, (r, c)) => {
        val ep = (r, c + 1)
        Some( State(b.swap((r, c), ep), ep) )
      }
    }
  }

// Up operator
case object Up extends Operator {
  // the apply method returns 
  override def apply (s: State): Option[State] =
    s match {
      case State(_, (1, _)) => None
      case State(b, (r, c)) => {
        val ep = (r - 1, c)
        Some( State(b.swap((r, c), ep), ep) )
      }
    }
  }

// Down operator
case object Down extends Operator {
  // the apply method returns 
  override def apply (s: State): Option[State] =
    s match {
      case State(_, (r, _)) if r == s.board.size => None
      case State(b, (r, c)) => {
        val ep = (r + 1, c)
        Some( State(b.swap((r, c), ep), ep) )
      }
    }
  }


/* Problem 3.2 */

// Plans are just sequences of operators

type Plan = List[Operator]

// given a state s and a plan p, attempt to execute p starting with s. 
// Return the final state if each operator is applicable to the current state;
// otherwise, raise an exception.
def execute(s: State, p: Plan): State = {
  println(s.board)
  p match {
    case Nil      => s
    case op :: p1 => 
      op(s) match {
        case None => throw new RuntimeException("Plan is infeasible") 
        case Some(s1) => execute(s1, p1)
      }
  }
}

// Executing sample plan List(Left, Left) from s1
execute(s1, List(Left, Left))

// Trying to execute an infeasible plan
execute(s1, List(Left, Left, Left))

// goal state for the 8-puzzle
val goal = goalState(3)

// define this start state
/*
     +-------+
     | 1 2 3 |
     | 4   7 |
     | 6 8 5 |
     +-------+
*/

val ms = Map((1,1) -> 1, (1,2) -> 2, (1,3) -> 3,
             (2,1) -> 4,             (2,3) -> 7,
             (3,1) -> 6, (3,2) -> 8, (3,3) -> 5
            )

val bs = Board(3, ms) 


val start = State( bs, (2,2) )


// fill in the list here with a solution plan for start
// (shorter solutions are possible)
val solution = List(Right, Down, Left, Up, Right, Down, Left, Up, Left, Down, Right, Up, Left, Down, Right, Right, Up, Left, Left, Down, Right, Right)

// verify that executing solution from start returns the goal state 
execute(start, solution)


