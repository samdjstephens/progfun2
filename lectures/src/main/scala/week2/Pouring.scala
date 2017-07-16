package week2

import scala.collection.immutable

/**
  * Created by sam on 16/07/2017.
  */
class Pouring(capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    def change(state: State) = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State) = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State) = {
      val spaceInTo = capacity(to) - state(to)
      val amountInFrom = state(from)

      if (amountInFrom < spaceInTo) state updated (from, 0) updated (to, state(to) + amountInFrom)
      else if (amountInFrom > spaceInTo) state updated (from, state(from) - spaceInTo) updated (to, capacity(to))
      else state updated (from, 0) updated (to, capacity(to))
    }
  }

  val glasses: Range = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  class Path(history: List[Move]) {
    def endState: State = trackState(history)  // NB this is the same as: (history foldRight initialState) (_ change _)
    private def trackState(xs: List[Move]): State = xs match {
      case Nil => initialState
      case move :: xs1 => move change trackState(xs1)
    }

    def extend(move: Move) = new Path(move :: history)

    override def toString: String = (history.reverse mkString " ") + "--> " + endState
  }

  val initialPath: Path = new Path(Nil)

  def from(paths: Set[Path]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {    // Generate all possible Path extensions of all current paths
        path <- paths
        next <- moves map path.extend
      } yield next
      paths #:: from(more)  // Extend the current stream recursively (infinitely). Because its a stream the tail is lazily evaluated
    }

  val pathSets: Stream[Set[Path]] = from(Set(initialPath))

  def solutions(target: Int) = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
  }
}
