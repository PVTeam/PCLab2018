package pc.modelling

// Basically the definition of a Rewrite System
trait CoreSystem[A] {

  def next(a: A): Set[A]
}

// A rich interface over CoreSystem, with basic "analysis" helpers
trait System[A] extends CoreSystem[A] {

  // a path concept, actually just a list
  type Path[A] = List[A]

  // a normal form is a state with no outgoing transitions
  def normalForm(a: A): Boolean =
    next(a).isEmpty

  // a complete path is one with normal form in last position
  def complete(p: Path[A]): Boolean =
    normalForm(p.last)

  // a stream of paths with lengh 'depth' from state 'a'
  def paths(a: A, depth: Int): Stream[Path[A]] = {
    if (depth == 0)
      Stream()
    else if (depth == 1 || normalForm(a))
      Stream(List(a))
    else
      for (path <- paths(a, depth - 1);
           next <- next(path.last)) yield (path :+ next)
  }

  // a stream of all (finite) paths
  // might loop if the automaton has loops, use with care!
  def completePaths(a: A): Stream[Path[A]] =
    Stream.iterate(1)(_+1) flatMap (paths(a,_)) filter (complete(_))
}

object System { // Our factory of Systems

  // Intensional specification: build a System out of whatever Function
  def ofFunction[A](f: PartialFunction[A,Set[A]]): System[A] =
    new CoreSystem[A] with System[A] {
      override def next(a: A) = f.applyOrElse(a,(x: A)=>Set[A]())
    }

  // Extensional specification: build a System out of a binary relationship
  def ofRelation[A](r: Set[(A,A)]): System[A] = ofFunction{
    case a => r filter (_._1 == a) map (_._2)
  }

  // Extensional with varargs.. a->b, a->b, a->b,...
  def ofTransitions[A](r: (A,A)*): System[A] = ofRelation(r.toSet)
}