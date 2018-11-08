package examples

import pc.modelling.{MSet, PetriNet, System}
import pc.modelling.PetriNet._

object MutualExclusionPN extends App {

  // Specification of my data-type for states
  object place extends Enumeration {
    val N,T,C = Value
  }
  type Place = place.Value
  import place._

  def pn: PetriNet[Place]  = PetriNet[Place](
    MSet(N) ~~> MSet(T),
    MSet(T) ~~> MSet(C) ^^^ MSet(C),
    MSet(C) ~~> MSet())

  def system: System[MSet[Place]] = PetriNet.toSystem(pn)

  println(system.paths(MSet(N,N),7).toList.mkString("\n"))
}