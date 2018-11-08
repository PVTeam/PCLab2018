package pc.modelling

import java.util.Random

trait CTMCSimulation[A] { self: CTMC[A] =>

  type Trace[A] = Stream[(Double,A)]

  def newSimulationTrace(a0: A, rnd: Random): Trace[A] =
    Stream.iterate( (0.0,a0) ){ case (t,a) => {
      val nextList = nextWithRate(a).toList.scanLeft((0.0, a0)) {
        case ((r,a),(r2,a2)) => (r+r2,a2) }
      if (nextList.size == 1) (t,a) else {
        val rnd1 = rnd.nextDouble() * nextList.last._1
        val choice = nextList.collectFirst{ case (p, a) if p >= rnd1 => a }.get
        (t + Math.log(1 / rnd.nextDouble()) / nextList.last._1, choice)
      }
    }}
}

object CTMCSimulation {
  def apply[A](ctmc: CTMC[A]): CTMCSimulation[A] =
    new CTMC[A] with CTMCSimulation[A]{
      override def nextWithRate(a: A) = ctmc.nextWithRate(a)
    }

  // facility to track time, just embed the computation in the input
  def timed[A](v: =>A):A = {
    val t0 = java.lang.System.nanoTime
    try{ v } finally println("Timed op (msec): "+(java.lang.System.nanoTime-t0)/1000000)
  }
}