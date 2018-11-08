package examples

import pc.modelling.CTMC

object StochasticChannel extends App {
  object State extends Enumeration {
    val IDLE,SEND,DONE,FAIL = Value
  }
  import State._
  type State = State.Value

  def channel: CTMC[State] = CTMC.ofTransitions(
    (IDLE,1.0,SEND),
    (SEND,100000.0,SEND),
    (SEND,200000.0,DONE),
    (SEND,100000.0,FAIL),
    (FAIL,100000.0,IDLE),
    (DONE,1.0,DONE)
  )

  State.values.foreach(s => println(s,channel.nextWithRate(s)))
}
