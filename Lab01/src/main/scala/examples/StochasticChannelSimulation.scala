package examples

object StochasticChannelSimulation extends App {

  import java.util.Random
  import pc.modelling.{CTMC, CTMCSimulation}
  import pc.modelling.CTMCSimulation._
  import StochasticChannel._
  import StochasticChannel.State._

  val channelAnalysis = CTMCSimulation(channel)
  timed{
    println(channelAnalysis.newSimulationTrace(IDLE, new Random).take(10).toList.mkString("\n"))
  }

  /*
  (0.0,IDLE)
(0.061021896703094575,SEND)
(0.06103319713005109,SEND)
(0.0610348720950209,DONE)
(0.5247736679290933,DONE)
(2.9306926482201616,DONE)
(5.226989895847614,DONE)
(6.36933545528588,DONE)
(6.660002091799253,DONE)
(6.667242756205486,DONE)
   */
}