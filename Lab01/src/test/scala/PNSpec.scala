import org.scalatest.FlatSpec
import pc.modelling.MSet

class PNSpec extends FlatSpec{
  "PN for mutual exclusion" should "properly generate 1-step state" in {
    import examples.MutualExclusionPN._
    import place._

    assertResult{ Set(MSet(T,N)) }{ examples.MutualExclusionPN.system.next(MSet(N,N)) }
  }

  "PN for mutual exclusion" should "properly generate 7-length paths" in {
    import examples.MutualExclusionPN._
    import place._

    val expected1 = List(MSet(N,N), MSet(T,N), MSet(T,T), MSet(C,T), MSet(T), MSet(C), MSet())
    val expected2 = List(MSet(N,N), MSet(T,N), MSet(C,N), MSet(C,T), MSet(T), MSet(C), MSet())
    val expected3 = List(MSet(N,N), MSet(T,N), MSet(C,N), MSet(N), MSet(T), MSet(C), MSet())

    assertResult{ Set(expected1,expected2,expected3) } { system.paths(MSet(N,N),7).toSet }
  }
}
