import org.scalatest.FunSpec

class SystemSpec extends FunSpec{
  describe("A Channel"){
    import examples.SystemChannel._, examples.SystemChannel.state._
    it("should properly identify normal forms"){
      assert( !channel.normalForm(IDLE) )
      assert( !channel.normalForm(SEND) )
      assert( !channel.normalForm(FAIL) )
      assert( channel.normalForm(DONE) )
    }
    it("should properly identify next states from SEND"){
      assertResult{
        Set(SEND, FAIL, DONE)
      }{
        channel.next(SEND)
      }
    }
    it("should generate the proper 4-length paths"){
      assertResult{
        Set(List(IDLE, SEND, FAIL, IDLE), List(IDLE, SEND, SEND, FAIL),
            List(IDLE, SEND, SEND, SEND), List(IDLE, SEND, SEND, DONE))
      }{
        channel.paths(IDLE,4).toSet
      }
    }
    it("should generate a given (rather long) path") {
      assert {
        val path = List(IDLE, SEND, FAIL, IDLE, SEND, SEND, SEND, SEND, SEND, DONE)
        channel.completePaths(IDLE).contains(path)
      }
    }
  }
}
