package matterhorn
package rts

trait Interruptor {
  def kill: Unit
  def interrupted: Boolean
  def newChild: Interruptor
}

object Interruptor {
  abstract class UnInterruptor extends Interruptor {
    def kill = {}
    def interrupted = false
  }

  val global = new UnInterruptor { def newChild: Interruptor = newInterruptor }
  val unintr = new UnInterruptor { def newChild: Interruptor = this }

  def newInterruptor: Interruptor = new Interruptor {
    @volatile
    private var _interrupted = false
    private var _children: List[Interruptor] = Nil
    def kill: Unit = _interrupted = true
    def interrupted: Boolean = _interrupted
    def newChild: Interruptor = {
      val intr = newInterruptor
      _children = intr :: _children.filter(!_.interrupted)
      intr
    }
  }
}
