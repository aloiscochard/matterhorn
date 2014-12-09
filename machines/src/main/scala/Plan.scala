package matterhorn

import scala.language.higherKinds

// TODO R and Z should be existansial
case class PlanT[K[_], O, M[_], A, R, Z](
  run:  (A => M[A]) =>
        (O => M[R] => M[R]) =>
        ((Z => M[R]) => K[Z] => M[R] => M[R]) =>
        M[R] =>
        M[R]
)

object Plan {
  def empty[K[_], O, M[_], A, R, Z] = PlanT[K, O, M, A, R, Z](_ => _ => _ => identity)

  /*
yield :: o -> Plan k o ()
yield o = PlanT (\kp ke _ _ -> ke o (kp ()))
*/

  // yield
  def produce[K[_], O, M[_], Z](o: O) =
   PlanT[K, O, M, Unit, Unit, Z](kp => ke => _ => _ => ke(o)(kp(())))

  def await[K[_], O, M[_], A, R, Z]: K[O] => PlanT[K, O, M, R, R, Z] = h =>
    ??? //PlanT[K, O, M, R, R, Z](kp => _ => kr => kr(kp(h)))


    /*
awaits :: k i -> Plan k o i
awaits h = PlanT $ \kp _ kr -> kr kp h
*/

}

    /*
-- @'await' = 'awaits' 'id'@
await :: Category k => Plan (k i) o i
await = PlanT (\kp _ kr kf -> kr kp id kf)
*/


/*
newtype PlanT k o m a = PlanT
  { runPlanT :: forall r.
      (a -> m r) ->                                     -- Done a
      (o -> m r -> m r) ->                              -- Yield o (Plan k o a)
      (forall z. (z -> m r) -> k z -> m r -> m r) ->    -- forall z. Await (z -> Plan k o a) (k z) (Plan k o a)
      m r ->                                            -- Fail
      m r
  }
  */
