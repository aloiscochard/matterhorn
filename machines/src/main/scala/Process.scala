package matterhorn

import scala.language.higherKinds

trait Is[A, B] {
  def refl: Is[A, A]
}

object Process {
  //type ProcessT m a b = MachineT m (Is a) b

  type ProcessT[M[_], A, B] = MachineT[M, ({type λ[α] = Is[A, α]})#λ, B]
}
