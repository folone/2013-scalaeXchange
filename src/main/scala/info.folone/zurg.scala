package info.folone
 
import shapeless._
import nat._
import ops.nat._
import ops.hlist.{ Prepend, RemoveAll, Selector }
 
 
object Zurg {
  // Util
  // _22 is the largest defined
  type _25 = Succ[Succ[Succ[_22]]]
 
  // The sum
  val _60 = nat(60)
  type _60 = _60.N
 
  trait Max[A <: Nat, B <: Nat] { type Out <: Nat }
 
  object Max {
    type Aux[A <: Nat, B <: Nat, C <: Nat] = Max[A, B] { type Out = C }
 
    implicit def maxAux0[A <: Nat, B <: Nat, C <: Nat]
      (implicit lteq: LT[A, B]): Aux[A, B, B] = new Max[A, B] { type Out = B }
    implicit def maxAux1[A <: Nat, B <: Nat, C <: Nat]
      (implicit lteq: LTEq[B, A]): Aux[A, B, A] = new Max[A, B] { type Out = A }
  }
  // end util
 
  sealed abstract   trait   Toy
  case object Buzz  extends Toy
  case object Woody extends Toy
  case object Rex   extends Toy
  case object Hamm  extends Toy
 
  class Time[A <: Toy, N <: Nat]
 
  implicit val buzzTime  = new Time[Buzz.type,  _5]
  implicit val woodyTime = new Time[Woody.type, _10]
  implicit val rexTime   = new Time[Rex.type,   _20]
  implicit val hammTime  = new Time[Hamm.type,  _25]
 
  type Toys = Buzz.type :: Woody.type :: Rex.type :: Hamm.type :: HNil
 
  class Cost[L <: HList, N <: Nat]
 
  object Cost {
    implicit val costEmpty = new Cost[HNil, _0]
    implicit def costNonEmpty[X <: Toy, L <: HList, C <: Nat, S <: Nat, D <: Nat]
      (implicit time: Time[X, S], cost: Cost[L, D], max: Max.Aux[S, D, C]) =
        new Cost[X :: L, C]
  }
 
  class Split[L <: HList, P <: HList, M <: HList]
 
  object Split {
    implicit def split[L <: HList, X <: Toy, Y <: Toy, M <: HList, N <: HList]
      (implicit member0: Selector[L, X], member1: Selector[L, Y],
        removed: RemoveAll.Aux[L, X :: Y :: HNil, M]) =
          new Split[L, X :: Y :: HNil, M]
  }
 
  sealed abstract trait St
  object Left  extends  St
  object Right extends  St
 
  sealed abstract trait           MoveTo
  class Left[A <: Toy]    extends MoveTo
  class Right[A <: HList] extends MoveTo
 
  class Move[St0 <: St, Hl0 <: HList, St1 <: St, Hl1 <: HList, X <: MoveTo, D <: Nat]
 
  object Move {
    implicit def move0[L1 <: HList, L2 <: HList, D <: Nat, M <: HList]
      (implicit split: Split[L1, M, L2], cost: Cost[M, D]) =
        new Move[Left.type, L1, Right.type, L2, Right[M], D]
    implicit def move1[L1 <: HList, L2 <: HList, X <: Toy, D <: Nat, R <: HList, M <: HList]
      (implicit t: RemoveAll.Aux[Toys, L1, R], s: Selector[R, X],
        added: Prepend.Aux[X :: HNil, L1, L2], time: Time[X, D]) =
          new Move[Right.type, L1, Left.type, L2, Left[X], D]
  }
 
  class Trans[St0 <: St, Hl0 <: HList, St1 <: St, Hl1 <: HList, Rem <: HList, C <: Nat]
 
  object Trans {
    implicit def trans0 = new Trans[Right.type, HNil, Right.type, HNil, HNil, _0]
    implicit def trans1[S0 <: St, H0 <: HList, T0 <: St, T1 <: HList,
      M <: MoveTo, X <: Nat, Y <: Nat, D <: Nat, U0 <: St, U1 <: HList,
      L <: HList, N <: HList](implicit move: Move[S0, H0, T0, T1, M, X],
        trans: Trans[T0, T1, U0, U1, N, Y], append: Prepend.Aux[M :: HNil, N, L],
        res: Sum.Aux[X, Y, D]) = new Trans[S0, H0, U0, U1, L, D]
  }
 
  class Cross[M <: HList, D <: Nat]
 
  object Cross {
    implicit def cross[M <: HList, D0 <: Nat, D <: Nat]
      (implicit trans: Trans[Left.type, Toys, Right.type, HNil, M, D0], lt: LTEq[D0, D]) =
        new Cross[M, D]
  }
 
  class Solution[M <: HList]
 
  object Solution {
    implicit def solution[M <: HList](implicit cross: Cross[M, _60]) = new Solution[M]
  }
}
