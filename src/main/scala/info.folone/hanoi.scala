package info.folone

import shapeless._
import nat._
import ops.nat._
import ops.hlist._


/**
  * Prolog code:
  *
  * move(1, L, R, C) :-
  *   write('Move top disk from '),
  *   write(L),
  *   write(' to '),
  *   write(R),
  *   nl.
  * move(N, L, R, C) :-
  *   N > 1,
  *   M is N - 1,
  *   move(M, L, C, R),
  *   move(1, L, R, C),
  *   move(M, C, R, L).
  */
object Hanoi {
  sealed abstract trait      Name
  case object Left   extends Name
  case object Right  extends Name
  case object Center extends Name

  trait Move[N <: Nat, L <: Name, R <: Name, C <: Name] { type Out <: HList }

  object Move {
    type Aux[N <: Nat, L <: Name, R <: Name, C <: Name, Out0 <: HList] =
      Move[N, L, R, C] { type Out = Out0 }

    implicit def move0[L <: Name,
                       R <: Name,
                       C <: Name]:
        Aux[_1, L, R, C, (L, R) :: HNil] =
          new Move[_1, L, R, C] {
            type Out = (L, R) :: HNil
          }
    implicit def move1[N <: Nat,
                       M <: Nat,
                       L <: Name,
                       R <: Name,
                       C <: Name,
                       Out0 <: HList,
                       Out1 <: HList,
                       Out2 <: HList,
                       Out3 <: HList,
                       Out4 <: HList]
      (implicit ev0 : LT[_1, N],
                ev1 : Diff.Aux[N, _1, M],
                ev2 : Aux[M, L, C, R, Out0],
                ev3 : Aux[_1, L, R, C, Out1],
                ev4 : Aux[M, C, R, L, Out2],
                ev5 : Prepend.Aux[Out1, Out2, Out3],
                ev6 : Prepend.Aux[Out0, Out3, Out4]):
        Aux[N, L, R, C, Out4] =
          new Move[N, L, R, C] {
            type Out = Out4
          }
  }
  // type Res = (Left.type, Center.type) :: (Left.type, Right.type) :: (Center.type, Right.type) :: HNil
  // implicitly[Move.Aux[_2, Left.type, Right.type, Center.type, Res]]
}
