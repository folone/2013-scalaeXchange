package info.folone

import shapeless._
import nat._
import ops.nat._


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

  trait Move[N <: Nat, L <: Name, R <: Name, C <: Name]

  object Move {
    // Direct translation of prolog code.
    implicit def move0[L <: Name,
                       R <: Name,
                       C <: Name]:
        Move[_1, L, R, C] =
          new Move[_1, L, R, C] {}
    implicit def move1[N <: Nat,
                       M <: Nat,
                       L <: Name,
                       R <: Name,
                       C <: Name]
      (implicit ev0 : LT[_1, N],
                ev1 : Diff.Aux[N, _1, M],
                ev2 : Move[M, L, C, R],
                ev3 : Move[_1, L, R, C],
                ev4 : Move[M, C, R, L]):
        Move[N, L, R, C] =
          new Move[N, L, R, C] {}
  }

  // implicitly[Move[_3, Left.type, Right.type, Center.type]]
}
