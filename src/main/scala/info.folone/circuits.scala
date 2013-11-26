package info.folone

import shapeless._
import nat._
import ops.hlist._
import ops.nat._

object Circuits {
  sealed abstract trait Bool
  trait True  extends   Bool
  trait False extends   Bool

  trait And[A <: Bool, B <: Bool, C <: Bool]

  object And {
    implicit def andTrue                        = new And[True, True, True] {}
    implicit def andFalse[A <: Bool, B <: Bool] = new And[A, B, False]      {} 
  }

  trait Or[A <: Bool, B <: Bool, C <: Bool]

  object Or {
    implicit def orTrue0[A <: Bool]            = new Or[A, True, True] {}
    implicit def orTrue1[B <: Bool]            = new Or[True, B, True] {}
    implicit def orFalse[A <: Bool, B <: Bool] = new Or[A, B, False]   {}
  }

  trait Not[A <: Bool, B <: Bool]

  object Not {
    implicit def notTrue  = new Not[True, False] {}
    implicit def notFalse = new Not[False, True] {}
  }

  trait AndGate[In1 <: Bool, In2 <: Bool, Out <: Bool]

  object AndGate {
    implicit def and[In1 <: Bool,
                     In2 <: Bool,
                     Out <: Bool]
      (implicit ev : And[In1, In2, Out]):
        AndGate[In1, In2, Out] =
          new AndGate[In1, In2, Out] {}
  }

  trait OrGate[In1 <: Bool, In2 <: Bool, Out <: Bool]

  object OrGate {
    implicit def or[In1 <: Bool,
                    In2 <: Bool,
                    Out <: Bool]
      (implicit ev : Or[In1, In2, Out]):
        OrGate[In1, In2, Out] =
          new OrGate[In1, In2, Out] {}
  }

  trait Inverter[In <: Bool, Out <: Bool]

  object Inverter {
    implicit def not[In <: Bool,
                     Out <: Bool]
      (implicit ev : Not[In, Out]):
        Inverter[In, Out] =
          new Inverter[In, Out] {}
  }

  trait Demux[In <: Bool, C <: HList] { type Out <: HList }

  object Demux {
    type Aux[In <: Bool,
              C <: HList,
           Out0 <: HList] =
      Demux[In, C] { type Out = Out0 }

    implicit def id[In <: Bool]: Aux[In, HNil, In :: HNil] =
      new Demux[In, HNil] { type Out = In :: HNil }
    implicit def otherwise[In <: Bool,
                            A <: Bool,
                           O1 <: Bool,
                           O2 <: Bool,
                            H <: Bool,
                            T <: HList,
                            L <: H :: T,
                         Out0 <: HList,
                        Outs1 <: HList,
                        Outs2 <: HList,
                            N <: Nat,
                            M <: Nat,
                          Res <: HList]
        (implicit ev0 : AndGate[In, H, O1],
                  ev1 : Inverter[H, A],
                  ev2 : AndGate[In, A, O2],
                  ev3 : Length.Aux[L, N],
                  ev4 : Div.Aux[N, _2, M],
                  ev5 : Split.Aux[Out0, M, (Outs1, Outs2)],
                  ev6 : Demux.Aux[O1, T, Outs1],
                  ev7 : Demux.Aux[O2, T, Outs2],
                  ev8 : Prepend.Aux[Outs1, Outs2, Res]):
        Aux[In, L, Res] =
          new Demux[In, L] { type Out = Res }
  }
}
