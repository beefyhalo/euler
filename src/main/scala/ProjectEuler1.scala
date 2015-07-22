object ProjectEuler1 {
  def e1(limit: Int) = (1 until limit).foldLeft(0) {
    case (acc, x) if x % 3 * x % 5 == 0 => acc + x
    case (acc, _)                       => acc
  }
  val out = e1(10)
  assert(out == 23)
}
object ProjectEuler1Shapeless {
  import shapeless._, nat._, ops.nat._, test.typed

  trait IfMultiple[N <: Nat, M <: HList] { type Out <: Nat }

  trait LowPriorityIfMultiple {
    type Aux[N <: Nat, M <: HList, Out0 <: Nat] = IfMultiple[N, M] { type Out = Out0 }

    implicit def isMultiple1[N <: Nat, H <: Nat, T <: HList](implicit ifMultiple: IfMultiple[N, T]): Aux[N, H :: T, ifMultiple.Out] = new IfMultiple[N, H :: T] {
      type Out = ifMultiple.Out
    }
  }

  object IfMultiple extends LowPriorityIfMultiple {
    implicit def ifMultiple0[N <: Nat]: Aux[N, HNil, _0] =
      new IfMultiple[N, HNil] {
        type Out = _0
      }

    implicit def ifMultiple2[N <: Nat, H <: Nat, T <: HList](implicit mod: Mod.Aux[N, H, _0]): Aux[N, H :: T, N] = new IfMultiple[N, H :: T] {
      type Out = N
    }
  }

  trait SumOfMultiples[N <: Nat, M <: HList] extends DepFn0 { type Out <: Nat }

  object SumOfMultiples {
    type Aux[N <: Nat, M <: HList, Out0 <: Nat] = SumOfMultiples[N, M] { type Out = Out0 }

    def apply[N <: Nat, M <: HList](implicit som: SumOfMultiples[N, M]): Aux[N, M, som.Out] = som

    implicit def sum0[M <: HList]: Aux[_1, M, _0] =
      new SumOfMultiples[_1, M] {
        type Out = _0
        def apply() = _0
      }

    implicit def sumN[P <: Nat, M <: HList, NV <: Nat, PT <: Nat, NT <: Nat](implicit ifMultiple: IfMultiple.Aux[P, M, NV],
                                                                             som: Aux[P, M, PT],
                                                                             sum: Sum.Aux[NV, PT, NT],
                                                                             wit: Witness.Aux[NT]): Aux[Succ[P], M, NT] =
      new SumOfMultiples[Succ[P], M] {
        type Out = NT
        def apply() = wit.value
      }
  }

}

