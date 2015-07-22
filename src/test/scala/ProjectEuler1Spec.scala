import org.specs2._
import shapeless._
import nat._
import test.typed

class ProjectEuler1Spec extends Specification {
  def is = s2"""

  Spec for Project Euler #1
    reg       is working           $t1
    shapeless is working           $t2
                                   """

  def t1 = ProjectEuler1.e1(10) must_== 23

  def t2 = {
    type _23 = Succ[_22]
    val result = ProjectEuler1Shapeless.SumOfMultiples[_10, _3 :: _5 :: HNil]
    typed[_23](result())
    ok
  }
}
