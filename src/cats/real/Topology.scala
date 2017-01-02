package cats.real

import scala.language.implicitConversions
import scala.language.higherKinds

object Topology {
  import MyPrelude._

  object PredLogic {

    // constructive mapping of logic value to types
    type Bound = { type It }

    sealed trait Base {

      // interface
      type L <: Bound
      val lExp: (L, L) => L
      val lInf: (L => L) => L

      // helpers
      implicit final class Helper(l1: L) {
        def -->(l2: L): L = lExp(l1, l2)
        def &(l2: L): L = lProd2(l1, l2)
        def |(l2: L): L = lSum2(l1, l2)
      }

      final def lSum0 = lInf { l => l }
      final def lProd0 = lInf { l => l-->l }
      final def lProd2(a: L, b: L) = lInf { l => (a-->(b-->l))-->l }
      final def lSum2(a: L, b: L) = lInf { l => (a-->l)-->((b-->l)-->l) }
      final def lIso(a: L, b: L) = lProd2 (a-->b, b-->a)

      implicit def fromBool(bool: Boolean): L = if (bool) lProd0 else lSum0
    }

    sealed trait Clazz[X <: Bound] extends Base {
      override type L = X
    }

    implicit object _2_ extends Clazz[_2_] {
      override val lExp = (a, b) => ((!a)||b)
      override val lInf = f => f(Types.True) && f(Types.False)
    }
  }

  case class Compacto(val aL: PredLogic.Base) {
    import aL._

    sealed trait Base { c1 =>
      type C
      val cInf: (C => L) => L

      def + (c2: Base) = new Clazz[c1.C + c2.C] {
        override val cInf =
          arg => c1.cInf{x1 => arg(Types.In1(x1))} & c2.cInf{x2 => arg(Types.In2(x2))}
      }

      def * (c2: Base) = new Clazz[c1.C * c2.C] {
        override val cInf =
          arg => c1.cInf{x1 => c2.cInf{x2 => arg(Types.MkPair(x1, x2))}}
      }
    }

    sealed trait Clazz[T] extends Base {
      override type C = T
    }

    object Clazz{
      def apply[T: Clazz] = implicitly[Clazz[T]]

      implicit object _0_ extends Clazz[_0_] {
        override val cInf = arg => true
      }

      implicit object _1_ extends Clazz[_1_] {
        override val cInf = arg => arg(Types.The)
      }

      implicit def Sum2 [T1: Clazz, T2: Clazz]: Clazz[T1 + T2] = Clazz[T1] + Clazz[T2]
      implicit def Prod2[T1: Clazz, T2: Clazz]: Clazz[T1 * T2] = Clazz[T1] * Clazz[T2]
    }
  }

  case class Inducto(val Comp: Compacto) {
    import Comp.aL._

    // every element refers to a compact
    trait Bound { b =>
      type C
      val iC: Comp.Base { type C = b.C }
    }

    trait BoundOf[X] extends Bound {
      override type C = X
    }

    sealed trait Base {
      type I <: Bound
      trait All { def apply(i: I): i.iC.C }
      val apply: Comp.Clazz[All]
    }

    sealed trait Clazz[X <: Bound] extends Base {
      override type I = X
    }

    object Clazz{
      def apply[X <: Bound: Clazz] = implicitly[Clazz[X]]

      // 0
      implicit object Clazz_0 extends Clazz[Nothing] {
        override object apply extends Comp.Clazz[All] {
          override val cInf = onall => true
        }
      }

      // 1
      sealed trait FamilyOne[T] extends BoundOf[T] {
        def elim(on: =>T): T
      }
      def MkOne(theOne: Comp.Base) = new FamilyOne[theOne.C] {
        override val iC = theOne
        override def elim(on: =>C) = on
      }

      implicit def ClazzOne(theOne: Comp.Base) =
        new Clazz[FamilyOne[theOne.C]] {
          case class AllOne(c: theOne.C) extends All {
            override def apply(i: I): i.iC.C = i.elim(c)
          }
          override object apply extends Comp.Clazz[All] {
            override val cInf = onall => theOne.cInf{c => onall(AllOne(c))}
          }
        }

      // +
      sealed trait FamilySum2[B1<:Base, B2<:Base] extends Bound {
        def elim(on1: B1#All, on2: B2#All): iC.C
      }
      def In1(b1: Base, b2: Base)(i1: b1.I) =
        new FamilySum2[b1.type, b2.type] {
          override type C = i1.C
          override val iC = i1.iC
          override def elim (on1: b1.All, on2: b2.All): iC.C = on1.apply(i1)
        }
      def In2(b1: Base, b2: Base)(i2: b2.I) =
        new FamilySum2[b1.type, b2.type] {
          override type C = i2.C
          override val iC = i2.iC
          override def elim (on1: b1.All, on2: b2.All): iC.C = on2.apply(i2)
        }

      implicit def ClazzSum2(b1: Base, b2: Base) =
        new Clazz[FamilySum2[b1.type, b2.type]] {
          case class AllOne(all1: b1.All, all2: b2.All) extends All {
            override def apply(i: I): i.iC.C = ???
          }
          override object apply extends Comp.Clazz[All] {
            override val cInf = onall => ???
          }
        }

    }
  }
}
