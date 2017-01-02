package cats.real

import scala.language.implicitConversions
import scala.language.higherKinds

object MyPrelude {

  object MySum0 {
    sealed trait Base {
      def apply[R]: R // eliminator
    }
  }

  object MyProd0 {
    sealed trait Base {
      def apply[R](onThe: =>R): R
    }
    case object MyThe extends Base {
      override def apply[R](onThe: =>R): R = onThe
    }
  }

  object MyBoolean {
    sealed trait Base {
      type It
      //type THIS = this.type

      def apply[R](onTrue: =>R, onFalse: =>R): R
      def unary_! : Base = apply(MyFalse, MyTrue)
      def ||(that: Base): Base = apply(this, that)
      def &&(that: Base): Base = apply(that, this)

      type APPLY[OnTrue, OnFalse]
    }
    sealed trait Clazz[X] extends Base {
      override type It = X
    }
    implicit case object MyTrue extends Clazz[_1_] {
      override type APPLY[OnTrue, OnFalse] = OnTrue
      override def apply[R](onTrue: =>R, onFalse: =>R): R = onTrue
    }
    implicit case object MyFalse extends Clazz[_0_] {
      override type APPLY[OnTrue, OnFalse] = OnFalse
      override def apply[R](onTrue: =>R, onFalse: =>R): R = onFalse
    }
    type TRUE = MyTrue.type
    type FALSE = MyFalse.type
    type NOT[T<:Base] = T#APPLY[FALSE, TRUE]
    type OR [A<:Base, B<:Base] = A#APPLY[A, B]
    type AND[A<:Base, B<:Base] = A#APPLY[B, A]
  }

  object MySum2 {
    sealed trait Base[T1, T2] {
      def apply[R](on1: T1=>R, on2: T2=>R): R
    }
    case class MyIn1[T1, T2](val t1: T1) extends Base[T1, T2] {
      override def apply[R](on1: T1=>R, on2: T2=>R): R = on1(t1)
    }
    case class MyIn2[T1, T2](val t2: T2) extends Base[T1, T2] {
      override def apply[R](on1: T1=>R, on2: T2=>R): R = on2(t2)
    }
  }

  object MyProd2 {
    sealed trait Base[T1, T2] {
      def apply[R](on: T1=>(T2=>R)): R
      def pr1: T1 = apply{t1=>t2=>t1}
      def pr2: T2 = apply{t1=>t2=>t2}
    }
    case class MyPair[T1, T2](val t1: T1, val t2: T2) extends Base[T1, T2] {
      override def apply[R](on: T1=>(T2=>R)): R = on(t1)(t2)
    }
  }

  object Types {
    type Initial = MySum0.Base
    type Terminal = MyProd0.Base
    val The = MyProd0.MyThe
    type Boolean = MyBoolean.Base
    val True = MyBoolean.MyTrue
    val False = MyBoolean.MyFalse
    type Sum2[T1, T2] = MySum2.Base[T1, T2]
    def In1[T1, T2]: T1 => Sum2[T1, T2] = MySum2.MyIn1[T1, T2]
    def In2[T1, T2]: T2 => Sum2[T1, T2] = MySum2.MyIn2[T1, T2]
    type Prod2[T1, T2] = MyProd2.Base[T1, T2]
    def MkPair[T1, T2]: (T1, T2) => Prod2[T1, T2] = MyProd2.MyPair[T1, T2]
    def Pr1[T1, T2]: Prod2[T1, T2] =>T1 = _.pr1
    def Pr2[T1, T2]: Prod2[T1, T2] =>T2 = _.pr2
  }

  type _0_ = Types.Initial
  type _1_ = Types.Terminal
  type _2_ = Types.Boolean
  type +[A, B] = Types.Sum2[A, B]
  type *[A, B] = Types.Prod2[A, B]

}
