package typeclasses

object EqTypeClass {
  sealed trait Eq[A] {
    def eqv(a: A, b: A): Boolean
  }

  object EqInstances {
    implicit val eqInt: Eq[Int] = new Eq[Int] {
      override def eqv(a: Int, b: Int): Boolean = a == b
    }

    implicit val eqString : Eq[String] = new Eq[String] {
      override def eqv(a: String, b: String): Boolean = a.equals(b)
    }
  }

  object EqInterface {
    def ===[A](a: A, b: A)(implicit eq: Eq[A]): Boolean = eq.eqv(a, b)
    def =!=[A](a: A, b: A)(implicit eq: Eq[A]): Boolean = ! ===(a, b)
  }

  object EqSyntax {
    implicit class EqOps[A](lhs: A) {
      def ===(rhs: A)(implicit eq: Eq[A]): Boolean = eq.eqv(lhs, rhs)
      def =!=(rhs: A)(implicit eq: Eq[A]): Boolean = ! ===(rhs)
    }
  }
}

object EqTypeClassTest extends App {

  import typeclasses.EqTypeClass.EqSyntax._
  import typeclasses.EqTypeClass.EqInstances._

  Console.println(s"545 === 545 :: ${545 === 545}")
  Console.println(s"545 === 45415 :: ${545 === 45415}")
  Console.println(s"str === str :: ${"str" === "str"}")
  Console.println(s"str === other_str :: ${"str " === "other_str"}")
}
