package monoid

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
}

object MonoidInstances {
  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  implicit val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
  }

  implicit val stringConcatMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  implicit val intAddMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit def optionMonoid[A](implicit monoid: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(xVal), Some(yVal)) => Some(monoid.combine(xVal, yVal))
      case _ => None
    }
  }
}

object MonoidSyntax {

  implicit class MonoidOps[A](lhs: A) {
    def |+|(rhs: A)(implicit monoid: Monoid[A]): A = monoid.combine(lhs, rhs)
  }

}

object MonoidLaws {
  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
}

object Adder {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(monoid.combine)
}

object MonoidTest extends App {

  import MonoidSyntax.MonoidOps
  import MonoidInstances._

  Console.println(Monoid[String].combine("Hi, ", "There"))

  Console.println("Hi, " |+| "There")
  Console.println(5 |+| 12)

  Console.println(Option("Hi, ") |+| Option("There"))
  Console.println(Option(5) |+| Option(12))
  Console.println(Option(5) |+| Option(12) |+| Option(300))

  Console.println(Adder.add(List(1, 2, 3, 4)))
  Console.println(Adder.add(List("Hi", " ", "There")))
}
