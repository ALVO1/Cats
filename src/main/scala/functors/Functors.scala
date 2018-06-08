package functors

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](f: F[A])(fa: A => B): F[B]
}

object FunctorInstances {

  import TreeDS._

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(fa: A => B): Tree[B] =
      tree match {
        case Leaf(value) => Leaf(fa(value))
        case Branch(left, right) => Branch(map(left)(fa), map(right)(fa))
      }
  }
}

object FunctorSyntax {
  implicit class FunctorOps[F[_], A](value: F[A]) {
    def map[B](fa: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map(value)(fa)
  }
}

object TreeDS {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  // Smart constructors:
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

}

object FunctorTest extends App {

  import FunctorInstances._
  import FunctorSyntax._
  import TreeDS._

  val tree: Tree[Int] =
    branch(
      branch(
        leaf(2),
        branch(
          leaf(3),
          leaf(4))),
      leaf(1))

  Console.println(tree) // Branch(Branch(Leaf(2),Branch(Leaf(3),Leaf(4))),Leaf(1))
  Console.println(tree.map(_ * 2)) // Branch(Branch(Leaf(4),Branch(Leaf(6),Leaf(8))),Leaf(2))
}
