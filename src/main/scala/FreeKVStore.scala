import KVStore.{KVStore, KVStoreState}
import cats.data.State
import cats.free.Free
import cats.~>
import KVStoreA.{Delete, Get, Put}

sealed trait KVStoreA[A]

object KVStoreA {
  final case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  final case class Get[T](key: String) extends KVStoreA[Option[T]]
  final case class Delete(key: String) extends KVStoreA[Unit]
}

object KVStore {

  //Create Free type based on ADT

  type KVStore[A] = Free[KVStoreA, A]
  type KVStoreState[A] = State[Map[String, Any], A]

  // Create smart constructors with liftF

  import cats.free.Free.liftF

  def put[T](key: String, value: T): KVStore[Unit] = liftF[KVStoreA, Unit](Put[T](key, value))

  def get[T](key: String): KVStore[Option[T]] = liftF[KVStoreA, Option[T]](Get[T](key))

  def delete(key: String): KVStore[Unit] = liftF(Delete(key))

  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      maybe <- get[T](key)
      _ <- maybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()
}

// Interpreter for the program
object PureInterpreter extends (KVStoreA ~> KVStoreState) {
  override def apply[A](fa: KVStoreA[A]): KVStoreState[A] = fa match {
      case Put(key, value) => State.modify(_.updated(key, value))
      case Get(key) => State.inspect(_.get(key).asInstanceOf[A])
      case Delete(key) => State.modify(_ - key)
  }
}

object Application extends App {

  // Build a program

  def program: KVStore[Option[Int]] = for {
      _ <- KVStore.put("black-cats", 2)
      _ <- KVStore.update[Int]("black-cats", _ + 12)
      _ <- KVStore.put("white-cats", 5)
      n <- KVStore.get[Int]("black-cats")
      _ <- KVStore.delete("white-cats")
    } yield n

  val (map, int) = program.foldMap(PureInterpreter).run(Map.empty).value
  println(map) // Map("black-cats" -> 14)
  println(int) // Some(14)
}