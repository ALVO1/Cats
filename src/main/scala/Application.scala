import KVStore.KVStore
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


object Application extends App {

  // Build a program

  def program: KVStore[Option[Int]] =
    for {
      _ <- KVStore.put("black-cats", 2)
      _ <- KVStore.update[Int]("black-cats", _ + 12)
      _ <- KVStore.put("white-cats", 5)
      n <- KVStore.get[Int]("black-cats")
      _ <- KVStore.delete("white-cats")
    } yield n

  // Interpreter for the program

  type KVStoreState[A] = State[Map[String, Any], A]

  /*val pureInterpreter: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
    def apply[A](storeManipulationFunction: KVStoreA[A]): KVStoreState[A] =
      storeManipulationFunction match {
        case Put(key, value) =>
          val mod: KVStoreState[A] = State.modify[Map[String, Any]](elem => elem.updated(key, value))
          mod
        case Get(key) => State(map => (map, map(key)))
        case Delete(key) => State(map => (map - key, 0.asInstanceOf[A]))
      }
  }

  val (map, int) = program.foldMap(pureInterpreter).run(Map.empty).value
  println(map)
  println(int)*/
}