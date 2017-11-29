import cats.{Id, ~>}

sealed trait Action[A]
final case class ReadData(port: Int) extends Action[String]
final case class TransformData(data: String) extends Action[String]
final case class WriteData(port: Int, data: String) extends Action[Unit]

object ActionFree {
  import cats.free.Free

  type ActionF[A] = Free[Action, A]

  def readData(port: Int): ActionF[String] = Free.liftF(ReadData(port))
  def transformData(data: String): ActionF[String] = Free.liftF(TransformData(data))
  def writeData(port: Int, data: String): ActionF[Unit] = Free.liftF(WriteData(port, data))
}

object Interpreter extends (Action ~> Id) {

  import ActionFree._

  override def apply[A](fa: Action[A]): Id[A] = fa match {
    case ReadData(port) => readData(port)
    case TransformData(data) => transformData(data)
    case WriteData(port, data) => writeData(port, data)
  }
}

object ReadWriteRunner extends App {

  import ActionFree._

  val program = for {
    d <- readData(123)
    t <- transformData(d)
    _ <- writeData(789, t)
  } yield ()

  program.foldMap(Interpreter)
}

