import Logo.Instruction
import cats.free.Free
import cats.{Id, ~>}

object Logo {

  final case class Position(x: Double, y: Double, heading: Degree)
  final case class Degree(private val d: Int){
    val value: Int = d % 360
  }

  sealed trait Instruction[A]
  final case class Forward(position: Position, length: Int) extends Instruction[Position]
  final case class Backward(position: Position, length: Int) extends Instruction[Position]
  final case class RotateLeft(position: Position, degree: Degree) extends Instruction[Position]
  final case class RotateRight(position: Position, degree: Degree) extends Instruction[Position]
  final case class ShowPosition(position: Position) extends Instruction[Unit]
  
  def forward(position: Position, length: Int): Free[Instruction, Position] = Free.liftF(Forward(position, length))
  def backward(position: Position, length: Int): Free[Instruction, Position] = Free.liftF(Backward(position, length))
  def rotateRight(position: Position, degree: Degree): Free[Instruction, Position] = Free.liftF(RotateRight(position, degree))
  def rotateLeft(position: Position, degree: Degree): Free[Instruction, Position] = Free.liftF(RotateLeft(position, degree))
  def showPosition(position: Position): Free[Instruction, Unit] = Free.liftF(ShowPosition(position))
}

/*
object Interpreter extends (Instruction ~> Option) {
  import Logo._

  val nonNegative: (Position) => Option[Position] = {
    p => if (p.x >= 0 && p.y >= 0) Some(p) else None
  }

  override def apply[A](fa: Instruction[A]): Option[A] = fa match {
    case Forward(p, length) => Option(forward(p, length))
    case Backward(p, length) => backward(p, length)
    case RotateLeft(p, degree) => rotateLeft(p, degree)
    case RotateRight(p, degree) => rotateRight(p, degree)
    case ShowPosition(p) => println(s"showing position $p")
  }
}
*/

object LogoRunner extends App {

  import Logo._

  val program: Position => Free[Instruction, Position] = {
    start: Position =>
      for {
        pos1 <- forward(start, 10)
        pos2 <- rotateRight(pos1, Degree(90))
        pos3 <- forward(pos2, 10)
      } yield pos3
  }

  val startPos = Position(0.0D, 0.0D, Degree(0))
//  val resultPos = program(startPos).foldMap(Interpreter)
//  println(resultPos)
}
