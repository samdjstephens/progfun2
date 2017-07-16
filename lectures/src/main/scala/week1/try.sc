import scala.util.control.NonFatal

case class Success[T](x: T) extends Try[T]
case class Failure(ex: Throwable) extends Try[Nothing]

abstract class Try[+T] {
  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) }
    case fail: Failure => fail
  }
  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }}

object Try {
  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(ex) => Failure(ex)
    }
}


val x = Try(1/0)


//-  Try flatMap f flatMap g

//-  Try(x) match {
//-   case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) }
//-   case fail: Failure => fail
//-  } match {
//-   case Success(y) => try g(y) catch { case NonFatal(ex) => Failure(ex) }
//-   case fail: Failure => fail
//-  }

//-  Try(x) match {
//-   case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) } match {
//-       case Success(y) => try g(y) catch { case NonFatal(ex) => Failure(ex) }
//-       case fail: Failure => fail
//-     }
//-   case fail: Failure => fail
//-  }

//-  Try(x) match {
//-   case Success(x) => try f(x) catch { case NonFatal(ex) => Failure(ex) } match {
//-       case Success(y) => try g(y) catch { case NonFatal(ex) => Failure(ex) }
//-       case fail: Failure => fail
//-     }
//-   case fail: Failure => fail
//-  }