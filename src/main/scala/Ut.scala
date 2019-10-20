
import scala.util.{Failure, Success, Try}
import ij._

object Ut {

  type ImgCellT = Byte

  def rg = scala.util.Random
  def rndLong(i: Long): Long = (rg.nextDouble()*i).toLong
  def tm[R](msg: String, code: => R, t: Long=System.nanoTime
           ,isPrint: Boolean = false): (R,Double) = {
    if (isPrint) IJ.log(s"${msg} begin ...")
    val res = (code,(System.nanoTime() - t)/1.0e9d)
    if (isPrint || res._2 > 3.0) IJ.log(s"${msg} end; time=${res._2};")
    res
  }
  def using[A, B](x: A)(cleanup: A => Unit)(exec: A => B): Try[B] = {
    try {
      Success(exec(x))
    } catch {
      case e: Exception => Failure(e)
    } finally {
      try {
        if (null != x) { cleanup(x) }
      } catch {
        case e: Exception => IJ.log(e.toString)
      }
    }
  }
}

