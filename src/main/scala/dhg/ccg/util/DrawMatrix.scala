package dhg.ccg.util

import dhg.util._
import dhg.ccg.parse.pcfg.Chart

object DrawMatrix {

  def drawMatrix[T](m: Array[Array[T]])(f: T => String)(out: String => Unit): Unit =
    drawMatrix(m.toVector.map(_.toVector))(f)(out)

  def drawMatrix[T](m: Seq[Seq[T]])(f: T => String)(out: String => Unit): Unit = {
    val xn = m.head.length
    val yn = m.length
    val s = m.zipWithIndex.map {
      case (row, i) => row.zipWithIndex.map {
        case (cell, j) => (if (i < j) f"($i,$j)" else "") +: f(cell).splitlines
      }
    }

    val h = s.flatten.map(_.size).max
    val w = s.flatten.flatten.map(_.size).max

    val hline = Vector.fill(xn)("-" * (w + 2)).mkString("+", "+", "+")

    out(hline)
    for ((row, i) <- s.zipWithIndex) {
      for (cellRow <- 0 until h) {
        out(row.map(_.drop(cellRow).headOption.getOrElse("").padRight(w)).mkString("| ", " | ", " |"))
      }
      out(hline)
    }

  }

  def drawMatrix[T](m: Chart[T])(f: T => String)(out: String => Unit): Unit = {
    drawMatrix(
      (0 until m.length).map { i =>
        (1 to m.length).map { j =>
          if (i < j) f(m(i, j))
          else ""
        }
      })(identity)(out)
  }

  def main(args: Array[String]): Unit = {
    drawMatrix(Array(
      Array("a12\nb", "c", "j", "m"),
      Array("d", "e\nf345\ng", "k", "n"),
      Array("h", "i", "l", "o")))(identity)(println)
  }

}
