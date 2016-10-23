package com.mandelmoc

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import scala.annotation.tailrec

object MandelMoc {

  val Colors = List(
    Color.CYAN,
    Color.MAGENTA,
    Color.YELLOW,
    Color.RED,
    Color.GREEN,
    Color.BLUE,
    Color.WHITE,
    Color.BLACK
  )

  def start(width : Int, maxIter : Int) : BufferedImage = {
    @tailrec
    def recurse(x : Double, y : Double, cx : Double, cy : Double, iterations : Int) : Int = {
      if(iterations > maxIter || math.pow(x, 2) + math.pow(y,2) > 4.0)
      {
        iterations
      }
      else
      {
        val x1 = math.pow(x, 2) - math.pow(y, 2) + cx
        val y1 = 2 * x * y + cy
        recurse(x1, y1, cx, cy, iterations + 1)
      }
    }

    val img = new BufferedImage(width, width, BufferedImage.TYPE_INT_RGB)

    def renderPixel(index : Int) : Unit = {
      val (x, y) = (index % width, index / width)
      val (cx, cy) = ((x - (width / 2.0) )* (4.0 / width), (y - (width / 2.0)) * (4.0 / width))

      val iter = recurse(cx, cy, cx, cy, 0)
      val color = genColor(iter, maxIter)
      img.setRGB(x, y, color.getRGB)
    }

    (0 until (width * width)).par.foreach(i => renderPixel(i)) // This line is where the parallelization occurs

    img
  }

  def genColor(iter : Int, maxIter : Int) : Color = Colors(iter % Colors.length)

  def main(args: Array[String]) : Unit = {
    val width = 2560
    val img = start(width, 1000)
    try
    {
      val outputfile = new File("saved.png")
      ImageIO.write(img, "png", outputfile)
    }
    catch {
      case c : Throwable => println(s"Something went wrong: $c")
    }
  }
}
