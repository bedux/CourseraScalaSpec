package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    println((from,end))
    val point:IndexedSeq[(Int,Int)] = for{
      y<- 0 until   src.height
      x<- from until end
    }yield (x,y)
      point.foreach{case (x,y) => {
              dst(x,y) = boxBlurKernel(src,x,y,radius)
      }
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val linePerTask:Int = src.width / Math.min(numTasks, src.width)
    val maxDrg = Math.min(numTasks-1, src.width)
    val tt = (0 until maxDrg).map{ case x => (x,linePerTask + x*linePerTask)}.toList
    val r1:List[(Int,Int)] =  (Math.min(maxDrg, src.width),Math.max(Math.min(maxDrg, src.width)*linePerTask,src.width)) :: tt

    r1.map{case (xs,xx) => task(blur(src,dst,xs,xx,radius))}.foreach{_.join()}
  }

}
