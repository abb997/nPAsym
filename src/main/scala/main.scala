import Ut._
import ij.gui.NewImage //ImgCellT

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import ij._
import ij.measure.ResultsTable
import ij.plugin.filter.PlugInFilter
import ij.process._
import java.awt._

object Svc1 {
  def list2area(ll: ListBuffer[Array[Ut.ImgCellT]]) : Area = {
    val X = if (ll.isEmpty) 0 else ll.head.length
    val A = new Array[Ut.ImgCellT](X * ll.length)
    var n = 0
    for (y <- ll) {
      for (i <- y) {
        A(n) = i
        n += 1
      }
    }
    new Area(X,A)
  }

  // for testing
  def strings2area(ll: Array[String], bkgch: ImgCellT = '.'): Area = {
    var X : Int = -1
    var aa = new ListBuffer[Array[Ut.ImgCellT]]
    for(l <- ll) {
      if (l.length < X) // stop on first short row
        return list2area(aa)
      if (X <= 0) X = l.length
      var b : Array[Ut.ImgCellT] = Array.fill(X)(Lbl.bkg)
      for ((c,i) <- l.zip(0 until X))
        b(i) = if (bkgch == c.toChar) Lbl.bkg else Lbl.obj
      aa += b
    }
    list2area(aa)
  }
}

//case class Area[T: Numeric](Xsz: Int, dat: Array[T]) // Ysz = (dat.size / Xsz).toInt -- right side must be int
class Area(val Xsz: Int, val dat: Array[Ut.ImgCellT], val imgProc: ImageProcessor = null) { // Ysz = (dat.size / Xsz).toInt -- right side must be int

  def this(ip: ImageProcessor) {
    this(ip.getWidth,ip.getPixels.asInstanceOf[Array[Ut.ImgCellT]],ip)
  }

  def toStr(): String = {
    dat.zipWithIndex.foldLeft(""){case (x,(y,i)) =>
      x + ((if (i>0 && 0 == (i % Xsz)) "\n" else "") + ('0'.toInt + y).toChar)
    }
  }

  def drawInfoXY(N: Int, obj: CellBorderXY, rt: ResultsTable): Unit = {
    //origin and circle
    val pa = Svc.PA(obj,Xsz,Lbl.obj,dat)*100.0
    //val r = obj.min
    val (x,y) = (obj.center.x.toInt,obj.center.y.toInt)
    val str = f"${N}; (${x},${y}); PA=${pa}%.2f;"
    IJ.log(str)
    rt.incrementCounter()
    rt.addValue("#",N); rt.addValue("X",x); rt.addValue("Y",y)
    rt.addValue("PA",pa)
    if (null != imgProc) {
      //drawing
      imgProc.setFont(new Font("SansSerif",Font.PLAIN,33))
      imgProc.setAntialiasedText(true)
      imgProc.drawString(s"$N",obj.center.x.toInt,obj.center.y.toInt)
    }
  }
}
//class Area

//class dat[T: Numeric](cp: Cmdl) {
class Img(val imgPlus: ImagePlus, val AA: Area, val bkgch: ImgCellT, noiseSize: Int) {
  exec()

  //create from ij image
  def this(imp: ImagePlus, bkgch: ImgCellT = 0, noiseSize: Int = 5) {
    this(imp, new Area(imp.getProcessor),bkgch,noiseSize)
  }

  def exec() : Unit = {
    val pic = new area2sets(AA.dat, AA.Xsz)
    // build union of pixels in objects
    var objAll = (for (a <- AA.dat.zipWithIndex if (a._1 == Lbl.obj)) yield a._2).toSet

    var objs0 = mutable.HashSet[CellBorderXY]()
    var ix = 0
    var sz = 1
    // process each object and remove its pixels from the set of all objects
    while (!objAll.isEmpty) {
      val cell = pic.objGet(objAll, objAll.head, AA.Xsz)
      objAll --= cell.pixels
      if (cell.pixels.size >= noiseSize) objs0.add(cell)
      else ix += 1
    }

    val max: Int = objs0.maxBy(x => x.pixels.size).pixels.size
    val min: Int = objs0.minBy(x => x.pixels.size).pixels.size
    var minSize: Int = min + (max - min) / 10
    val msg = s"Input minimum object size [$min .. $max]"
    var m = -1
    var is0 = true
    // loop on object processing, uncomment if necessary
    //   exit if cancel button pressed in the dialogue window
    do {
      m = IJ.getNumber(msg, minSize).toInt
      //always process first loop,
      // else only if value changed
      if ((m >= 0 && m != minSize) || is0) {
        is0 = false
        minSize = Math.min(Math.max(m, min), max)
        val objs1 = objs0.filter(x => x.pixels.size >= minSize)
        IJ.log(s"removed ${objs0.size - objs1.size} objects of size < ${minSize}; (small)")
        IJ.log(s"removed ${ix} objects of size < ${noiseSize}; (noise)")

        //clear image, draw objects
        AA.dat.zipWithIndex.foreach(x => AA.dat(x._2) = Lbl.bkg)
        objs1.par.foreach(x => {
          x.pixels.foreach(AA.dat(_) = Lbl.mark)
          x.border.foreach(AA.dat(_) = Lbl.obj)
          AA.dat(Svc.l2xy_1(x.center, AA.Xsz)) = Lbl.obj
        })

        // output results
        val rt = new ResultsTable()
        objs1.toArray.sortWith { (a, b) => {
          Svc.l2xy_1(a.center, AA.Xsz) < Svc.l2xy_1(b.center, AA.Xsz)
        }}.zipWithIndex.foreach(b => AA.drawInfoXY(b._2 + 1, b._1, rt))
        imgPlus.updateAndDraw();
        rt.show("Results")
      }
    } while (m > 0)
  }
}
//Img

class nPAsym_ extends PlugInFilter {
  var minObjSize = 5
  override def setup(arg: String, imp: ImagePlus): Int = {
    if ("about" == arg) {
      showAbout()
      Lbl.DONE
    } else {
      Lbl.DOES_8G + Lbl.DOES_STACKS + Lbl.SUPPORTS_MASKING
    }
  }

  override def run(proc: ImageProcessor): Unit = {
    //new dat(proc,0.asInstanceOf[ImgCellT])
    val imgPlus = NewImage.createByteImage("Copy",proc.getWidth,proc.getHeight,1,NewImage.GRAY8)
    val ip = imgPlus.getProcessor
    ip.copyBits(proc,0,0,Blitter.COPY)
    imgPlus.show()
    new Img(imgPlus,0.asInstanceOf[Byte],minObjSize)
  }

  def showAbout(): Unit = {IJ.showMessage(
"""nPAsym is a user-friendly ImageJ plugin allowing to quantify nuclear shape asymmetry
in digital images captured from cytologic and histologic preparations.""")
  }
}
