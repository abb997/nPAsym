import Ut._
import ij.gui.NewImage

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import ij._
import ij.measure.ResultsTable
import ij.plugin.filter.PlugInFilter
import ij.process._
import ij.IJ._
import java.awt._

import ij.gui._
import ij.plugin.ImageCalculator

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
    val (x,y) = (obj.center.x.toInt,obj.center.y.toInt)
    val str = f"${N}; (${x},${y}); PA=${obj.pa}%.2f;"
    IJ.log(str)
    rt.incrementCounter()
    rt.addValue("#",N); rt.addValue("X",x); rt.addValue("Y",y)
    rt.addValue("PA",obj.pa)
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
      //IJ.log(s"added at (${cell.center.x.toInt},${cell.center.y.toInt}) sz=${cell.pixels.size}") //debug
      if (cell.pixels.size >= noiseSize) objs0.add(cell)
      else ix += 1
    }

    val max: Int = objs0.map(_.pixels.size).max
    val min: Int = objs0.map(_.pixels.size).min
    val maxS: Double = objs0.map(_.pa).max
    //objs0.foreach(cell => IJ.log(s"added at (${cell.center.x.toInt},${cell.center.y.toInt}) sz=${cell.pixels.size}; pa=${cell.pa}")) //debug
    var minSize: Int = min + (max - min) / 10
    var maxSize: Int = max
    var maxSymm0: Double = maxS //currently possible maximum
    var maxSymm: Double = maxS  //user input
    val msg = s"Input minimum object size [$min .. $max]"
    var isExit=false
    // loop on object processing, uncomment if necessary
    //   exit if cancel button pressed in the dialogue window
    do {
      val gd = new GenericDialog(msg)
      gd.addNumericField(s"value >= $min", minSize, 0, 6, "pixes")
      gd.addNumericField(s"value <= $max", maxSize, 0, 6, "pixes")
      gd.addNumericField(f"nPAsym <= $maxS%.3f", maxSymm, 2)
      gd.showDialog()
      isExit = gd.wasCanceled()
      if (!isExit) {
        val mi = gd.getNextNumber.toInt
        val ma = gd.getNextNumber.toInt
        val maS = gd.getNextNumber.toDouble
        minSize = Math.min(Math.max(mi, min), max)
        maxSize = Math.max(Math.min(ma, max), min)
        maxSymm = Math.max(Math.min(maS,maxS), 0.0)
        val os1 = objs0.filter(x => x.pixels.size >= minSize && x.pixels.size <= maxSize)
        IJ.log(s"removed ${ix} objects of size < ${noiseSize}; (noise)")
        IJ.log(s"removed ${objs0.size - os1.size} objects of size between ${minSize} and ${maxSize} ; (small and big)")
        val objs1 = os1.filter(x => {x.pa <= maxSymm})
        IJ.log(s"removed ${os1.size - objs1.size} objects of PA greter than ${maxSymm}; (overlapped cells)")

        //clear image, draw objects
        AA.dat.zipWithIndex.foreach(x => AA.dat(x._2) = Lbl.bkg)
        objs1.par.foreach(x => {
          x.pixels.foreach(AA.dat(_) = Lbl.mark)
          x.border.foreach(AA.dat(_) = Lbl.obj)
          x.paSet.foreach(AA.dat(_) = Lbl.obj)
          AA.dat(Svc.l2xy_1(x.center, AA.Xsz)) = Lbl.obj
        })
 
        // output results
        val rt = new ResultsTable()
        objs1.toArray.sortWith { case (a,b) => {
          Svc.l2xy_1(a.center, AA.Xsz) < Svc.l2xy_1(b.center, AA.Xsz)
        }}.zipWithIndex.foreach {case (b,i) => AA.drawInfoXY(i + 1, b, rt)}
        imgPlus.updateAndDraw();
        rt.show("Results")
      }
    } while (!isExit) //m>0
  }
}
//Img

class nPAsym_ extends PlugInFilter {
  var minObjSize = 5

  case class ColorThreshold(mmin: Int, mmax: Int, filter: String = "pass", is0: Boolean=true) {
    def this(mmin: Int, mmax: Int, filter: String) = this(mmin-128,mmax-128,filter,false)
  }

  def prep22(ip0: ImageProcessor): ImageProcessor = {
    if (ip0.isInstanceOf[ByteProcessor])
        ip0
    else {
      val stack = ip0.convertToRGB().asInstanceOf[ColorProcessor].getHSBStack

      val thr = Array(
         ColorThreshold(  0,255) //        "Hue"
        ,ColorThreshold(  0,255) // "Saturation"
        ,ColorThreshold(166,255) // "Brightness"
        //  ColorThreshold(  0,212) //        "Hue"
        // ,ColorThreshold(100,255) // "Saturation"
        // ,ColorThreshold(166,255) // "Brightness"
        )

      val ipp =
        thr.zipWithIndex.map {case (c,i) => {
          val ip = stack.getProcessor(i+1)     //getProcessor counts from 1
          //if ("stop" == c.filter) ip.copyBits(ip,0,0,Blitter.COPY_INVERTED)
          var pp = ip.getPixels.asInstanceOf[Array[Byte]]
          for (i <- 0 until pp.length if (pp(i)<c.mmin || pp(i)>c.mmax)) {pp(i)=0xff.toByte}
          ip
        }}
      var ip = ipp(0) // stack.getProcessor(1)
      ip.copyBits(ipp(1),0,0,Blitter.AND)
      ip.copyBits(ipp(2),0,0,Blitter.AND)
      //ip.copyBits(ip,0,0,Blitter.COPY_INVERTED)
      //ip.threshold(157) //decreasing threshold increases background (white) part
      ip.threshold(99) //decreasing threshold increases background (white) part
      val pp = ip.getPixels.asInstanceOf[Array[Byte]]
      if (pp.count(_ != 0) < (pp.size/2).toInt) ip.copyBits(ip,0,0,Blitter.COPY_INVERTED)
      ip.convertToByte(false)
      ip
    }
  }

  override def setup(arg: String, imp: ImagePlus): Int = {
    if ("about" == arg) {
      showAbout()
      Lbl.DONE
    } else {
      //Lbl.DOES_8G + Lbl.DOES_STACKS + Lbl.SUPPORTS_MASKING
      Lbl.DOES_ALL + Lbl.DOES_STACKS + Lbl.SUPPORTS_MASKING
    }
  }

  override def run(proc: ImageProcessor): Unit = {
    //new dat(proc,0.asInstanceOf[ImgCellT])
    val ip0 = prep22(proc)
    val imgPlus = NewImage.createByteImage("Copy",ip0.getWidth,ip0.getHeight,1,NewImage.GRAY8)
    val ip = imgPlus.getProcessor
    ip.copyBits(ip0,0,0,Blitter.COPY)
    imgPlus.show()
    new Img(imgPlus,0.asInstanceOf[Byte],minObjSize)
  }

  def showAbout(): Unit = {IJ.showMessage(
"""nPAsym is a user-friendly ImageJ plugin allowing to quantify nuclear shape asymmetry
in digital images captured from cytologic and histologic preparations.""")
  }
}
