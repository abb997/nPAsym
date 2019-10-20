
import scala.annotation.tailrec
import ij._

object Lbl {
  val bkg : Ut.ImgCellT = -1 //white
  val obj : Ut.ImgCellT =  0  //black
  val mark: Ut.ImgCellT = -99 //-64
  val text: Ut.ImgCellT = -93
  //////////////////////////////////
  val DONE: Int = 4096
  val DOES_8G: Int = 1
  val DOES_STACKS: Int = 32
  val SUPPORTS_MASKING: Int = 64
}

case class Labels(  bkg : Ut.ImgCellT = Lbl.bkg
                  , obj : Ut.ImgCellT = Lbl.obj
                  , mark: Ut.ImgCellT = Lbl.mark
                 ) {
  def this(bkg: Ut.ImgCellT,obj: Ut.ImgCellT) =
    this(bkg,obj,Lbl.mark)
  def this() = this(Lbl.bkg,Lbl.obj,Lbl.mark)
}

case class I2(i: Int, j: Int)
case class IJcoord(i: Int, j: Int)

//cartesian coordinates
object XY {
  def apply(a: IJcoord): XY = {
    XY.apply(a.i,a.j)
  }
  def apply(i: Int, j: Int): XY = {
    XY(i.toDouble,j.toDouble)
  }
}
case class XY(x: Double = 0.0, y: Double = 0.0) {
  def +(b: XY): XY = XY(this.x+b.x,this.y+b.y)
  def /(b: Double): XY = XY(this.x/b,this.y/b)
}

case class CellBorderXY(center: XY, min: Double, max: Double, pixels: Set[Int], border: Set[Int], Xsz: Int = 0)

//polar coordinates (distance, angle)
case class RAlpha(r: Double = 0.0, a: Double = 0.0)
//cell with the border in polar coordinates
case class CellBorderPolar(
                       center: XY = XY()
                      ,min: Double = 0.0, max: Double = 0.0
                      ,border: Array[RAlpha] = Array()
                      ,pixels: Set[IJcoord] = Set()
                      ,Xsz: Int = 0
                      ){
  /*
    fills object with background (n2)
  */
  def rmObj(A: Area, n2: Ut.ImgCellT = Lbl.bkg) =
    pixels.foreach(x => A.dat(Svc.l2ij_1(x,A.Xsz)) = n2)
}

//collection of service functions
object Svc {
  /*
    if f == _ == _ then maps c0 |-> c2
    if f == _ != _ then maps all except c0 to c2
    returns: number of mapped pixels
    */
  def mapColors( A: Array[Ut.ImgCellT]
                ,c0: Ut.ImgCellT
                ,c2: Ut.ImgCellT
                ,f: (Ut.ImgCellT,Ut.ImgCellT) => Boolean = _ == _): Int = {
    var n = 0
    for (i <- 0 until A.length if (f(A(i),c0))) {
      A(i) = c2
      n += 1
    }
    n
  }

  // returns first available label
  def getAvail(v: Set[Ut.ImgCellT]
              ,min: Ut.ImgCellT = 0) = {
    val max = Math.max(128,v.size + 7)
    var i: Int = min
    // by definition of max such i always exists
    while (v(i.asInstanceOf[Ut.ImgCellT]))
      i += 1
    i.asInstanceOf[Ut.ImgCellT]
  }

  /*
    0---> x  IJcoord.i
    \
    \
    v y  IJcoord.j
    */
  def l2ij(l: Int, Xsz: Int) = IJcoord(l % Xsz,(l / Xsz).toInt)
  def l2ij_1(a: IJcoord, Xsz: Int) = {a.i + a.j * Xsz}
  def l2xy(l: Int, Xsz: Int): XY = XY(l2ij(l,Xsz)) + XY()
  def l2xy_1(a: XY, Xsz: Int): Int =
    l2ij_1(IJcoord(Math.round(a.x).toInt,Math.round(a.y).toInt),Xsz)

  /*
    Draws 1 pixel wide frame in the image,
    no inner frame, objects touching border will be removed
    */
  def imageFrame(A: Array[Ut.ImgCellT]
                 ,mark: Ut.ImgCellT = Lbl.mark
                 ,bkg: Ut.ImgCellT = Lbl.bkg
                 ,Xsz: Int) = {
    for (i <- 0 until Xsz) A(i) = mark
    for (i <- A.length - Xsz until A.length) A(i) = mark
    for (i <- 0 until A.length by Xsz) A(i) = mark
    for (i <- Xsz-1 until A.length by Xsz) A(i) = mark
  }

  /*
    converts cell number to positive angle
    quadrants are
      2|3
      -+-
      1|0
    origin  - origin
    l   - cell number
    Xsz - width of the image
    return: (r,phi) - polar coordinates, phi in [0.0,4.0[
    phi := q + (y/r)^2 = q + sin(a)^2, q in {0,1,2,3}
    opposite point is (phi + 2) % 4
    */
  def l2angle(origin: XY, l: Int, Xsz: Int) : RAlpha = xy2angle(origin,l2xy(l,Xsz))
  def xy2angle0(origin: XY, d: XY): RAlpha = {
    val (x,y) = (origin.x - d.x,origin.y - d.y)
    if ((0.0,0.0) == (x,y)) RAlpha(0.0,0.0)
    else {
      val r = x*x + y*y
      RAlpha( math.sqrt(r),
        ( if    (x>=0.0 && y>=0.0)   { 0.0 }
        else if (x> 0.0 && y< 0.0)   { 1.0 }
        else if (x< 0.0 && y<=0.0)   { 2.0 }
        else                         { 3.0 }) + (y*y / r))
    }
  }

  def l2angle_1(r: RAlpha, Xsz: Int) : Int = {
    l2xy_1(XY(r.r * Math.cos(r.a),r.r * Math.sin(r.a)),Xsz)
  }

  def xy2angle(origin: XY, d: XY): RAlpha = {
    val (x,y) = (d.x - origin.x,d.y - origin.y)
    if ((0.0,0.0) == (x,y)) RAlpha(0.0,0.0)
    else {
      val y2 = y*y
      val r = x*x + y2
      val a = math.asin(math.sqrt(y2/r))
      RAlpha(math.sqrt(r),
        if      (x>=0.0 && y>=0.0) { a }
        else if (x> 0.0 && y< 0.0) { math.Pi - a }
        else if (x< 0.0 && y<=0.0) { math.Pi + a }
        else                       { 2.0 * math.Pi - a}
      )
    }
  }

  def M(a: Array[Double]
       ,f: (Double,Int) => Double = math.pow(_,_)
       ,n: Int = 1): Double = {
    a.map(x => f(x,n)).sum / a.length
  }

  def PA( cell: CellBorderXY, Xsz: Int, ll: Ut.ImgCellT
        , A: Array[Ut.ImgCellT]
        ): Double = {
    val ix = cell.center.x.toInt
    val iy = cell.center.y.toInt
    val pp = cell.pixels.filterNot( x => {
      val a = l2ij(x, Xsz)
      cell.pixels.contains(l2ij_1(IJcoord(2 * ix - a.i, 2 * iy - a.j), Xsz))
    })
    //drawing
    pp.foreach(A(_) = ll)
    pp.size.toDouble / cell.pixels.size.toDouble
  }
  /////////////////////////////////////////////////////
} //Svc

case class ObjBorder(obj: Set[Int],border: Set[Int])

//picture split to 2 subsets object and background
class area2sets(A: Array[Ut.ImgCellT], Xsz: Int) {
  def rmBorderObjects(A: Array[Ut.ImgCellT], Xsz: Int, ll: Labels) = {
    // union of pixels in all objects ()
    val obj0 = A.zipWithIndex.filter(_._1 == ll.obj).map(_._2).toSet
    val n = A.length
    IJ.log("rmBorderObjects() ...")
    val objs =
      ((for (i <- Xsz+1 until 2 * Xsz           if (ll.obj == A(i))) yield {IJ.log(s"pix: ${Svc.l2xy(i,Xsz)}"); objGet(obj0,i,Xsz)}) ++
       (for (i <- n - 2 * Xsz until n - Xsz     if (ll.obj == A(i))) yield {IJ.log(s"pix: ${Svc.l2xy(i,Xsz)}"); objGet(obj0,i,Xsz)}) ++
       (for (i <- 1 until A.length by Xsz       if (ll.obj == A(i))) yield {IJ.log(s"pix: ${Svc.l2xy(i,Xsz)}"); objGet(obj0,i,Xsz)}) ++
       (for (i <- Xsz - 2 until A.length by Xsz if (ll.obj == A(i))) yield {IJ.log(s"pix: ${Svc.l2xy(i,Xsz)}"); objGet(obj0,i,Xsz)})).toSet
    IJ.log(s"rmBorderObjects(); found objects: ${objs.size};")
    objs.par.foreach(x => {
      IJ.log(s"rmBorderObjects(); removing object at ${x.center}; size: ${x.pixels.size};")
      x.pixels.foreach(A(_) = ll.bkg)
      x.border.foreach(A(_) = ll.bkg)
    })
  }

  var t: Double = System.currentTimeMillis
  var t1: Double = t

  // map all except object to background
  Svc.mapColors(A,Lbl.obj,Lbl.bkg,_ != _)
  for (i <- 0 until A.length if (Lbl.obj != A(i))) A(i) = Lbl.bkg
  Svc.imageFrame(A,Lbl.mark,Lbl.bkg,Xsz)
  rmBorderObjects(A,Xsz,Labels())

  //connection cpmponent of (1,1)
  // set of background pixels
  val (obj0,_) = Ut.tm("Set of background pixels"
    ,A.zipWithIndex.filter(_._1 == Lbl.bkg).map(_._2).toSet)
  IJ.log(s"(${Xsz},${(A.size/Xsz).toInt}), |all|=${A.size}, |background|=${obj0.size};")

  //connection cpmponent of (1,1)
  val obj1 = obj0.toSet
  val (ob,_) = Ut.tm("Connection component"
                    ,objGet(obj1,Xsz+1,Xsz,Labels(Lbl.obj,Lbl.bkg),isAll = false,isPrint = true))

  // following are object service functions of an object determination
  //def neighb1(l: Int, Xsz: Int): Set[Int] = Set[Int](l-1,l+1,l-Xsz,l+Xsz)
  def neighb( ret: scala.collection.mutable.Set[Int], l: Int, Xsz: Int
             ,obj: scala.collection.mutable.Set[Int]
             ,obj0: Set[Int]) = {
    // 4 neighbours
    if (!obj(l-1)   && obj0(l-1))   ret.add(l-1)
    if (!obj(l+1)   && obj0(l+1))   ret.add(l+1)
    if (!obj(l-Xsz) && obj0(l-Xsz)) ret.add(l-Xsz)
    if (!obj(l+Xsz) && obj0(l+Xsz)) ret.add(l+Xsz)
    /* // uncomment for 8 neighbours.
       // no big difference in objects geometry, but slower
    if (!obj(l-Xsz-1) && obj0(l-Xsz-1)) ret.add(l-Xsz-1)
    if (!obj(l-Xsz+1) && obj0(l-Xsz+1)) ret.add(l-Xsz+1)
    if (!obj(l+Xsz-1) && obj0(l+Xsz-1)) ret.add(l+Xsz-1)
    if (!obj(l+Xsz+1) && obj0(l+Xsz+1)) ret.add(l+Xsz+1)
    // **** */
  }
  def isBorder(l: Int, Xsz: Int, obj: Set[Int]) =
    !(obj(l-1) && obj(l+1) && obj(l-Xsz) && obj(l+Xsz))

  @tailrec
  private
  def nextStep(obj: scala.collection.mutable.Set[Int]
              ,next: scala.collection.mutable.Set[Int]
              ,obj0: Set[Int]
              ,Xsz: Int
              ,lvl: Int
  ):
  Unit = {
    if (next.nonEmpty) {
      obj ++= next
      var s2 = scala.collection.mutable.Set[Int]()
      next.foreach(y => {
        neighb(s2,y,Xsz,obj,obj0)
        A(y) = Lbl.mark
      })
      if (System.currentTimeMillis > 1000.0 + t1) {
        t1 = System.currentTimeMillis
        val tt: Double = (t1 - t) / 1000.0
        val s = f"neighb(); step=$lvl; |obj|: ${obj.size}; |border|: ${next.size}; " +
                f" t=$tt (s); avg=${lvl/tt}%.3f (step/s)"
        IJ.log(s)
        //IJ.showStatus(s"t=$tt (s); step=$lvl; avg=${lvl/tt} (step/s)")
        IJ.showProgress(obj.size.toDouble/A.length.toDouble)
      }
      nextStep(obj,s2,obj0,Xsz,lvl+1)
    }
  }
  // ^^^ these are service functions of an object determination

  def objGet(obj0: Set[Int] // set of coordinates of pixels of all objects
            ,l0: Int // initial pixel
            ,Xsz: Int
            ,ll: Labels = Labels()
            ,isAll: Boolean = true    // store information about origin, border, etc
            ,isPrint: Boolean = false
            ): CellBorderXY = {
    val lvl = 0
    t = System.currentTimeMillis
    var objx = scala.collection.mutable.Set[Int]()
    Ut.tm("processing object",nextStep(
       objx
      ,scala.collection.mutable.Set[Int](l0)
      ,obj0,Xsz,lvl),isPrint=isPrint)
    val (obj1,_) = Ut.tm("mutable.Set -> Set",objx.toSet,isPrint = isPrint)
    if (isAll) {
      val center =
        obj1.foldLeft(XY(0.0,0.0))((x,l) => x + XY(Svc.l2ij(l,Xsz))) / obj1.size.toDouble
      val (border,_) = Ut.tm("global border",obj1.par.filter(isBorder(_,Xsz,obj1)).seq.toSet)
      val dd = border.map(l => {
                val a = XY(Svc.l2ij(l,Xsz))
                Math.pow(center.x - a.x,2.0) + Math.pow(center.y - a.y,2.0)})
      CellBorderXY(center,Math.sqrt(dd.min),Math.sqrt(dd.max),obj1,border,Xsz)
    }
    else {
      //map everything except object to bkg
      IJ.log(s"objGet(); removing all;")
      for (i <- 0 until A.size) A(i) = if (ll.mark == A(i)) ll.obj else ll.bkg
      CellBorderXY(XY(-1.0, -1.0), -1.0, -1.0, A.zipWithIndex.filter(_._1 == ll.bkg).map(_._2).toSet, Set[Int](),Xsz)
    }
  }
}

