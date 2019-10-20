import Labels._
import CellBorderPolar._
import XY._
import IJcoord._
import org.scalatest.FlatSpec

class SymmTests extends FlatSpec {
val picture = """
....................................................................................................
......111111111111111111111111111111....................111111111111111111111111111111....111111....
......111111111111111111111111111111....................111111111111.11111111111111111...11.........
....1111111.....111111111..11111111.........11........1111111111111.11111111111111111...11..........
....111111111...111111111111111111111.......11........111111111111.1111111111111111111111...........
.........1111111111111111111111111111......................1111111111111111111111111111.11..........
.........1111111111111111111111111111.......1..............1111111111111111111111111111..1111.......
....................................................................................................
"""


  "Symm" must "create Labels" in {
    info("new Labels")
    val L = new Labels
    //it must "xxx" is (pending)
    info("Label mustxxx")
    assert(Lbl.bkg == L.bkg && Lbl.obj == L.obj && Lbl.mark == L.mark)
  }

  val xsz = 11
  val sz = 111
  val a = new Area(xsz,new Array[Ut.ImgCellT](sz))
  it must "create Area" in {
    info("Area.Xsz")
    assert(xsz == a.Xsz)
    info("Area size")
    assert(sz == a.dat.size)
  }
  it must "remove CellBorderPolar" in {
    val c = new CellBorderPolar(pixels = (0 to sz-1).map(Svc.l2ij(_,xsz)).toSet)
    c.rmObj(a)
    assert(Lbl.bkg == a.dat(sz/2))
  }

  // create picture
  val strs = picture.split("\n").map(_.trim).filter(_.length>0).toArray
  val aa = Svc1.strings2area(strs)
  it must "width = 100, height = 10" in {
    info(s"width: ${aa.Xsz}")
    assert(100 === aa.Xsz)
    info(s"height: ${strs.length}")
    assert(8 === strs.length)
  }
  it must "create test area" in {
    assert(Lbl.obj === aa.dat(Svc.l2ij_1(IJcoord(44,3),aa.Xsz)))
  }
  it must "map colors" in {
    val n0 = Svc.mapColors(aa.dat,Lbl.bkg,Lbl.mark)
    val n1 = Svc.mapColors(aa.dat,Lbl.mark,Lbl.bkg)
    info(s"replaced: $n0;\n${aa.toStr()}")
    assert(Lbl.bkg === aa.dat(0))
    assert(n0 > 0 && n0 === n1)
  }
  it must "create frame" in {
    Svc.imageFrame(aa.dat,Xsz = aa.Xsz)
    info(s"\n${aa.toStr()}")
    info(s"frame at: (0,0);")
    assert(Lbl.mark === aa.dat(0))
    info(s"background at: (1,1);")
    assert(Lbl.bkg === aa.dat(Svc.l2ij_1(IJcoord(1,1),aa.Xsz)))
  }
}