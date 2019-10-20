import scala.collection.SortedMap

case class XException(v: String) extends Exception

case class CmdlParam(opt: String, v: String, descr: String, isOption: Boolean = false) {
  def toStr(): String =
      //s"${opt}\t${descr}\t${if (isOption) "Optional" else ""};"
    s"\t${opt}\t-- ${descr} " + (if (null == v) "" else s"default: ${v}") +
      s"; is optional = ${isOption};"
}

class Cmdl(args: Array[String], cmdlParamsIn: Array[CmdlParam], descr: String = "") {
  type Cmdl = Map[String, String]
  type CmdlPMap = Map[String, CmdlParam]
  type CmdlP = Array[CmdlParam]
  val  optSet = cmdlParamsIn.map(_.opt).toSet

  val cmdlParams = proc(args, cmdlParamsIn)

  //def get(y: String): Option[String] =
  //  try   {Some(cmdlParams(y))}
  //  catch {case e: Exception => None}

  def get(y: String) = cmdlParams(y)

  def isParams(): Boolean = {cmdlParams.size>0}
  def toStr() = cmdlParamsIn.map(_.toStr()).reduce(_ + '\n' + _)
  def usagePrint() = {
    if ("" != descr) println(descr)
    println(s"Usage: thisProgram [--option[=value] | [--option[=value]]]\nOptions:\n${toStr()}")
  }
  //
  def errorPrint(msg: String, a: Iterable[String] = args): Unit = {
    println(s"Error: $msg")
    args.foreach(println)
    usagePrint()
  }
  def isExit(a: Map[String, String]): Boolean = {
    if (a.contains("help")) {
      usagePrint()
      return true
    }
    val ouSet = a.keys.toSet
    if (!ouSet.subsetOf(optSet)) {
      errorPrint("Error: unknown command line options: ", ouSet.diff(optSet))
      return true
    }
    val d = a.filterKeys(x => !optSet.contains(x)).map(x => s"${x._1}=${x._2}").toSet
    //test required parameeters
    val p = cmdlParamsIn.filter({ v => (null == v.v) && !v.isOption }).map(_.opt).toSet
    if (!p.subsetOf(ouSet)) {
      errorPrint("Error: missing required parameters", p.diff(ouSet))
      return true
    }
    false
  }
  def proc(args: Array[String], cm0: CmdlP): Cmdl = {
    val a = args.filter(_.startsWith("--"))
      .map(x => {
        val a = x.split("=",2)
        a(0).stripPrefix("--") -> {if (a.length > 1) a(1).trim else null}
      }).toMap
    if (isExit(a)) Map[String, String]()
    else cm0.filterNot(_.isOption) //remove options
            .map(x => x.opt -> x.v).toMap ++ a.map({case (k, v) => k -> v})
  }

  if (optSet.size < cmdlParamsIn.size) {
    errorPrint("Duplicated command line options in the program configuration")
    throw XException("")
  }
}