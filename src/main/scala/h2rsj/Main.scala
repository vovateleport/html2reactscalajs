package h2rsj

import scala.annotation.tailrec
import scala.collection.JavaConversions._

import java.io._

import net.ruippeixotog.scalascraper.browser.Browser
import org.jsoup.nodes.{TextNode, Node}

object App {

  def main(args: Array[String]) {
    val file = "data/input"
    val pathOutput = "data/output"
    println(pwd)

    val pw = new PrintWriter(new File(pathOutput))

    val br = new Browser
    val doc = br.parseFile(file)
    convert(pw, doc.childNodes().toSeq, 0, 0)

    pw.close()
  }

  def convert(pw:PrintWriter, nodes: Seq[Node], offset:Int, countAttrib: Int):Int = {
    nodes.foldLeft(0)((countPrev, n)=> {
      n match {
        case t:TextNode =>
          text(t) match {
            case Some(x)=>
              val sb = new StringBuilder
              sb append {if ((countPrev+countAttrib)>0) ",\n" else "\n"}
              sb append "\t" * offset
              sb.append(s""""${x}"""")
              pw.write(sb.toString)
              countPrev+1
            case None =>
              countPrev
          }
        case _=>
          val count1 = convertOneBefore(pw, n, offset, (countAttrib + countPrev)>0)
          val count2 = convert(pw, n.childNodes().toSeq, offset + 1, count1)
          convertOneAfter(pw, n, offset, count1 + count2)
          countPrev + 1
      }
    })
  }

  def text(t: TextNode):Option[String] = {
    val t0 = t.text.replaceAll("""(?m)\s+$""", "").replaceAll("""(?m)^\s+""", "")
    if (t0.length>0) Some(t0) else None
  }

  def convertOneBefore(pw:PrintWriter, n: Node, offset: Int, appendComma: Boolean):Int = {
    val sb = new StringBuilder
    sb append {if (appendComma) ",\n" else "\n"}
    sb append "\t" * offset
    sb append s"<.${n.nodeName}("

    val count =
    if (n.hasAttr("class")) {
      sb append s"""^.cls:="${n.attr("class")}""""
      1
    } else 0

    pw.write(sb.toString)
    count
  }

  def convertOneAfter(pw:PrintWriter, n: Node, offset: Int, countPrev: Int) {
    val sb = new StringBuilder
    if (countPrev > 1) {
      sb append "\n"
      sb append "\t" * offset
    }
    sb append ")"
    pw.write(sb.toString)
  }

  def pwd = new java.io.File( "." ).getCanonicalPath
}
