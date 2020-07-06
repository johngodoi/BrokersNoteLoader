import java.io.File

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.PDFTextStripper

import scala.util.{Failure, Success, Try}

object PDFTextStripper{
  def apply(startPage: Int, endPage: Int): PDFTextStripper = {
    val stripper = new PDFTextStripper()
    stripper.setStartPage(startPage)
    stripper.setEndPage(endPage)
    stripper
  }
}
case class Opts(startPage: Int, endPage: Int, filename:String)

object PdfToText {

  def main(args: Array[String]) {
    val pdfContent = Try(Opts(args(0).toInt, args(1).toInt, args(2))).recoverWith({
        case e:ArrayIndexOutOfBoundsException => Failure(new RuntimeException("Usage: pdftotext startPage endPage filename", e))
      })
      .filter(opts => opts.startPage <= opts.endPage).recoverWith({
        case e:NoSuchElementException => Failure(new RuntimeException("endPage must be >= startPage", e))
      })
      .map(opts =>
        (PDFTextStripper(opts.startPage, opts.endPage), opts.filename))
      .flatMap(opts => getTextFromPdf(opts._1, opts._2))

    pdfContent match {
      case Success(content) => println(content)
      case Failure(e) => println(exceptionToString(e))
    }
  }

  def exceptionToString(e: Throwable):String = {
    if(e == null) ""
    else s"${e.getMessage}\n${e.getStackTrace.mkString("\n")} ${exceptionToString(e.getCause)}"
  }

  def getTextFromPdf(stripper: PDFTextStripper, filename: String): Try[String] = {
    Try[PDDocument](PDDocument.load(new File(filename))).map(pdf => stripper.getText(pdf))
  }

}

