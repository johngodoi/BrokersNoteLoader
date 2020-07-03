import java.io.File

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.PDFTextStripper

import scala.util.Try

object PDFTextStripper{
  def apply(startPage: Int, endPage: Int): PDFTextStripper = {
    val stripper = new PDFTextStripper()
    stripper.setStartPage(startPage)
    stripper.setEndPage(endPage)
    stripper
  }
}

object PdfToText {

  def main(args: Array[String]) {
    if (args.length != 3) printUsageAndExit()

    val startPage = args(0).toInt
    val endPage = args(1).toInt
    val filename = args(2)

    // sanity check
    if (startPage > endPage) printUsageAndExit()
    val stripper = PDFTextStripper(startPage, endPage)

    getTextFromPdf(stripper, filename).recover({
      case e => s"${e.getMessage}\n${e.getStackTrace.mkString("\n")}"
    }).foreach(println(_))
  }

  def printUsageAndExit() {
    println("")
    println("Usage: pdftotext startPage endPage filename")
    println("       (endPage must be >= startPage)")
    System.exit(1)
  }

  def getTextFromPdf(stripper: PDFTextStripper, filename: String): Try[String] = {
    Try[PDDocument](PDDocument.load(new File(filename))).map(pdf => stripper.getText(pdf))
  }

}


