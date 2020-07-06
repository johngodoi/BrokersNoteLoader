import java.time.LocalDate
import java.time.format.DateTimeFormatter

trait Operation
case class Sale(market:String, marketType:String, asset:String, assetType:String, quantity:Int, price:Double, total:Double) extends Operation
case class Purchase(market:String, marketType:String, asset:String, assetType:String, quantity:Int, price:Double, total:Double) extends Operation

object BrokersNoteParser {
  val marketPattern = "1-BOVESPA"
  val operationTypePattern = "( C | V )"
  val marketTypePattern = "(FRACIONARIO|VISTA)"
  val assetPattern = s"($marketTypePattern[A-Z \\/]+[ ]{3})"
  val assetTypePattern = "( ON | PN )"

  def convertOperationStringToOperationObject(str: String):Operation = {
    val market = marketPattern.r.findFirstIn(str)
    val operationType = operationTypePattern.r.findFirstIn(str).map(_.trim)
    val marketType = marketTypePattern.r.findFirstIn(str)
    val asset = assetPattern.r.findFirstIn(str).map(_.replaceAll(s"($marketTypePattern| {2})", "").trim)
    val assetType = assetTypePattern.r.findFirstIn(str).map(_.trim)
    val quantity = " [\\d]+ ".r.findFirstIn(str).map(_.trim.toInt)
    val prices = (for(m <- "[\\d]+,[\\d]+".r.findAllMatchIn(str)) yield m.group(0).replace(",",".").toDouble).toList
    if(operationType.contains("C")) Purchase(market.get, marketType.get, asset.get, assetType.get, quantity.get, prices.head, prices(1))
    else Sale(market.get, marketType.get, asset.get, assetType.get, quantity.get, prices.head, prices(1))
  }

  def extractOperationsList(str: String):List[Operation] = extractOperationsStringList(str).map(convertOperationStringToOperationObject _)


  def extractOperationsStringList(str: String):List[String] = {
    str.substring(
      str.indexOf("D/C"), str.indexOf("NOTA DE NEGOCIAÇÃO")
    ).replaceAll("(D/C|\r)", "")
      .split("\n")
      .filter(_.nonEmpty)
      .toList
  }

  def extractContractNoteDate(str: String): LocalDate = LocalDate.parse(str.substring(str.indexOf("Data pregão"), str.indexOf("CLEAR"))
    .replaceAll("(Data pregão|[\r\n| ])", ""),
    DateTimeFormatter.ofPattern("dd/MM/yyyy")
  )
}
