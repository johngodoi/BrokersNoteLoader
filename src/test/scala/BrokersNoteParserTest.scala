import java.time.LocalDate
import java.util.Date

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BrokersNoteParserTest extends AnyFlatSpec with should.Matchers {

  "Given a string of Broker's Note" should "extract the date of the operation" in {
    BrokersNoteParser.extractContractNoteDate("""
      |Folha
      |1
      |Data pregão
      |10/08/2019
      |CLEAR CORRETORA - GRUPO XP
      |""") should be (LocalDate.of(2019,8,10))
  }

  it should "extract list of strings with operations" in {
    val results = BrokersNoteParser.extractOperationsStringList(
      """
        |Negócios realizados
        |Q Negociação C/V Tipo mercado Prazo Especificação do título Obs. (*) Quantidade Preço / Ajuste Valor Operação / Ajuste D/C
        |1-BOVESPA C FRACIONARIO AMBEV S/A          ON 1 18,05 18,05 D
        |1-BOVESPA C FRACIONARIO BANCO INTER          PN N1 7 63,56 444,92 D
        |1-BOVESPA C FRACIONARIO ITAUUNIBANCO          PN ED N1 # 1 34,09 34,09 D
        |1-BOVESPA C FRACIONARIO MAGAZ LUIZA          ON NM 1 191,99 191,99 D
        |NOTA DE NEGOCIAÇÃO
      """.stripMargin)
    results should contain theSameElementsAs List[String](
      "1-BOVESPA C FRACIONARIO AMBEV S/A          ON 1 18,05 18,05 D",
      "1-BOVESPA C FRACIONARIO BANCO INTER          PN N1 7 63,56 444,92 D",
      "1-BOVESPA C FRACIONARIO ITAUUNIBANCO          PN ED N1 # 1 34,09 34,09 D",
      "1-BOVESPA C FRACIONARIO MAGAZ LUIZA          ON NM 1 191,99 191,99 D"
    )
  }

  it should "extract list of operations" in {
    BrokersNoteParser.extractOperationsList(
      """
        |Negócios realizados
        |Q Negociação C/V Tipo mercado Prazo Especificação do título Obs. (*) Quantidade Preço / Ajuste Valor Operação / Ajuste D/C
        |1-BOVESPA C FRACIONARIO AMBEV S/A          ON 1 18,05 18,05 D
        |1-BOVESPA V FRACIONARIO BANCO INTER          PN N1 7 63,56 444,92 D
        |1-BOVESPA C FRACIONARIO ITAUUNIBANCO          PN ED N1 # 1 34,09 34,09 D
        |1-BOVESPA C FRACIONARIO MAGAZ LUIZA          ON NM 1 191,99 191,99 D
        |NOTA DE NEGOCIAÇÃO
      """.stripMargin) should contain theSameElementsAs List[Operation](
      Purchase("1-BOVESPA", "FRACIONARIO", "AMBEV S/A", "ON", 1, 18.05, 18.05),
      Sale("1-BOVESPA", "FRACIONARIO", "BANCO INTER", "PN", 7, 63.56, 444.92),
      Purchase("1-BOVESPA", "FRACIONARIO", "ITAUUNIBANCO", "PN", 1, 34.09, 34.09),
      Purchase("1-BOVESPA", "FRACIONARIO", "MAGAZ LUIZA", "ON", 1, 191.99, 191.99)
    )
  }

  "Given a purchase string" should "convert it to a Purchase object" in {
    val result = BrokersNoteParser.convertOperationStringToOperationObject("1-BOVESPA C FRACIONARIO AMBEV S/A          ON 2 18,05 36,10 D")
    result should be (Purchase("1-BOVESPA", "FRACIONARIO", "AMBEV S/A", "ON", 2, 18.05, 36.10))
  }

  "Given a sale string" should "convert it to a Sale object" in {
    val result = BrokersNoteParser.convertOperationStringToOperationObject("1-BOVESPA V FRACIONARIO BANCO INTER          PN N1 7 63,56 444,92 D")
    result should be (Sale("1-BOVESPA", "FRACIONARIO", "BANCO INTER", "PN", 7, 63.56, 444.92))
  }

}
