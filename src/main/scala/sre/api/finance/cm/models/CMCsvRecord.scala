package sre.api.finance.cm

import java.time.LocalDate

case class CMCsvRecord(
  date: String,
  dateValue: String,
  amount: String,
  label: String,
  balance: String
) {
  def toStatement(id: String, accountId: String): CMStatement =
    CMCsvRecord.toStatement(id, accountId, this)
}

object CMCsvRecord {
  import java.time.format.DateTimeFormatterBuilder

  private val format = new DateTimeFormatterBuilder()
    .appendPattern("dd/MM/yyyy")
    .toFormatter();

  private def parseDateOrFail(s: String): LocalDate =
    LocalDate.parse(s, format)

  def toStatement(id: String, accountId: String, csvRecord: CMCsvRecord): CMStatement = {
    val date = parseDateOrFail(csvRecord.date)
    CMStatement(
      id,
      accountId,
      date,
      csvRecord.amount.toFloat,
      csvRecord.label,
      csvRecord.balance.toFloat,
      accurateBalance = true
    )
  }

  def parseOrFail(line: String): CMCsvRecord = {
    line.split(";").toList match {
      case date :: valueDate :: amount :: label :: balance :: Nil =>
        CMCsvRecord(date, valueDate, amount, label, balance)

      case _ =>
        sys.error(s"Unable to parse $line as CMCsvLine")
    }
  }
}
