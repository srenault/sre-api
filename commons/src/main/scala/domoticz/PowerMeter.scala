package sre.api
package domoticz

import java.time.LocalDate
import io.circe._
import io.circe.Decoder

case class PowerMeter1(date: LocalDate, value: Float)

object PowerMeter1 {

  implicit val decoder = new Decoder[PowerMeter1] {

    final def apply(c: HCursor): Decoder.Result[PowerMeter1] =
      for {
        date <- c.downField("d").as[LocalDate]
        value <- c.downField("v").as[String]
      } yield {
        PowerMeter1(date, value.toFloat)
      }
  }
}

case class PowerMeter2(date: LocalDate, hp: Float, hc: Float)

object PowerMeter2 {

  implicit val decoder = new Decoder[PowerMeter2] {

    final def apply(c: HCursor): Decoder.Result[PowerMeter2] =
      for {
        date <- c.downField("d").as[LocalDate]
        hp <- c.downField("v").as[String].map(_.toFloat)
        hc <- c.downField("v2").as[String].map(_.toFloat)
      } yield {
        PowerMeter2(date, hp.toFloat, hc.toFloat)
      }
  }
}

case class PowerMeter3(
    date: LocalDate,
    hpCounter: Float,
    hpUsage: Float,
    hcCounter: Float,
    hcUsage: Float
)

object PowerMeter3 {

  implicit val decoder = new Decoder[PowerMeter3] {

    final def apply(c: HCursor): Decoder.Result[PowerMeter3] =
      for {
        date <- c.downField("d").as[LocalDate]
        hpUsage <- c.downField("v").as[String].map(_.toFloat)
        hcUsage <- c.downField("v2").as[String].map(_.toFloat)
        hpCounter <- c.downField("c1").as[String].map(_.toFloat)
        hcCounter <- c.downField("c3").as[String].map(_.toFloat)
      } yield {
        PowerMeter3(date, hpCounter, hpUsage, hcCounter, hcUsage)
      }
  }
}
