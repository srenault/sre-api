package sre.api.finance.tasks.models

import io.circe.Decoder
import io.circe.generic.semiauto._    
import sre.api.finance.cm.CMOtpRequest

final case class CheckOtpEvent(otpRequest: CMOtpRequest)

object CheckOtpEvent {
    implicit val decoder: Decoder[CheckOtpEvent] = deriveDecoder[CheckOtpEvent]
}