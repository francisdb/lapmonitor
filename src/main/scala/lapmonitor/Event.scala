package lapmonitor

import lapmonitor.UniqueId.uniqueIdToString

import java.time.Duration

sealed trait Event

sealed trait Status
object Status {

  case object None      extends Status
  case object Running   extends Status
  case object Paused    extends Status
  case object Completed extends Status

  private final val STATUS_NONE      = 0
  private final val STATUS_RUNNING   = 1
  private final val STATUS_PAUSED    = 2
  private final val STATUS_COMPLETED = 3

  def apply(b: Int): Status = b match {
    case STATUS_COMPLETED => Completed
    case STATUS_PAUSED    => Paused
    case STATUS_RUNNING   => Running
    case STATUS_NONE      => None
    case other =>
      throw new RuntimeException(s"MSG: Unexpected status $other")
  }
}
case class StatusEvent(status: Status, uuid: Array[Byte]) extends Event {
  override def toString: String = s"Status $status ${uniqueIdToString(uuid)}"
}

object LapEvent {
  def apply(tansponderId: Int, elapsedSinceStartHectoSeconds: Long, lapNumber: Long): LapEvent = {
    val realDuration = Duration.ofMillis(elapsedSinceStartHectoSeconds * 10)
    new LapEvent(tansponderId, realDuration, lapNumber)
  }
}
case class LapEvent(transponderId: Int, elapsedSinceStart: Duration, lap: Long) extends Event {
  override def toString: String = s"transponder $transponderId lap $lap at $elapsedSinceStart"
}
