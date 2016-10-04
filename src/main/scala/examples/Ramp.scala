package examples

import annotation.affordance
import Global._

abstract class Ramp extends Physical {
  val width: Double = _;
  val height: Double = _;
  def angle = height / width;

  /** If {{obj}} is on the ramp, returns x-value of obj.  Otherwise, returns empty */
  def on(obj: Physical): Option[Double] = _;

  @affordance
  def slide() = {
    val obj: Physical = _
    val y0 = obj.y()
    val t0 = Global.time

    while on(obj) -> {
      obj.y = { () =>
        y0 - 0.5 * G * Math.pow(Global.time - t0, 2)
      }
      obj.x = { () =>
        obj.y() * 1.0 / Math.tan(angle)
      }
    }
  }
}

