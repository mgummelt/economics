package examples

class WhitneyGin {
  var angularVelocity: Double = _
  var cotton: Option[Cotton] = _

  // affordance
  def gin(): Unit = {
    if (cotton.isDefined) {
      cotton.get.removeSeeds()
      time = time + 2 * Math.PI / angularVelocity
    }
  }
}

class Cotton {
  var hasSeeds: Boolean = _

  def removeSeeds(): Unit = {
    hasSeeds = false
  }

  def isProduct(): Boolean = {
    !hasSeeds
  }
}
