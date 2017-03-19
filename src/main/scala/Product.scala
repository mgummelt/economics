package com.mgummelt.economics

import Utils._

/** Suite. */
abstract class Suite extends Map[Product, Double] {
  /** Price. */
  def price: Double = {
    map {
      case (product, quantity) =>
        product.price * quantity
    }.sum
  }
}

/** FoL Statement. */
abstract class Statement

/** Double or Enumeration. */
abstract class Feature

/** Types of product processes. */
object Process extends Enumeration {
  val DESIGN, MANUFACTURE, PRODUCT_IMPLEMENTATION = Value
}

/** Product.
  *
  * @param interface All true statements about the Product's behavior.
  * @param quantity Quantity currently in existence.
  */
class Product(
    val interface: Set[Statement],
    val quantity: Double) {

  /** Copy constructor. */
  def this(other: Product) {
    this(other.interface, other.quantity)
  }

  /** True if this product is profitable. */
  def profitable: Boolean = {
    profit > 0
  }

  /** Total profit */
  def profit: Double = {
    revenue - cost
  }

  /** Change in total revenue from one additional unit. */
  def marginalRevenue(quantity: Double): Double = {
    // d(revenue)/d(quantity)
    _
  }

  /** Total revenue. */
  def revenue: Double = {
    price * quantity
  }

  /** Price. */
  def price: Double = {
    if (quantity > 0) {
      inv(demand)(quantity)
    } else {
      Double.PositiveInfinity
    }
  }

  /** Quantity the market will purchase at a given price. */
  def demand(price: Double): Double = {
    // Copy of this product, with [[copy.price]] set to [[price]].
    val copy: Product = new Product(this) {
      override def price: Double = price
    }

    // Sum the quantity of [[copy]] that would be used to create each output.
    copy.outputs.map { _.inputs.get(copy) }.sum
  }

  /** Total average cost. */
  def averageCost: Double = {
    cost / quantity
  }

  /** Average cost of producing the given quantity. */
  def averageCost(quantity: Double): Double = {
    cost(quantity) / quantity
  }

  /** Total cost. */
  def cost: Double = {
    cost(quantity)
  }

  /** Cost of producing one more unit. */
  def marginalCost(quantity: Double): Double = {
    // d(cost)/d(quantity)
    _
  }

  /** Total cost of producing the given quantity.
    *
    * Economies of scale: decreases initially. fixed costs are amortized, purchasing power
    *     increases, workers specialize, etc.
    * Diseconomies of scale: increases eventually. inputs become scarce, production becomes less
    *     efficient.
    */
  def cost(quantity: Double): Double = {
    inputs(quantity)
      .map(_.price)
      .getOrElse(Double.PositiveInfinity)
  }

  /** Quantity the firm will produce. */
  private def quantityInDemand: Double = {
    first(quantity => marginalCost(quantity) == marginalRevenue(quantity))
  }

  /** Simplification of [[interface]]. */
  private def features: List[Feature] = _

  /** Outputs which use this product. */
  private def outputs: Set[Product] = {
    all(product => product.inputs.get.contains(this))
  }

  /** The cheapest suite that produces this product at the current [[quantity]]. */
  private def inputs: Option[Suite] = {
    inputs(quantity)
  }

  /** The cheapest suite that produces [[quantity]] units.. */
  private def inputs(quantity: Double): Option[Suite] = {
    val suite = possibleInputs(quantity).minBy(_.price)
    Option(suite)
  }

  /** The set of suites able to produce {{quantity}} units. */
  private def possibleInputs(quantity: Double): Set[Suite] = {
    all(suite => producible(quantity, suite))
  }

  /** True if {{inputs}} can produce {{quantity}} units.
    *
    * One fixed cost of any product is design, which means {{inputs}} must include the design labor costs.
    *
    * @param quantity Quantity of this product for {{inputs}} to produce.
    * @param inputs Suite to produce this product.
    */
  private def producible(quantity: Double, inputs: Suite): Double = _
}