package com.mgummelt.economics

// Assume one product per firm
abstract class Firm {
  val product: Product

  // cost of producing `quantity` units
  //
  // economies of scale: decreases initially. fixed costs are amortized, purchasing power
  //                     increases, workers specialize, etc.
  // diseconomies of scale: increases eventually. inputs become scarce, production becomes less
  //                        efficient
  def cost(quantity: Double): Double = {
    product.inputs(quantity).price
  }

  // total cost
  def cost: Double = {
    cost(supply)
  }

  // cost of producing one more unit
  def marginalCost(quantity: Double): Double = {
    // derivative(cost, quantity)
    null
  }

  // average total cost of producing `quantity` units
  def averageCost(quantity: Double): Double = {
    cost(quantity) / quantity
  }

  def averageCost: Double = {
    cost / supply
  }

  // units of `product` firm will produce at `price`
  def supply(price: Double): Double = {
    // q | marginalCost(q) == marginalRevenue(q)
    null
  }

  // units produced
  def supply: Double = {
    supply(product.price)
  }

  // total revenue
  def revenue: Double = {
    product.price * supply
  }

  def marginalRevenue(quantity: Double) = {
    // in a competitive firm, marginalRevenue = price
    // derivative(revenue, quantity)
    null
  }

  // total profit
  def profit: Double = {
    revenue - cost
  }

  def competitive: Boolean = {
    !monopoly
  }

  // sole seller of the product
  def monopoly: Boolean = {
    // !E.firm | firm.product == product
    null
  }

  // a single firm can provide the product at a lower price than two or more firms at all
  // quantities demanded by the market
  def naturalMonopoly: Boolean = {
    // cost(q) is decreasing for all q demanded by the market
    null
  }
}
