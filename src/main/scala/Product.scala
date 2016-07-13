package com.mgummelt.economics

// Set of inputs
abstract class Suite extends Map[Product, Double] {
  def price: Double = {
    map { case (product, quantity) => product.price * quantity}.sum
  }

  //def priceForOutput(output: Product): Unit = {
  //  val inputQuantity: Array[Double] = production^{-1}(1)(this
  //}
}

abstract class Product {
  val outputs: Set[Product] = {
    // p | p.inputs.contains(this)
    null
  }

  // The price of the good
  def price: Double = {
    if (competitive) {
      // p | demand(p) == supply(p)
      null
    } else {
      // demand^(-1)(quantity) | marginalCost(q) == marginalRevenue(q)
      null
    }
  }

  // The number of units produced
  def quantity: Double = {
    demand(price)
  }

  // Eq.demand(p') = cost(p) ^ p' > p
  def profitable: Boolean = null

  // buyers and sellers are so numerous that no single buyer or seller can affect the price
  def competitive: Boolean


  // How many units the suppliers will produce at `price`
  //
  // non-decreasing
  def supply(price: Double): Double = {

    null
  }

  // How many units the market will purchase at `price`
  //
  // non-increasing
  def demand(price: Double): Double = {
    // assume one output
    val output: Product = null

    // complement suite
    val inputs: Seq[Product] = _

    // quantity of each input in the complement suite
    val inputQuantity: Array[Double] = production^{-1}(1)(inputs)

    // index of this product in the input suite
    val thisIndex = inputs.indexOf(this)

    // The cost of the rest of the inputs in the suite to produce one `output`
    val complementPrice = (0 to inputs.length-1)
      .filter(_ != thisIndex)
      .map(i => inputs(i).price * inputQuantity(i))
      .sum

    // cheapest competitor
    val competitor = output.possibleInputs(1)
      .filter(!_.contains(this))
      .minBy(_.price)

    // Customer are only willing to pay as much as the output can resell for,
    // or as much as the nearest competitor costs
    val cap = Math.min(output.price, competitor.price)

    val thisPrice = (cap - complementPrice) / inputQuantity(thisIndex)

    null
  }

  // The set of suites that are able to produce `quantity` units of `this`
  def possibleInputs(quantity: Double): Set[Suite]

  // The cheapest suite that produces `quantity` units of `this`
  def inputs(quantity: Double): Suite = possibleInputs(quantity).minBy(_.price)

  // quantity of `this` produced by `quantity` of `inputs`
  def production(inputs: Suite, quantity: Array[Double]): Double = {
    // depends on `producible`
    null
  }

  // derivative of `production`
  def marginalProduct(input: Any): Double

  // How many units the suppliers will produce at `price`
  //
  // Firms will enter and exit the market until there is zero profit, meaning
  // firm.averageCost = price.  Supply affects average cost, since it affects the demand function
  // of the inputs, and therefore their price. Thus supply is the quantity that yields this
  // equation.
  //
  // non-decreasing
  def supply(price: Double): Double = {
    val copy = new Product {
      override def price = price
      override def quantity = q
    }

    val firm = new Firm {
      override val product: Product = copy
    }

    q | (fmin(firm.averageCost _) == price)
  }

  // How many units the market will purchase at `price`
  //
  // non-increasing
  def demand(price: Double): Double = {
    // assume one output
    val output: Product = null

    // complement suite
    val inputs: Seq[Product] = _

    // quantity of each input in the complement suite
    val inputQuantity: Array[Double] = production^{-1}(1)(inputs)

    // index of this product in the input suite
    val thisIndex = inputs.indexOf(this)

    // The cost of the rest of the inputs in the suite to produce one `output`
    val complementPrice = (0 to inputs.length-1)
      .filter(_ != thisIndex)
      .map(i => inputs(i).price * inputQuantity(i))
      .sum

    // cheapest competitor
    val competitor = output.possibleInputs(1)
      .filter(!_.contains(this))
      .minBy(_.price)

    // Customer are only willing to pay as much as the output can resell for,
    // or as much as the nearest competitor costs
    val cap = Math.min(output.price, competitor.price)

    val thisPrice = (cap - complementPrice) / inputQuantity(thisIndex)

    null
  }

  // True if `suite` can produce `quantity` of `this`
  def producible(quantity: Double, suite: Suite): Boolean = {
    /**
      *
      * This predicate must operate on an "object" structure, meaning it is an object in the "meta-structure", or outer
      * structure, that talks about it, which in this case is economics.  The products in the object structure have
      * state, so their methods are not pure functions, and therefore can't be represented as functions of a
      * mathematical structure, but rather transitions on a state machine, just as register machines are represented in
      * FoL.  This "embedding" is the same relationship found in Godel's Incompleteness Theorem (and, incidentally, the
      * Completeness Theorem).
      *
      * It's possible that this too pedantically forces the functions of economic theory to be a mathematical structure.
      * If we instead modeled the entire system as a stateful machine, it's possible no embedding would be necessary.
      *
      * object structure/theory:
      *
      * structure
      *   domain - some are products
      *   functions - state(), isProduct()
      * theory
      *   axioms
      *     each object provides state transitions (i.e. methods)
      *     initial state
      *     some correspondence between state transitions and time
      *     isProduct()
      *
      * class Object {
      *   // enables us to distinguish between e.g. cotton with seeds stuck to it vs. cotton w/o seeds stuck to
      *   // it.
      *   def isProduct(): Boolean
      *
      *   // isSentient() enables us to distinguish between objects that can initiate actions, like humans, and those
      *   // that are only reactive
      *   def isSentient(): Boolean
      *
      *   // objects have first order logic `statements`, which allows us to define an object as a set, rathern than a
      *   // specific instance.  e.g. a list that is sorted vs. a fully defined list with all elements known.
      *   def statements(): Set[Statement]
      *
      *   def transition1()
      *   def transition2(var x)
      * }
      *
      * To determine if a set of inputs can produce an output, we search all paths originating at the transtitions for
      * any sentient object (e.g. human).  If we can produce an instance of the output object, where isProduct() is
      * true, then we return yes, otherwise, return no.
      **/

    null
  }
}

