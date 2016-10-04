package com.mgummelt.economics

/** Set of inputs */
abstract class Suite extends Map[Product, Double] {
  def price: Double = {
    map { case (product, quantity) => product.price * quantity}.sum
  }
}

/** FoL Statement */
abstract class Statement

/** Product */
abstract class Product {
  val outputs: Set[Product] = {
    // p | p.inputs.contains(this)
    _
  }

  /** Price of this product. */
  def price: Double = {
    if (competitive) {
      p | demand(p) == supply(p)
    } else {
      demand^{-1}(quantity) | marginalCost(q) == marginalRevenue(q)
    }
  }

  /** Quantity of units produced. */
  def quantity: Double = {
    demand(price)
  }

  /** True if this product is profitable */
  def profitable: Boolean = {
    // True if, for some quantity, the demand is greater than the supply
    _
  }

  /** True if buyers and sellers are so numerous that no single buyer or seller can affect the price */
  def competitive: Boolean

  /** Set of all outputs this product can produce */
  def outputs: Set[Product] = _

  /** Quantity suppliers will produce if the Product is priced at a given price.
    *
    * NOTE: This function computes the long-run supply curve.  It doesn't take production ramp-up time into account.
    * In reality, the quantity suppliers will produce at any given time depends on the stage of production.
    * For example, a new product may be very cheap to produce, but it will have a small supply (i.e. quantity) if
    * there isn't yet a factory for it.
    *
    * dependency chain:
    *   price
    *     firm.averageCost
    *       inputs.price
    *         inputs.demand
    *           quantity
    *
    * @param price price of this Product
    */
  def supply(price: Double): Double = {
    if (competitive) {
      competitiveSupply(price);
    } else {
      _ // competitive firms have no supply curve
    }
  }

  /** Quantity suppliers will produce in a competitive market if the Product is priced at a given price. */    */
  private def competitiveSupply(price: Double): Double = {
    val copy = new Product {
      override def price = price
      override def quantity = q
    }

    val firm = new Firm {
      override val product: Product = copy
    }

    // In a competitive market, firms will enter and exit the market until there is zero profit, meaning
    // {{firm.averageCost == price}}.  Thus supply is the quantity at which this equation holds true.
    q | (firm.averageCost == price)
  }

  /** The cost to design an implementation of this product's interface.
    *
    * Related to the cost of computing {{producible(quantity, suite)}}.  This reduces down to the human creative process,
    * which is outside the scope of this model.
    */
  def designCost(quantity: Double, suite: Suite): Double = _


  /** Quantity the market will purchase at a given price.
    *
    * dependency chain:
    *   quantity
    *     output.quantity
    *       output.demand
    *       output.price
    *         output.firm.averageCost
    *           complements.price
    *             price
    *
    *   @param price price of this Product
    */
  def demand(price: Double): Double = {
    // Create a copy of this product, with {{copy.price}} set to {{price}}.  Over all of its outputs, sum the quantity
    // that would be used of this copy to create the output.
    val copy: Product = _

    copy.outputs.map { output =>
      // output.quantity must also be computed w/ copy
      val suite = inputs^{-1}(output.quantity)
      suite[copy]
    }.sum
  }

  // The cheapest suite that produces {{quantity}} units of {{this}}
  def inputs(quantity: Double): Option[Suite] = {
    val suite = possibleInputs(quantity).minBy(_.price)
    Option(suite)
  }

  // The set of suites that are able to produce {{quantity}} units of {{this}}
  def possibleInputs(quantity: Double): Set[Suite] = {
    // depends on {{production}}
    _
  }

  /** Derivative of [[production]].
    *
    * @param input
    * @return
    */
  def marginalProduct(input: Any): Double

  /** Quantity of {{this}} produced by {{suite}} */
  def production(suite: Suite): Double = {
    // depends on {{producible}}
    _
  }

  /** True if {{suite}} can produce {{quantity}} of {{this}}.
    *
    * Since the design of any product is a cost in itself, {{suite}} must include some "inventor", who must spend time
    * (and therefore money) going through a similar process as {{producible}} itself.  Thus this method is informally
    * self-referential.
    */
  def producible(quantity: Double, suite: Suite): Boolean = {
    /**
      * This predicate asks "Does there exist a path from the initial state (where {{suite}} exists) to a goal
      * state (where this product exists)".
      *
      * Hardware and software modeling are different.  I'm still not sure how to deal with both in a standard way.
      * One way to model hardware is as a set of affordances.  These are methods that run when some condition is true.
      * It could be implemented by the observer pattern.  But for now I'm going to assume software.
      *      *
      * class Object {
      *   // Enables us to distinguish between e.g. cotton with seeds stuck to it vs. cotton w/o seeds stuck to it.
      *   def isProduct(): Boolean
      *
      *   @affordance
      *   def pickup()
      *
      *   @affordance
      *   def putdown()
      *   ...
      * }
      *
      * To determine if a set of inputs can produce an output, we start the computation, and return true if we end in a
      * goal state.  A goal state is defined by a state with {{quantity}} of this product, where isProduct() is true.
      **/
    _
  }

  /** True statements on the behavior of this {{Product}} */
  def interface(): Set[Statement] = {
    _
  }
}

