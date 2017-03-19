package com.mgummelt.economics

class Company {
  val riskTolerance: Double
  def target: Product = {
    Product.all.maxBy { product =>
      riskTolerance * product.profit.mean - product.profit.stddev
    }
  }
}
