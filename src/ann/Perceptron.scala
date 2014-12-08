package ann

case class Perceptron(numberOfWeights: Int) {
    val rnd = new scala.util.Random
    var prevInputs: List[Double] = List()
    var weights = List.tabulate[Double](numberOfWeights)(x =>
      {val random = rnd.nextDouble()
	  10 * random - 5}
    )
    
    def dotProduct(inputs: List[Double]):Double = (for {i <- inputs; w <- weights} yield i * w) sum
    def sigmoid(beta: Double):Double = 1.0 / (1.0 + math.pow(math.E,-(beta)))
    def sigmoidPrime(beta:Double):Double = beta * (1.0 - beta)
    def forwardFeed(inputs: List[Double]): Double = {
      prevInputs = inputs
	  sigmoid(dotProduct(inputs))
    }
    def outputNodeLearn(scalingConstant: Double, error: Double): Unit = {
	  weights = for(i <- weights) 
	    yield scalingConstant * (error * sigmoidPrime(dotProduct(prevInputs)) * forwardFeed(prevInputs)) + i
    }
    def hiddenAndInputNodeLearn(scalingConstant: Double): Unit = {
      weights = for(i <- weights) 
	    yield scalingConstant * (forwardFeed(prevInputs) * (1-forwardFeed(prevInputs)) * 
	        ((for(i <- prevInputs; w <- weights) yield sigmoidPrime(i)*w) sum))
    }
}