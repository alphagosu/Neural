package ann

case class ANN(decVector: List[Double]) {
	val decLength:Int = decVector(0).toInt
	var depth = decVector(1).toInt
	var layers = decVector(2).toInt
	var rate = decVector(3)
	val network = Array.tabulate[Array[Perceptron]] (layers) (x =>
		if ( x == 0 ) Array.tabulate[Perceptron] (depth) (y => new Perceptron(decLength))
		else if ( x != layers-1 ) Array.tabulate[Perceptron] (depth) (y => new Perceptron(depth))
		else Array.fill[Perceptron] (1) (new Perceptron(depth))
	) 
	
	def forwardFeed(fun: Function): Int = {
		def hiddenLayerProp(ins: List[Double], layer: Int): List[Double] = {
			if (layer == network.length-1) ins
			else hiddenLayerProp( for(i <- network(layer).toList) yield i.forwardFeed(ins), layer +1 )
		}
		val inputs: List[Double] = fun.getAttributes
	 	val firstLayerOutputs:List[Double] = for(i <- network(0).toList) yield i.forwardFeed(fun.getAttributes)
	 	val lastHiddenLayerOutputs:List[Double] = hiddenLayerProp(firstLayerOutputs, 1)
	 	fun.eval(network.last.last.forwardFeed(lastHiddenLayerOutputs)) 
	}
	def getDecision = decVector
	def learn(model: Function): Unit = {
	  def hiddenLayerBackProp(layer: Int):Unit = {
		if (layer == -1){}
		else { 
		  for(i <- network(layer).toList) 
		    i.hiddenAndInputNodeLearn(rate)
		  hiddenLayerBackProp(layer-1)
	  	}
	  }
	  
	  val guess = forwardFeed(model)
	  val error = model.getAnswer - guess
	  val squaredError = .5 * math.pow(error,2)
	  network.last.last.outputNodeLearn(rate, squaredError)
	  hiddenLayerBackProp(network.length-2)
	}
}