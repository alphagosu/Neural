package ann

case class GeneticAlgorithm(popSize: Int) {
	val rnd = new scala.util.Random
	var population: List[List[Double]] = getPopulation(popSize,List())
	var bestScore: Double = 0.0
	var prevBestScore: Double = 0.0
	var progress: Double = 1.0
	var epsilon: Double = .1
	var exitCondition: Boolean = true
	var best: List[Double] = null
	
	def genDec(pos: Int) = {
		if (pos == 1) rnd.nextInt(5) + 1
		else if (pos == 2) rnd.nextInt(5) + 2     //if == 1, its not an ANN (and it breaks the ANN but shhhhh)
		else rnd.nextDouble()
	}
	def genDecisions:List[Double] = List(genDec(1).toDouble,genDec(2).toDouble,genDec(3))
	def getPopulation(number: Int, acc: List[List[Double]]): List[List[Double]] = {
		if( number <= 0 ) acc
		else {
			val fun = new ann.ChessFunction
			getPopulation(number-1, acc :+ (fun.getAttriLength +: genDecisions))
		}
	}
	def fitEval: List[(List[Double],Double)] = {
		for(i <- population) yield {
			var counter:Double = 0
			val max:Int = 1000
			var score:Double = 0.0
			val neural = ann.ANN(i)
			for(j <- 1 to max) {
				val fun = new ann.ChessFunction
				val answer = fun.eval
				if(fun.correctClassification != neural.forwardFeed(fun)){
					neural.learn(fun)
					counter = counter + 1
				}
			}
			score = counter / max
			(i,score)
		}	
		/*
		for(i <- population) {
			AmazonSQS sqs = new AmazonSQSClient(credentials);
			sqs.setRegion(Region.getRegion(Regions.US_EAST_1));
			CreateQueueRequest queueIn = new CreateQueueRequest("cs493-in");
			SendMessageRequest smr = new SendMessageRequest(inUrl,"message");
			smr.addMessageAttributesEntry("decision-vector-1", "value");
			smr.addMessageAttributesEntry("decision-vector-2", "value");
			smr.addMessageAttributesEntry("decision-vector-3", "value");
			smr.addMessageAttributesEntry("decision-vector-4", "value");
			smr.addMessageAttributesEntry("decision-vector-5", "value");
			sqs.sendMessage(smr);
		}
		*/
	}
	def selection = {
		val weaklings = popSize * .9
		val tupled = fitEval
		val sortedEvalPop = tupled.sortWith(_._2 > _._2)
		val temp = for(i <- sortedEvalPop) yield i._1
		bestScore = sortedEvalPop(0)._2
		population = for( i <- temp.dropRight(weaklings.toInt) ) yield i
	}
	def crossover = {
	  def getPartialList(crossItem: List[Double], pos: Int, desiredPos: Int, acc: List[Double]): List[Double] =
	  	if (pos == desiredPos) acc
	  	else getPartialList(crossItem, pos + 1, desiredPos, acc :+ crossItem(pos))
	  
	  var eliteChildren: List[List[Double]] = List()
	  for(i <- 0 until population.size if i % 2 == 0) {
	  	val pos = rnd.nextInt(2) + 1
	  		val firstChild:List[Double] = (getPartialList(population(i),0,pos,List()) :::
				getPartialList(population(i+1),pos,4,List()))
			eliteChildren = eliteChildren :+ firstChild
			val secondChild = getPartialList(population(i+1),0,pos,List()) :::
				getPartialList(population(i),pos,4,List())
			eliteChildren = eliteChildren :+ secondChild
		}
		population = eliteChildren
	}
	def mutation = {
		for(i <- population if rnd.nextInt(2) == 1) {
			val pos = rnd.nextInt(2) + 1
			val tempDecVec = i
			population = population.filter(_ != i)
			population = population :+ tempDecVec.updated(pos,genDec(pos))
		}
	}
	
	for(i <- 1 to 10){
	  selection
	  if (bestScore > prevBestScore) {
	  	progress = prevBestScore / bestScore
	  	prevBestScore = bestScore
	  }
	  if (progress < epsilon) {
	  	exitCondition = false
	  	best = population(0)
	  }
	  crossover
	  mutation
	  population = getPopulation(popSize - population.length, population)
	}
	println((best,bestScore))
}