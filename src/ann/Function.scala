package ann

trait Function {
	val attributes:List[Double]
	val answer:Int
	val bias:Double = 1.0
	
	def getAttributes: List[Double] = attributes :+ bias
	def getAttriLength:Double = (attributes.length + 1)
	def getAnswer = answer
	def eval(sum: Double): Int = if( sum < .5 ) 1 else -1
}