package ann
import chess._

case class ChessFunction extends Function {
	val function = new ann.BoardGenerator
	val eval = new Evaluate
	val attributes: List[Double] = for(i <- function.getBoard._1) yield i.toDouble
	val answer = eval.evalPos(function.getBoard._2)
	def correctClassification = if (answer > 0) 1 else -1
}