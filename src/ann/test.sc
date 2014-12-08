package ann
 
object test {
  
  class Trainer {
  	
  }

  for(i<-1 to 5)
		GeneticAlgorithm(20)              //> (List(13.0, 3.0, 4.0, 0.27619514716126004),0.532)
                                                  //| (List(13.0, 1.0, 5.0, 0.9380826052447453),0.526)
                                                  //| (List(13.0, 5.0, 5.0, 0.9896953245189898),0.535)
                                                  //| (List(13.0, 5.0, 6.0, 0.949072003757127),0.53)
                                                  //| (List(13.0, 4.0, 4.0, 0.6548209280681497),0.527)
  /*
  var counter:Double = 0
  val max:Int = 1000
  var score:Double = 0.0
  for(i <- 1 to max) {
  	val fun = new ann.ChessFunction
  	val neural = ann.ANN(List(4,30,30,.001))
  	val answer = fun.eval
  	if(fun.correctClassification != neural.forwardFeed(fun)){
  		neural.learn(fun)
  		counter = counter + 1
  	}
  }
  score = counter / max
  */
 }