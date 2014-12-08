package cs493

import java.util.ArrayList
import java.util.List
import java.util.Map.Entry

import com.amazonaws.auth.AWSCredentials
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.regions.Region
import com.amazonaws.regions.Regions
import com.amazonaws.services.sqs.AmazonSQS
import com.amazonaws.services.sqs.AmazonSQSClient
import com.amazonaws.services.sqs.model.CreateQueueRequest
import com.amazonaws.services.sqs.model.Message
import com.amazonaws.services.sqs.model.MessageAttributeValue
import com.amazonaws.services.sqs.model.ReceiveMessageRequest
import com.amazonaws.services.sqs.model.SendMessageRequest
import scala.collection.JavaConverters._

object Trainer extends App {
  var credentials = new BasicAWSCredentials(
    "AKIAIBTROGP34X2AX45A", "PdGnDSDSn0K3OIhM3iAOqdGuzJleKUuvRPvx0NlM")
  val sqs = new AmazonSQSClient(credentials)

  sqs.setRegion(Region.getRegion(Regions.US_EAST_1))

  val queueIn = new CreateQueueRequest("cs493-in")
  val queueOut = new CreateQueueRequest("cs493-out")
  val inUrl = sqs.createQueue(queueIn).getQueueUrl()
  val outUrl = sqs.createQueue(queueOut).getQueueUrl()
  var counter = 0

  while (true) {

    val rmr = new ReceiveMessageRequest()
    val attrs = new java.util.ArrayList[String]()
    attrs.add("*")
    rmr.setMessageAttributeNames(attrs)
    rmr.setQueueUrl(inUrl)
    val messages = sqs.receiveMessage(rmr).getMessages()
    if (messages.size > 0) {
      val m = messages.get(0)
      println(m.toString())
      var dv1 = new MessageAttributeValue()
      var dv2 = new MessageAttributeValue()
      var dv3 = new MessageAttributeValue()
      var dv4 = new MessageAttributeValue()
      var dv5 = new MessageAttributeValue()
      println(" MessageId:    " + m.getMessageId())
      System.out
        .println(" ReceiptHandle " + m.getReceiptHandle())
      println(" Body          " + m.getBody())
      println(" Attributes")

      for (
        entry <- m
          .getMessageAttributes().entrySet().asScala
      ) {
        // parse the decision vector here
        println("  Name:   " + entry.getKey())
        println("  Value:  " + entry.getValue())
        println(entry.getKey())
        println(entry.getValue())
        if (entry.getKey().equals("decision-vector-1")) { // or
          // whatever
          dv1 = entry.getValue()
        } else if (entry.getKey().equals("decision-vector-2")) {
          dv2 = entry.getValue()
        } else if (entry.getKey().equals("decision-vector-3")) {
          dv3 = entry.getValue()
        } else if (entry.getKey().equals("decision-vector-4")) {
          dv4 = entry.getValue()
        }
      }
        var counter:Double = 0
        val max:Int = 1000
        var score:Double = 0.0
        for(i <- 1 to max) {
        	val fun = new ChessFunction
        	val neural = ANN(List(dv1,dv2,dv3,dv4))
        	val answer = fun.eval
        	if(fun.correctClassification != neural.forwardFeed(fun)){
        		neural.learn(fun)
        		counter = counter + 1
        	}
        }
      score = counter / max
      val smr = new SendMessageRequest(outUrl,
        "message-" + counter)

      // add attributes to the message (our tuple)
      smr.addMessageAttributesEntry("decision-vector-1", dv1)
      smr.addMessageAttributesEntry("decision-vector-2", dv2)
      smr.addMessageAttributesEntry("decision-vector-3", dv3)
      smr.addMessageAttributesEntry("decision-vector-4", dv4)
      smr.addMessageAttributesEntry("decision-vector-5", dv5)

      val score = new MessageAttributeValue()

      // TODO:
      score.setStringValue("set this to the score we compute")
      score.setDataType("String")

      smr.addMessageAttributesEntry("score", score)
      sqs.sendMessage(smr)
      println("Decision was sent to: " + outUrl)

    } else {
      println("No messages...")

    }
  }

}
