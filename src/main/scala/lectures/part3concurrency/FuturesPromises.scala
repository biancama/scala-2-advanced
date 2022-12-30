package lectures.part3concurrency


import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object FuturesPromises extends App {


  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  implicit val ec:ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors()))
  val aFuture = Future[Int] {
    calculateMeaningOfLife
  }

  println(aFuture.value) // Option[Try[Int]]

  println("Waiting for the future")
  aFuture.onComplete {
    case Success(meaningOfLife) => println(s"the meaning of life is $meaningOfLife")
    case Failure(ex) => println(s"I have failed because $ex")
  } // SOME thread

  Thread.sleep(3000)
}
