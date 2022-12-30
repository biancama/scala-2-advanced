package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {
  /*
     interface Runnable {
       public void run()
     }
    */
  // JVM threads
  val runnable = new Runnable {
    override def run(): Unit = println("Running in parallel")
  }
  val aThread = new Thread(runnable)
  // aThread.start() // gives the  signal to the JVM to start a JVM thread
  // create a JVM thread => OS thread

  runnable.run() // doesn't do anything in parallel!
  aThread.join() // blocks until aThread finishes running


  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))

  //  threadHello.start()
  //  threadGoodbye.start()
  // different runs produce different results!

  // executors
  val pool = Executors.newFixedThreadPool(10)
  //pool.execute(() => println("something in the thread pool"))

//    pool.execute(() => {
//      Thread.sleep(1000)
//      println("done after 1 second")
//    })
//
//    pool.execute(() => {
//      Thread.sleep(1000)
//      println("almost done")
//      Thread.sleep(1000)
//      println("done after 2 seconds")
//    })

   // pool.shutdown()
  //pool.execute(() => println("should not appear")) // throws an exception in the calling thread

  // pool.shutdownNow()
  println(pool.isShutdown) // true


  def runInParallel = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)
  }
  //for (_ <- 1 to 10000) runInParallel
  // race condition 2 or more threads try to access to a shared memory

  class BankAccount(var amount: Int) {
    override def toString: String = "" + amount

    def buy(account: BankAccount, thing: String, price: Int) = {
      account.amount -= price
//      println("I've bought " + thing)
//      println(s"my account now is $account")
    }
    // option #1: use synchronized()
    def buySafe(account: BankAccount, thing: String, price: Int) =
      account.synchronized {
        // no two threads can evaluate this at the same time
        account.amount -= price
        println("I've bought " + thing)
        println("my account is now " + account)
      }

    // option #2: use @volatile

  }

  for (_ <- 1 to 10000) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => account.buy(account, "shoes", 3000))
    val thread2 = new Thread(() => account.buy(account, "iPhone12", 4000))

    thread1.start()
    thread2.start()
    Thread.sleep(10)
    if (account.amount != 43000) println("AHA: " + account.amount)
//    println()
    /*
        thread1 (shoes): 50000
          - account = 50000 - 3000 = 47000
        thread2 (iphone): 50000
          - account = 50000 - 4000 = 46000 overwrites the memory of account.amount
       */
  }

  /**
   * Exercises
   *
   * 1) Construct 50 "inception" threads
   * Thread1 -> thread2 -> thread3 -> ...
   * println("hello from thread #3")
   * in REVERSE ORDER
   *
   */
  /*
      2
     */
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())

/*
    1) what is the biggest value possible for x? 100
    2) what is the SMALLEST value possible for x? 1
*/

  /*
      3 sleep fallacy
     */

  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })

  message = "Scala sucks"
  awesomeThread.start()
  Thread.sleep(1001)
  awesomeThread.join() // wait for the awesome thread to join
  println(message)

/*
    what's the value of message? almost always "Scala is awesome"
    is it guaranteed?
*/
}
