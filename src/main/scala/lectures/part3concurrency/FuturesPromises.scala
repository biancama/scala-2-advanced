package lectures.part3concurrency


import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
object FuturesPromises extends App {


  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

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

  // mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) =
      println(s"${this.name} poking ${anotherProfile.name}")
  }
  object SocialNetwork {
    // "database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )
    val random = Random


    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetching from the DB
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // client: mark to poke bill
  //val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
//  mark.onComplete {
//    case Success(markProfile) =>
//      val bill = SocialNetwork.fetchBestFriend(markProfile)
//        bill.onComplete {
//          case Success(billProfile) => markProfile.poke(billProfile)
//          case Failure(e) => e.printStackTrace()
//        }
//    case Failure(e1) => e1.printStackTrace()
//  }

  // functional composition of futures
  // map, flatMap, filter
  /*
  val nameOnTheWall = mark.map(profile => profile.name)
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))  // if fail with no element exception

  // for-comprehensions
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)
*/
  Thread.sleep(1000)
  // fallbacks
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackResult =  SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  // online banking app
  case class User(name: String)

  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some processes
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch the user from the DB
      // create a transaction
      // WAIT for the transaction to finish
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      Await.result(transactionStatusFuture, 2.seconds) // implicit conversions -> pimp my library
    }
  }

  println(BankingApp.purchase("Daniel", "iPhone 12", "rock the jvm store", 3000))
  // promises
  val promise = Promise[Int]() // "controller" over a future
  val future = promise.future
  // thread 1 - "consumer"
  future.onComplete {
    case Success(r) => println("[consumer] I've received " + r)
  }
  // thread 2 - "producer"
  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(500)
    // "fulfilling" the promise
    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)

  /*
      1) fulfill a future IMMEDIATELY with a value
      2) inSequence(fa, fb)
      3) first(fa, fb) => new future with the first value of the two futures
      4) last(fa, fb) => new future with the last value
      5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
     */

  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  def inSequence[A](fa: Future[A], fb: Future[A]):  Future[A] = for {
    _ <- fa
    b <- fb
  } yield b

  def first[A](fa: Future[A], fb: Future[A]):  Future[A] = {
    val promise = Promise[A]
    fa.onComplete(res => promise.tryComplete(res))
    fb.onComplete(promise.tryComplete)

    /*
      this is equivalent
    fa.onComplete {
      case Success(r) => try {  we need a try because the promise can be already completed
        promise.success(r)
      } catch {
        case _ =>
      }
      case Failure(ex) => try {
        promise.failure(ex)
        } catch {
          case _ =>
        }
      }
    }
    */
    promise.future
  }

  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete = (t: Try[A]) => {
      if (!bothPromise.tryComplete(t))
        lastPromise.tryComplete(t)
    }

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }
  first(fast, slow).foreach(f => println("FIRST: " + f))
  last(slow,fast).foreach(l => println("LAST: " + l))


  // retry until
  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8)))
      .recoverWith {
        case _ => retryUntil(action, condition)
      }

  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 10).foreach(result => println("settled at " + result))
  Thread.sleep(10000)


  def scalaFoo = Future {
    Thread.sleep(10 * 1000) // sleep for 10 seconds
    List(1, 2, 3)
  }

  def scalaBar = Future {
    Thread.sleep(10 * 1000)
    List(4, 5, 6)
  }

  def scalaBaz = Future {  // <--- USE VAL otherwise won't run in parallel
    Thread.sleep(10 * 1000)
    List(7, 8, 9)
  }

  val flatRes: Future[List[Int]] = for {
    scalaFooRes <- scalaFoo
    scalaBarRes <- scalaBar
    scalaBazRes <- scalaBaz
  } yield (scalaFooRes ++ scalaBarRes ++ scalaBazRes)

  flatRes onComplete {
    case Success(li) => println(s"calculation: ${li.foldLeft(0)(_ + _)}")
    case Failure(e) => println(e.getMessage)
  }
  Thread.sleep(20000)
}
