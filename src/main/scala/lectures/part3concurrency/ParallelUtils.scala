package lectures.part3concurrency
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.AtomicReference
import scala.collection.parallel.immutable.ParVector
import collection.parallel.CollectionConverters.*
import scala.collection.parallel.{ForkJoinTaskSupport, Task, TaskSupport}
object ParallelUtils extends App {
  // 1 - parallel collections

  val parList = List(1,2,3).par

  val aParVector = ParVector[Int](1,2,3)

  /*
      Seq
      Vector
      Array
      Map - Hash, Trie
      Set - Hash, Trie
     */
  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  // val list = (1 to 100000000).toList  // if we decrease the number of elements in the list => it's better serial
  val list = (1 to 100).toList  // if we decrease the number of elements in the list => it's better serial
  val serialTime = measure {
    list.map(_ + 1)
  }
  println("serial time: " + serialTime)

  val parallelTime = measure {
    list.par.map(_ + 1)
  }

  println("parallel time: " + parallelTime)

  /*
      Map-reduce model
      - split the elements into chunks - Splitter
      - operation
      - recombine - Combiner
     */

  // map, flatMap, filter, foreach, reduce, fold
  println(List(1, 2, 3).reduce(_ - _))
  println(List(1, 2, 3).par.reduce(_ - _))

  // synchronization
  var sum = 0
  List(1, 2, 3).par.foreach(sum += _)
 // println(sum) // race conditions!

  // configuring
  aParVector.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(2))
  /*
      alternatives
      - ThreadPoolTaskSupport - deprecated
      - ExecutionContextTaskSupport(EC)
     */

//  aParVector.tasksupport = new TaskSupport {
//    override val environment: AnyRef = ???
//
//    override def execute[R, Tp](fjtask: Task[R, Tp]): () => R = ???
//
//    override def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = ???
//
//    override def parallelismLevel: Int = ???
//  }
  val atomic = new AtomicReference[Int](2)

  val currentValue = atomic.get() // thread-safe read
  atomic.set(4) // thread-safe write

  atomic.getAndSet(5) // thread safe combo

  atomic.compareAndSet(38, 56)
  // if the value is 38, then set to 56
  // reference equality
  atomic.updateAndGet(_ + 1) // thread-safe function run
  atomic.getAndUpdate(_ + 1)
  atomic.accumulateAndGet(12, _ + _) // thread-safe accumulation  it will take the value inside the value and will do the binary operator
  atomic.getAndAccumulate(12, _ + _)
}
