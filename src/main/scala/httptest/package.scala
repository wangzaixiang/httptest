import org.asynchttpclient._
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}

import scala.annotation.tailrec
import scala.concurrent.{CancellationException, ExecutionException, Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.collection.mutable

package object httptest {

    case class SampleResult
    (
      sample: Sample,
      result: Try[Response]
    )

    // random dispatcher
    // Sample1:w1, Sample2:w2, ...
    // random(w1+w2+..) pick One and execute it
    // after success, call again

    def randomDispatch( samples: List[Sample], concurrent: Int = 1, requests: Int ): Future[TestBench] = {
        new RandomDispatcher(samples, concurrent = concurrent, requests = requests)
          .run()
    }

    def lookfor(): Unit = {

    }


    def runBench( samples: List[Sample], concurrent: Int = 1, requests: Int, timeout: Duration = 60.seconds ): Future[List[SampleResult]] = {

        val promise = Promise[List[SampleResult]]()

        def runSamples(samples: List[Sample], promise: Promise[List[SampleResult]], completed: List[SampleResult]) : Unit = {
            samples match {
                case head :: tail =>
                    val future = runSample(head)
                    val callback: Runnable = () => {
                        val result: Try[Response] =
                            try { Success(future.get) }
                            catch { case ex: Throwable => Failure(ex) }
                        runSamples(tail, promise, SampleResult(head, result) :: completed)
                    }
                    future.addListener( callback, null)
                case Nil =>
                    promise.success(completed)
            }
        }

        runSamples(samples, promise, Nil)
        promise.future

    }

    def runSample( sample: Sample ): ListenableFuture[Response] = {
        val builder = Dsl.asyncHttpClient()
          .prepare(sample.request.method, sample.request.url)

        sample.request.parameters.foreach { para: (String, String) =>
            builder.addQueryParam(para._1, para._2)
        }

        sample.request.headers.foreach { header =>
            builder.addHeader(header._1, header._2)
        }

        if(sample.request.method == "POST" && sample.request.body.isDefined ) {
            builder.setBody(sample.request.body.get)
        }

        builder.setRequestTimeout(sample.timeout.toMillis.toInt)

        builder.execute()
    }

    trait BodyCompare {
        def compare(real: String, expect: String): Boolean
    }

    object StringSame extends BodyCompare {
        override def compare(real: String, expect: String): Boolean = real == expect
    }

    object JSONContains extends BodyCompare {
        override def compare(real: String, expect: String): Boolean = {
            try {
                val realValue = ujson.read(real)
                val expectValue = ujson.read(expect)
                compareContains(realValue, expectValue)
            }
            catch {
                case ex => false
            }
        }
        def compareContains(obj: Value, expect: Value): Boolean = {
            (obj, expect) match {
                case (x: Obj, y: Obj) =>
                    compareObjContains(x, y)
                case (x: Arr, y: Arr) =>
                    compareArrContains(x, y)
                case (x: Bool, y: Bool) => x == y
                case (x: Num, y: Num) => x == y
                case (x: Str, y: Str) => x == y
                case _ => false
            }
        }

        def compareObjContains(obj: Obj, expect: Obj): Boolean = {
            expect.value.keys.forall { key =>
                compareContains(obj.value.getOrElse(key, Null), expect.value.apply(key))
            }
        }

        def compareArrContains(obj: Arr, expect: Arr): Boolean  = {
            expect.value.zipWithIndex.forall { case (x, idx) =>
                obj.value.size > idx &&
                  compareContains(obj.value(idx), x)
            }
        }
    }

}
