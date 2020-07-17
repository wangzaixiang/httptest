package httptest

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import httptest.Sample.{Request, Response}

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * a HTTP test sample
 */
object Sample {
    case class Request
    (
        method: String,
        url: String,
        version: String = "1.1",
        parameters: List[(String, String)] = Nil,
        headers: Map[String, String] = Map(),
        body: Option[String] = None,
        contentType: Option[String] = None
    )

    case class Response
    (
        version: Option[String] = None,
        status: Option[Int] = None,
        headers: Map[String, String] = Map(),
        body: Option[String] = None
    )

    private val _nextSampleId = new AtomicInteger(0)

    def nextSampleId(): Int = _nextSampleId.getAndIncrement()

}
case class Sample (
    id: Int = Sample.nextSampleId(),
    request: Request,
    response: Response,
    weight: Int = 1,
    timeout: Duration = 60.seconds,
    bodyCompare: BodyCompare = StringSame
)

class SampleProgress {
    val submited = new AtomicInteger(0)

    val completed = new AtomicInteger(0)
    val completedRTT = new AtomicLong(0L)

    val failed = new AtomicInteger(0)
    val _2xx = new AtomicInteger(0)
    val _3xx = new AtomicInteger(0)
    val _4xx = new AtomicInteger(0)
    val _5xx = new AtomicInteger(0)
    val _xxx = new AtomicInteger(0) // not _2xx, _3xx, _4xx, _5xx

    val matched = new AtomicInteger(0)
    val unmatched = new AtomicInteger(0)

    // TP90, TP95, TP99
    val cnt_N_1ms: Array[AtomicInteger] = allocateArray() // 0ms - 999ms
    val cnt_N_10ms: Array[AtomicInteger] = allocateArray() // 1s - 11s
    val cnt_N_100ms : Array[AtomicInteger] = allocateArray() // 11s - 111s
    val cnt_N_1s : Array[AtomicInteger] = allocateArray() // 111s - 1111s


    def allocateArray(): Array[AtomicInteger] = {
        val array = new Array[AtomicInteger](1000)
        (0 until 1000).foreach { i => array(i) = new AtomicInteger(0) }
        array
    }

    // report
    def complete(result: Try[org.asynchttpclient.Response], matched: Boolean, time: Int): Unit = {
        completed.incrementAndGet()
        completedRTT.addAndGet(time)

        result match {
            case Success(response) =>
                val code = response.getStatusCode
                if(code >= 200 && code < 300)
                    _2xx.incrementAndGet()
                else if(code >= 300 && code < 400)
                    _3xx.incrementAndGet()
                else if(code >= 400 && code < 500)
                    _4xx.incrementAndGet()
                else if(code >= 500 && code < 600)
                    _5xx.incrementAndGet()
                else
                    _xxx.incrementAndGet()

            case Failure(exception) =>
                failed.incrementAndGet()
        }

        if(matched)
            this.matched.incrementAndGet()
        else
            this.unmatched.incrementAndGet()

        if(time < 1000) {
            cnt_N_1ms(time).incrementAndGet()
        }
        else if(time < 11_000) {
            cnt_N_10ms( (time - 1000)/10 ).incrementAndGet()
        }
        else if(time < 111_000 ) {
            cnt_N_100ms( (time - 11_000)/100 ).incrementAndGet()
        }
        else if(time < 1111_000 ) {
            cnt_N_1s( (time - 111_000)/1000 ).incrementAndGet()
        }
        else cnt_N_1s(999).incrementAndGet()

    }

    def averageRTT: Double = {
        if(completed.get() == 0)  Double.NaN
        else completedRTT.get * 1.0d / completed.get
    }
}

case class TestBench(samples: List[(Sample, SampleProgress)], concurrent:Int,
                     requests: Int,
                     totalTime: Long) {



    def toString(sample: Sample, progress: SampleProgress): String = {
        val parts: List[String] =
            "%50s".format(sample.request.url) ::
              "%6d".format(progress.completed.get)  ::
              "%6d".format(progress.failed.get) ::
              "%6.2f".format(progress.averageRTT) ::
              "%6d".format(progress._2xx.get) ::
              "%6d".format(progress._3xx.get) ::
              "%6d".format(progress._4xx.get) ::
              "%6d".format(progress._5xx.get) ::
              "%6d".format(progress._xxx.get) ::
              "%6d".format(progress.matched.get) ::
              "%6d".format(progress.unmatched.get) ::
              Nil

        parts.mkString("\t")
    }

    def title: String = {
        val parts =
            "%50s".format("url") ::
              "%6s".format("runs") ::
              "%6s".format("failed") ::
              "%6s".format("rtt") ::
              "%6s".format("2xx") ::
              "%6s".format("3xx") ::
              "%6s".format("4xx") ::
              "%6s".format("5xx") ::
              "%6s".format("xxx") ::
              "%6s".format("match") ::
              "%6s".format("~match") ::
              Nil
        parts.mkString("\t")
    }

    def report(): Unit = {
        println(s"run request:$requests concurrent:$concurrent in ${totalTime}ms")
        println(title)
        for( (sample, progress) <- samples ){
            println(toString(sample, progress))
        }
    }

}
