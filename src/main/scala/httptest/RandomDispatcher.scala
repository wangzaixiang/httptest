package httptest

import java.util.concurrent.atomic.AtomicInteger

import org.asynchttpclient.{Dsl, ListenableFuture, Response}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

case class RandomDispatcher(samples: List[Sample], concurrent:Int = 1, requests:Int) {

    val LOG = LoggerFactory.getLogger(getClass)

    case class SampleRecord(sample: Sample, score:Int, progress: SampleProgress )

    val weightSum = samples.map(_.weight).sum
    val samplesWithScore: Array[SampleRecord] = {
        val weights = new Array[SampleRecord](samples.size)
        var base = 0
        for( i <- 0 until samples.size) {
            val sample = samples(i)
            weights(i) = SampleRecord(sample, base, new SampleProgress)
            base += sample.weight
        }
        weights
    }
    val random = new scala.util.Random()

    val submited = new AtomicInteger(0)
    val completed = new AtomicInteger(0)

    val ahc = Dsl.asyncHttpClient()

    var beginTime: Long = _
    var endTime: Long = _

    // weightBase[it] <= base < weightBase[it+1]
    def lookfor(base: Int): SampleRecord = {
        @tailrec
        def lookfor(base:Int, l:Int, t:Int) : SampleRecord = {
            if(l == t) samplesWithScore(l)
            else if(t == l + 1) {
                if(base >= samplesWithScore(t).score) samplesWithScore(t)
                else samplesWithScore(l)
            }
            else {
                val m = (l + t)/2  // m != l && m != t
                if( samplesWithScore(m).score > base )
                    lookfor(base, l, m)
                else
                    lookfor(base, m, t)
            }
        }

        // assert(base < weightSum)
        lookfor(base, 0, samplesWithScore.size-1)
    }

    def fetchSample(): Option[SampleRecord] = {
        if(submited.get() < requests) {
            val r = random.nextInt(weightSum)
            Some(lookfor(r))
        }
        else None
    }

    def fetchAndRun(promise: Promise[TestBench]): Unit = {
        fetchSample() match {
            case Some(sampleRecord) =>
                val time0 = System.currentTimeMillis()
                val future = runSample(sampleRecord.sample)
                val callback: Runnable = () => {
                    val time1 = System.currentTimeMillis()
                    val result: Try[Response] =
                        try { Success(future.get) }
                        catch { case ex: Throwable => Failure(ex) }
                    reportSample(sampleRecord.sample, sampleRecord.progress, time1-time0, result)

                    val num = completed.incrementAndGet()
//                    println(s"completed $num requests")
                    if(num == requests) {
                        val bench = samplesWithScore.map( x => (x.sample, x.progress) ).toList
                        endTime = System.currentTimeMillis()
                        promise.success(TestBench(bench, concurrent, requests, endTime-beginTime))
                    }
                    else fetchAndRun(promise)
                }
                future.addListener(callback, null)
            case None =>
//                println(s"fetch complete")
        }
    }

    def reportSample(sample: Sample, progress: SampleProgress, timeMillis: Long, result: Try[Response]): Unit = {

        val matched = if (sample.response.body.isDefined) {
            result match {
                case Success(response) =>
                    val real = response.getResponseBody
                    val expect = sample.response.body.get
                    val ok = sample.bodyCompare.compare(real, expect)
                    if(!ok){
                        LOG.error(s"unmatched sample:${sample.id}:${sample.request.url} response: [${real}]")
                    }
                    ok
                case Failure(ex) =>
                    false
            }
        }
        else true


        progress.complete(result, matched, timeMillis.toInt)
    }

    // TODO return a Future which completed when all request completed
    def run(): Future[TestBench]= {

        beginTime = System.currentTimeMillis()

        val promise = Promise[TestBench]()

        for(i <- 0 until concurrent) {
            fetchAndRun(promise)
        }
        promise.future
    }

    def runSample( sample: Sample ): ListenableFuture[Response] = {
        val builder = ahc
          .prepare(sample.request.method, sample.request.url)

        sample.request.parameters.foreach { para: (String, String) =>
            builder.addQueryParam(para._1, para._2)
        }

        sample.request.headers.foreach { header =>
            builder.addHeader(header._1, header._2)
        }
        if(sample.request.contentType.isDefined){
            builder.addHeader("Content-Type",sample.request.contentType.get)
        }

        if(sample.request.method == "POST" && sample.request.body.isDefined) {
            builder.setBody(sample.request.body.get)
        }

        builder.setRequestTimeout(sample.timeout.toMillis.toInt)

        val num = submited.incrementAndGet()
//        println(s"sumbit $num requests")
        builder.execute()
    }


}
