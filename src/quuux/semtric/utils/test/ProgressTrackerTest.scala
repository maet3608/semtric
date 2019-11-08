package quuux.semtric.utils.test

import org.scalatest._
import quuux.semtric.utils._

class ProgressTrackerTest extends FlatSpec with Matchers {
  var progress = 0.0
  var nCalls = 0
  def listener(value: Double) { progress = value; nCalls += 1 }

  "ProgressTracker" should "track from 0 to 1" in {
    var progress = 0.0
    def listener(value: Double) { progress = value }

    val tracker = ProgressTracker(100, listener)

    tracker.start(10)
    progress should be(0.0)
    for (i <- 1 to 10) {
      tracker.increment()
      progress should be(i / 10.0)
    }
    tracker.end()
    progress should be(1.0)
  }

  it should "update no more than nUpdates" in {
    var nCalls = 0
    def listener(value: Double) { nCalls += 1 }

    val nUpdates = 10
    val tracker = ProgressTracker(nUpdates, listener)

    tracker.start(100)
    nCalls = 0
    for (i <- 1 until 100) {
      tracker.increment()
    }
    assert(nCalls <= nUpdates)
    tracker.end()
  }

}