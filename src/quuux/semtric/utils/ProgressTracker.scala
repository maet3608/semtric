package quuux.semtric.utils

import scala.math.min

/** Keeps track of progress, e.g. to update progress bars. 
 *  nUpdates is the maximum number the progress listener will be called. 
 */
class ProgressTracker(val nUpdates:Int) {
  private var _nCurrent: Int = 0
  private var _nMax: Int = 0
  private var _progress: Double => Unit = null
  private var _nSteps: Int = 1

  /** Returns true if there is no progress listener function to call. */
  private def noListener = _progress == null
  /** Computes and returns the current progress [0,1]. */
  private def compute = if (_nMax > 0) min(1.0, 1.0 * _nCurrent / _nMax) else 0.0

  /** Sets the progress listener. */
  def setListener(progress: Double => Unit) { _progress = progress }
  
  /** Starts the tracker. nMax is the maximum number of increment calls. */
  def start(nMax: Int) {
    if (noListener) return
    _nCurrent = 0
    _nMax = nMax
    _nSteps = 1 + nMax / nUpdates
    _progress(0.0)
  }

  /** Increments the tracker. Must be called nMax times. */
  def increment() {
    if (noListener) return
    _nCurrent += 1
    if (_nCurrent % _nSteps == 0) _progress(compute)
  }

  /** Ends the progress tracking. */
  def end() {
    if (noListener) return
    _progress(1.0)
  }
}

/** Factory for trackers. */
object ProgressTracker {
  /** Creates tracker with no listener and 10 updates. */
  def apply(): ProgressTracker = new ProgressTracker(20)
  
  /** Creates a tracker with the given number of updates and listener. */
  def apply(nUpdates:Int, progress: Double => Unit): ProgressTracker = {
    val tracker = new ProgressTracker(nUpdates)
    tracker.setListener(progress)
    tracker
  } 
}