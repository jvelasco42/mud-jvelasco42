package mud

import akka.actor.Actor
import akka.actor.ActorRef
import adt.UnsortedArrayPQ
import adt.BinaryHeapPQ

class ActivityManager extends Actor {
  import ActivityManager._
  //val pq = new UnsortedArrayPQ[Event](_.time < _.time)
  val pq = new BinaryHeapPQ[Event](_.time < _.time)
  private var now = 0

  def receive = {
    case CheckQueue =>
      // Check queue and send messages
      while(!pq.isEmpty && pq.peek.time <= now) {
        val event = pq.dequeue()
        event.recipient ! event.msg
      }
      now += 1
    case ScheduleEvent(delay, recipient, msg) =>
      // Add event to queue
      pq.enqueue(new Event(delay + now, recipient, msg))
    case m => println("Unhandled message in ActivityManager: " + m)
  }
}

object ActivityManager {
  case class Event(time: Int, recipient: ActorRef, msg: Any)
  case object CheckQueue
  case class ScheduleEvent(delay: Int, recipient: ActorRef, msg: Any)
}