package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

class NPCManager extends Actor {
  import NPCManager._
  def receive = {
    case NewNPC(location, name) =>
      val npc = context.actorOf(Props(new NPC(location, name)), name)
      println(name)
      Main.roomManager ! RoomManager.AddNPCToRoom(npc, location)
    case m => println("Unhandled message in PlayerManager: " + m)
  }
}

object NPCManager {
  case class NewNPC(location: String, name: String)
}