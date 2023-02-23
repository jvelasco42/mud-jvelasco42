package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.Buffer
import adt.BSTMap

class RoomManager extends Actor {
  val rooms = readRooms()
  for(child <- context.children) child ! Room.LinkRooms(rooms)

  def readRooms(): Map[String, ActorRef] = {
    val xmlData = xml.XML.loadFile("world.xml")
    // val roomData = (xmlData \ "room").map(readRoom)
    // val bst = new BSTMap[String, ActorRef](_ < _)
    // for(i <- 0 until roomData.length) {
    //   bst += roomData(i)
    // }
    (xmlData \ "room").map(readRoom).toMap
  }

  def readRoom(node: xml.Node): (String, ActorRef) = {
    val name = (node \ "@name").text
    val key = (node \ "@keyword").text
    val desc = (node \ "@desc").text
    val items = (node \\ "item").map(n => (Item((n \ "@itemName").text, (n \ "@damage").text.toInt, 
      (n \ "@speed").text.toInt, (n \ "@itemDesc").text))).toBuffer
    val exits = (node \ "@exits").text.split(",").map(_.trim)
    key -> context.actorOf(Props(new Room(name, key, desc, items, exits)), key)
  }

  import RoomManager._
  def receive = {
    case AddPlayerToRoom(player, key) =>
      player ! Player.TakeExit(rooms.get(key))
    case AddNPCToRoom(npc, key) => 
      npc ! NPC.TakeExit(rooms.get(key))
    case m => println("Unhandled message in RoomManager: " + m)
  }
}

object RoomManager {
  case class AddPlayerToRoom(player: ActorRef, key: String)
  case class AddNPCToRoom(npc: ActorRef, key: String)
}