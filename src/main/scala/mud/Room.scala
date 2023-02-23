package mud

import scala.collection.mutable.Buffer
import akka.actor.Actor
import akka.actor.ActorRef
import adt.BSTMap

class Room(val name: String, val key: String, val desc: String, private var items: Buffer[Item], private val exitNames: Array[String]) extends Actor {

  private var exits: Array[Option[ActorRef]] = null
  private var players = Buffer[ActorRef]()
  // private var npcs = Buffer[ActorRef]()

  import Room._
  def receive = {
    case LinkRooms(links) => exits = exitNames.map(links.get)
    case PrintDescription => sender ! Player.PrintMessage(description())
    case GetExit(dir) => sender ! Player.TakeExit(getExit(dir))
    case GetNPCExit(dir) => sender ! NPC.TakeExit(getExit(dir))
    case GetItem(itemName) => sender ! Player.TakeItem(getItem(itemName))
    case DropItem(item) => dropItem(item)
    case AddPlayer => players += sender
    case RemovePlayer => players = players.filter(_ != sender)
    // case AddNPC => npcs += sender
    // case RemoveNPC => npcs = npcs.filter(_ != sender)
    case SayToRoom(name, msg) => 
      for(i <- players) {
        i ! Player.PrintMessage(name + ": " + msg)
      }
    case IsHerePlayer(name) => sender ! Player.TargetHere(isHere(name))
    case IsHereNPC(name) => sender ! Player.TargetHere(isHere(name))
    case m => println("Unhandled message in Room: " + m)
  }

  def description(): String = {
    var north = ""
    if (exits(0) != None) north = "North "
    var south = ""
    if (exits(1) != None) south = "South "
    var east = ""
    if (exits(2) != None) east = "East "
    var west = ""
    if (exits(3) != None) west = "West "
    var up = ""
    if (exits(4) != None) up = "Up "
    var down = ""
    if (exits(5) != None) down = "Down "

    var exitString = north + south + east + west + up + down
    var inroom = ""
    for (index <- 0 until items.size) {
      inroom = inroom + items(index).name + " "
    }
    var playerString = ""
    for(index <- 0 until players.size) {
      playerString = playerString + players(index) + " "
    }

    name + "\n" + desc + "\n" + "Exits: " + exitString + "\n" + "Items: " + inroom + "\n" + "People: " + playerString + "\n"
  }

  def getExit(dir: Int): Option[ActorRef] = {
    exits(dir)
  }

  def getItem(itemName: String): Option[Item] = {
    items.find(_.name.toLowerCase == itemName.toLowerCase) match {
      case Some(item) =>
        items = items.filter(_ != item)
        Some(item)
      case None => None
    }
  }

  def dropItem(item: Item): Unit = items += item

  def isHere(name: String): ActorRef = {
    var target: ActorRef = null
    for(i <- 0 until players.size) {
      if(players(i).toString.contains(name)) target = players(i)
    }
    target
  }
}

object Room {
  case class LinkRooms(links: Map[String, ActorRef])
  case class GetExit(dir: Int)
  case class GetNPCExit(dir: Int)
  case class GetItem(itemName: String)
  case class DropItem(item: Item)
  case class SayToRoom(name: String, msg: String)
  case class IsHerePlayer(name: String)
  case class IsHereNPC(name: String)
  case object PrintDescription
  case object AddPlayer
  case object RemovePlayer
}