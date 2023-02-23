package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import java.net.Socket
import java.io.BufferedReader
import java.io.PrintStream

class PlayerManager extends Actor {
  import PlayerManager._
  def receive = {
    case NewPlayer(name, location, sock, in, out) =>
      val player = context.actorOf(Props(new Player(name, sock, in, out)), name)
      Main.roomManager ! RoomManager.AddPlayerToRoom(player, location)
    case CheckAllInputs =>
      for(child <- context.children) child ! Player.CheckInput
    case PrivateMessage(msg) =>
      val input = msg.split(" +", 2)
      for(child <- context.children) {
        if(child.path.name == input(0) || child.path.name == sender.path.name) {
          child ! Player.PrintMessage("(private) " + input(0) + ": " + input(1))
        }
      }
    case SendToAll(msg) =>
      for(child <- context.children) child ! Player.PrintMessage(msg) 
    case m => println("Unhandled message in PlayerManager: " + m)
  }
}

object PlayerManager {
  case class NewPlayer(name: String, location: String, sock: Socket, in: BufferedReader, out: PrintStream)
  case class PrivateMessage(msg: String)
  case class SendToAll(msg: String)
  case object CheckAllInputs
}