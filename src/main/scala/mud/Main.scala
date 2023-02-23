package mud

import scala.io.StdIn._
import scala.collection.mutable
import mud.PlayerManager.NewPlayer
import akka.actor.ActorSystem
import akka.actor.Props
import java.io.PrintStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.net.ServerSocket
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {
	val system = ActorSystem("MUD")
	val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
  val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
  val npcManager = system.actorOf(Props[NPCManager], "NPCManager")
  val activityManager = system.actorOf(Props[ActivityManager], "ActivityManager")
  system.scheduler.schedule(0.1 seconds, 0.1 seconds, playerManager, PlayerManager.CheckAllInputs)
  
  npcManager ! NPCManager.NewNPC("Home", "Bob")
  npcManager ! NPCManager.NewNPC("Home", "Tim")
  npcManager ! NPCManager.NewNPC("Home", "Larry")

  system.scheduler.schedule(1.seconds, 0.1.seconds, activityManager, ActivityManager.CheckQueue)

  val ss = new ServerSocket(4040)
  while(true) {
    val sock = ss.accept()
    Future{
      val out = new PrintStream(sock.getOutputStream())
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream()))
      println("What is your name?")
			val name = readLine()
      playerManager ! NewPlayer(name, "Home", sock, in, out)
    }
  }
}
