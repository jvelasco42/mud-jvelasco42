package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

class NPC(private var location: String, val name: String) extends Actor {
  private var loc: ActorRef = null
  val book = new Item("book", 3, 10, "Comp sci textbook")
  private var health = 5
  private var armorClass = 10
  private var weapon: Item = book
  private var target: ActorRef = null
  private var endCombat = false
  private var alive = true
  //Main.activityManager ! ActivityManager.ScheduleEvent(util.Random.nextInt(50)+100, self, NPC.AskForExit)
  
  if(health <= 0) alive = false
    if(alive == false) {
        println(name + " died!")
    }

  import NPC._
  def receive = {
    case TakeExit(exit) => 
      exit match{
        case Some(room) =>
          if(loc != null) {
            loc ! Room.RemovePlayer
            loc ! Room.SayToRoom(name, "Left the room.")
          }
          loc = room
          loc ! Room.AddPlayer
          Main.activityManager ! ActivityManager.ScheduleEvent(util.Random.nextInt(50)+100, self, NPC.AskForExit)
        case None => loc ! Room.GetNPCExit(util.Random.nextInt(6))
      }
    case AskForExit => 
      loc ! Room.GetNPCExit(util.Random.nextInt(6))
      println(name)
    case EndCombat => 
      target = null
      endCombat = true
    case Attack(attacker) =>
      target = attacker
      attack()
    case TargetHit(attacker, dmg, dead) =>
    case Damage(attacker, hit, dmg) => 
      if(!endCombat) {
        if(hit > armorClass) health -= dmg
        if(health > 0) {
          if(target == null) {
            target = attacker
            attack()
          }
          attacker ! TargetHit(self, dmg, false)
        } else attacker ! TargetHit(self, dmg, true)
      } else endCombat = false
    case m => println("Unhandled message in PlayerManager: " + m)
  }

  def attack(): Unit = {
    Main.activityManager ! ActivityManager.ScheduleEvent(weapon.speed, target, 
      NPC.Damage(self, (Math.random * 20 + 1).toInt, (Math.random * weapon.dmg + 1).toInt))
  }

}

object NPC {
  case class TakeExit(exit: Option[ActorRef])
  case class Attack(attacker: ActorRef)
  case class TargetHit(attacker: ActorRef, dmg: Int, dead: Boolean)
  case class Damage(attacker: ActorRef, hit: Int, dmg: Int)
  case object EndCombat
  case object AskForExit
}