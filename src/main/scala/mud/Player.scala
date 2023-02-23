package mud

import scala.collection.mutable.Buffer
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import mud.Player.CheckInput
import java.io.PrintStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.net.Socket

class Player(name: String, sock: Socket, in: BufferedReader, out: PrintStream) extends Actor {
    private var inv: Buffer[Item] = Buffer.empty
    val fist = new Item("fist", 2, 20, "Your barefist")
    private var location: ActorRef = null
    private var health = 5
    private var armorClass = 13
    private var weapon: Item = fist
    private var target: ActorRef = null
    private var targetHit: Int = 0
    private var endCombat = false
    private var alive = true

    if(health <= 0) alive = false
    if(alive == false) {
        out.println("You died!")
        sock.close
        context.stop(self)
    }

    import Player._
    def receive = {
        case PrintMessage(desc) => out.println(desc)
        case EndCombat => 
            out.println("Target has fled.")
            target = null
            endCombat = true
        case Miss(attacker) => 
            out.println("You missed " + attacker.path.name + ".")
            attack()
        case Attack(attacker) =>
            out.println("You attack " + attacker.path.name + ".")
            target = attacker
            attack()
        case TargetHit(attacker, dmg, dead) =>
            out.println("You deal " + dmg + " damage to " + attacker.path.name)
        case TargetHere(name) => 
            target = name
        case Damage(attacker, hit, dmg) => 
            if(!endCombat) {
                targetHit = hit
                if(hit > armorClass){
                    health -= dmg
                    out.println(attacker.path.name + " deals " + dmg + " damage to you. You have " + health + " remaining.")
                } else out.println(attacker.path.name + " missed you.")
                if(health > 0) {
                    if(target == null) {
                        out.println(attacker.path.name + " attacks you!")
                        target = attacker
                        attack()
                    }
                    attacker ! TargetHit(self, dmg, false)
                } else attacker ! TargetHit(self, dmg, true)
            } else endCombat = false
        case TakeItem(item) => 
            item match {
               case Some(itemName) =>
                    out.println("Added " + itemName.getName + " to inventory.")
                    addToInventory(itemName)
                case None => out.println("There's no such item here.")
            }
        case TakeExit(exit) => 
            exit match {
                case Some(room) =>
                if(location != null) location ! Room.RemovePlayer
                    location = room
                    location ! Room.AddPlayer
                    location ! Room.PrintDescription
                case None => out.println("Not an exit.")
            }
        case CheckInput => 
            if(in.ready) {
                val input = in.readLine
                println(input)
                processCommand(input)
            }
        case m => println("Unhandled message in Player: " + m)
    }

    def processCommand(command: String): Unit = { //Parse and act on a command
        val comArg = command.split(" +", 2)
        comArg(0).toLowerCase match {
            case "help" => help()
            case "north" | "n" => move(0)
            case "south" | "s" => move(1)
            case "east"  | "e" => move(2)
            case "west"  | "w" => move(3)
            case "up"    | "u" => move(4)
            case "down"  | "d" => move(5)
            case "inv" | "inventory" => out.println(inventoryListing)
            case "look" => location ! Room.PrintDescription
            case "say" => location ! Room.SayToRoom(name, comArg(1))
            case "tell" => context.parent ! PlayerManager.PrivateMessage(comArg(1))
            case "get" => location ! Room.GetItem(comArg(1).toLowerCase)
            case "drop" => 
                getFromInventory(comArg(1).toLowerCase) match {
                    case Some(item) =>
                        out.println("Removed " + item.getName + " from inventory.")
                        location ! Room.DropItem(item)
                    case None => out.println("That's not in your inventory.")
                }
            case "equip" => getFromInventory(comArg(1).toLowerCase) match {
                    case Some(item) => 
                        out.println("Equipped " + item.getName + ".")
                        weapon = item
                    case None => out.println("That's not in your inventory.")
                }
            case "unequip" => 
                if(comArg(1) == fist) out.println("You can't unequip your fist!")
                else if(weapon.getName == comArg(1)) {
                    out.println("Unequipped " + comArg(1) + ".")
                    weapon = fist
                } else out.println("That's not your current weapon.")
            case "kill" => 
                location ! Room.IsHerePlayer(comArg(1)) 
                if(target != null) attack
                else out.println("That person isn't here.")
            case "flee" => 
                if((Math.random * 20 + 1).toInt >= targetHit) {
                    Main.roomManager ! RoomManager.AddPlayerToRoom(self, "Home")
                    target ! Player.EndCombat
                    target = null
                    endCombat = true
                } else out.println("Unable to flee!")
            case "exit" => 
                out.println("Goodbye!")
                sock.close
                context.stop(self)
            case _ => println("That's not a valid command.")
        }
    } 

    def attack(): Unit = {
        Main.activityManager ! ActivityManager.ScheduleEvent(weapon.speed, target, 
            Player.Damage(self, (Math.random * 20 + 1).toInt, (Math.random * weapon.dmg + 1).toInt))
    }

    def getFromInventory(itemName: String): Option[Item] = { //Pull an item out of the inventory (if it exists) and return it.
        inv.find(_.name.toLowerCase == itemName.toLowerCase) match {
            case Some(item) =>
                inv = inv.filter(_ != item)
                Some(item)
            case None => None
        }
    }

    def addToInventory(item: Item): Unit = { //Add the given item to inventory.
        inv += item
    } 

    def inventoryListing(): String = {
        var invList = ""
        if(inv.isEmpty) invList = "Your inventory is empty."
        else {
            for(i <- inv) {
                invList = invList + i.getName + ": " + i.getDesc + "\n"
            }
        }
        invList
    } 

    //Move the player in a particular direction if possible.
    def move(dir: Int): Unit = { 
        location ! Room.GetExit(dir)
    }

    def help() = {
        println("""Available Commands:
        North                   Moves player to a room to the north.
        South                   Moves player to a room to the south.
        East                    Moves player to a room to the east.
        West                    Moves player to a room to the west.
        Up                      Moves player to a room above.
        Down                    Moves player to a room below.
        Look                    Looks around the current room.
        Kill                    Initiates combat with target.
        Flee                    Flees combat and takes player to starting toom.
        Say                     Sends message to all players in the room.
        Tell                    Sends a private message to another player.
        Inv                     Displays current items.
        Get                     Gets an item from room and adds to player's inventory.
        Drop                    Drops item from inventory into current room.
        Equip                   Equips an item as your current weapon.
        Unequip                 Unequips current weapon and lets you use your fists.
        Exit                    Leaves the game.""")
    }
}

object Player {
    case class PrintMessage(desc: String)
    case class Miss(attacker: ActorRef)
    case class Attack(attacker: ActorRef)
    case class TargetHit(attacker: ActorRef, dmg: Int, dead: Boolean)
    case class TargetHere(name: ActorRef)
    case class Damage(attacker: ActorRef, hit: Int, dmg: Int)
    case class TakeItem(item: Option[Item])
    case class TakeExit(exit: Option[ActorRef])
    case object EndCombat
    case object CheckInput
}