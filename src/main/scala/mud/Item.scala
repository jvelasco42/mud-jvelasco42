package mud

case class Item(name: String, dmg: Int, speed: Int, desc: String) {
    def getName(): String = name

    def getDamage(): Int = dmg

    def getSpeed(): Int = speed

    def getDesc(): String = desc
}