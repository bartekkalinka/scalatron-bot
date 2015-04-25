// bartekk bot 04.2015


/** Based on reference bot, influenced by marpiec bot
  */
object ControlFunction
{

    import GameObjects._

    def forMaster(bot: Bot) = {
        val cells = prepareView(bot.view)
        commonStrategy(bot, cells, cellValueMaster)
    }

    def commonStrategy(bot: Bot, cells: List[(Char, XY)], cellValue: (Char, Int) => Double) {
        // input params
        val lastDir = bot.inputAsIntOrElse("lastDirection", 0)

        // spawning
        if(bot.energy > 100) {
            val unitDelta = XY.fromDirection45(lastDir).negate
            bot.spawn(unitDelta)
        }

        // determine movement direction
        val directionFromView = analyzeView(cells, cellValue)
        val bestDirection45 =
            if(directionFromView == XY(0, 0))
                lastDir
            else
                directionFromView.toDirection45
        val direction = XY.fromDirection45(bestDirection45)
        bot.move(direction)
        bot.set("lastDirection" -> bestDirection45)

        // debug status
        bot.status("---")
    }

    def forSlave(bot: MiniBot) = reactAsTwin(bot)

    def reactAsTwin(bot: MiniBot): Unit = {
        val cells = prepareView(bot.view)
        if(!explodeIfPaysOff(bot, cells)) {
            commonStrategy(bot, cells, cellValueSlave(bot.isFat))
        }
    }

    def explodeIfPaysOff(bot: MiniBot, cells: List[(Char, XY)]): Boolean = {
        val blowable = cells.filter({ case (cell, pos) => List(ENEMY_MASTER, ENEMY_SLAVE, BAD_BEAST).contains(cell) })
        val closeBlowCount =  blowable.filter(_._2.length <= 2).size
        if(closeBlowCount > 0) {
            bot.explode(3)
            true
        }
        else {
            false
        }
    }

    def cellValueMaster(cell: Char, stepDistance: Int): Double = cell match {
        case ENEMY_SLAVE => // another slave
            -1000 / stepDistance //avoid
        case GOOD_BEAST => // good beast: valuable, but runs away
            1200 / (stepDistance + 1)
        case GOOD_PLANT => // good plant: less valuable, but does not run
            1200 / (stepDistance + 1)
        case BAD_BEAST => // bad beast: dangerous, but only if very close
            if(stepDistance < 4) -400 / stepDistance else -50 / stepDistance
        case BAD_PLANT => // bad plant: bad, but only if I step on it
            if(stepDistance < 2) -1000 else 0
        case WALL => // wall: 
            if(stepDistance < 2) -1000 else 0
        case _ => 0.0
    }

    def cellValueSlave(isFat: Boolean)(cell: Char, stepDistance: Int): Double = cell match {
        case ENEMY_MASTER => // another master
            500 / (stepDistance + 1) //let's explode it!!!
        case ENEMY_SLAVE => // another slave
            400 / (stepDistance + 1) //why not explode it
        case MY_SLAVE => // our own slave
            -1000 / stepDistance //avoid brothers - create network
        case MY_MASTER =>
            if(isFat) 500 else 0
        case GOOD_BEAST => // good beast: valuable, but runs away
            1200 / (stepDistance + 1)
        case GOOD_PLANT => // good plant: less valuable, but does not run
            1200 / (stepDistance + 1)
        case BAD_BEAST => // bad beast: dangerous, but only if very close
            if(stepDistance < 4) -400 / stepDistance else -50 / stepDistance
        case BAD_PLANT => // bad plant: bad, but only if I step on it
            if(stepDistance < 2) -1000 else 0
        case WALL => // wall: 
            -200 / (stepDistance + 1) //moderately avoid - too many problems with minibots getting stuck
        case _ => 0.0
    }

    /* View -> List of cells with relative positions*/
    def prepareView(view: View): List[(Char, XY)] = {
        var cellsWithRelPos: List[(Char, XY)] = Nil

        val cells = view.cells
        val cellCount = cells.length
        for(i <- 0 until cellCount) {
            val cellRelPos = view.relPosFromIndex(i)
            if(cellRelPos.isNonZero) {
                cellsWithRelPos ::= (cells(i), cellRelPos)
            }
        }
        cellsWithRelPos
    }

    /** Analyze the view, summing weighted vectors of visible objects directions */
    def analyzeView(cells: List[(Char, XY)], cellValue: (Char, Int) => Double): XY = {
        var directionSum = XY(0, 0)

        val cellCount = cells.length
        for(i <- 0 until cellCount) {
            val cellRelPos = cells(i)._2
            if(cellRelPos.isNonZero) {
                val stepDistance = cellRelPos.stepCount
                val value = cellValue(cells(i)._1, stepDistance)
                directionSum += (cellRelPos * (value / cellRelPos.length))
            }
        }
        directionSum
    }
}


// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------

class ControlFunctionFactory {
    def create = (input: String) => {
        val (opcode, params) = CommandParser(input)
        opcode match {
            case "React" =>
                val bot = new BotImpl(params)
                if( bot.generation == 0 ) {
                    ControlFunction.forMaster(bot)
                } else {
                    ControlFunction.forSlave(bot)
                }
                bot.toString
            case _ => "" // OK
        }
    }
}


// -------------------------------------------------------------------------------------------------


trait Bot {
    val debugMode = false

    // inputs
    def inputOrElse(key: String, fallback: String): String
    def inputAsIntOrElse(key: String, fallback: Int): Int
    def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY
    def view: View
    def energy: Int
    def isFat: Boolean
    def time: Int
    def generation: Int

    // outputs
    def move(delta: XY) : Bot
    def say(text: String) : Bot
    def status(text: String) : Bot
    def spawn(offset: XY, params: (String,Any)*) : Bot
    def set(params: (String,Any)*) : Bot
    def log(text: String) : Bot
}

trait MiniBot extends Bot {
    // inputs
    def offsetToMaster: XY

    // outputs
    def explode(blastRadius: Int) : Bot
}


case class BotImpl(inputParams: Map[String, String]) extends MiniBot {
    // input
    def inputOrElse(key: String, fallback: String) = inputParams.getOrElse(key, fallback)
    def inputAsIntOrElse(key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)
    def inputAsXYOrElse(key: String, fallback: XY) = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

    val view = View(inputParams("view"))
    val energy = inputParams("energy").toInt
    val isFat = energy >= 200
    val time = inputParams("time").toInt
    val generation = inputParams("generation").toInt
    def offsetToMaster = inputAsXYOrElse("master", XY.Zero)


    // output

    private var stateParams = Map.empty[String,Any]     // holds "Set()" commands
    private var commands = ""                           // holds all other commands
    private var debugOutput = ""                        // holds all "Log()" output

    /** Appends a new command to the command string; returns 'this' for fluent API. */
    private def append(s: String) : Bot = { commands += (if(commands.isEmpty) s else "|" + s); this }

    /** Renders commands and stateParams into a control function return string. */
    override def toString = {
        var result = commands
        if(!stateParams.isEmpty) {
            if(!result.isEmpty) result += "|"
            result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
        }
        if(!debugOutput.isEmpty) {
            if(!result.isEmpty) result += "|"
            result += "Log(text=" + debugOutput + ")"
        }
        result
    }

    def log(text: String) = { debugOutput += text + "\n"; this }
    def move(direction: XY) = if(!debugMode) { append("Move(direction=" + direction + ")") } else { this }
    def say(text: String) = append("Say(text=" + text + ")")
    def status(text: String) = append("Status(text=" + text + ")")
    def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")
    def spawn(offset: XY, params: (String,Any)*) =
        append("Spawn(direction=" + offset +
          (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
          ")")
    def set(params: (String,Any)*) = { stateParams ++= params; this }
    def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y); this }
}


// -------------------------------------------------------------------------------------------------


/** Utility methods for parsing strings containing a single command of the format
  * "Command(key=value,key=value,...)"
  */
object CommandParser {
    /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
    def apply(command: String): (String, Map[String, String]) = {
        /** "key=value" => ("key","value") */
        def splitParameterIntoKeyValue(param: String): (String, String) = {
            val segments = param.split('=')
            (segments(0), if(segments.length>=2) segments(1) else "")
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)
        val opcode = segments(0)
        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
        (opcode, keyValuePairs)
    }
}


// -------------------------------------------------------------------------------------------------


/** Utility class for managing 2D cell coordinates.
  * The coordinate (0,0) corresponds to the top-left corner of the arena on screen.
  * The direction (1,-1) points right and up.
  */
case class XY(x: Int, y: Int) {
    override def toString = x + ":" + y

    def isNonZero = x != 0 || y != 0
    def isZero = x == 0 && y == 0
    def isNonNegative = x >= 0 && y >= 0

    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x + dx, y)
    def addToY(dy: Int) = XY(x, y + dy)

    def +(pos: XY) = XY(x + pos.x, y + pos.y)
    def -(pos: XY) = XY(x - pos.x, y - pos.y)
    def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

    def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean
    def length: Double = math.sqrt(x * x + y * y) // Phythagorean

    def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
    def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)

    /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
    def toDirection45: Int = {
        val unit = signum
        unit.x match {
            case -1 =>
                unit.y match {
                    case -1 =>
                        if(x < y * 3) Direction45.Left
                        else if(y < x * 3) Direction45.Up
                        else Direction45.UpLeft
                    case 0 =>
                        Direction45.Left
                    case 1 =>
                        if(-x > y * 3) Direction45.Left
                        else if(y > -x * 3) Direction45.Down
                        else Direction45.LeftDown
                }
            case 0 =>
                unit.y match {
                    case 1 => Direction45.Down
                    case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
                    case -1 => Direction45.Up
                }
            case 1 =>
                unit.y match {
                    case -1 =>
                        if(x > -y * 3) Direction45.Right
                        else if(-y > x * 3) Direction45.Up
                        else Direction45.RightUp
                    case 0 =>
                        Direction45.Right
                    case 1 =>
                        if(x > y * 3) Direction45.Right
                        else if(y > x * 3) Direction45.Down
                        else Direction45.DownRight
                }
        }
    }

    def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
    def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
    def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
    def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)


    def wrap(boardSize: XY) = {
        val fixedX = if(x < 0) boardSize.x + x else if(x >= boardSize.x) x - boardSize.x else x
        val fixedY = if(y < 0) boardSize.y + y else if(y >= boardSize.y) y - boardSize.y else y
        if(fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
    }
}


object XY {
    /** Parse an XY value from XY.toString format, e.g. "2:3". */
    def apply(s: String) : XY = { val a = s.split(':'); XY(a(0).toInt,a(1).toInt) }

    val Zero = XY(0, 0)
    val One = XY(1, 1)

    val Right     = XY( 1,  0)
    val RightUp   = XY( 1, -1)
    val Up        = XY( 0, -1)
    val UpLeft    = XY(-1, -1)
    val Left      = XY(-1,  0)
    val LeftDown  = XY(-1,  1)
    val Down      = XY( 0,  1)
    val DownRight = XY( 1,  1)

    def fromDirection45(index: Int): XY = index match {
        case Direction45.Right => Right
        case Direction45.RightUp => RightUp
        case Direction45.Up => Up
        case Direction45.UpLeft => UpLeft
        case Direction45.Left => Left
        case Direction45.LeftDown => LeftDown
        case Direction45.Down => Down
        case Direction45.DownRight => DownRight
    }

    def fromDirection90(index: Int): XY = index match {
        case Direction90.Right => Right
        case Direction90.Up => Up
        case Direction90.Left => Left
        case Direction90.Down => Down
    }

    def apply(array: Array[Int]): XY = XY(array(0), array(1))
}


object Direction45 {
    val Right = 0
    val RightUp = 1
    val Up = 2
    val UpLeft = 3
    val Left = 4
    val LeftDown = 5
    val Down = 6
    val DownRight = 7
}


object Direction90 {
    val Right = 0
    val Up = 1
    val Left = 2
    val Down = 3
}

object GameObjects {
  final val WALL = 'W'
  final val MY_MASTER = 'M'
  final val ENEMY_MASTER = 'm'
  final val MY_SLAVE = 'S'
  final val ENEMY_SLAVE = 's'
  final val GOOD_PLANT = 'P'
  final val BAD_PLANT = 'p'
  final val GOOD_BEAST = 'B'
  final val BAD_BEAST = 'b'
}

// -------------------------------------------------------------------------------------------------


case class View(cells: String) {
    val size = math.sqrt(cells.length).toInt
    val center = XY(size / 2, size / 2)

    def apply(relPos: XY) = cellAtRelPos(relPos)

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

    def offsetToNearest(c: Char) = {
        val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
        if( matchingXY.isEmpty )
            None
        else {
            val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
            Some(nearest)
        }
    }
}

