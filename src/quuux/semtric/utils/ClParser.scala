package quuux.semtric.utils

/**
 * A simple command line parser that can parse command lines of the form:
 * command <required_param> [optional_param] --bool_flag --flag=314
 * 
 * Flags can appear anywhere with one or two hyphens or none if a value is set.
 * For instance the following command line is equal to the first 
 * command flag=314 <required_param> -bool_flag [optional_param]
 * 
 * Values for parameters have to be in order and for required parameters have to be
 * provided. Optional parameters will take on their default values if not provided. 
 */

/** Some command line argument value. */
abstract class ClArgument {
  var str: String = ""
  def bool = str.toBoolean
  def int = str.toInt
  def float = str.toFloat
  def double = str.toDouble
}

/** Command line parameter. Position within command is relevant. */
case class ClParameter(val name: String, val description: String, val isRequired: Boolean = false,
                       val default: String = "") extends ClArgument {
  override def toString = {
    val pName = if (isRequired) "<" + name + ">" else "[" + name + "]"
    val pValue = if (default.isEmpty) "" else " = " + default
    "  " + pName + pValue + " : " + description
  }
}

/** Command line flag. Can appear at an arbitrary position. */
case class ClFlag(val name: String, val description: String, val default: String)
  extends ClArgument {
  override def toString = {
    val fName = "--" + name
    val fValue = if (default.isEmpty) "" else " = " + default
    "  " + fName + fValue + " : " + description
  }
}

/** Command with parameters and flags as arguments. */
abstract class ClCommand(val name: String, val description: String, arguments: ClArgument*) {
  val parameters: List[ClParameter] = arguments.collect { case p: ClParameter => p }.toList
  val flags: Set[ClFlag] = arguments.collect { case f: ClFlag => f }.toSet

  /** Override to execute code when command is called. */
  def execute(): Unit

  /** Return value of parameter with the given name. */
  def parameter(name: String): ClArgument = parameters.find(_.name == name) match {
    case Some(p) => p
    case _       => throw new IllegalArgumentException("Unknown parameter: " + name)
  }

  /** Return value of flag with the given name. */
  def flag(name: String): ClArgument = flags.find(_.name == name) match {
    case Some(f) => f
    case _       => throw new IllegalArgumentException("Unknown flag: " + name)
  }

  /** Throws IllegalArgumentException with the given message. */
  private def error(message: String) = {
    throw new IllegalArgumentException(message)
  }

  private def setFlag(name: String, value: String): ClFlag = {
    flags.find(_.name == name) match {
      case Some(flag) => { flag.str = value; return flag }
      case _          => error("Unknown flag: " + name)
    }
  }

  def parseArgs(args: List[String], ps: Seq[ClParameter] = parameters, fs: Set[ClFlag] = flags) {
    if (args.isEmpty) {
      ps.foreach(p => if (p.isRequired) error("Missing required parameter: " + p) else p.str = p.default)
      fs.foreach(f => f.str = f.default)
      return
    }
    val flagvalue = "-?-?(.+)=(.+)".r
    val flagonly = "--?(.+)".r
    args.head match {
      case flagvalue(name, value) => parseArgs(args.tail, ps, fs - setFlag(name, value))
      case flagonly(name)         => parseArgs(args.tail, ps, fs - setFlag(name, "true"))
      case value if !ps.isEmpty   => {ps.head.str = value; parseArgs(args.tail, ps.tail, fs) }
      case value => error("Unknown argument: " + value)
    }
  }

  override def toString =
    name + " : " + description + "\n" + parameters.mkString("\n") + flags.mkString("\n")
}

/** Command line parser. Takes collections of commands it recognizes. Executes parsed commands. */
class ClParser(commands: ClCommand*) {
  private val baseCmds = Seq(ClHelp)
  private val name2cmd: Map[String, ClCommand] =
    (commands ++ baseCmds).map(cmd => (cmd.name, cmd)).toMap

  object ClHelp extends ClCommand("help", "Shows all commands. Use help [command] for specific help",
    ClParameter("command", "Name of a command, e.g. exit")) {
    def execute() {
      val name = parameter("command").str
      if (!name2cmd.contains(name)) {
        for (cmd <- name2cmd.values.toSeq.sortBy(_.name))
          printf("  %-10s : %s\n", cmd.name, cmd.description)
      } else {
        println(name2cmd(name))
      }
    }
  }

  /** Executes the given command line arguments. */
  def execute(args: Array[String]) {
    val command :: arguments = args.toList
    if (!name2cmd.contains(command)) {
      throw new IllegalArgumentException(
        "Unknown command: " + command + "\nUse 'help' for list of commands.")
    }
    val clCommand = name2cmd(command)
    clCommand.parseArgs(arguments)
    clCommand.execute()
  }
}