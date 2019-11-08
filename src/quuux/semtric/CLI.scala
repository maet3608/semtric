package quuux.semtric

import scala.io.StdIn
import quuux.semtric.utils._
import quuux.semtric.io.reader.NetworkReader
import quuux.semtric.query.Query

/**
 * A simple command line interface to load, show and query networks. Note that queries
 * must not contain white spaces.
 * Example session:
 * help show
 * load test/storage.network
 * info
 * show lines=10 type=true
 * ? person(<name*>;<age*>)
 * ? ?count(person)
 * exit
 */
object CLI {
  private var isRunning = true
  private var network = Network()
  protected val parser = new ClParser(ClExit, ClInfo, ClLoad, ClShow, ClQuery)

  object ClExit extends ClCommand("exit", "Exits the shell") {
    def execute() { isRunning = false; println("done.") }
  }

  object ClInfo extends ClCommand("info", "Shows basic info of the network") {
    def execute() {
      println("network has " + network.items.size + " items and "
        + network.relations.size + " relations.")
    }
  }

  object ClLoad extends ClCommand("load", "Load a network",
    ClParameter("filepath", "Path to network file", true)) {
    def execute() {
      def getPassword = { print("enter password: "); StdIn.readLine() }
      def progress(progress: Double) { print(".") }
      val filepath = parameter("filepath").str
      print("loading ")
      val reader = NetworkReader(filepath, getPassword)
      reader.setProgressListener(progress)
      network = reader.readFromFile(filepath)
      println(" network with " + network.items.size + " items loaded.")
    }
  }

  object ClShow extends ClCommand("show", "Shows the network as a tree",
    ClFlag("lines", "Show max number of lines (0 = all)", "100"),
    ClFlag("levels", "Show max number of levels (0 = all)", "0"),
    ClFlag("type", "Show type of item value", "false"),
    ClFlag("uuid", "Show n digits of items uuid", "0")) {
    def execute() {
      network.print(flag("uuid").int, flag("type").bool,
        flag("levels").int, flag("lines").int)
    }
  }

  object ClQuery extends ClCommand("?", "Query the network",
    ClParameter("query", "The actual query", true)) {
    def execute() {
      val results = Query(parameter("query").str, network)
      println("------ QUERY RESULTS -------")
      if (results.isEmpty) println("EMPTY") else results.foreach(println)
      println("----------------------------")
    }
  }

  def main(args: Array[String]) {  
    while (isRunning) {
      print("semtric: ")
      val input = StdIn.readLine().split("\\s+")
      try { parser.execute(input) }
      catch { case e: Exception => println("ERROR: " + e.getLocalizedMessage) }
    }
  }
}