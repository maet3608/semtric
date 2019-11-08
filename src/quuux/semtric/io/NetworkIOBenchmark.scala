package quuux.semtric.io

import quuux.semtric.io.reader._
import quuux.semtric.io.writer._
import quuux.semtric._
import quuux.semtric.Item._
import quuux.semtric.Relation._
import scala.util.Random

/**
 * Some benchmarking of different network readers and writers.
 * Comparison of writing and reading times (msec) of plain and binary writers with and without
 * compression and encryption for a network with 100012 items and 1100011 relations:
 *                 Writer           Reader
 *              plain  binary    plain  binary
 *  default      329    1052      593    1341  
 *  comp         1672   2045      733    1429  
 *  enc          468    1120      859    1454  
 *  comp-enc     1686   2067      749    1426
 * Summary: Plain format is faster (2x) to read and write than binary format. Compression is
 * slow (5x) for plain format. Compressed file is 2x-10x smaller and encryption does not
 * affect file size. Binary file are smaller (2x) even with compression.
 * Results vary with data and the network structure.
 */
object NetworkIOBenchmark {
  def main(args: Array[String]): Unit = {
    val repeats = 10

    def timeInMSec(doSomething: => Unit) = {
      var startTime = System.currentTimeMillis
      doSomething
      System.currentTimeMillis - startTime
    }

    def evaluateReader(reader: NetworkReader, filepath: String): Long = {
      (0 to 5).foreach(_ => reader.readFromFile(filepath)) // warming up.
      (0 to repeats).map(_ => timeInMSec(reader.readFromFile(filepath))).min
    }

    def evaluateWriter(writer: NetworkWriter, filepath: String, net: Network): Long = {
      (0 to 5).foreach(_ => writer.writeToFile(net, filepath)) // warming up.
      (0 to repeats).map(_ => timeInMSec(writer.writeToFile(net, filepath))).min
    }

    println("Generating network items and relations...")
    val rnd = new Random
    val (n, m) = (100000, 10)
    val items1 = (0 to n).map(i => IString(rnd.nextLong + "item" + i))
    val items2 = (0 to m).map(j => IString(rnd.nextLong + "item" + j))
    val relations = for (i1 <- items1; i2 <- items2) yield Relation(i1, i2)
    val itemSet = (items1 ++ items2).toSet[AnyItem]

    println("Creating network ...")
    val net = Network(itemSet.toVector, relations.toVector)
    println("#items=" + itemSet.size)
    println("#relations=" + relations.size)

    println("Benchmarking...")
    val pathBase = "test/benchmark_network"
    val params = List(("default", false, ""), ("comp", true, ""), ("enc", false, "pwd"),
      ("comp-enc", true, "pwd"))

    println("               Writer           Reader")
    println("            plain  binary    plain  binary")
    for ((mode, compressed, password) <- params) {
      val plnPath = pathBase + ".plain." + mode
      val binPath = pathBase + ".binary." + mode
      val readerPln = new NetworkReaderPlain(compressed, password)
      val writerPln = new NetworkWriterPlain(compressed, password)
      val readerBin = new NetworkReaderBinary(compressed, password)
      val writerBin = new NetworkWriterBinary(compressed, password)

      printf("%-12s %-6d %-6d    %-6d %-6d\n",
        mode,
        evaluateWriter(writerPln, plnPath, net),
        evaluateWriter(writerBin, binPath, net),
        evaluateReader(readerPln, plnPath),
        evaluateReader(readerBin, binPath))
    }

    println("finished.")
  }

}