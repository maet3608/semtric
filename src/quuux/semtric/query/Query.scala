package quuux.semtric.query

import quuux.semtric._

object Query {
  private val parser = new QParser()
  def apply(query: String): QNode = parser.read(query)
  def apply(query: String, network: Network): Iterator[QResult] =
    parser.read(query).matches(network, INone, QResult())
}

object QueryExamples extends App {
  import quuux.semtric.io.importer.ImporterNetworkSimple
  //val relations = "N|E N|O N|L N|S O|1 E|2 O|3 E|4 O|5 E|6 L|4 L|5 L|6 S|1 S|2 S|3"
  //val relations = "J|K J|S J|R K|k1 k1|s1 S|s1 k1|r1 R|r1 K|k2 k2|s2 S|s2"
  val relations = """Contacts|Contact Contact|Name|Fred Contact|Age|35
                       Contacts|Contact Contact|Name|Ann Contact|Phone|0314 Contact|Age|23:int"""
  val network = ImporterNetworkSimple.fromString(relations)
  
//  val (a,b,c) = (IString("A"), IString("B"), IString("C"))
//  val relations = Vector(Relation(a,b),Relation(c,b),Relation(b,a))
//  val network = Network(relations)
  
  network.print(showUUID = 0, showKind = true)

  //val query = "?kind(<>, int)"
  //val query = "?kind(N<>, string)"
  //val query = "?take(?kind(<>, string), 5)"
  //val query = "?regex(<name>, '[A-Za-z]+d')"
  //val query = "/[A-Za-z]+d/"
  //val query = "Contact (<Name*> & Age 23)"    
  //val query = "Contact (<Name*> & <Phone*>)"
  //val query = "Contact (<Name*> | <Phone*>)"
  //val query = "Contact (<Name*> ; <Phone*>)"
  //val query = "Contact (<Name*> - <Name> & Phone)"
  //val query = "<K*> & (<K><s> & S<s> ; <K><r> & R<r>)"
  //val query = "O<num> - S<num>"
  val query = "?unique(O<num> | S<num>)"
  //val query = "?unique(O<num> | S<num>)"
  //val query = "?unique(N<><num>)"
  //val query = "?unique(<><><num>)"
  //val query = "(O|E)<number>"
  
  val results = Query(query, network)
  println("------ QUERY RESULTS -------")
  if (results.isEmpty) println("EMPTY") else results.foreach(println)
  println("----------------------------")
}
