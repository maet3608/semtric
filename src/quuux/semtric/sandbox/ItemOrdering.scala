package quuux.semtric.sandbox

import quuux.semtric._
import quuux.semtric.Item._


//class Box[T](val value: T) {
//  
//  override def toString = value.toString()
//}

//object Box {
//  implicit def boxIsOrdered[T <% Ordered[T]](lhs : Box[T]) = new Ordered[Box[T]] {
//    def compare(rhs : Box[T]) = lhs.value compare rhs.value
//  }
//}

//class Box[T](val value: T)(implicit ordering: Ordering[T]) extends Ordering[Box[T]] {
//  def compare(x: Box[T], y: Box[T]): Int = ordering.compare(x.value, y.value)
//  override def toString = value.toString()
//}


//class BoxOrdering[T](ordering: Ordering[T]) extends Ordering[Box[T]] {
//  def compare(x: Box[T], y: Box[T]): Int = ordering.compare(x.value, y.value)
//}



//class BoxInt(value:Int)(implicit ordering: Ordering[Int]) extends Box(value) with Ordering[BoxInt] {
//  def compare(x: BoxInt, y: BoxInt): Int = ordering.compare(x.value, y.value)
//}
//
//class BoxString(value:String) extends Box(value) {
//  
//}

class ItemOrdering[T](ordering: Ordering[T]) extends Ordering[Item[T]] {
  def compare(x: Item[T], y: Item[T]): Int = ordering.compare(x.value, y.value)
}


//   // Ordering of item by value.
//  implicit def ordering[T <% Ordered[T]](lhs : Item[T]) = new Ordered[Item[T]] {
//    def compare(rhs : Item[T]) = lhs compare rhs
//  }


object ItemOrdering {
  
  
  //def sort[T](boxes: Seq[Box[T]])(implicit ordering: Ordering[Box[T]]) = boxes.sorted(ordering)
  
  def main(args: Array[String]) {
    //val items:List[AnyItem] = List(IInt(2), IInt(1), IInt(3))
//    val items = List(IInt(2), IInt(1), IInt(3))
//    println(items.sorted(ItemOrdering))
    
//    val numList = List(new BoxInt(11), new BoxInt(3), new BoxInt(2))
//    val strList = List(new BoxString("11"), new BoxString("3"), new BoxString("2"))
//    //println(numList.sorted)
//    //println(strList.sorted)
//    val box1:Box[_] = new Box(1)
//    val box2:Box[_] = new Box(2)
////    val box1 = new Box(1)
////    val box2 = new Box(2)
//    println( (new Box(1)).compare(new Box(2)) )
//    println( box1.compare(box2) )
  }
}