package quuux.semtric.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Item._

class StorageTest extends FlatSpec with Matchers {
  def fixture = new {
    val persons = List(Person("Lisa", 25), Person("Fred", 42), Person("Ann", 23))
    val contacts = Contacts("work", persons)
    val storage = Storage()
    val root = storage.root
  }

  "Storage" should "write and read group" in {
    val f = fixture
    val default = IString("default")
    val group = f.storage.writeGroup(f.root, "group")
    f.storage.commit()
    f.storage.readGroup(f.root, "group") should be(group)
    f.storage.readGroup(f.root, "group", default) should be (group)
    f.storage.readGroup(f.root, "missing", default) should be (default)
  }

  it should "write and read groups" in {
    val f = fixture
    val groups = List(IString("group"), IString("group"))
    groups.foreach(f.storage.writeGroup(f.root, _))
    f.storage.commit()
    f.storage.readGroups(f.root, "group") should be (groups)
    f.storage.readGroups(f.root, "missing") should be (Iterable.empty)
  }

  it should "write and read value with default value" in {
    val f = fixture
    val group = f.storage.writeValue(f.root, "number", 12)
    f.storage.commit()
    f.storage.readValue[Int](f.root, "number") should be (12)
    f.storage.readValue[Int](f.root, "number", -1) should be (12)
    f.storage.readValue[Int](f.root, "missing", -1) should be (-1)
  }

  it should "write and read values" in {
    val f = fixture
    val values = List(IInt(1), IInt(2))
    val group = f.storage.writeValues(f.root, "numbers", values)
    f.storage.commit()
    f.storage.readValues[Int](f.root, "numbers") should be (Iterable(1, 2))
  }

  it should "read and write storagable" in {
    val f = fixture
    val person = Person("Fred", 33)
    f.storage.writeStoragable(f.root, "person", person)
    f.storage.commit()
    val person_ = f.storage.readStoragable(f.root, "person", Person)
    person_ should be (person)
  }

  it should "read and write same storagable only once" in {
    val f = fixture
    val person = Person("Fred", 33)
    val friends = f.storage.writeGroup(f.root, "Friends")
    val work = f.storage.writeGroup(f.root, "Work")
    f.storage.writeStoragable(friends, "person", person)
    f.storage.writeStoragable(work, "person", person)
    f.storage.commit()
    val person1_ = f.storage.readStoragable(friends, "person", Person)
    val person2_ = f.storage.readStoragable(work, "person", Person)
    assert(person1_ eq person2_)
  }

  it should "read and write storagables" in {
    val f = fixture
    f.storage.writeStoragables(f.root, "person", f.persons)
    f.storage.commit()
    val persons_ = f.storage.readStoragables(f.root, "person", Person)
    persons_ should be (f.persons)
  }

  it should "read and write same storagables only once" in {
    val f = fixture
    val friends = f.storage.writeGroup(f.root, "Friends")
    val work = f.storage.writeGroup(f.root, "Work")
    f.storage.writeStoragables(friends, "person", f.persons)
    f.storage.writeStoragables(work, "person", f.persons)
    f.storage.commit()
    val friends_ = f.storage.readStoragables(friends, "person", Person)
    val work_ = f.storage.readStoragables(work, "person", Person)
    for (friend <- friends_) assert(work_.filter(_ eq friend).size == 1)
  }

  it should "read and write storagables in sequence" in {
    val f = fixture
    val persons = f.persons.reverse
    f.storage.writeStoragables(f.root, "person", persons)
    f.storage.commit()
    val persons_ = f.storage.readStoragables(f.root, "person", Person)
    persons_ should be(persons)
  }
  
  it should "read and write same storagables in sequence only once" in {
    val f = fixture
    val persons_fwd = f.persons
    val persons_bkw = f.persons.reverse
    val friends = f.storage.writeGroup(f.root, "Friends")
    val work = f.storage.writeGroup(f.root, "Work")
    f.storage.writeStoragables(friends, "person", persons_fwd)
    f.storage.writeStoragables(work, "person", persons_bkw)    
    f.storage.commit()
    val friends_ = f.storage.readStoragables(friends, "person", Person)
    val work_ = f.storage.readStoragables(work, "person", Person)    
    for (friend <- friends_) assert(work_.filter(_ eq friend).size == 1)
    friends_ should be(work_.toList.reverse)
  }

  it should "read and write hierachies of storagables" in {
    val f = fixture
    f.storage.writeStoragable(f.root, "contacts", f.contacts)
    f.storage.commit()
    val contacts = f.storage.readStoragable(f.root, "contacts", Contacts)
    contacts should be(f.contacts)
  }

  it should "be saveable and loadable" in {
    val f = fixture
    val filepath = "test/storage.network"
    f.storage.writeStoragable(f.root, "contacts", f.contacts)
    f.storage.save(filepath)
    val storage = Storage.load(filepath)
    val contacts = storage.readStoragable(storage.root, "contacts", Contacts)
    contacts should be(f.contacts)
  }
}

/** Test class that describes a person with name and ages as part of a contacts list. */
class Person(val name: String, val age: Int) extends StorageWritable {
  def write(storage: Storage, to: AnyItem) {
    storage.writeValue(to, "name", name)
    storage.writeValue(to, "age", age)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case a: AnyRef if this eq a => true
      case p: Person              => this.name == p.name && this.age == p.age
      case _                      => false
    }
  }

  override def toString = "Person(" + name + "," + age + ")"
}

/** Factory for Person objects. */
object Person extends StorageReadable[Person] {
  def apply(name: String, age: Int) = new Person(name, age)
  def read(storage: Storage, from: AnyItem) = {
    val name = storage.readValue[String](from, "name")
    val age = storage.readValue[Int](from, "age")
    Person(name, age)
  }
}

/** Test class that describes a contacts list with a label and a collection of person data. */
class Contacts(val label: String, val persons: Iterable[Person]) extends StorageWritable {
  def write(storage: Storage, to: AnyItem) {
    storage.writeValue(to, "label", label)
    storage.writeStoragables(to, "person", persons)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case a: AnyRef if this eq a => true
      case c: Contacts            => this.label == c.label && this.persons == c.persons
      case _                      => false
    }
  }

  override def toString = "Contacts(" + label + "," + persons.mkString(",") + ")"
}

/** Factory for Contacts objects. */
object Contacts extends StorageReadable[Contacts] {
  def apply(label: String, persons: Iterable[Person]) = new Contacts(label, persons)
  def read(storage: Storage, from: AnyItem) = {
    val label = storage.readValue[String](from, "label")
    val persons = storage.readStoragables(from, "person", Person)
    Contacts(label, persons)
  }
}  
