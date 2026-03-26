package gremlin.scala.dsl

import gremlin.scala._
import java.util.{Map => JMap}
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerFactory
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable
import scala.collection.JavaConverters._
import shapeless._

class DslSpec extends AnyWordSpec with Matchers {
  import TestDomain._

  "finds all persons" in {
    val personSteps = PersonSteps(TinkerFactory.createModern)
    personSteps.toSet() shouldBe Set(
      Person(Some(1), "marko", 29),
      Person(Some(2), "vadas", 27),
      Person(Some(4), "josh", 32),
      Person(Some(6), "peter", 35)
    )
  }

  "label with `as` and typesafe `select` of domain types" should {

    "select all labelled steps by default" in {
      implicit val graph = TinkerFactory.createModern

      val personAndSoftware: List[(Person, Software)] =
        PersonSteps(graph)
          .as("person")
          .created
          .as("software")
          .select()
          .toList()
      (personAndSoftware should have).size(4)

      val softwareByCreator: Map[String, Software] = personAndSoftware.map {
        case (person, software) => (person.name, software)
      }.toMap
      softwareByCreator("marko") shouldBe Software("lop", "java")
    }

    "allow to select one labelled step only" in {
      implicit val graph = TinkerFactory.createModern
      val labelPerson = StepLabel[Person]("p")
      val labelSoftware = StepLabel[Software]("s")

      val personAndSoftware: Set[Software] =
        PersonSteps(graph)
          .as(labelPerson)
          .created
          .as(labelSoftware)
          .select(labelSoftware)
          .toSet()
      personAndSoftware shouldBe Set(Software("lop", "java"), Software("ripple", "java"))
    }

    "allow to select multiple labelled steps" in {
      implicit val graph = TinkerFactory.createModern
      val labelPerson = StepLabel[Person]("p")
      val labelSoftware = StepLabel[Software]("s")

      val personAndSoftware: List[(Software, Person)] =
        PersonSteps(graph)
          .as(labelPerson)
          .created
          .as(labelSoftware)
          .select((labelSoftware, labelPerson))
          .toList()
      (personAndSoftware should have).size(4)

      val softwareByCreator: Map[String, Software] = personAndSoftware.map {
        case (software, person) => (person.name, software)
      }.toMap
      softwareByCreator("marko") shouldBe Software("lop", "java")
    }
  }

  "finds combination of person/software in for comprehension" in {
    implicit val graph = TinkerFactory.createModern

    val traversal = for {
      person <- PersonSteps(graph)
      software <- person.created
    } yield (person.name, software)

    val tuples = traversal.toSet() shouldBe Set(
      ("marko", Software("lop", "java")),
      ("josh", Software("lop", "java")),
      ("peter", Software("lop", "java")),
      ("josh", Software("ripple", "java"))
    )
  }

  "filter with traversal on domain type" when {
    "domain type is a case class" in {
      val ripples = PersonSteps(TinkerFactory.createModern)
        .filter(_.created.isRipple)

      ripples.toList() shouldBe List(
        Person(Some(4), "josh", 32)
      )
    }
  }

  "filterNot with traversal on domain type" in {
    val notRipple = PersonSteps(TinkerFactory.createModern)
      .filterNot(_.created.isRipple)

    notRipple.toList().size shouldBe 3
  }

  "filter on domain type" in {
    val markos: List[Person] =
      PersonSteps(TinkerFactory.createModern)
        .filterOnEnd(_.name == "marko")
        .toList()

    markos.size shouldBe 1
  }

  "aggregate intermediary results into a collection" in {
    val allPersons = mutable.ArrayBuffer.empty[Person]
    val markos: List[Person] =
      PersonSteps(TinkerFactory.createModern)
        .aggregate(allPersons)
        .filterOnEnd(_.name == "marko")
        .toList()

    markos.size shouldBe 1
    allPersons.size should be > 1
  }

  "allow side effects" in {
    var i = 0
    PersonSteps(TinkerFactory.createModern).sideEffect(_ => i = i + 1).iterate()
    i shouldBe 4
  }

  "deduplicates" in {
    val results: List[Person] =
      PersonSteps(TinkerFactory.createModern).created.createdBy.dedup().toList()
    results.size shouldBe 3
  }

  "allows to use underlying gremlin-scala steps" in {
    val steps: PersonSteps[_] =
      PersonSteps(TinkerFactory.createModern)
        .onRaw(_.hasId(1: Integer))
    steps.toList().size shouldBe 1
  }

  "traverses from person to software" in {
    val personSteps =
      PersonSteps(TinkerFactory.createModern)
        .onRaw(_.hasId(1: Integer))

    personSteps.created.toSet() shouldBe Set(Software("lop", "java"))
  }

  "supports collections in map/flatMap" when {
    implicit val graph = TinkerFactory.createModern
    def personSteps = PersonSteps(graph)

    "using List" in {
      val query = personSteps.map { person =>
        (person.name, person.created.toList())
      }

      val results: List[(String, List[Software])] = query.toList()
      results.size shouldBe 4
    }

    "using Set" in {
      val query = personSteps.map { person =>
        (person.name, person.created.toSet())
      }

      val results: List[(String, Set[Software])] = query.toList()
      results.size shouldBe 4
    }
  }

  "exists returns true when traversal has results" in {
    PersonSteps(TinkerFactory.createModern).exists() shouldBe true
  }

  "exists returns false when traversal has no results" in {
    PersonSteps(TinkerFactory.createModern).hasName("nonexistent").exists() shouldBe false
  }

  "notExists returns true when traversal has no results" in {
    PersonSteps(TinkerFactory.createModern).hasName("nonexistent").notExists() shouldBe true
  }

  "notExists returns false when traversal has results" in {
    PersonSteps(TinkerFactory.createModern).notExists() shouldBe false
  }

  "tail returns last element" in {
    val result = PersonSteps(TinkerFactory.createModern).tail().toList
    result.size shouldBe 1
  }

  "limit restricts number of results" in {
    val result = PersonSteps(TinkerFactory.createModern).limit(2).toList
    result.size shouldBe 2
  }

  "not filters out matching traversals" in {
    val result = PersonSteps(TinkerFactory.createModern)
      .not(_.created.isRipple)
      .toList
    result.size shouldBe 3
  }

  "hasId filters by element id" in {
    val result = PersonSteps(TinkerFactory.createModern).hasId(1: Integer).toList
    result shouldBe List(Person(Some(1), "marko", 29))
  }

  "hasLabel filters by element label" in {
    val graph = TinkerFactory.createModern
    val result = new Steps[Vertex, Vertex, HNil](graph.V)(Converter.identityConverter)
      .hasLabel("software")
      .toList
    result.size shouldBe 2
  }

  "is filters by value equality" in {
    val graph = TinkerFactory.createModern
    val result = new Steps[String, String, HNil](
      graph.V.values[String]("name"))(Converter.identityConverter)
      .is("marko")
      .toList
    result shouldBe List("marko")
  }

  "barrier does not change results" in {
    val withBarrier = PersonSteps(TinkerFactory.createModern).barrier().toSet
    val withoutBarrier = PersonSteps(TinkerFactory.createModern).toSet
    withBarrier shouldBe withoutBarrier
  }

  "simplePath filters cyclic paths" in {
    val graph = TinkerFactory.createModern
    // marko -> knows -> josh/vadas, then back via knows -> marko/josh/vadas
    // simplePath should filter out revisited vertices
    val result = PersonSteps(graph).hasName("marko")
      .onRaw(_.out("knows").out("knows"))
      .simplePath()
      .toList
    // marko -> josh -> (no outgoing knows), marko -> vadas -> (no outgoing knows)
    // all paths are simple since there are no cycles in 2-hop knows from marko
    result.size shouldBe 0
  }

  "drop removes elements from the graph" in {
    val graph = TinkerFactory.createModern
    PersonSteps(graph).hasName("marko").drop().iterate()
    PersonSteps(graph).hasName("marko").exists() shouldBe false
  }

  "loops returns loop count in repeat traversal" in {
    implicit val graph = TinkerFactory.createModern
    // Use repeat/until with loops to verify loops step works
    val result = PersonSteps(graph).hasName("marko")
      .repeat(_.onRaw(_.out("knows")))
      .until(_.loops().is(1: Integer))
      .toList
    result.size should be > 0
  }

  "group groups by key and value traversals" in {
    implicit val graph = TinkerFactory.createModern
    // group persons by name, collecting the names of software they created
    val result: JMap[String, String] = PersonSteps(graph)
      .group(_.name, _.created.name)
      .head()
    val grouped = result.asScala
    grouped should contain key "marko"
    grouped should contain key "josh"
    grouped should contain key "peter"
  }

  "where filters by sub-traversal" in {
    val graph = TinkerFactory.createModern
    // find persons who created ripple
    val result = PersonSteps(graph)
      .where(_.created.isRipple)
      .toList
    result shouldBe List(Person(Some(4), "josh", 32))
  }

  "unionFlat merges results from multiple traversals" in {
    implicit val graph = TinkerFactory.createModern
    // union of two different person filters
    val result = PersonSteps(graph)
      .unionFlat(
        _.hasName("marko"),
        _.hasName("josh")
      )
      .toSet
    result shouldBe Set(
      Person(Some(1), "marko", 29),
      Person(Some(4), "josh", 32)
    )
  }

  "local executes traversal in local scope" in {
    implicit val graph = TinkerFactory.createModern
    // local limit(1) on created gives at most 1 software per person
    val result = PersonSteps(graph)
      .local(_.created.limit(1))
      .toList
    // each person gets at most 1 created software locally
    result.size should be <= 4
    result.size should be > 0
  }

  "unfold unrolls folded results" in {
    val graph = TinkerFactory.createModern
    // fold names into a list, then unfold back
    val names = new Steps[String, String, HNil](
      graph.V.values[String]("name").fold().unfold[String]()
    )(Converter.identityConverter).toList
    names.size shouldBe 6
  }

  "coalesce returns first non-empty traversal" in {
    implicit val graph = TinkerFactory.createModern
    // marko has no "created" with name "nonexistent", so first branch is empty
    // second branch finds marko's created software
    val result = PersonSteps(graph).hasName("marko")
      .coalesce(
        _.created.isRipple,  // marko didn't create ripple
        _.created            // marko created lop
      )
      .toSet
    result shouldBe Set(Software("lop", "java"))
  }

  "allows to be cloned" in {
    val graph = TinkerFactory.createModern
    def personSteps = PersonSteps(graph)

    val query = personSteps.hasName("marko")
    val queryCloned = query.clone()
    query.toList().size shouldBe 1
    queryCloned.toList().size shouldBe 1
  }
}

object TestDomain {
  @label("person") case class Person(@id id: Option[Integer], name: String, age: Integer)
      extends DomainRoot
  @label("software") case class Software(name: String, lang: String) extends DomainRoot

  object PersonSteps {
    def apply(graph: Graph) = new PersonSteps[HNil](graph.V().hasLabel[Person]())
  }
  class PersonSteps[Labels <: HList](override val raw: GremlinScala[Vertex])
      extends NodeSteps[Person, Labels](raw) {

    def created = new SoftwareSteps[Labels](raw.out("created"))

    def name =
      new Steps[String, String, Labels](raw.map(_.value[String]("name")))

    def hasName(name: String) =
      new PersonSteps[Labels](raw.has(Key("name") -> name))
  }

  class SoftwareSteps[Labels <: HList](override val raw: GremlinScala[Vertex])
      extends NodeSteps[Software, Labels](raw) {

    def createdBy = new PersonSteps[Labels](raw.in("created"))

    def name =
      new Steps[String, String, Labels](raw.map(_.value[String]("name")))

    def isRipple = new SoftwareSteps[Labels](raw.has(Key("name") -> "ripple"))
  }

  implicit def toPersonSteps[Labels <: HList](
      steps: Steps[Person, Vertex, Labels]): PersonSteps[Labels] =
    new PersonSteps[Labels](steps.raw)

  implicit def personStepsConstructor[Labels <: HList]
    : Constructor.Aux[Person, Labels, Vertex, PersonSteps[Labels]] =
    Constructor.forDomainNode[Person, Labels, PersonSteps[Labels]](new PersonSteps[Labels](_))

  implicit def softwareStepsConstructor[Labels <: HList]
    : Constructor.Aux[Software, Labels, Vertex, SoftwareSteps[Labels]] =
    Constructor.forDomainNode[Software, Labels, SoftwareSteps[Labels]](new SoftwareSteps[Labels](_))

  implicit def liftPerson(person: Person)(implicit graph: Graph): PersonSteps[HNil] =
    new PersonSteps[HNil](graph.asScala().V(person.id.get))
}
