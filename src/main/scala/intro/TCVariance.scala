package intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._ // Eq[Int] TC instance
  import cats.instances.option._ // construct a Eq[Option[Int]] TC instance
  import cats.syntax.eq._

  val aComparison: Boolean = Option(2) === Option(3)
  // val anInvalidComparison: Boolean = Some(2) === None // Eq[Some[Int]] not found event thought Some is instace of Option

  //Variance
  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagates backwards to the generic type
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule:"Has a T" -> covariant, "ACTS on T" -> contravariant

  // contravariant TC
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")

  makeSound[Animal] // Ok - TC defined above
  makeSound[Cat] // Ok - TC instance for animal is also applicable to Cats
  // rule 1: Contravariant TCs can use the superclass instances if nothing is available strictly for that type

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // Covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "Animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "Other show"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // rule 2: covariant TCs will always use the more specific TC instance for that type
  // but may confuse the compiler is the TC is also present

  // rule 3: you cant have both
  // Cats uses invariant classes
  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - the compile inject CatShow as implicit
    // println(organizeShow[Animal]) // will not compile - ambiguous values
  }

}
