/*
 * Copyright 2019 OVO Energy
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dynosaur
package generic
package macros

import cats.implicits._
import dynosaur.codec.{Decoder, Encoder, Schema}
import dynosaur.generic.macros.TestData._
import dynosaur.generic.semiauto._
import dynosaur.model.{AttributeName, AttributeValue}
import org.scalatest.{Assertion, FunSuite, Matchers}
import shapeless.test.illTyped

class SemiautoDerivationTest extends FunSuite with Matchers {

  test("derive[primitive] should fail") {
    illTyped("deriveSchema[String]")
  }

  test("derive for simple case class with only primitives") {
    val user = User("marco", 30)

    val expectedEncoded = AttributeValue.m(
      AttributeName("name") -> AttributeValue.s(user.name),
      AttributeName("age") -> AttributeValue.n(user.age)
    )

    roundtrip(deriveSchema[User], user, expectedEncoded)
  }

  test(
    "derive for simple case class with only primitives - overwrite primitive schema"
  ) {
    val user = User("marco", 30)

    implicit val newSchemaForInt: Schema[Int] = Schema.record { fields =>
      fields("intFieldName", Schema.num, i => i + 1)
    }

    val expectedEncoded = AttributeValue.m(
      AttributeName("name") -> AttributeValue.s(user.name),
      AttributeName("age") -> AttributeValue.m(
        AttributeName("intFieldName") -> AttributeValue.n(user.age + 1)
      )
    )

    roundtrip(
      deriveSchema[User],
      user,
      expectedEncoded,
      skipDecodedEqualityCheck = true
    )
  }

  test("derive for case class with nested case class with no schema") {
    illTyped("""deriveSchema[Employee]""")
  }

  test("derive for case class with nested case class with schema") {
    implicit val schemaForUser: Schema[User] = deriveSchema[User]

    val user = User("marco", 30)
    val employee = Employee("id", user)
    val expectedEncoded = AttributeValue.m(
      AttributeName("id") -> AttributeValue.s(employee.id),
      AttributeName("user") -> AttributeValue.m(
        AttributeName("name") -> AttributeValue.s(user.name),
        AttributeName("age") -> AttributeValue.n(user.age)
      )
    )

    roundtrip(deriveSchema[Employee], employee, expectedEncoded)
  }

  test(
    "derive for case class with nested case class with schema in its companion"
  ) {
    val teacher = Teacher("marco", University)
    val student = Student("id", teacher)

    val expectedEncoded = AttributeValue.m(
      AttributeName("id") -> AttributeValue.s(student.id),
      AttributeName("teacher") -> AttributeValue.m(
        AttributeName("name") -> AttributeValue.s(teacher.name),
        AttributeName("uni") -> AttributeValue.m()
      )
    )

    roundtrip(deriveSchema[Student], student, expectedEncoded)
  }

  test(
    "derive for case class with nested case class with schema in its companion that is overwritten"
  ) {
    val teacher = Teacher("marco", University)
    val student = Student("id", teacher)

    implicit val newSchemaForTeacher: Schema[Teacher] =
      Schema.record[Teacher] { field =>
        (
          field("professor", Schema.str, teacher => teacher.name),
          field("univ", Schema.str, _ => "Cambridge")
        ).mapN { case (name, _) => Teacher(name, University) }
      }

    val expectedEncoded = AttributeValue.m(
      AttributeName("id") -> AttributeValue.s(student.id),
      AttributeName("teacher") -> AttributeValue.m(
        AttributeName("professor") -> AttributeValue.s(teacher.name),
        AttributeName("univ") -> AttributeValue.s("Cambridge")
      )
    )

    roundtrip(deriveSchema[Student], student, expectedEncoded)
  }

  test("derive for empty case class") {
    val caseClass = JustACaseClass()

    val expectedEncoded = AttributeValue.m()

    roundtrip(deriveSchema[JustACaseClass], caseClass, expectedEncoded)
  }

  test("derive for empty class") {
    val clazz = new JustAClass()

    val expectedEncoded = AttributeValue.m()

    roundtrip(
      deriveSchema[JustAClass],
      clazz,
      expectedEncoded,
      skipDecodedEqualityCheck = true
    )
  }

  test("derive for case object") {
    val obj = JustACaseObject

    val expectedEncoded = AttributeValue.m()

    roundtrip(deriveSchema[JustACaseObject.type], obj, expectedEncoded)
  }

  test("derive for object") {
    val obj = JustAnObject

    val expectedEncoded = AttributeValue.m()

    roundtrip(deriveSchema[JustAnObject.type], obj, expectedEncoded)
  }

  test("derive for non sealed trait should fail") {
    illTyped("deriveSchema[JustATrait]")
  }

  /*
    this causes this error: knownDirectSubclasses of JustASealedTrait observed before subclass $anon registered

    see: https://circe.github.io/circe/codecs/known-issues.html#knowndirectsubclasses-error

    test("derive for sealed trait without subtypes should fail") {
      deriveSchema[JustASealedTrait]
    }
   */

  test("derive for coproduct - sealed trait") {
    val open = Open(9)
    val halfOpen = HalfOpen
    val closed = Closed
    val null_ = Null("npe")

    implicit val scmForOpen: Schema[Open] = deriveSchema[Open]
    implicit val scmForHalfOpen: Schema[HalfOpen.type] =
      deriveSchema[HalfOpen.type]
    implicit val scmForClosed: Schema[Closed.type] = deriveSchema[Closed.type]
    implicit val scmForNull: Schema[Null] = deriveSchema[Null]

    roundtrip(
      deriveSchema[Status],
      open,
      AttributeValue.m(
        AttributeName("open") -> AttributeValue.m(
          AttributeName("num") -> AttributeValue.n(open.num)
        )
      )
    )

    roundtrip(
      deriveSchema[Status],
      halfOpen,
      AttributeValue.m(
        AttributeName("halfOpen") -> AttributeValue.m()
      )
    )

    roundtrip(
      deriveSchema[Status],
      closed,
      AttributeValue.m(
        AttributeName("closed") -> AttributeValue.m()
      )
    )

    roundtrip(
      deriveSchema[Status],
      null_,
      AttributeValue.m(
        AttributeName("null") -> AttributeValue.m(
          AttributeName("msg") -> AttributeValue.s(null_.msg)
        )
      )
    )
  }

  test("derive for coproduct - sealed abstract class") {
    val nn = Nn
    val sm = Sm("hello")

    implicit val schemaForNn: Schema[Nn.type] = deriveSchema[Nn.type]
    implicit val schemaForSm: Schema[Sm] = deriveSchema[Sm]

    roundtrip(
      deriveSchema[Opt],
      nn,
      AttributeValue.m(
        AttributeName("nn") -> AttributeValue.m()
      )
    )

    roundtrip(
      deriveSchema[Opt],
      sm,
      AttributeValue.m(
        AttributeName("sm") -> AttributeValue.m(
          AttributeName("a") -> AttributeValue.s(sm.a)
        )
      )
    )
  }

  test("derive for nested data") {
    implicit val schemaForNn: Schema[Nn.type] = deriveSchema[Nn.type]
    implicit val schemaForSm: Schema[Sm] = deriveSchema[Sm]
    implicit val schemaForOpt: Schema[Opt] = deriveSchema[Opt]

    implicit val schemaForRobo: Schema[Robo] = deriveSchema[Robo]
    implicit val schemaForCop: Schema[Cop] = deriveSchema[Cop]

    val robo = Robo("roboName", Teacher("john", University))
    val cop = Cop(99, Nn)
    val cop2 = Cop(100, Sm("hello"))

    roundtrip(
      deriveSchema[NestedData],
      robo,
      AttributeValue.m(
        AttributeName("robo") -> AttributeValue.m(
          AttributeName("name") -> AttributeValue.s(robo.name),
          AttributeName("teacher") -> AttributeValue.m(
            AttributeName("name") -> AttributeValue.s(robo.teacher.name),
            AttributeName("uni") -> AttributeValue.m()
          )
        )
      )
    )

    roundtrip(
      deriveSchema[NestedData],
      cop,
      AttributeValue.m(
        AttributeName("cop") -> AttributeValue.m(
          AttributeName("ammo") -> AttributeValue.n(cop.ammo),
          AttributeName("status") -> AttributeValue.m(
            AttributeName("nn") -> AttributeValue.m()
          )
        )
      )
    )

    roundtrip(
      deriveSchema[NestedData],
      cop2,
      AttributeValue.m(
        AttributeName("cop") -> AttributeValue.m(
          AttributeName("ammo") -> AttributeValue.n(cop2.ammo),
          AttributeName("status") -> AttributeValue.m(
            AttributeName("sm") -> AttributeValue.m(
              AttributeName("a") -> AttributeValue.s(
                cop2.status.asInstanceOf[Sm].a
              )
            )
          )
        )
      )
    )
  }

  test("derive for recursive coproduct") {
    pendingUntilFixed {
      info("Support recursive schemas")

      val emptyMyList = MyNil
      val singletonMyList = MyCons("a1", MyNil)
      val myList = MyCons("a1", MyCons("a1", MyNil))

      implicit val schemaForMyNil: Schema[MyNil.type] = deriveSchema[MyNil.type]
      implicit lazy val schemaForMyList: Schema[MyList] = deriveSchema[MyList]
      implicit lazy val schemaForMyCons: Schema[MyCons] = deriveSchema[MyCons]

      roundtrip(
        deriveSchema[MyList],
        emptyMyList,
        AttributeValue.m(AttributeName("myNil") -> AttributeValue.m())
      )

      roundtrip(
        deriveSchema[MyList],
        singletonMyList,
        AttributeValue.m(
          AttributeName("myCons") -> AttributeValue.m(
            AttributeName("h") -> AttributeValue.s(singletonMyList.h),
            AttributeName("t") -> AttributeValue.m(
              AttributeName("myNil") -> AttributeValue.m()
            )
          )
        )
      )

      roundtrip(
        deriveSchema[MyList],
        myList,
        AttributeValue.m(
          AttributeName("myCons") -> AttributeValue.m(
            AttributeName("h") -> AttributeValue.s(myList.h),
            AttributeName("t") -> AttributeValue.m(
              AttributeName("myCons") -> AttributeValue.m(
                AttributeName("h") -> AttributeValue.s(
                  myList.t.asInstanceOf[MyCons].h
                ),
                AttributeName("t") -> AttributeValue.m(
                  AttributeName("myNil") -> AttributeValue.m()
                )
              )
            )
          )
        )
      )
    }
  }

  def roundtrip[A](
      schema: Schema[A],
      a: A,
      expectedEncoded: AttributeValue,
      skipDecodedEqualityCheck: Boolean = false
  ): Assertion = {
    val encoded = Encoder.fromSchema(schema).write(a).toOption.get
    val decoded = Decoder.fromSchema(schema).read(encoded).toOption.get

    println(s"encoded ______ $encoded")
    println(s"decoded ______ $decoded")

    if (!skipDecodedEqualityCheck) assert(decoded == a)
    assert(encoded == expectedEncoded)
  }

}
