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

package codec

import java.time.Instant

import avro._
import cats.implicits._
import dynosaur.UnitSpec
import dynosaur.codec.{Decoder, Encoder, Schema}

case class User(name: String, age: Int)

case class User2(name: String, age: Int)

object User2 {
  implicit val schemaForUser: Schema[User2] = null
}

case class Employee(foo: String, user: User, tm: Long)

case class Employee2(foo: String, user: User2, tm: Instant)
case class UserRec(name: String, age: Int, child: Option[UserRec])

class DerivationTest extends UnitSpec {

  "AvroMacro" should {

    "derive schema for a case class - basic case - fully derived" in {
      val user = User("marco", 32)
      import AvroMacro._
      roundtrip(user)
    }

    "derive schema for a case class - rec case" in {
      val user = Employee("FFF", User("marco", 32), 1234L)
      import AvroMacro._

      implicit val schemaForInstant: Schema[Long] =
        Schema.record[Long]("instantNspc", "instantName") { field =>
          field("inName", Schema.num, _.toInt).map(_.longValue())
        }

      roundtrip(user)
    }

    "derive schema for a case class - rec case - custom userSchema" in {
      val user = Employee("FFF", User("marco", 32), 1234L)
      import AvroMacro._

      implicit val schemaForInstant: Schema[Long] =
        Schema.record[Long]("instantNspc", "instantName") { field =>
          field("inName", Schema.num, _.toInt).map(_.longValue())
        }

      implicit val schemaForUser: Schema[User] = null

      roundtrip(user)
    }

    "derive schema for a case class - rec case - custom userSchema in companion object" in {
      val user = Employee2("FFF", User2("marco", 32), Instant.now())
      import AvroMacro._

      implicit val schemaForInstant: Schema[Instant] =
        Schema.record[Instant]("instantNspc", "instantName") { field =>
          field("inName", Schema.num, _.toEpochMilli.toInt).map(i => Instant.ofEpochMilli(i.toLong))
        }

      roundtrip(user)
    }

//    "derive recursive" in {
//      val userNone = UserRec("marco", 32, None)
//      val userSome = UserRec("marco", 32, Some(userNone))
//      import AvroMacro._
//
//
//      roundtrip(userNone)
//      roundtrip(userSome)
//    }

  }

  private val serialiser = AvroSerialiser.create
  private val deserialiser = AvroDeserialiser.create

  def roundtrip[A](a: A)(implicit schema: Schema[A]): Unit = {
    val _ = (for {
      encodedAvroType <- Encoder.fromSchema(schema).write(a)
      serialisedBytes <- serialiser.serialise(encodedAvroType).asRight
      deserialisedAvroType <- deserialiser.deserialise(serialisedBytes, encodedAvroType.schema).asRight
      decodedA <- Decoder.fromSchema(schema).read(deserialisedAvroType)
    } yield {
      println(s"a _____________________________ $a")
      println(s"encodedAvroType _______________ $encodedAvroType")
      println(s"encodedRawAvroType ____________ ${encodedAvroType.toRawAvro}")
      println(s"writerSchema __________________ ${encodedAvroType.schema}")
      println(s"writerRawSchema _______________ ${encodedAvroType.schema.toRawAvro}")
      println(s"serialisedBytes _______________ $serialisedBytes")
      println(s"deserialisedAvroType __________ $deserialisedAvroType")
      println(s"deserialisedRawAvroType _______ ${deserialisedAvroType.toRawAvro}")
      println(s"readerSchema __________________ ${deserialisedAvroType.schema}")
      println(s"readerRawSchema _______________ ${deserialisedAvroType.schema.toRawAvro}")
      println(s"decodedA ______________________ $decodedA")
    }).leftMap(println)
  }
}
