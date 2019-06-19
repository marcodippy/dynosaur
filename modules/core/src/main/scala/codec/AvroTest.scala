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
package codec

import avro._
import cats.implicits._
import Schema._

object AvroTest extends App {
  val serialiser = AvroSerialiser.create
  val deserialiser = AvroDeserialiser.create

  def roundtrip[A](schema: Schema[A], a: A): Unit = {
    (for {
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

  case class User(name: String, age: Int)

  val userSchema: Schema[User] = record("com.mdp2", "User") { field =>
    (
      field("name", str, _.name),
      field("age", num, _.age)
    ).mapN(User.apply)
  }

  val user = User("marco", 32)
//  roundtrip(userSchema, user)

//  roundtrip(Schema.num, 123)
//  roundtrip(Schema.str, "asd")

  case class TestNested(id: String, user: User)
  val testNestedSchema: Schema[TestNested] = record("com.mdp", "Nested") { field =>
    (
      field("id", str, _.id),
      field("user", userSchema, _.user)
    ).mapN(TestNested.apply)
  }

  val testNested = TestNested("testId", user)

  roundtrip(testNestedSchema, testNested)

}
