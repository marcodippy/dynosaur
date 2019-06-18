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

import dynosaur.codec.model._
import dynosaur.codec.conversion._
import cats.implicits._
import dynosaur.codec._
import dynosaur.codec.Schema._

object AvroTest extends App {
  import Serde._

  case class User(name: String, age: Int)

  val userSchema = record[User] { field =>
    (
      field("name", str, _.name),
      field("age", num, _.age)
    ).mapN(User.apply)
  }

  val testUser = User("marco", 32)

  val userAvroType = Encoder.fromSchema(userSchema).write(testUser).right.get

  val bytes = serialise(userAvroType, toAvroSchema(userSchema))
  println(s"serialised " + bytes)

  val deserialised = Decoder
    .fromSchema(userSchema)
    .read(
      deserialise(bytes, toAvroSchema(userSchema))
    )
    .right
    .get

  println("deserialised " + deserialised)
}

object Serde {
  import java.io._
  import org.apache.avro.generic._
  import org.apache.avro.io._
  import scala.collection.JavaConverters._

  def serialise(avroType: AvroType, avroSchema: AvroSchema): Array[Byte] = {
    val outputStream = new ByteArrayOutputStream()
    val datumWriter = new GenericDatumWriter[GenericRecord](toRawAvroSchema(avroSchema))
    val encoder = EncoderFactory.get.binaryEncoder(outputStream, null)

    val record = toRawAvro(avroType, avroSchema).asInstanceOf[GenericRecord] //TODO
    datumWriter.write(record, encoder)

    encoder.flush()

    outputStream.toByteArray
  }

  def deserialise(bytes: Array[Byte], avroSchema: AvroSchema): AvroType = {
    val rawAvroSchema = toRawAvroSchema(avroSchema)
    val datumReader = new GenericDatumReader[GenericRecord](rawAvroSchema)
    val decoder = DecoderFactory.get.binaryDecoder(bytes, null)

    val record = datumReader.read(null, decoder)

    //only records, one level only for now...
    val fields = avroSchema
      .asInstanceOf[AvroRecordSchema]
      .fields
      .map {
        case (fieldName, fieldSchema) => fieldName -> fromRawAvro(record.get(fieldName), fieldSchema)
      }

    AvroRecord(fields)
  }
}
