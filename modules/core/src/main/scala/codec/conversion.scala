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

import dynosaur.codec.Schema.structure
import dynosaur.codec.model._
import org.apache.avro.generic.{GenericData, GenericRecord}
import org.apache.avro.util.Utf8
import org.apache.avro.{Schema => RawAvroSchema}

object conversion {
  def toRawAvro(avroSchema: AvroSchema): RawAvroSchema =
    RawAvroSchema.create(RawAvroSchema.Type.ARRAY)

  def toRawAvro(avroType: AvroType, rawAvroSchema: RawAvroSchema): AnyRef =
    avroType match {
      case model.AvroInt(i) => int2Integer(i)
      case model.AvroString(s) => new Utf8(s)
      case model.AvroRecord(fields) =>
        fields.foldLeft(new GenericData.Record(rawAvroSchema)){
          case (r, (fieldName, fieldVal)) => r.put(fieldName, toRawAvro(fieldVal))
        }
    }

  def fromRawAvro(rawAvroSchema: RawAvroSchema): AvroSchema = ???

  def fromRawAvro(v: AnyRef, avroSchema: AvroSchema): AvroType =
    v match {
      case a: java.lang.Integer => AvroInt(a)
      case s: java.lang.String => AvroString(s)
      case _ => throw new RuntimeException("value not handled")
    }


  def toAvroSchema[A](schema: Schema[A]): AvroSchema = {
    import structure._
    schema match {
      case Num => AvroIntSchema()
      case Str => AvroStringSchema()
      case Rec(p) => AvroRecordSchema()
      case Sum(alt) => ???
    }
  }
}
