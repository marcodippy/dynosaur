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

import cats.~>
import dynosaur.codec.Schema.structure
import dynosaur.codec.model._
import org.apache.avro.generic.{GenericData, GenericRecord}
import org.apache.avro.util.Utf8
import org.apache.avro.{SchemaBuilder, Schema => RawAvroSchema}

object conversion {
  def toRawAvroSchema(avroSchema: AvroSchema): RawAvroSchema =
    avroSchema match {
      case AvroIntSchema() => RawAvroSchema.create(RawAvroSchema.Type.INT)
      case AvroStringSchema() => RawAvroSchema.create(RawAvroSchema.Type.STRING)
      case AvroRecordSchema(fields) =>
        fields
          .foldLeft(
            SchemaBuilder
              .record("recordName")
              .namespace("recordNamespace")
              .fields()
          ) {
            case (sb, (fieldName, fieldSchema)) =>
              sb.name(fieldName).`type`(toRawAvroSchema(fieldSchema)).noDefault()
          }
          .endRecord()
    }

  def toRawAvro(avroType: AvroType, avroSchema: AvroSchema): AnyRef =
    avroType match {
      case model.AvroInt(i) => int2Integer(i)
      case model.AvroString(s) => new Utf8(s)
      case model.AvroRecord(fields) =>
        fields.foldLeft(new GenericData.Record(toRawAvroSchema(avroSchema))) {
          case (r, (fieldName, fieldVal)) => {
            //passing avroSchema here is wrong, it should be the schema of the field
            r.put(fieldName, toRawAvro(fieldVal, avroSchema))
            r
          }
        }
    }

  def fromRawAvro(v: AnyRef, avroSchema: AvroSchema): AvroType =
    avroSchema match {
      case AvroIntSchema() => AvroInt(v.toString.toInt)
      case AvroStringSchema() => AvroString(v.toString)
      case AvroRecordSchema(fields) =>
        val r = v.asInstanceOf[GenericRecord]
        AvroRecord(
          fields
            .map { case (name, schema) => name -> fromRawAvro(r.get(name), avroSchema) } //passing the avroSchema here is wrong
        )
      case a => throw new RuntimeException("value not handled " + a)
    }

  def toAvroSchema[A](schema: Schema[A]): AvroSchema = {
    import structure._

    def translateSchema[R](record: Ap[Field[R, ?], R]): AvroRecordSchema =
      record.analyze {
        λ[Field[R, ?] ~> λ[a => AvroRecordSchema]] { field =>
          AvroRecordSchema(Map(field.name -> toAvroSchema(field.elemSchema)))
        }
      }

    schema match {
      case Num => AvroIntSchema()
      case Str => AvroStringSchema()
      case Rec(record) => translateSchema(record)
      case Sum(alt) => throw new RuntimeException()
    }
  }
}
