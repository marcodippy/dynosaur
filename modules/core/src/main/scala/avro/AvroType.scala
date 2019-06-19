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

package avro

import cats._

sealed trait AvroType {
  def schema: AvroSchema
  lazy val toRawAvro: AnyRef = AvroType.toRawAvro(this)
}

case class AvroInt(value: Int) extends AvroType {
  override val schema: AvroSchema = AvroIntSchema
}

case class AvroString(value: String) extends AvroType {
  override val schema: AvroSchema = AvroStringSchema
}

case class AvroRecord(name: String, namespace: String, fields: Map[String, AvroType]) extends AvroType {
  override val schema: AvroSchema = AvroRecordSchema(name, namespace, fields.mapValues(_.schema))
}

object AvroType {

  //TODO see avro4s.AvroSchemaMerge
  implicit val monoidForAvroRecord: Monoid[AvroRecord] =
    new Monoid[AvroRecord] {
      override def empty: AvroRecord = AvroRecord("", "", Map.empty)

      override def combine(x: AvroRecord, y: AvroRecord): AvroRecord =
        AvroRecord(x.name, x.namespace, x.fields ++ y.fields)
    }

  def toRawAvro(avroType: AvroType): AnyRef =
    avroType match {
      case AvroInt(i) => int2Integer(i)
      case AvroString(s) => new org.apache.avro.util.Utf8(s)
      case ar @ AvroRecord(_, _, fields) =>
        fields.foldLeft(new org.apache.avro.generic.GenericData.Record(ar.schema.toRawAvro)) {
          case (r, (fieldName, fieldVal)) =>
            r.put(fieldName, toRawAvro(fieldVal))
            r
        }
    }

  def fromRawAvro(a: AnyRef, avroSchema: AvroSchema): AvroType =
    avroSchema match {
      case AvroIntSchema => AvroInt(a.toString.toInt)
      case AvroStringSchema => AvroString(a.toString)
      case AvroRecordSchema(name, namespace, fields) =>
        val r = a.asInstanceOf[org.apache.avro.generic.GenericRecord]
        AvroRecord(name, namespace, fields.map { case (name, schema) => name -> fromRawAvro(r.get(name), schema) })
      case a => throw new RuntimeException("value not handled " + a)
    }
}
