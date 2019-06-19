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

sealed trait AvroSchema {
  lazy val toRawAvro: org.apache.avro.Schema = AvroSchema.toRawAvro(this)
}

case object AvroIntSchema extends AvroSchema
case object AvroStringSchema extends AvroSchema
case class AvroRecordSchema(name: String, namespace: String, fields: Map[String, AvroSchema]) extends AvroSchema

object AvroSchema {

  def toRawAvro(avroSchema: AvroSchema): org.apache.avro.Schema = {
    import org.apache.avro.SchemaBuilder

    avroSchema match {
      case AvroIntSchema => org.apache.avro.Schema.create(org.apache.avro.Schema.Type.INT)
      case AvroStringSchema => org.apache.avro.Schema.create(org.apache.avro.Schema.Type.STRING)
      case AvroRecordSchema(name, namespace, fields) =>
        fields
          .foldLeft(
            SchemaBuilder
              .record(name)
              .namespace(namespace) //TODO change this
              .fields()
          ) {
            case (sb, (fieldName, fieldSchema)) => sb.name(fieldName).`type`(toRawAvro(fieldSchema)).noDefault()
          }
          .endRecord()

    }
  }
}
