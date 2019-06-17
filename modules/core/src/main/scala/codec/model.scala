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

import cats.kernel.Monoid

object model {

  sealed trait AvroType
  case class AvroInt(value: Int) extends AvroType
  case class AvroString(value: String) extends AvroType
  case class AvroRecord(fields: Map[String, AvroType]) extends AvroType

  object AvroType {

    //TODO see avro4s.AvroSchemaMerge
    implicit val monoidForAvroRecord: Monoid[AvroRecord] =
      new Monoid[AvroRecord] {
        override def empty: AvroRecord = AvroRecord(Map.empty)

        override def combine(x: AvroRecord, y: AvroRecord): AvroRecord =
          AvroRecord(x.fields ++ y.fields)
      }

  }

  sealed trait AvroSchema
  case class AvroIntSchema() extends AvroSchema
  case class AvroStringSchema() extends AvroSchema
  case class AvroRecordSchema() extends AvroSchema

}
