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

import cats._
import cats.implicits._
import cats.data.Chain
import Schema.structure._
import avro._

case class WriteError() extends Exception

trait Encoder[A] {
  def write(a: A): Either[WriteError, AvroType]
}
object Encoder {

  def instance[A](f: A => Either[WriteError, AvroType]): Encoder[A] =
    new Encoder[A] {
      def write(a: A): Either[WriteError, AvroType] = f(a)
    }

  def fromSchema[A](s: Schema[A]): Encoder[A] = {
    type Res = Either[WriteError, AvroType]

    def encodeInt: Int => Res = AvroInt(_).asRight

    def encodeString: String => Res = AvroString(_).asRight

    def encodeObject[R](name: String, namespace: String, record: Ap[Field[R, ?], R], v: R): Res =
      record
        .analyze {
          λ[Field[R, ?] ~> λ[a => Either[WriteError, AvroRecord]]] { field =>
            fromSchema(field.elemSchema).write(field.get(v)).map { avroType =>
              AvroRecord(name, namespace, Map(field.name -> avroType))
            }
          }
        }
        .map(_.copy(name = name, namespace = namespace))
        .widen[AvroType]

    def encodeSum[C](cases: Chain[Alt[C]], v: C): Res =
      cases
        .foldMapK { alt =>
          alt.prism.tryGet(v).map { e =>
            fromSchema(alt.caseSchema).write(e)
          }
        }
        .getOrElse(WriteError().asLeft)

    s match {
      case Num => Encoder.instance(encodeInt)
      case Str => Encoder.instance(encodeString)
      case Rec(name, namespace, rec) => Encoder.instance(v => encodeObject(name, namespace, rec, v))
      case Sum(cases) => Encoder.instance(v => encodeSum(cases, v))
    }
  }
}
