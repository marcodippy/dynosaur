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
import implicits._
import cats.data.Chain
import Schema.structure._
import avro._

case class ReadError() extends Exception

trait Decoder[A] {
  def read(v: AvroType): Either[ReadError, A]
}
object Decoder {

  def instance[A](f: AvroType => Either[ReadError, A]): Decoder[A] =
    new Decoder[A] {
      def read(v: AvroType) = f(v)
    }

  def fromSchema[A](s: Schema[A]): Decoder[A] = {
    type Res[B] = Either[ReadError, B]

    def decodeInt: AvroType => Res[Int] = {
      case AvroInt(int) => int.asRight[ReadError]
      case _ => ReadError().asLeft[Int]
    }

    def decodeString: AvroType => Res[String] = {
      case AvroString(str) => str.asRight[ReadError]
      case _ => ReadError().asLeft[String]
    }

    def decodeObject[R](record: Ap[Field[R, ?], R], v: AvroRecord): Res[R] =
      record.foldMap {
        λ[Field[R, ?] ~> Res] { field =>
          v.fields
            .get(field.name)
            .toRight(ReadError())
            .flatMap { v =>
              fromSchema(field.elemSchema).read(v)
            }
        }
      }

    def decodeSum[B](cases: Chain[Alt[B]], v: AvroType): Res[B] =
      cases
        .foldMapK { alt =>
          fromSchema(alt.caseSchema).read(v).map(alt.prism.inject).toOption
        }
        .toRight(ReadError())

    s match {
      case Num => Decoder.instance(decodeInt)
      case Str => Decoder.instance(decodeString)
      case Rec(_, _, rec) =>
        Decoder.instance {
          case avroRecord: AvroRecord => decodeObject(rec, avroRecord)
          case _ => ReadError().asLeft[A]
        }
      case Sum(cases) => Decoder.instance { decodeSum(cases, _) }
    }
  }
}
