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
package generic

import dynosaur.codec.Schema
import shapeless.{LabelledGeneric, Lazy}

trait SchemaDeriver[A] {
  def derive: Schema[A]
}

object SchemaDeriver {

  implicit def deriveSchemaDeriver[A, R](
      implicit gen: LabelledGeneric.Aux[A, R],
      reprSchema: Lazy[ReprSchema[A, R]]
  ): SchemaDeriver[A] =
    new SchemaDeriver[A] {
      override def derive: Schema[A] = reprSchema.value.derive
    }

}
