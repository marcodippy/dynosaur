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
import dynosaur.generic.macros.SchemaDeriverMacro

trait ReprSchema[A, R] {
  def derive: Schema[A]
}

object ReprSchema {
  implicit def deriveReprSchema[A, Repr]: ReprSchema[A, Repr] =
    macro SchemaDeriverMacro.deriveSchemaDeriver[A, Repr]
}
