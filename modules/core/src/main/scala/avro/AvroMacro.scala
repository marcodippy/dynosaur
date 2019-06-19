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

import dynosaur.codec.Schema
import shapeless.LowPriority

import scala.reflect.macros.blackbox
import scala.util.Random

object AvroMacro {
  implicit def createSchemaFor[A](implicit ev: LowPriority): Schema[A] = macro AvroMacroImpl.impl[A]
  def apply[A](implicit s: Schema[A]): Schema[A] = s
}

private class AvroMacroImpl(val c: blackbox.Context) {
  def impl[A: c.WeakTypeTag](ev: c.Expr[LowPriority]): c.Expr[Schema[A]] = {
    import c.universe._

    val aType = weakTypeOf[A]

    println("entro con " + aType.toString.trim)

    val fields =
      aType.decls.sorted.collect {
        case m: MethodSymbol if m.isCaseAccessor =>
          val fieldName = m.name.decodedName.toString
          val fieldType = m.info.resultType
          q"""new FieldBuilder[$aType]().apply[$fieldType]($fieldName, avro.AvroMacro[$fieldType], _.$m)"""
      }

    val nm = "com.mdp2." + Random.nextBoolean().toString + Random.nextInt(100).toString
    val nmspc = "NAME" + Random.nextInt(100)

    c.Expr[Schema[A]](q"""
       import dynosaur.codec._
       import dynosaur.codec.Schema._
       import cats.implicits._

       val schema: Schema[$aType] = record($nmspc, $nm) { field =>
           (..$fields).mapN(${aType.typeSymbol.companion}.apply)
         }

       schema
    """)
  }
}
