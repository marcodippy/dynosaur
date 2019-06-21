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
package macros

import shapeless.{Coproduct, HList}

import scala.reflect.macros.blackbox

class SchemaDeriverMacro(val c: blackbox.Context) extends MacroUtils {
  import c.universe._

  private[this] val HListType: Type = typeOf[HList]
  private[this] val CoproductType: Type = typeOf[Coproduct]

  def deriveSchemaDeriver[A: c.WeakTypeTag, R: c.WeakTypeTag]
      : c.Expr[ReprSchema[A, R]] = {
    val tpe = weakTypeOf[A]
    val R = weakTypeOf[R]
    val isHList = R <:< HListType
    val isCoproduct = !isHList && R <:< CoproductType

    log(s"Deriving Schema[${tpe.toString}] \nRepr = ${R.toString}")

    if (!isHList && !isCoproduct) fail(tpe)
    else {
      val members = Members.fromType(R)

      if (isHList && members.underlying.isEmpty) deriveForEmpty[A, R]
      else if (isHList) deriveForProduct[A, R]
      else deriveForCoproduct[A, R](members)
    }
  }

  private[this] def deriveForEmpty[A: c.WeakTypeTag, R: c.WeakTypeTag]
      : c.Expr[ReprSchema[A, R]] = {
    val tpe = weakTypeOf[A]
    val R = weakTypeOf[R]

    val isCaseClass =
      tpe.typeSymbol.asClass.isCaseClass &&
        !tpe.typeSymbol.asClass.isModuleClass // excludes case objects

    val isClass =
      !tpe.typeSymbol.asClass.isCaseClass &&
        !tpe.typeSymbol.asClass.isModuleClass

    val prism =
      if (isCaseClass) {
        q"""Prism[$tpe, Unit](_ => Some(()), _ => ${tpe.typeSymbol.companion}())"""
      } else if (isClass) {
        q"""Prism[$tpe, Unit](_ => Some(()), _ => new ${tpe.typeSymbol}())"""
      } else { // objects & case objects
        q"""Prism[$tpe, Unit](_ => Some(()), _ => ${tpe.termSymbol.name.toTermName})"""
      }

    c.Expr[ReprSchema[A, R]](q"""
      import dynosaur.codec.Prism
      import dynosaur.codec.Schema
      import dynosaur.codec.Schema.structure.Alt
      
      val schema: Schema[$tpe] = Schema.alternatives(cats.data.Chain(new Alt[$tpe] {
        override type B = Unit
        override def caseSchema: Schema[Unit] = Schema.emptyRecord
        override def prism: Prism[$tpe, Unit] = $prism
      }))

      new ReprSchema[$tpe, $R] { override def derive: Schema[$tpe] = schema }
     """)
  }

  private[this] def deriveForProduct[A: c.WeakTypeTag, R: c.WeakTypeTag]
      : c.Expr[ReprSchema[A, R]] = {
    val tpe = weakTypeOf[A]
    val R = weakTypeOf[R]

    val fields = tpe.decls.sorted.collect {
      case m: MethodSymbol if m.isCaseAccessor =>
        val fieldName = m.name.decodedName.toString
        val fieldType = m.info.resultType

        log(s"\tAdding field: $fieldName : $fieldType")

        q"""new dynosaur.codec.Schema.FieldBuilder[$tpe]().apply[$fieldType]($fieldName, dynosaur.codec.Schema[$fieldType], _.$m)"""
    }

    val mapFieldsToRecord =
      if (fields.length == 1) {
        q"""(..$fields).map(${tpe.typeSymbol.companion}.apply)"""
      } else {
        q"""(..$fields).mapN(${tpe.typeSymbol.companion}.apply)"""
      }

    c.Expr[ReprSchema[A, R]](q"""
      import cats.implicits._

      val schema: Schema[$tpe] = Schema.fields($mapFieldsToRecord)

      new ReprSchema[$tpe, $R] { override def derive: Schema[$tpe] = schema }
    """)
  }

  private[this] def deriveForCoproduct[A: c.WeakTypeTag, R: c.WeakTypeTag](
      members: Members
  ): c.Expr[ReprSchema[A, R]] = {
    val tpe = weakTypeOf[A]
    val R = weakTypeOf[R]

    val fields = members.underlying.map { member =>
      val caseType = member.valueType
      val caseName = Character.toLowerCase(member.label.charAt(0)) + member.label
        .substring(1)

      log(s"\tAdding case: $caseName : $caseType")

      q"""new dynosaur.codec.Schema.AltBuilder[$tpe]().apply[$caseType](dynosaur.codec.Schema.tag($caseName)(dynosaur.codec.Schema[$caseType]))"""
    }

    val altFields = fields.fold(
      q"""Monoid[cats.data.Chain[dynosaur.codec.Schema.structure.Alt[$tpe]]].empty"""
    )(
      (t1, t2) => q""" $t1 |+| $t2 """
    )

    c.Expr[ReprSchema[A, R]](q"""
      import cats.Monoid
      import cats.implicits._

      val schema: Schema[$tpe] = Schema.alternatives($altFields)

      new ReprSchema[$tpe, $R] { override def derive: Schema[$tpe] = schema }
    """)
  }

}
