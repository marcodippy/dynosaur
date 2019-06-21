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

import shapeless.{CNil, HList, HNil}
import shapeless.labelled.KeyTag

import scala.reflect.macros.blackbox

/*
  This code has been copied from circe
 */
trait MacroUtils {
  val c: blackbox.Context
  import c.universe._

  private[macros] case class Member(
      label: String,
      keyType: Type,
      valueType: Type,
      acc: Type,
      accTail: Type
  )
  private[macros] class Members(val underlying: List[Member])

  private[macros] object Members {
    private[this] val ShapelessSym = typeOf[HList].typeSymbol.owner
    private[this] val HNilSym = typeOf[HNil].typeSymbol
    private[this] val HConsSym = typeOf[shapeless.::[_, _]].typeSymbol
    private[this] val CNilSym = typeOf[CNil].typeSymbol
    private[this] val CConsSym = typeOf[shapeless.:+:[_, _]].typeSymbol
    private[this] val ShapelessLabelledType = typeOf[shapeless.labelled.type]
    private[this] val KeyTagSym = typeOf[KeyTag[_, _]].typeSymbol
    private[this] val ShapelessTagType = typeOf[shapeless.tag.type]
    private[this] val ScalaSymbolType = typeOf[scala.Symbol]

    case class Entry(label: String, keyType: Type, valueType: Type)

    object Entry {
      def unapply(tpe: Type): Option[(String, Type, Type)] = tpe.dealias match {
        case RefinedType(parents, scope) =>
          parents.reverse match {
            case TypeRef(lt, KeyTagSym, List(tagType, taggedFieldType)) :: refs
                if lt =:= ShapelessLabelledType && internal
                  .refinedType(refs.reverse, scope) =:= taggedFieldType =>
              tagType.dealias match {
                case RefinedType(
                    List(
                      st,
                      TypeRef(
                        tt,
                        _,
                        ConstantType(Constant(fieldKey: String)) :: Nil
                      )
                    ),
                    _
                    ) if st =:= ScalaSymbolType && tt =:= ShapelessTagType =>
                  Some((fieldKey, tagType, taggedFieldType))
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }
    }

    def fromType(tpe: Type): Members = {
      tpe.dealias match {
        case TypeRef(ThisType(ShapelessSym), HNilSym | CNilSym, Nil) =>
          new Members(Nil)
        case acc @ TypeRef(
              ThisType(ShapelessSym),
              HConsSym | CConsSym,
              List(fieldType, tailType)
            ) =>
          fieldType match {
            case Entry(label, keyType, valueType) =>
              new Members(
                Member(label, keyType, valueType, acc, tailType) :: fromType(
                  tailType
                ).underlying
              )
            case _ => fail(tpe)
          }
        case _ => fail(tpe)
      }
    }
  }

  private[macros] def log(msg: String): Unit = ()
  // uncomment this to get the output
  // c.info(null, msg, force = true)

  private[macros] def fail(tpe: Type): Nothing =
    c.abort(c.enclosingPosition, s"Cannot generically derive instance: $tpe")
}
