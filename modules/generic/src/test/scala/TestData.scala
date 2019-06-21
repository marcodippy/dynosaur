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

object TestData {

  case class User(name: String, age: Int)
  case class Employee(id: String, user: User)

  object University

  case class Student(id: String, teacher: Teacher)
  case class Teacher(name: String, uni: University.type)

  object Teacher {
    import dynosaur.codec.Schema
    import dynosaur.generic.semiauto._

    implicit val schemaForUni: Schema[University.type] =
      deriveSchema[University.type]

    implicit val schemaForTeacher: Schema[Teacher] = deriveSchema[Teacher]
  }

  case class JustACaseClass()

  class JustAClass

  case object JustACaseObject

  object JustAnObject

  trait JustATrait

  sealed trait JustASealedTrait
  object JustASealedTrait {
    val create: JustASealedTrait = new JustASealedTrait {}
  }

  sealed trait Status
  case class Open(num: Int) extends Status
  object HalfOpen extends Status
  case object Closed extends Status
  case class Null(msg: String) extends Status

  sealed abstract class Opt
  case object Nn extends Opt
  case class Sm(a: String) extends Opt

  sealed trait NestedData
  case class Robo(name: String, teacher: Teacher) extends NestedData
  case class Cop(ammo: Int, status: Opt) extends NestedData

  sealed trait MyList
  object MyNil extends MyList
  case class MyCons(h: String, t: MyList) extends MyList

}
