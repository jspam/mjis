package mjis.eval

import mjis.Position
import mjis.ast._
import mjis.Builtins.{IntDecl, BooleanDecl, VoidDecl, NullDecl}

import scala.collection.mutable.{Map => MMap}

object Instance {

  val True = Instance.ofBoolean(true)
  val False = Instance.ofBoolean(false)
  val Void = Instance.apply(VoidDecl)
  val Null = Instance.apply(NullDecl)

  def apply(decl: ClassDecl): Instance = {
    decl match {
      case IntDecl => Instance(IntDecl, MMap("value" -> (0: Any)))
      case BooleanDecl => Instance(BooleanDecl, MMap("value" -> (false: Any)))
      case _ => Instance(decl, MMap[String, Any]() ++ decl.fields.map(field => (field.name, null: Any)))
    }
  }
  def zero(decl: ClassDecl): Instance = {
    decl match {
      case IntDecl => Instance(IntDecl, MMap("value" -> (0: Any)))
      case BooleanDecl => Instance(BooleanDecl, MMap("value" -> (false: Any)))
      case _ => Instance(null, MMap("value" -> (null: Any)))
    }
  }
  def uninitializedLocal(typ: TypeDef): Instance =
    typ match {
      case typ: TypeBasic => Instance.zero(typ.decl)
      case arr: TypeArray => Instance(null, MMap("value" -> (null: Any)))
    }
  def dummyArrayDecl(elementClass: ClassDecl, dimension: Int) =
    new ClassDecl(s"array[$elementClass]/dim:$dimension", Nil, Nil)(Position.NoPosition)
  def ofInt(value: Int) =
    new Instance(IntDecl, MMap("value" -> (value: Any)))
  def ofBoolean(value: Boolean) =
    new Instance(BooleanDecl, MMap("value" -> (value: Any)))
  def ofArray(elementClass: ClassDecl, dimension: Int) =
    // Instance.zero(elementClass) is not quite correct, because the value needs to be derived from
    // dummyArrayDecl(elementClass, dimension-1) but we are using this mostly for debugging anyway ...
    new Instance(
      dummyArrayDecl(elementClass, dimension),
      MMap("value" -> (Array.fill[Any](dimension)(Instance.zero(elementClass)): Any)))
}

final case class Instance(val decl: ClassDecl,var fieldsAndValues: MMap[String, Any])
