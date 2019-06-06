package app

import java.lang.reflect.{Field, Modifier}

object FieldBuster {
  final case class FullFieldName(declaringClass: String, fieldName: String)

  sealed trait JvmObject[+R]
  object JvmObject {
    final case object Null
      extends JvmObject[Nothing]
    final case class Array[R](contents: List[R])
      extends JvmObject[R]
    final case class Dict[R](className: String, contents: Map[FullFieldName, R])
      extends JvmObject[R]
  }

  /** Get all superclasses of the class. */
  def getSuperclasses[T](cls: Class[T]): List[Class[_]] = {
    val sc = cls.getSuperclass
    if (sc == null) Nil else sc :: getSuperclasses(sc)
  }

  /** Get all fields in the class and its superclasses. */
  def getAllClassFields[T](cls: Class[T]): List[Field] =
    (cls :: getSuperclasses(cls)).flatMap(c => c.getDeclaredFields)

  final class Ref(val value: AnyRef) {
    override def equals(that: Any): Boolean =
      if (!that.isInstanceOf[Ref]) false
      else that.asInstanceOf[Ref].value eq this.value

    override def hashCode(): Int = System.identityHashCode(value)

    def project: JvmObject[Ref] = value match {
      case null => JvmObject.Null

      case coll: Array[AnyRef] =>
        JvmObject.Array(
          coll.indices
            .map(index => new Ref(coll(index)))
            .toList
        )

      case _ =>
        JvmObject.Dict(
          value.getClass.getName,
          getAllClassFields(value.getClass)
            .filter(f => !f.getType.isPrimitive)
            .filter(f => !Modifier.isStatic(f.getModifiers))
            .map { f =>
              f.setAccessible(true)
              val declaringClass = f.getDeclaringClass.getName
              val fieldName      = f.getName
              val fullName       = FullFieldName(declaringClass, fieldName)

              fullName -> new Ref(f.get(value))
            }
            .toMap
        )
    }
  }
}