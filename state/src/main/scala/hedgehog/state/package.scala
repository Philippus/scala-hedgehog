package hedgehog

import scala.reflect.ClassTag

package object state {

  type TypeMap = Map[Name, ClassTag[_]]
}
