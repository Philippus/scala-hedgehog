package hedgehog

import scala.collection.immutable.SortedMap
import scala.reflect.ClassTag

package object state {

  type TypeMap = SortedMap[Name, ClassTag[_]]
}
