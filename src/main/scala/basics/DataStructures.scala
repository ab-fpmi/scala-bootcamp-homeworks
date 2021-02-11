package basics

object DataStructures {
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    map.groupMap(_._2)(_._1).toList.map {
      case (k, lv) => (lv.toSet, k)
    }
  }
}
