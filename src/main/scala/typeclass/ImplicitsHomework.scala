package typeclass

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ImplicitsHomework {

  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {

      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      // Don't use map.scoreSize here because we only care about pure data size, not
      // about overall map size
      private def dataSize: SizeScore = map.iterator.map {
        case (k, v) => k.sizeScore + v.sizeScore
      }.sum

      @tailrec
      private def freeSpace(need: SizeScore): mutable.LinkedHashMap[K, V] =
        if (need <= 0)
          map
        else {
          val (k, v) = map.head
          map.remove(k)
          freeSpace(need - k.sizeScore - v.sizeScore)
        }

      def put(key: K, value: V): Unit = {
        val needSpace = dataSize + key.sizeScore + value.sizeScore - maxSizeScore
        freeSpace(needSpace)
        map.addOne(key, value)
      }

      def get(key: K): Option[V] = map.get(key)
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }

    object Iterate {
      def apply[F[_]: Iterate]: Iterate[F] = implicitly[Iterate[F]]
    }

    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object Iterate2 {
      def apply[F[_, _]: Iterate2]: Iterate2[F] = implicitly[Iterate2[F]]
    }

    object instances {

      import syntax._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }

      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }

      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keysIterator

        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val mutableMapIterate2: Iterate2[mutable.Map] = new Iterate2[mutable.Map] {
        override def iterator1[T, S](f: mutable.Map[T, S]): Iterator[T] = f.keysIterator

        override def iterator2[T, S](f: mutable.Map[T, S]): Iterator[S] = f.valuesIterator
      }

      implicit val packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map(_._1).iterator

        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map(_._2).iterator
      }

      object Primitive {
        val byteScore = 1
        val charScore = 2
        val intScore = 4
        val longScore = 8
        val objHeaderScore = 12
      }

      implicit val byteGetSizeScore: GetSizeScore[Byte] = _ => Primitive.byteScore
      implicit val charGetSizeScore: GetSizeScore[Char] = _ => Primitive.charScore
      implicit val intGetSizeScore: GetSizeScore[Int] = _ => Primitive.intScore
      implicit val longGetSizeScore: GetSizeScore[Long] = _ => Primitive.longScore
      implicit val stringGetSizeScore: GetSizeScore[String] = s => Primitive.objHeaderScore + s.length * Primitive.charScore

      /*
      * Now that's actually incorrect cause Iterate and Iterate2 say nothing about data structure
      * and it's "size". They only say that underlying data is something that can be iterated over.
      *
      * Other problem is that we could naturally implement Iterate for Map and then compiler will have
      * problems choosing which instance of GetSizeScore to use.
      *
      * So let's pretend that's a learning task only and main goal is to implement some typeclass for
      * anything that already have an instance of another typeclass.
      */

      implicit def iterateGetSizeScore[T: GetSizeScore, F[_]: Iterate]: GetSizeScore[F[T]] =
        l => Primitive.objHeaderScore + Iterate[F].iterator(l).map(_.sizeScore).sum

      implicit def iterate2GetSizeScore[K: GetSizeScore, V: GetSizeScore, F[_, _]: Iterate2]: GetSizeScore[F[K, V]] =
        m => Primitive.objHeaderScore +
          Iterate2[F].iterator1(m).map(_.sizeScore).sum +
          Iterate2[F].iterator2(m).map(_.sizeScore).sum
    }

  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {

    import SuperVipCollections4s._
    import syntax._
    import instances._

    final case class Twit(
      id: Long,
      userId: Int,
      hashTags: Vector[String],
      attributes: PackedMultiMap[String, String],
      fbiNotes: List[FbiNote],
    )

    final case class FbiNote(
      month: String,
      favouriteChar: Char,
      watchedPewDiePieTimes: Long,
    )

    trait TwitCache {
      def put(twit: Twit): Unit

      def get(id: Long): Option[Twit]
    }

    implicit val fbiNoteGetSizeScore: GetSizeScore[FbiNote] = n =>
      n.month.sizeScore + n.favouriteChar.sizeScore + n.watchedPewDiePieTimes.sizeScore

    implicit val twitGetSizeScore: GetSizeScore[Twit] = t =>
      Primitive.objHeaderScore +
        t.id.sizeScore +
        t.userId.sizeScore +
        t.hashTags.sizeScore +
        t.attributes.sizeScore +
        t.fbiNotes.sizeScore

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      private val map = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = map.put(twit.id, twit)
      override def get(id: Long): Option[Twit] = map.get(id)
    }
  }

}
