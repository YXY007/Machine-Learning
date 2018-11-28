package utils.collection

import java.util.concurrent.atomic.AtomicIntegerArray


object RichJava {

  implicit class RichAtomicIntegerArray(s: AtomicIntegerArray) {

    /**
      * We assume that s DOES NOT change after it is wrapped
      * @return
      */
    def wrappedAsIndexedSeq = {
      new IndexedSeq[Int]() {
        override def length: Int = s.length()
        override def apply(idx: Int): Int = s.get(idx)
      }
    }

  }

}
