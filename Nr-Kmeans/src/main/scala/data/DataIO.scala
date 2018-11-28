package data

import better.files.File
import breeze.linalg._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuilder


object DataIO {
  /**
    * This function loads the data of the given file into a row-based data matrix
    *
    * @param f
    * @param separator the value separator,
    * @param labelCols column indices which contain labels
    */
  def loadCsvWithIntMultiLabelsAsSeq(f: File, labelCols: IndexedSeq[Int], separator: String = ";"): (IndexedSeq[DenseVector[Double]], IndexedSeq[IndexedSeq[Int]]) = {
    val dpSeq = new mutable.ArrayBuilder.ofRef[DenseVector[Double]]
    val lines = f.lines
    val nrOfCols = lines.head.split(separator).length
    val nrOfFeatures = nrOfCols - labelCols.size
    val labelSeq = Array.fill[ArrayBuilder[Int]](labelCols.length)(new ArrayBuilder.ofInt())

    for {
      l <- lines
    } {
      val parts = l.split(separator)
      val labels = Array.ofDim[Int](labelCols.length)
      val features = new DenseVector[Double](nrOfFeatures)
      var fIdx = 0
      var lIdx = 0
      for {
        p <- parts.indices
      } {
        if (labelCols.contains(p)) {
          labelSeq(lIdx) += parts(p).trim.toInt
          lIdx += 1
        } else {
          features(fIdx) = parts(p).trim.toDouble
          fIdx += 1
        }
      }
      dpSeq += features
    }
    dpSeq.result().toIndexedSeq -> labelSeq.map(x => mutable.WrappedArray.make(x.result()))
  }
}
