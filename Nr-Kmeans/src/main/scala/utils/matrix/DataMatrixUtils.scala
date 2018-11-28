package utils.matrix

import breeze.linalg.{*, DenseMatrix, DenseVector}

object DataMatrixUtils {

  def fromDataSeq(data: IndexedSeq[DenseVector[Double]]): DenseMatrix[Double] = DenseVector.horzcat(data: _*)

  def from2DArray(m: Array[Array[Double]]): DenseMatrix[Double] = DenseMatrix(m: _*)


  /**
    *
    * @param m
    * @return 二维数组
    */
  def to2dArray(m: DenseMatrix[Double]): Array[Array[Double]] = {
    //TODO make  查看哪一个比较快
    //m(*, ::).map(_.toArray).toArray
    val d = Array.ofDim[Double](m.rows, m.cols)
    for {
      r <- 0 until m.rows
      c <- 0 until m.cols
    } {
      d(r)(c) = m(r, c)
    }
    d
  }

}
