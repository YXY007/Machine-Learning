package utils.matrix

import breeze.linalg._
import breeze.linalg.eig.{DenseEig, Eig}
import breeze.linalg.eigSym.{DenseEigSym, EigSym}
import breeze.numerics.abs

object MatrixUtils {

  def robustInvert(d: DenseMatrix[Double], addSkalar : Double = 1e-5): DenseMatrix[Double] = {
    try {
      inv(d)
    } catch {
      case x: MatrixSingularException =>
        robustInvert(d + DenseMatrix.eye[Double](d.rows) * addSkalar)
    }
  }


  /**
    * 特征值重组,结果从小到大排序
    *
    * @param data
    * @param absoluteValue true：以绝对值排序，false：直接排序
    */
  def sortedEig(data: DenseMatrix[Double], ascending: Boolean, absoluteValue: Boolean): DenseEig = {
    val res = eig(data)
    val resIndexed = res.eigenvalues.toArray.zipWithIndex
    val sortedRes = if (ascending) {
      if (absoluteValue) {
        resIndexed.sortBy(x => abs(x._1)).map(_._2)
      } else {
        resIndexed.sortBy(_._1).map(_._2)
      }
    } else {
      if (absoluteValue) {
        resIndexed.sortBy(x => abs(x._1) * -1d).map(_._2)
      } else {
        resIndexed.sortBy(_._1 * -1d).map(_._2)
      }
    }

    val eigvec = res.eigenvectors.copy
    val eigvals = res.eigenvalues.copy
    val eigvalsClx = res.eigenvaluesComplex.copy
    for {
      i <- 0 until res.eigenvalues.length
    } {
      eigvec(::, i) := res.eigenvectors(::, sortedRes(i))
      eigvals(i) = res.eigenvalues(sortedRes(i))
      eigvalsClx(i) = res.eigenvaluesComplex(sortedRes(i))
    }
    Eig(eigvals, eigvalsClx, eigvec)
  }


  /**
    * 特征值重组，结果从小到大排序
    * @param data 一个对称矩阵
    */
  def sortedEigSym(data: DenseMatrix[Double], ascending: Boolean): DenseEigSym = {
    val res = eigSym(data)


    val resIndexed = res.eigenvalues.toArray.zipWithIndex
    val sortedRes = if (ascending) {
      resIndexed.sortBy(_._1).map(_._2)
    } else {
      resIndexed.sortBy(_._1 * -1d).map(_._2)
    }

    val eigvec = res.eigenvectors.copy
    val eigvals = res.eigenvalues.copy
    for {
      i <- 0 until res.eigenvalues.length
    } {
      eigvec(::, i) := res.eigenvectors(::, sortedRes(i))
      eigvals(i) = res.eigenvalues(sortedRes(i))
    }
    EigSym(eigvals, eigvec)
  }


  /**
    * 重构矩阵, 再重新计算特征向量
    *
    * @param m
    * @param f
    * @return
    */
  def applyToEigVals(m: DenseMatrix[Double], f: Double => Double): DenseMatrix[Double] = {
    val res = eig(m)
    res.eigenvectors * diag(res.eigenvalues.mapValues(f)) * res.eigenvectors.t
  }
}
