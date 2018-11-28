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
    * Performs an eigenvalue decomposition,
    * but the result is sorted ascending from smallest to biggest (the breeze eig function sorts also ascending but the absolute value!)
    *
    * @param data
    * @param absoluteValue if true we sort based on the absolute value
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
    * Performs an eigenvalue decomposition,
    * but the result is sorted ascending from smallest to biggest (the breeze eig function does not sort!)
    *
    * @param data The matrix has to be symmetric!
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
    * Applies an operation to the eigenvalues of a matrix.
    * It decomposes the matrix, applies the operation and multiplies the eigenvectors again.
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
