package algorithm

import breeze.linalg._

case class NrkmeansConfig(Vt: DenseMatrix[Double], subspaceClusterings: IndexedSeq[SubspaceClustering], data: IndexedSeq[DenseVector[Double]]) {

  val nrOfDims = data.head.length

  val nrOfDataPoints = data.length

  val nrOfSubspaces = subspaceClusterings.length


  def labelsAsSeq = {
    val clusterSpace = subspaceClusterings.filter(_.nrOfClusters > 1)
    data.indices.map{dpi =>
      clusterSpace.map(ss => ss.label2Cluster(ss.labels(dpi)))
    }
  }
}

case class Projection(pDims: IndexedSeq[Int], nrOfFullDim: Int) {
  private def projectionMatrix(nrOfDims: Int, pDims: IndexedSeq[Int]): DenseMatrix[Double] = {
    assert(pDims.forall(x => x >= 0 && x < nrOfDims))
    val m = DenseMatrix.zeros[Double](nrOfDims, pDims.size)
    for {
      idx <- pDims.indices
    } {
      m(pDims(idx), idx) = 1d
    }
    m
  }

  def isEmpty = pDims.isEmpty

  lazy val projT = projectionMatrix(nrOfFullDim, pDims).t

  def projectDps(dps: IndexedSeq[DenseVector[Double]]): IndexedSeq[DenseVector[Double]] = {
    val vecLength = pDims.length
    dps.map { dp =>
      val projDp = DenseVector.zeros[Double](vecLength)
      for {
        idx <- pDims.indices
      } {
        projDp(idx) = dp(pDims(idx))
      }
      projDp
    }
  }

  /**
    * 计算P.t * V.t
    */
  def multiplyWithVt(Vt: DenseMatrix[Double]) = Vt(pDims, ::).toDenseMatrix
}

object Projection {
  /**
    * 返回子空间到整空间的映射
    *
    * @return
    */
  def complement(proj: Projection): Projection = {
    val pDimsSet = proj.pDims.toSet
    Projection((0 until proj.nrOfFullDim).filterNot(pDimsSet.contains), proj.nrOfFullDim)
  }
}

/**
  * @param 映射矩阵
  */
case class SubspaceClustering(projection: Projection,
                              clusters: IndexedSeq[ClusterStats],
                              nrOfDpsPerCluster: IndexedSeq[Int],
                              label2Cluster: Map[Int, Int],
                              labels: IndexedSeq[Int]) {
  val m: Int = projection.pDims.length

  @inline
  def nrOfClusters: Int = clusters.length

  val sumOfScatterMatrices: DenseMatrix[Double] = {
    var sum = clusters.head.fullDimScatterMatrix
    for {
      c <- clusters.tail
    } {
      sum = sum + c.fullDimScatterMatrix
    }
    sum
  }

  def dpIdsPerCluster : IndexedSeq[Set[Int]] =
      labels.zipWithIndex.groupBy(_._1).toSeq.sortBy(x => label2Cluster(x._1)).map{_._2.map(_._2).toSet}.toIndexedSeq

}

/**
  * @param fullDimMean
  * @param fullDimScatterMatrix
  */
case class ClusterStats(fullDimMean: DenseVector[Double],
                        fullDimScatterMatrix: DenseMatrix[Double])