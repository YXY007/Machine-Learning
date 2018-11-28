package algorithm

import java.util.concurrent.atomic.AtomicIntegerArray

import breeze.linalg._
import breeze.stats.distributions.RandBasis
import utils.collection.RichJava._
import statistics.EmpiricalStatistics


object NonRedHelpers extends NonRedHelpers

trait NonRedHelpers {

  /**
    * 随机产生一个有d个维度的旋转矩阵
    */
  def generateRandomV(d: Int)(implicit rand: RandBasis): DenseMatrix[Double] = {
    val a = DenseMatrix.rand(d, d, rand.uniform)
    qr(a).q
  }


  /**
    *
    * @param nrOfDims
    * @param pDims    子空间到整个空间的映射，索引是是投影空间的索引，值是完整空间的索引
    * @param rotation in column form 每一列代表一个数据点的映射方向
    * @return
    */
  def expandRotation2Full(nrOfDims: Int, pDims: IndexedSeq[Int], rotation: DenseMatrix[Double]): DenseMatrix[Double] = {
    if (rotation.rows < nrOfDims) {
      val m = DenseMatrix.eye[Double](nrOfDims)
      for {
        idxInProjA <- pDims.indices
        idxInProjB <- pDims.indices
      } {
        val idxInFullA = pDims(idxInProjA)
        val idxInFullB = pDims(idxInProjB)

        m(idxInFullA, idxInFullB) = rotation(idxInProjA, idxInProjB)
      }
      m
    } else {
      rotation
    }
  }



  def subspaceClusteringInitializer(clusterCenterData: IndexedSeq[DenseVector[Double]],
                                    clusterStatsData: IndexedSeq[DenseVector[Double]],
                                    proj: Projection, k: Int, clusterSampler: (IndexedSeq[DenseVector[Double]], Int) => IndexedSeq[DenseVector[Double]]): SubspaceClustering = {
    // 初始化
    val clusterCentersA = clusterSampler(clusterCenterData, k).zipWithIndex
    val labelsArrayA = new AtomicIntegerArray(clusterCenterData.length)
    val clusterCountsA = new AtomicIntegerArray(k)
    for {
      dpIdx <- clusterCenterData.indices.par
    } {
      val dpA = clusterCenterData(dpIdx)
      val (_, clusterIdA) = clusterCentersA.minBy { mcd =>
        val p1 = dpA - mcd._1
        val r = p1 dot p1
        r
      }
      labelsArrayA.set(dpIdx, clusterIdA)
      clusterCountsA.incrementAndGet(clusterIdA)
    }

    val clusterLabelsFilteredA = (0 until k).filter(k => clusterCountsA.get(k) > 0)
    val label2ClusterMapA = clusterLabelsFilteredA.zipWithIndex.toMap

    val clusterCountsRedA = clusterLabelsFilteredA.map(l => clusterCountsA.get(l))

    val labelsA = labelsArrayA.wrappedAsIndexedSeq

    val meanAndScattersA = EmpiricalStatistics.dataMeanAndScatter(clusterStatsData, labelsA, clusterCountsRedA, label2ClusterMapA)

    val clustersA = (0 until label2ClusterMapA.size).map { cidx =>
      val (mean, scatter) = meanAndScattersA(cidx)
      ClusterStats(mean, scatter)
    }
    if (clustersA.length < k) {
      println("warn empty cluster in init -> retry")
      subspaceClusteringInitializer(clusterCenterData, clusterStatsData, proj, k, clusterSampler)
    } else {
      SubspaceClustering(proj, clustersA, clusterCountsRedA, label2ClusterMapA, labelsA)
    }
  }


  /**
    * 进行assignment step，确定每个簇的中心和散射矩阵
    *
    * @param data
    * @param subspaceClustering
    * @param VT
    * @return
    */
  def findClusterAssignmentSubspace(data: IndexedSeq[DenseVector[Double]],
                                    subspaceClustering: SubspaceClustering,
                                    VT: DenseMatrix[Double]
                                   ): SubspaceClustering = {
    val clusters = subspaceClustering.clusters
    if (clusters.size > 1) {
      val proj = subspaceClustering.projection
      val k = clusters.length
      val labelsArray = new AtomicIntegerArray(data.length)
      val clusterCounts = new AtomicIntegerArray(k)

      val subspaceMapperT = proj.multiplyWithVt(VT)

      val mappedClusters = clusters.zipWithIndex.map { case (x, idx) =>
        val mappedMean = subspaceMapperT * x.fullDimMean
        idx -> mappedMean
      }

      for {
        dpIdx <- data.indices.par
      } {
        val dp = subspaceMapperT * data(dpIdx)
        val (clusterId, _) = mappedClusters.minBy { mcd =>
          val p1 = dp - mcd._2
          val r = p1 dot p1
          r
        }
        labelsArray.set(dpIdx, clusterId)
        clusterCounts.incrementAndGet(clusterId)
      }
      val clusterLabelsFiltered = (0 until k).filter(k => clusterCounts.get(k) > 0)
      val clusterCountsRed = clusterLabelsFiltered.map(l => clusterCounts.get(l))
      val label2ClusterMap = clusterLabelsFiltered.zipWithIndex.toMap

      val labels = labelsArray.wrappedAsIndexedSeq

      val meanAndScatters = EmpiricalStatistics.dataMeanAndScatter(data, labels, clusterCountsRed, label2ClusterMap)

      val newClusters = (0 until label2ClusterMap.size).map { cidx =>
        val label = clusterLabelsFiltered(cidx)
        val (mean, scatter) = meanAndScatters(cidx)
        ClusterStats(mean, scatter)
      }
      SubspaceClustering(proj, newClusters, clusterCountsRed, label2ClusterMap, labels)
    } else {
      subspaceClustering
    }
  }

  /**
    * 计算损失函数
    */
  def costs(nrc: NrkmeansConfig): Double = {
    val Vt = nrc.Vt

    nrc.subspaceClusterings.foldLeft(0d) { case (psum, sc) =>
      val proj = sc.projection
      val PtVt = proj.multiplyWithVt(Vt)
      val VP = PtVt.t

      val scatterSum = sc.sumOfScatterMatrices

      val scCosts: Double = trace(PtVt * scatterSum * VP)
      psum + scCosts
    }
  }
}


object SubspaceReferrer {

  /**
    * 计算特征向量，更新各个子空间的维度
    * 为了保证每个子空间第一个特征值对应的维度是最重要的，需要对特征值进行排序
    */
  type SubspaceReferrer = DenseVector[Double] => (Range, Range)
  /**
    * 根据特征值确定新的m
    * 把特征值小于1e-8的特征值对应的部分划分到噪声空间
    */
  val pushDimsToSubB: DenseVector[Double] => (Range, Range) = { (eigenvals: DenseVector[Double]) =>
    val allEigVals = eigenvals.toArray
    val maxVal = allEigVals.head
    //We put components with small eigenvalues to the noise space
    val firstOfB = max(allEigVals.count(_ / maxVal > 1e-8), 1)
    val subA = 0 until firstOfB
    val subB = firstOfB until eigenvals.length
    subA -> subB.reverse
  }

  /**
    * 把特征值为负的特征向量划分到子空间A，特征值为正的划分到子空间B
    * 论文里特征值为0的可以放到任意一个子空间，这里统一划分到B
    */
  val fairSubspaces: DenseVector[Double] => (Range, Range) = { (eigenvals: DenseVector[Double]) =>
    val allEigVals = eigenvals.toArray
    val nrOfDims = allEigVals.length
    val lastA = allEigVals.lastIndexWhere(_ < 0d)
    val firstB = allEigVals.indexWhere(_ > 0d)
    if (lastA == -1) {
      (0 until 0, allEigVals.indices)
    } else if (firstB == -1) {
      (allEigVals.indices, nrOfDims until nrOfDims)
    } else {
      val firstZero = lastA + 1
      val lastZero = firstB - 1
      val nrOfZeros = lastZero - firstZero + 1
      if (nrOfZeros % 2 == 0) {
        //偶数
        val forEeach = nrOfZeros / 2
        (0 to (lastA + forEeach), (firstB - forEeach) until nrOfDims reverse)
      } else {
        val forEeach = nrOfZeros / 2
        (0 to (lastA + forEeach), (firstB - forEeach - 1) until nrOfDims reverse)
      }
    }
  }
}

case class InitClustersAndRotation(rotationMatrix: DenseMatrix[Double], //The rotation matrix under which the initial configuration was found
                                   labelsA: IndexedSeq[Int],
                                   labelsB: IndexedSeq[Int],
                                   clustersCountsA: IndexedSeq[Int],
                                   clustersCountsB: IndexedSeq[Int])

