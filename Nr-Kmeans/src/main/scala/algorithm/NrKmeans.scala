package algorithm

import breeze.linalg._
import breeze.stats.distributions.RandBasis
import clustering.scores.NormalizedMutualInformation
import statistics.EmpiricalStatistics
import utils.matrix.MatrixUtils


object NrKmeans {

  val MAX_NR_OF_ITERATIONS = 50 // 最大迭代次数
  val ROUNDS_WITHOUT_PRUNING_M = 4 //提前结束迭代的条件：加入连续四轮没有更新m，结束迭代
  val MAX_NMI_DIFFERENCE = 0.0001 // 收敛条件，前后变化差

  def run(ks: IndexedSeq[Int], data: IndexedSeq[DenseVector[Double]], initSubspaceSizes: Option[IndexedSeq[Int]] = None,
          containsNoiseSpace: Boolean = true, fixedSubspaceSize: Boolean = false)(implicit rnd: RandBasis): NrkmeansConfig = {
    val initConfig = init(if (containsNoiseSpace) {
      ks :+ 1
    } else {
      ks
    }, data, initSubspaceSizes)
    run(initConfig, fixedSubspaceSize)
  }


  def run(initConfig: NrkmeansConfig, fixedSubspaceSize: Boolean)(implicit rnd: RandBasis): NrkmeansConfig = {
    var currentConfig: NrkmeansConfig = initConfig
    var counter = 0

    var converged: Boolean = false
    var lastConfig = initConfig

    do {
      counter += 1
      currentConfig = updateRotation(currentConfig, !fixedSubspaceSize && counter >= ROUNDS_WITHOUT_PRUNING_M)
      currentConfig = assignmentAndClusterStatistics(currentConfig)


      converged = currentConfig.subspaceClusterings.zip(lastConfig.subspaceClusterings).forall { case (c, l) =>
        val nmi = NormalizedMutualInformation.forLabelSeq(c.labels, l.labels)
        nmi > 1d - MAX_NMI_DIFFERENCE
      }

      lastConfig = currentConfig
    } while (!converged && (counter < MAX_NR_OF_ITERATIONS) || (!fixedSubspaceSize && counter <= ROUNDS_WITHOUT_PRUNING_M))

    currentConfig
  }

  // 初始化
  def init(ks: IndexedSeq[Int], data: IndexedSeq[DenseVector[Double]], initSubspaceSizes: Option[IndexedSeq[Int]])(implicit rnd: RandBasis): NrkmeansConfig = {
    val nrOfDims = data.head.length
    // 随机生成一个正交的旋转矩阵
    val randVt = NonRedHelpers.generateRandomV(nrOfDims)
    val nrOfSubspaces = ks.size
    val initmRanges = if (initSubspaceSizes.isDefined) {
      val sizes = initSubspaceSizes.get
      assert(sizes.length == ks.length)
      assert(sizes.sum == nrOfDims)
      var firstDim = 0
      // 把维度平均分配到每个子空间
      for {
        k <- 0 until nrOfSubspaces
      } yield {
        val curSize = sizes(k)
        val r = firstDim until (firstDim + curSize)
        firstDim += curSize
        r
      }
    } else {
      val initMSize = nrOfDims / nrOfSubspaces
      var firstDim = 0
      (0 until nrOfSubspaces).map { k =>
        val endOfRange = if (k == nrOfSubspaces - 1) nrOfDims else firstDim + initMSize
        val range = firstDim until endOfRange
        firstDim += initMSize
        range
      }
    }
    // 旋转
    val rotData = data.map(dp => randVt * dp)

    // 计算每个子空间里的P
    val initSubspaces = ks.zipWithIndex.map { case (k, ssIdx) =>
      val range = initmRanges(ssIdx)
      val proj = Projection(range, nrOfDims)

      val dpProj = proj.projectDps(rotData)
      NonRedHelpers.subspaceClusteringInitializer(dpProj, data, proj, k, KMeansPlusPlus.kMeansPlusPlus) //InitClusterCentroids.useRandomDps)
    }.toIndexedSeq

    NrkmeansConfig(randVt, initSubspaces, data)
  }

  // assignment step
  private def assignmentAndClusterStatistics(oldConfig: NrkmeansConfig): NrkmeansConfig = {
    val newSubspaces = oldConfig.subspaceClusterings.map { subspaceClustering =>
      NonRedHelpers.findClusterAssignmentSubspace(oldConfig.data, subspaceClustering, oldConfig.Vt)
    }
    oldConfig.copy(subspaceClusterings = newSubspaces)
  }

  // 更新V
  private def updateRotation(oldConfig: NrkmeansConfig, adjustSubspaceDims: Boolean): NrkmeansConfig = {
    val totalNrOfDims = oldConfig.nrOfDims
    var remainingScatterSums = oldConfig.subspaceClusterings.map(s => (s.projection, s.sumOfScatterMatrices, s.nrOfClusters))
    var newVt = oldConfig.Vt
    var lastSubBProj: Projection = null
    val newProjections = for {
      j <- 0 until (oldConfig.subspaceClusterings.length - 1)
    } yield {
      var (subAProj, subAScatterSum, _) = remainingScatterSums(0)
      //根据Vt旋转
      remainingScatterSums = for {
        i <- 1 until remainingScatterSums.length
      } yield {
        val (subBProj, subBScatterSum, subBNrOfClusters) = remainingScatterSums(i)
        val pDimsA = subAProj.pDims
        val pDimsB = subBProj.pDims
        val combinedpDims = pDimsA ++ pDimsB
        val inverseMap = combinedpDims.zipWithIndex.map(_.swap).toMap

        //旋转散射
        val combinedProj = Projection(combinedpDims, totalNrOfDims)
        val mapperVt = combinedProj.multiplyWithVt(newVt)
        val setup = {
          val a = mapperVt * (subAScatterSum - subBScatterSum) * mapperVt.t
          (a + a.t) / 2d 
        }
        val eigRes = MatrixUtils.sortedEigSym(setup, ascending = true)
        val ranges = if (adjustSubspaceDims) {
          if (subBNrOfClusters == 1) {
            SubspaceReferrer.pushDimsToSubB(eigRes.eigenvalues)
          } else {
            SubspaceReferrer.fairSubspaces(eigRes.eigenvalues)
          }
        } else {
          pDimsA.indices -> (pDimsA.length until combinedpDims.length reverse)
        }
        newVt = NonRedHelpers.expandRotation2Full(totalNrOfDims, combinedpDims, eigRes.eigenvectors.t) * newVt
        subAProj = Projection(ranges._1.map(x => inverseMap(x)), totalNrOfDims)
        val newProjB = Projection(ranges._2.map(x => inverseMap(x)), totalNrOfDims)
        lastSubBProj = newProjB

        (newProjB, subBScatterSum, subBNrOfClusters)
      }
      subAProj
    }

    val newProjSeq = newProjections.:+(lastSubBProj)

    val newSubspaceConfig = oldConfig.subspaceClusterings zip newProjSeq map { case (ss, proj) => ss.copy(projection = proj) }
    oldConfig.copy(Vt = newVt, subspaceClusterings = newSubspaceConfig)
  }
}
