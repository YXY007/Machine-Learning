package clustering.scores


import breeze.linalg.DenseVector

import scala.math._
import scala.reflect.ClassTag

/**
  * Evaluation of the clsutering performance of models based on the normalized mutual information
  * Same as in scikit learn
  * See https://github.com/scikit-learn/scikit-learn/blob/412996f/sklearn/metrics/cluster/supervised.py#L712
  */
object NormalizedMutualInformation extends AbstractScore {


  def forClusterMap[RealClId, FoundClId, DpId](realClusters: Map[RealClId, Set[DpId]], foundClusters: Map[FoundClId, Set[DpId]], nrOfDps: Int): Double = {
    if ((realClusters.isEmpty && foundClusters.isEmpty) ||
      (realClusters.size == 1 && foundClusters.size == 1 && realClusters.head._2 == foundClusters.head._2)) {
      1.0
    } else {
      val hx = labelEntropy(realClusters, nrOfDps)
      val hy = labelEntropy(foundClusters, nrOfDps)
      val miScore = MutualInformation.forClusterMap(realClusters, foundClusters, nrOfDps)
      miScore / max(sqrt(hx * hy), 1e-10)
    }
  }

  protected[scores] def labelEntropy[RealClId, DpId](clustering: Map[RealClId, Set[DpId]], nrOfDps: Int): Double = {
    clustering.foldLeft(0d) { case (parSum, (_, xi)) => parSum + selfInformation(xi.size.toDouble / nrOfDps.toDouble) }
  }

  @inline
  private def selfInformation(p: Double): Double = {
    if (p > 0d)
      -1 * p * log(p)
    else
      0d
  }
}

