package clustering.scores

import breeze.linalg.DenseVector

import scala.reflect.ClassTag

trait AbstractScore {

  def forLabelVector[DpId](trueLabels: DenseVector[DpId], predictedLabels: DenseVector[DpId])(implicit tag: ClassTag[DpId]): Double = forLabelSeq(trueLabels.toArray, predictedLabels.toArray)


  def forClusterSet[DpId](realClusters: Seq[Set[DpId]], foundClusters: Seq[Set[DpId]], nrOfDps: Int): Double = {
    val rc = realClusters.zipWithIndex.map(x => x._2 -> x._1).toMap
    val fc = foundClusters.zipWithIndex.map(x => x._2 -> x._1).toMap
    forClusterMap(rc, fc, nrOfDps)
  }

  def forLabelSeq[CLIDA, CLIDB](trueLabels: Seq[CLIDA], predictedLabels: Seq[CLIDB]): Double = {
    assert(trueLabels.size == predictedLabels.size, "Nr of data points in the sequence has to be equal")
    val X = trueLabels.zipWithIndex.groupBy(_._1).map { case (label, seq) => label -> seq.map(_._2).toSet }
    val Y = predictedLabels.zipWithIndex.groupBy(_._1).map { case (label, seq) => label -> seq.map(_._2).toSet }
    if (X.size == Y.size && (X.isEmpty || X.size == 1)) {
      1d
    } else {
      forClusterMap(X, Y, trueLabels.size)
    }
  }

  def forClusterMap[RealClId, FoundClId, DpIds](realClusters: Map[RealClId, Set[DpIds]], foundClusters: Map[FoundClId, Set[DpIds]], nrOfDps: Int): Double

}
