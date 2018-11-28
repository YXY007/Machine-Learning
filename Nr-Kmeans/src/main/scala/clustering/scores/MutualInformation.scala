package clustering.scores

import scala.math._

object MutualInformation extends AbstractScore  {

  //  MI(U,V)=\sum_{i=1}^R \sum_{j=1}^C P(i,j)\log\\frac{P(i,j)}{P(i)P'(j)}
  def forClusterMap[RealClId, FoundClId, DpIds](realClusters: Map[RealClId, Set[DpIds]], foundClusters: Map[FoundClId, Set[DpIds]], nrOfDps: Int): Double = {
    realClusters.foldLeft(0d) {
      case (pSumX, (xlabel, xIds)) =>
        pSumX + foundClusters.foldLeft(0d) {
          case (pSumY, (ylabel, yIds)) =>
            val pij = xIds.intersect(yIds).size.toDouble / nrOfDps
            val pi = xIds.size.toDouble / nrOfDps
            val pj = yIds.size.toDouble / nrOfDps

            val partMi = if (pij > 0d)
              pij * log(pij / (pi * pj))
            else
              0d
            pSumY + partMi
        }
    }
  }
}
