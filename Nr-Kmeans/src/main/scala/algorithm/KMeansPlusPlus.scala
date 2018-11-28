package algorithm

import breeze.linalg.DenseVector
import breeze.stats.distributions.{Rand, RandBasis}

object KMeansPlusPlus {

  /**
    * Run K-means++ init on the weighted point set `data`.
    * Adopted from LocalKMeans in Spark (Apache License)
    */
  def kMeansPlusPlusWeighted(data: IndexedSeq[DenseVector[Double]],
                             weights: IndexedSeq[Double],
                             k: Int
                            )(implicit rand: RandBasis): IndexedSeq[DenseVector[Double]] = kMeansPlusPlusGen(data, weights.apply, weights.sum, k)


  /**
    * Run K-means++ init on the weighted point set `data`.
    * Adopted from LocalKMeans in Spark (Apache License)
    */
  def kMeansPlusPlus(data: IndexedSeq[DenseVector[Double]],
                     k: Int
                    )(implicit rand: RandBasis): IndexedSeq[DenseVector[Double]] =  kMeansPlusPlusGen(data, _ => 1d , data.length.toDouble, k)

  private def kMeansPlusPlusGen(data: IndexedSeq[DenseVector[Double]],
                                idx2Weight: Int => Double,
                                totalWeights: Double,
                                k: Int
                               )(implicit rand: RandBasis): IndexedSeq[DenseVector[Double]] = {

    val centers = new Array[DenseVector[Double]](k)

    val uniform = rand.uniform

    // Initialize centers by sampling using the k-means++ procedure.
    centers(0) = pickWeighted(uniform, data, idx2Weight,totalWeights)
    val costArray = data.map { dp =>
      val p2 = centers(0) - dp
      p2.t * p2
    }.toArray

    for (i <- 1 until k) {
      val sum = costArray.zipWithIndex.map(p => p._1 * idx2Weight(p._2)).sum
      val r = uniform.sample() * sum
      var cumulativeScore = 0.0
      var j = 0
      while (j < data.length && cumulativeScore < r) {
        cumulativeScore += idx2Weight(j) * costArray(j)
        j += 1
      }
      if (j == 0) { //TODO
        println("kMeansPlusPlus initialization ran out of distinct points for centers." +
          s" Using duplicate point for center k = $i.")
        centers(i) = data(0)
      } else {
        centers(i) = data(j - 1)
      }

      // update costArray
      for (p <- data.indices) {
        costArray(p) = math.min({
          val p2 = centers(i) - data(p)
          p2.t * p2
        }, costArray(p))
      }
    }
    centers
  }


  private def pickWeighted(rand: Rand[Double], data: IndexedSeq[DenseVector[Double]], idx2Weight: Int => Double, totalWeights : Double): DenseVector[Double] = {
    val r = rand.sample() * totalWeights
    var i = 0
    var curWeight = 0.0
    while (i < data.length && curWeight < r) {
      curWeight += idx2Weight(i)
      i += 1
    }
    data(i - 1)
  }
}
