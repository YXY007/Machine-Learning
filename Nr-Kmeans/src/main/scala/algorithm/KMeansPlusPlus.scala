package algorithm

import breeze.linalg.DenseVector
import breeze.stats.distributions.{Rand, RandBasis}

object KMeansPlusPlus {

  /**
    * 使用Kmeans++来初始化中心点
    */
  def kMeansPlusPlusWeighted(data: IndexedSeq[DenseVector[Double]],
                             weights: IndexedSeq[Double],
                             k: Int
                            )(implicit rand: RandBasis): IndexedSeq[DenseVector[Double]] = kMeansPlusPlusGen(data, weights.apply, weights.sum, k)


  /**
    * 使用Kmeans++来初始化中心点
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

  /** 随机产生第一个点作为第一个类的初始中心点
    * 计算每个点到初始点的距离，取最长的点作为第二个中心点
    * 计算每个点到已确定的中心点的距离，取最短距离作为d
    * 取d最大的点作为下一个中心点
    * 以此类推
    */
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

      // 更新损失函数
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
