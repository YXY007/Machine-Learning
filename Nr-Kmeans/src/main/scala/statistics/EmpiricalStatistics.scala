package statistics

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.math._


object EmpiricalStatistics {

  /**
    *  数据统计：计算均值和散射值
    *
    * @param data             已分好子空间的数据
    * @param labels           标签
    * @param dpCounter        每个类数据点的数量
    * @param label2ClusterMap 类标签和索引的映射
    * @return
    */
  def dataMeanAndScatter(data: IndexedSeq[DenseVector[Double]], labels: IndexedSeq[Int], dpCounter: IndexedSeq[Int], label2ClusterMap: Map[Int, Int]): IndexedSeq[(DenseVector[Double], DenseMatrix[Double])] = {
    assert(data.length == labels.length)
    val dims = data.head.length
    val dpRange = data.indices
    val meansAndScatters = Array.fill(label2ClusterMap.size)((DenseVector.zeros[Double](dims), DenseMatrix.zeros[Double](dims, dims)))
    for {
      dpIdx <- dpRange
    } {
      val clIdx = label2ClusterMap(labels(dpIdx))
      meansAndScatters(clIdx)._1 :+= data(dpIdx)
    }
    for {
      clIdx <- meansAndScatters.indices
    } {
      meansAndScatters(clIdx)._1 :/= dpCounter(clIdx).toDouble

    }
    for {
      dpIdx <- dpRange
    } {
      val (mean, scatter) = meansAndScatters(label2ClusterMap(labels(dpIdx)))

      val dpArray = (data(dpIdx) - mean).data
      for {
        dpi <- 0 until dims
        dpj <- dpi until dims
      } {
        val p = dpArray(dpi) * dpArray(dpj)
        scatter(dpi, dpj) += p
        if (dpj > dpi) {
          scatter(dpj, dpi) += p
        }
      }
    }
    meansAndScatters
  }

  // 计算均值
  def dataMean(data: Seq[DenseVector[Double]]): DenseVector[Double] = {
    val tmpV = DenseVector.zeros[Double](data.head.length)
    data.foreach(dv => tmpV :+= dv)
    tmpV / data.size.toDouble
  }

  // 计算均值和协方差
  def dataMeanAndCov(data: Seq[DenseVector[Double]], adjustCov: Boolean) = {
    val mean = dataMean(data)
    val meaned = data.map(r => r - mean)
    val cov = determineCov(meaned, adjustCov)
    (mean, cov)
  }

  // 计算均值和散射
  def dataMeanAndScatter(data: Seq[DenseVector[Double]]) = {
    val mean = dataMean(data)
    val scatter = determineScatterUncentered(data, mean)
    (mean, scatter)
  }

  // 计算协方差
  def dataCov(data: Seq[DenseVector[Double]], assumeCentered: Boolean, adjustCov: Boolean) = {
    if (assumeCentered) {
      determineCov(data, adjustCov)
    } else {
      dataMeanAndCov(data, adjustCov)._2
    }
  }

  // 计算协方差
  private def determineCov(centered: Seq[DenseVector[Double]], adjustCov: Boolean): DenseMatrix[Double] = {
    val scatter = determineScatterCentered(centered)
    if (adjustCov) {
      scatter / (centered.size.toDouble - 1)
    } else {
      scatter / centered.size.toDouble
    }
  }

  //计算散射
  private def determineScatterCentered(centered: Seq[DenseVector[Double]]): DenseMatrix[Double] = {
    val dims = centered.head.length
    val scatter = DenseMatrix.zeros[Double](dims, dims)
    for {
      dp <- centered
    } {
      val dpArray = dp.data
      for {
        dpi <- 0 until dims
        dpj <- dpi until dims
      } {
        val p = dpArray(dpi) * dpArray(dpj)

        scatter(dpi, dpj) += p
        if (dpj > dpi) {
          scatter(dpj, dpi) += p
        }
      }
    }
    scatter
  }


  //计算散射
  private def determineScatterUncentered(uncentered: Seq[DenseVector[Double]], mean: DenseVector[Double]): DenseMatrix[Double] = {
    val dims = mean.length
    val scatter = DenseMatrix.zeros[Double](dims, dims)
    val meanArray = mean.data
    for {
      dp <- uncentered
    } {
      val dpArray = dp.data
      for {
        dpi <- 0 until dims
        dpj <- dpi until dims
      } {
        val centeredi = dpArray(dpi) - meanArray(dpi)
        val centeredj = dpArray(dpj) - meanArray(dpj)
        val p = centeredi * centeredj

        scatter(dpi, dpj) += p
        if (dpj > dpi) {
          scatter(dpj, dpi) += p
        }
      }
    }
    scatter
  }
  
}
