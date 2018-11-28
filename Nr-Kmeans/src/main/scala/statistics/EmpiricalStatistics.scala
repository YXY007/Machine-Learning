package statistics

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.math._


object EmpiricalStatistics {

  /**
    *  Provides simple stats for partitions like mean and scatter of each partition
    *
    * @param data             the data partitioned according to the labels seq
    * @param labels           the labels
    * @param dpCounter        this contains the number of data points for each cluster
    * @param label2ClusterMap this object maps between cluster labels and cluster indices
    * @return
    */
  def dataMeanAndScatter(data: IndexedSeq[DenseVector[Double]], labels: IndexedSeq[Int], dpCounter: IndexedSeq[Int], label2ClusterMap: Map[Int, Int]): IndexedSeq[(DenseVector[Double], DenseMatrix[Double])] = {
    assert(data.length == labels.length)
    val dims = data.head.length
    val dpRange = data.indices
    val meansAndScatters = Array.fill(label2ClusterMap.size)((DenseVector.zeros[Double](dims), DenseMatrix.zeros[Double](dims, dims)))
    //TODO we might want parallelize this if nrofClusters and #data is big enough
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

  def dataMean(data: Seq[DenseVector[Double]]): DenseVector[Double] = {
    val tmpV = DenseVector.zeros[Double](data.head.length)
    data.foreach(dv => tmpV :+= dv)
    tmpV / data.size.toDouble
  }


  def dataMeanAndCov(data: Seq[DenseVector[Double]], adjustCov: Boolean) = {
    val mean = dataMean(data)
    val meaned = data.map(r => r - mean)
    val cov = determineCov(meaned, adjustCov)
    (mean, cov)
  }

  def dataMeanAndScatter(data: Seq[DenseVector[Double]]) = {
    val mean = dataMean(data)
    val scatter = determineScatterUncentered(data, mean)
    (mean, scatter)
  }

  def dataCov(data: Seq[DenseVector[Double]], assumeCentered: Boolean, adjustCov: Boolean) = {
    if (assumeCentered) {
      determineCov(data, adjustCov)
    } else {
      dataMeanAndCov(data, adjustCov)._2
    }
  }

  private def determineCov(centered: Seq[DenseVector[Double]], adjustCov: Boolean): DenseMatrix[Double] = {
    val scatter = determineScatterCentered(centered)
    if (adjustCov) {
      scatter / (centered.size.toDouble - 1)
    } else {
      scatter / centered.size.toDouble
    }
  }

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
