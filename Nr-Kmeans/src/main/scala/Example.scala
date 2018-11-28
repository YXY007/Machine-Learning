import algorithm.{NonRedHelpers, NrKmeans}
import breeze.stats.distributions.{Rand, RandBasis}
import better.files._
import clustering.scores.{NormalizedMutualInformation, PairCountingF1Measure}
import data.DataIO


object Example extends App {
  // 生成随机数
    implicit val rnd = RandBasis.withSeed(42)

  if (System.getProperty("com.github.fommil.netlib.BLAS") == null) {
    System.setProperty("com.github.fommil.netlib.BLAS", "com.github.fommil.netlib.NativeRefBLAS")
  }

  // 数据集
  // CSV文件:
  // 每行是一个数据点，前几个数是label，后面的是features
  // 行间由;隔开
  // 以下导入了3个数据集，需要用其中一个的时候需要注释另外两个数据集

  // 小人数据集，有3个子空间，每个子空间k的值为3
  val datasetFile = file"dataset/stickfigures_3sub.data"
  val datasetName = "Stickfigures"
  val labelColumns =  IndexedSeq(0, 1, 2) //前三个值是在三个子空间里的label
  val clustersPerSubspace = IndexedSeq(3,3,3)
  val noiseSpace = true //是否需要噪声空间


  //ALOI数据集
//  val datasetFile = file"dataset/aloiIntro.data"
//  val datasetName = "ALOI-Intro"
//  val labelColumns =  IndexedSeq(0, 1) //前三个值是在三个子空间里的label
//  val clustersPerSubspace = IndexedSeq(2,2)
//  val noiseSpace = true //是否需要噪声空间

 // PCA数据集
// val datasetFile = file"dataset/aloiIntro_pca.data"
//  val datasetName = "ALOI-Intro with PCA"
//  val labelColumns =  IndexedSeq(0, 1) //前三个值是在三个子空间里的label
//  val clustersPerSubspace = IndexedSeq(2,2)
//  val noiseSpace = true //是否需要噪声空间

  //Load data and perform standardization (zero mean and unit variance for all features)
  val (data, labels) = DataIO.loadCsvWithIntMultiLabelsAsSeq(datasetFile,labelColumns)

  //The pair-counting F1 measure expects the labels in a different format
  val goldLabelSeqForF1 = {
    data.indices.map { dpi =>
      labels.map(glabels => glabels(dpi))
    }
  }

  //跑十次，取最优解
  val resC = (for {
    i <- 1 to 10
  } yield {
    val seed = rnd.randInt.sample()
    val res = NrKmeans.run(clustersPerSubspace, data, containsNoiseSpace = noiseSpace)(RandBasis.withSeed(seed))
    val f1 = PairCountingF1Measure.applyForRealWorldNonRed(goldLabelSeqForF1, res.labelsAsSeq)
    val costs = NonRedHelpers.costs(res)
    println(s"candidate: f1: $f1 costs: $costs (seed:$seed)")
    (res, costs, f1)
  }).minBy(_._2)
  val res = resC._1

  println(s"pairF1: ${resC._3}")
  println(s"costs: ${resC._2}")

  for {
    (ss, ssid) <- res.subspaceClusterings.zipWithIndex
  } {
    val (bestLabelsNmi, bestLabelsIdx) = labels.zipWithIndex.maxBy(g => NormalizedMutualInformation.forLabelSeq(g._1, ss.labels))

    println(s"Subspace $ssid")
    println(s"Corresponding features in rotated space: ${ss.projection.pDims.mkString(",")}")
    println(s"Best fitting class labels idx: $bestLabelsIdx with an NMI of ${ NormalizedMutualInformation.forLabelSeq(bestLabelsNmi, ss.labels)}")
  }




}
