import algorithm.{NonRedHelpers, NrKmeans}
import breeze.stats.distributions.{Rand, RandBasis}
import better.files._
import clustering.scores.{NormalizedMutualInformation, PairCountingF1Measure}
import data.DataIO


/**
  * This is a minimal example for the Nr-Kmeans algorithm proposed in the paper
  * 'Discovering Non-Redundant K-means Clusterings in Optimal Subspaces'
  */
object Example extends App {
  //Initialize random number generator
    implicit val rnd = RandBasis.withSeed(42)

  //We need to initialize netlib the correct parameter may vary for your system!
  //For details see: https://github.com/fommil/netlib-java
  if (System.getProperty("com.github.fommil.netlib.BLAS") == null) {
    System.setProperty("com.github.fommil.netlib.BLAS", "com.github.fommil.netlib.NativeRefBLAS")
  }

  //The dataset we want to cluster.
  // (Format is a simple CSV-based text file:
  // Each line represents a data point. Some columns represent class labels (each label column represents a different possible partition)
  // other columns represent features;
  //Columns are separated by a semicolon

  //Stickfigures dataset with 3 subspaces
  val datasetFile = file"dataset/stickfigures_3sub.data"
  val datasetName = "Stickfigures"
  val labelColumns =  IndexedSeq(0, 1, 2) //first three columns are the class labels (integer) of the different subspaces
  val clustersPerSubspace = IndexedSeq(3,3,3)
  val noiseSpace = true //We also want an additional noise space


  //Intro example
//  val datasetFile = file"dataset/aloiIntro.data"
//  val datasetName = "ALOI-Intro"
//  val labelColumns =  IndexedSeq(0, 1) //first three columns are the class labels (integer) of the different subspaces
//  val clustersPerSubspace = IndexedSeq(2,2)
//  val noiseSpace = true //We also want an additional noise space

 // Intro example with PCA as discussed in the paper
// val datasetFile = file"dataset/aloiIntro_pca.data"
//  val datasetName = "ALOI-Intro with PCA"
//  val labelColumns =  IndexedSeq(0, 1) //first three columns are the class labels (integer) of the different subspaces
//  val clustersPerSubspace = IndexedSeq(2,2)
//  val noiseSpace = true //We also want an additional noise space

  //Load data and perform standardization (zero mean and unit variance for all features)
  val (data, labels) = DataIO.loadCsvWithIntMultiLabelsAsSeq(datasetFile,labelColumns)

  //The pair-counting F1 measure expects the labels in a different format
  val goldLabelSeqForF1 = {
    data.indices.map { dpi =>
      labels.map(glabels => glabels(dpi))
    }
  }

  //We run Nr-Kmeans 10 times and take the candidate with the elast costs
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
