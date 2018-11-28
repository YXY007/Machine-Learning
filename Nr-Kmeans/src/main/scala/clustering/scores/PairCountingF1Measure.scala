package clustering.scores

/**
  * Pair counting F1-Measure
  * According to "Evaluation of Clusterings - Metrics and Visual Support"
  * by Elke Achtert, Sascha Goldhofer, Hans-Peter Kriegel, Erich Schubert, Arthur Zimek
  * ICDE 2012
  *
  * Wei used this measure in his paper "Generalized Independent Subspace Clustering", ICDM 2016
  */
object PairCountingF1Measure {

  //For a signle clustering
  def forClusterMap[RealClId, FoundClId](realClusters: Map[RealClId, Set[Int]], foundClusters: Map[FoundClId, Set[Int]], nrOfDps: Int): Double = {
    forClusterSets(IndexedSeq(realClusters.values.toIndexedSeq), IndexedSeq(foundClusters.values.toIndexedSeq), nrOfDps)
  }

  def forClusterSets(realClusters: IndexedSeq[IndexedSeq[Set[Int]]], foundClusters: IndexedSeq[IndexedSeq[Set[Int]]], nrOfDps: Int): Double = {
    var tp = 0d
    var fp = 0d
    var fn = 0d
    var tn = 0d
    for {
      i <- 0 to nrOfDps
      j <- (i + 1) to nrOfDps
    } {
      if (sameCluster(foundClusters, i, j)) {
        if (sameCluster(realClusters, i, j)) {
          tp += 1d
        } else {
          fp += 1d
        }
      } else {
        if (sameCluster(realClusters, i, j)) {
          fn += 1d
        } else {
          tn += 1d
        }
      }
    }
    val precision = if (tp == 0d) 0d else tp / (tp + fp)
    val recall = if (tp == 0d) 0d else tp / (tp + fn)
    if (precision == 0d && recall == 0d) 0d else 2 * precision * recall / (precision + recall)
  }

  /**
    *
    * @param gold
    * @param prediction First iterates over all data points second over the assignment in each subspace
    */
  def applyForRealWorldNonRed(gold: IndexedSeq[IndexedSeq[Int]], prediction: IndexedSeq[IndexedSeq[Int]]): Double = {
    var tp = 0d
    var fp = 0d
    var fn = 0d
    var tn = 0d
    for {
      i <- 0 to gold.length - 2
      j <- (i + 1) until gold.length
    } {
      if (sameCluster(prediction(i), prediction(j))) {
        if (sameCluster(gold(i), gold(j))) {
          tp += 1d
        } else {
          fp += 1d
        }
      } else {
        if (sameCluster(gold(i), gold(j))) {
          fn += 1d
        } else {
          tn += 1
        }
      }
    }
    val precision = if (tp == 0d) 0d else tp / (tp + fp)
    val recall = if (tp == 0d) 0d else tp / (tp + fn)
    if (precision == 0d && recall == 0d) 0d else 2 * precision * recall / (precision + recall)
  }


  private def sameCluster[A](dpa: IndexedSeq[A], dpb: IndexedSeq[A]): Boolean = {

    for {
      i <- dpa.indices
    } {
      if (dpa(i) == dpb(i)) {
        return true
      }
    }
    false
  }


  private def sameCluster[A](clusterings: IndexedSeq[IndexedSeq[Set[A]]], dpa: A, dpb: A): Boolean =
    clusterings.exists(_.exists(c => c.contains(dpa) && c.contains(dpb)))

}

/*
The original Matlab code

function f=SameCluster(objectA, objectB)
col=size(objectA,2);
t=0;
for j=1:col
    if objectA(j)==objectB(j)
        t=t+1;
    end
end
if t==0
    f=0;
else
    f=1;
end


==============
function f=Fmeasure(gold, test)
%'gold' is gold standard label
%'test' is test clustering label
[row,col]=size(gold);
[~,col1]=size(test);
TP=0;
FP=0;
FN=0;
TN=0;
for i=1:(row-1)
    for j=(i+1):row
        if SameCluster(test(i,:), test(j,:))==1
            if SameCluster(gold(i,:), gold(j,:))==1
                TP=TP+1;
            else
                FP=FP+1;
            end
        else
            if SameCluster(gold(i,:), gold(j,:))==1
                FN=FN+1;
            else
                TN=TN+1;
            end
        end

    end
end
precision = TP / (TP + FP);
recall = TP / (TP + FN);
f1 = 2 * precision * recall / (precision + recall);
f=f1;
 */