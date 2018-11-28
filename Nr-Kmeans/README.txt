The provided implementation of Nr-Kmeans uses Scala, Breeze and Sbt. 
More information can be found at http://www.scala-lang.org/ and http://www.scala-sbt.org/

After installing Scala and Sbt one can run the provided example by running
> sbt run

This starts the application in  ./src/main/scala/Example.scala, which uses the example dataset provided under './datasets/stickfigures_3subs.dat'.
More details can be found in the code comments.

Please note that the provided implementation is a prove of concept and faithful to the algorithm presented in the paper.
Yet, being faithful to the proposed algorithm means that this implementation is not optimized with regards to performance or memory consumption and left even some easy optimizations out, such as:
    - using proposed extensions exploiting the triangle inequality for optimizing the data point assignment step
    - currently the means and the scatter matrices are completely recalculated for each iteration. It might be faster to just trace migrated data points and modify the statistics accordingly.
    - use a (numerical stable) single pass algorithms to determine mean values and scatter matrices
    - empty clusters are just ignored. Instead one might want to replace this behavior with a more elaborate strategy ***
    - and many more...
  
Notes:
***In our experiments we discarded results, which contained less than the specified number of clusters.
