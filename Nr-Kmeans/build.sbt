scalaVersion := "2.12.4"


lazy val approx = Project(id = "Nr-Kmeans", base = file("."))

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"


val breezeVersion = "1.0-RC2"
libraryDependencies += "org.scalanlp" %% "breeze" % breezeVersion
libraryDependencies += "org.scalanlp" %% "breeze-natives" % breezeVersion

libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.4.0"

libraryDependencies += "org.typelevel" %% "spire" % "0.14.1"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "latest.release"
 
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

val catsVersion = "1.0.1"
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion

libraryDependencies += "com.github.pathikrit" %% "better-files" % "latest.release"


fork := true

javaOptions in run ++= Seq(
    "-Dcom.github.fommil.netlib.BLAS=com.github.fommil.netlib.NativeRefBLAS",
	"-Dcom.github.fommil.netlib.LAPACK=com.github.fommil.netlib.NativeRefLAPACK",
	"-Dcom.github.fommil.netlib.ARPACK=com.github.fommil.netlib.NativeRefARPACK"
)
