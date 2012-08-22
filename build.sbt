import AssemblyKeys._ // put this at the top of the file

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-Ydependent-method-types", "-deprecation")

name := "nmf"

organization := "org.richardweiss"

version := "1.0"

assemblySettings

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    //case "javax/servlet/SingleThreadModel.class" => MergeStrategy.concat
    //case "javax/servlet/RequestDispatcher.class" => MergeStrategy.concat
    //case "org/apache/jasper/compiler/Node$ChildInfo.class" => MergeStrategy.concat
    case x => old(x)
    //  old(x)
  }
}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {x =>
      x.data.getName == "joda-time-2.1.jar" ||
      x.data.getName == "sqlite-jdbc-3.7.2.jar" ||
      x.data.getName == "postgresql-8.4-701.jdbc4.jar"
  }
}

libraryDependencies ++= {
  Seq(
    "joda-time" % "joda-time" % "2.1" % "compile",
    "org.joda" % "joda-convert" % "1.2"  % "compile",
    "org.scalatest" %% "scalatest" % "1.8" % "test",
    "org.scalacheck" %% "scalacheck" % "1.9" % "test",
    "org.scalaz" %% "scalaz-core" % "6.0.4",
    "org.yaml" % "snakeyaml" % "1.10",
    "com.googlecode.matrix-toolkits-java" % "mtj" % "0.9.14",
    "org.spark-project" %% "spark-core" % "0.5.1-SNAPSHOT"
  )
}

resolvers ++= Seq("Cloudera Maven Repository" at "https://repository.cloudera.com/content/repositories/releases/",
              "Packaged Avro" at "http://nicta.github.com/scoobi/releases/")