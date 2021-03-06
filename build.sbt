import AssemblyKeys._ // put this at the top of the file

scalaVersion := "2.9.2"

name := "nmf"

organization := "org.richardweiss"

version := "1.2"

assemblySettings

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("javax", "servlet", xs @ _*) => MergeStrategy.first
    case PathList("org", "apache", "jasper", xs @ _*) => MergeStrategy.first
    case PathList("org", "apache", "beanutils", xs @ _*) => MergeStrategy.first
    case x => old(x)
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
    "org.scalatest" %% "scalatest" % "1.8" % "test",
    "org.scalacheck" %% "scalacheck" % "1.9" % "test",
    "com.googlecode.matrix-toolkits-java" % "mtj" % "0.9.14",
    "org.scalanlp" %% "breeze-math" % "0.1-SNAPSHOT",
    "org.spark-project" %% "spark-core" % "0.5.1-SNAPSHOT"
  )
}


resolvers ++= Seq(
            "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)


