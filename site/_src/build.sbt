libraryDependencies += "com.twitter" % "scalding-core_2.10" % "0.15.0"
libraryDependencies += "com.twitter" % "scalding-repl_2.10" % "0.15.0"
libraryDependencies += "org.apache.hadoop" % "hadoop-core" % "1.2.1"

tutSettings ++ Seq(
  tutSourceDirectory := new File("src"),
  tutTargetDirectory := new File("gen")
)
