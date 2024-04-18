name := "eventsim"

version := "1.1.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"

libraryDependencies += "de.jollyday" % "jollyday" % "0.5.1"

libraryDependencies += "org.rogach" %% "scallop" % "4.0.1"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.6.1"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.1"

libraryDependencies += "org.apache.kafka" % "kafka_2.10" % "0.8.2.1"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"

assemblyMergeStrategy in assembly := {
   case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
   case PathList("META-INF", xs @ _*) => xs match {
     case "MANIFEST.MF" :: Nil => MergeStrategy.discard
     case "module-info.class" :: Nil => MergeStrategy.discard // Handle module-info specifically
     case _ => MergeStrategy.first  // Or choose another strategy like `MergeStrategy.discard`
   }
   case "module-info.class" => MergeStrategy.discard
   case x => (assemblyMergeStrategy in assembly).value(x) // Fallback to the default strategy
}
