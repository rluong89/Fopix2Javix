name := "Trac"

version := "0.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

scalacOptions ++= Seq("-feature")

/* In case of StackOverflow during runs... */
// fork in run := true
// javaOptions += "-Xss100m"
