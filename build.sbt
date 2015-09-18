name := "twitter4food"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "amateras-repo" at "http://amateras.sourceforge.jp/mvn/"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "latest.integration" % "test",
  "junit" % "junit" % "latest.integration" % "test",
  "com.novocode" % "junit-interface" % "latest.integration" % "test",
  "edu.arizona.sista" %% "processors" % "latest.integration",
  "edu.arizona.sista" %% "processors" % "latest.integration" classifier "models",
  "ch.qos.logback" % "logback-classic" % "latest.integration",
  "org.json" % "json" % "latest.integration",
  "org.twitter4j" % "twitter4j-core" % "latest.integration",
  "org.twitter4j" % "twitter4j-stream" % "latest.integration",
  "org.apache.lucene" % "lucene-core" % "4.2.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.1",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.1",
  "org.apache.lucene" % "lucene-highlighter" % "4.2.1",
  "jp.sf.amateras.solr.scala" %% "solr-scala-client" % "latest.integration",
  "cc.mallet" % "mallet" % "latest.integration",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "latest.integration"
)
