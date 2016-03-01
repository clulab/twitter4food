name := "twitter4food"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "amateras-repo" at "http://amateras.sourceforge.jp/mvn/"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.clulab" %% "processors" % "5.7.3",
  "org.clulab" %% "processors" % "5.7.3" classifier "models",
  "ch.qos.logback" % "logback-classic" % "1.0.10",
  "com.typesafe" % "config" % "1.2.1",
  "org.json" % "json" % "latest.integration",
  "org.twitter4j" % "twitter4j-core" % "4.0.4",
  "org.twitter4j" % "twitter4j-stream" % "4.0.4",
  "org.apache.lucene" % "lucene-core" % "4.2.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.1",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.1",
  "org.apache.lucene" % "lucene-highlighter" % "4.2.1",
  "jp.sf.amateras.solr.scala" %% "solr-scala-client" % "latest.integration",
  "cc.mallet" % "mallet" % "2.0.7",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.6.3"
)
