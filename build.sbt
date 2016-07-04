name := "twitter4food"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "amateras-repo" at "http://amateras.sourceforge.jp/mvn/",
  Resolver.sonatypeRepo("public")
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.clulab" %% "processors" % "5.9.0",
  "org.clulab" %% "processors" % "5.9.0" classifier "models",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "com.typesafe" % "config" % "1.2.1",
  "org.json" % "json" % "latest.integration",
  "org.twitter4j" % "twitter4j-core" % "4.0.4",
  "org.twitter4j" % "twitter4j-stream" % "4.0.4",
  "edu.cmu.cs" % "ark-tweet-nlp" % "0.3.2",
  "org.apache.lucene" % "lucene-core" % "4.2.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "4.2.1",
  "org.apache.lucene" % "lucene-queryparser" % "4.2.1",
  "org.apache.lucene" % "lucene-highlighter" % "4.2.1",
  "jp.sf.amateras.solr.scala" %% "solr-scala-client" % "latest.integration",
  "cc.mallet" % "mallet" % "2.0.8",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.6.3",
  "com.github.scopt" %% "scopt" % "3.4.0",
  "me.tongfei" % "progressbar" % "0.4.0"
)
