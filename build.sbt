name := "twitter4food"

version := "2.1"

//scalaVersion := "2.11.8"

resolvers ++= Seq(
  "amateras-repo" at "http://amateras.sourceforge.jp/mvn/",
  Resolver.sonatypeRepo("public")
)

libraryDependencies ++= Seq(
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
  "cc.mallet" % "mallet" % "2.0.8",
  "net.sf.trove4j" % "trove4j" % "2.0.2",
  "me.tongfei" % "progressbar" % "0.4.0",
  "com.google.maps" % "google-maps-services" % "0.1.20",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.clulab" % "processors-main_2.11" % "6.2.0",
  "jp.sf.amateras.solr.scala" % "solr-scala-client_2.11" % "0.0.12",
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.6.3",
  "com.github.scopt" % "scopt_2.10" % "3.7.0"
)

dependencyOverrides += "net.sf.trove4j" % "trove4j" % "2.0.2"

updateOptions := updateOptions.value.withGigahorse(false)
