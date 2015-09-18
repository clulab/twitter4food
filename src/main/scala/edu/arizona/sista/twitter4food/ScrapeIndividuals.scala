package edu.arizona.sista.twitter4food

import twitter4j._
import twitter4j.conf.ConfigurationBuilder

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter
import java.util.Date
import scala.collection.JavaConversions.asScalaBuffer

class ScrapeIndividuals {
  val SLEEP = 3100

  def dateToString(d: Date): String = if(d == null) "NIL" else d.toString

  def placeToString(p: Place): String = {
    if (p == null) return "NIL"
    val os = new StringBuilder
    os.append(c(p.getPlaceType()))
    os.append("/" + c(p.getFullName()))
    os.append("/" + c(p.getCountryCode()))
    os.append("/" + c(p.getBoundingBoxType()))
    val gs = p.getBoundingBoxCoordinates
    if (gs != null) {
      for {
        i <- 1 until gs.length
        j <- 1 until gs(i).length
      } {
        os.append("/" + geoLocationToString(gs(i)(j)))
      }
    }
    os.toString
  }

  def geoLocationToString(g: GeoLocation): String = {
    if(g == null) return "NIL"
    c(g.getLatitude.toString) + "|" + c(g.getLongitude.toString)
  }

  def c(s: String): String = {
    if(s == null) return "NIL"
    if(s.length() == 0) return "NIL"
    s.replaceAll("[\\t\\n\\r]+", " ")
  }

  def main(args: Seq[String]) = {
    val threshold = 5
    val ratedIndividualsFiles = "/edu/arizona/sista/twitter4food/rated_individuals.csv"
    val bufferedSource = io.Source.fromURL(ratedIndividualsFiles)
    val userWeight = (for (line <- bufferedSource.getLines) yield {
      val Array(userHandle, label) = line.split(",").map(_.trim)
      userHandle -> label
    }).toMap
    bufferedSource.close
    val userHandles = userWeight.keys
    var number = 0

    for(userHandle <- userHandles){
      number += 1
      val weight = userWeight(userHandle)
      println("userHandle=" + userHandle + " weight=" + weight)
      println("number=" + number)

      val directoryRoot = "/data/nlp/corpora/twitter4food/ratedIndividuals/"
      val fileRoot = new File(directoryRoot)
      if(!fileRoot.exists()){
        fileRoot.mkdir()
      }
      val directory = directoryRoot + weight
      val file = new File(directory)
      if(!file.exists()){
        file.mkdir()
      }
      val output = directory + "/" + userHandle + ".txt"
      val userfile = new File(output)
      if(!userfile.exists()){
        val pw = new PrintWriter(new FileOutputStream(output, true))
        val cb = new ConfigurationBuilder()
        cb.setDebugEnabled(false)
        // app name: search4food ***** Twitter4Food
        //first

        //      cb.setOAuthConsumerKey("ZkaRnLMRDSDmNQTx7x4MVqKQh")
        //      cb.setOAuthConsumerSecret("Tk4Kb1xvTEhAH4IXGaTnI0ORTxDgCDmvDzL9YJBUTmkNqfLr1b")
        //      cb.setOAuthAccessToken("3009667412-N1yzqXGAq6Lf9XT5C3nOoZb0KPEX75YHegRa59z")
        //      cb.setOAuthAccessTokenSecret("Gh1kdmLl6dO39QnqdCWGKPUVwfTQNXG7IXDNWdBzHhhpT")

        //fail
        //      cb.setOAuthConsumerKey("BMCLeAq8l3i9P9ItN8gN2M8S1")
        //      cb.setOAuthConsumerSecret("Wp4UOYjDfwyEUufcoJNGk8aS3PxwIcCUOGJdK9Mgm76bFv4oxH")
        //      cb.setOAuthAccessToken("3009667412-vAcM2jY3djC5fQrh94DDoo11rsbVE8UKTzEzmtE")
        //      cb.setOAuthAccessTokenSecret("Wqdqy5YEF7fVADg61NkxZuTLMjkdQBKPoz9amr00O3PaU")


        //    cb.setOAuthConsumerKey("MSNMEjdIfDzlvQjUbuSiYAW0N")
        //    cb.setOAuthConsumerSecret("mPnh221Inejs3DNOdo9HkKMXGYSxmLlUreUFXhdGRyzvzZZJQF")
        //    cb.setOAuthAccessToken("3009667412-iYm5qRoJEC2PDTt76YIckPhwfQfrngohdFe1d1H")
        //    cb.setOAuthAccessTokenSecret("ggWwUTDsAZ3eNtH3L60ANCCiLJAmpcVN61gzlFXQATzRb")

        //
        //    cb.setOAuthConsumerKey("g0wtTOCPV3Cy7Kj4BG1myEzpB")
        //    cb.setOAuthConsumerSecret("R0gyiNKPZQ8Rou5LzJgV0ZnZaaCnK2uQR2hVteDNE90icWy3If")
        //    cb.setOAuthAccessToken("3009667412-XznfdBn1iA1simoqMFZbJBeypHz8y1ULvXZoXy2")
        //    cb.setOAuthAccessTokenSecret("iIfTytbEKDSHZtSw4Bw3ik8nSEguENrIA1RMWfQxx1dKw")

        //
        //      cb.setOAuthConsumerKey("51EQaP0iG52HfNkC07JBzEBJS")
        //      cb.setOAuthConsumerSecret("xmLcNe3dlkkiOCdFzJEzMRDYpDgTtfrnRA3spX0TZjD9MB6Y6l")
        //      cb.setOAuthAccessToken("3009667412-AF8kqhLnRdR21tJJ8TLaSgwnrR4mDUHCeGFO25l")
        //      cb.setOAuthAccessTokenSecret("7fHYlMtVF8oEn7FF8kGsbyUW6hcgLVO1ADpo3w8R2eAKr")


        //       app name: search4food2
        //      cb.setOAuthConsumerKey("yE7D3rO2t2MTh8qJIVuRCQ")
        //      cb.setOAuthConsumerSecret("UxQ63nx6knphOfZsSdrSqGuIhKnoeFQPUGJFYXN2QU")
        //      cb.setOAuthAccessToken("1928309594-1BaKz4G9leasxJ8zcutjH40zxsUgSAxzNOLDLzJ")
        //      cb.setOAuthAccessTokenSecret("1fHLEA31PeIKNGjFFxLJkNDyFog8z70B8fvTubGbTYg")


        //for DELL computer
        //	       cb.setOAuthConsumerKey("7ysmBVadqTWToZjKpcadmrtpR")
        //   cb.setOAuthConsumerSecret("gDhu1pCGQSCtdBGbwMtZBBY0vO6wQr4yOLQp3zVC8jBR7EVLE5")
        //   cb.setOAuthAccessToken("3009667412-mOUPA8VAHGfs6I4X1sIoduwN7LF4mJo21fqgC8u")
        //   cb.setOAuthAccessTokenSecret("00CCZV2u6leQN9C8vNuijGiM2L7Qy2ByxmiaCBVDYJksI")
        ////
        //


        //   xps 0
        cb.setOAuthConsumerKey("CBnHVyQnRg9KIFobTROtkbHRA")
        cb.setOAuthConsumerSecret("KmHxrNtKQK3GmtxNGPV5qMREhEFQWIbijVDk8LgmzLQsdohTsb")
        cb.setOAuthAccessToken("3177229782-7DBC7LPqbjsiTvyYudQFtBqfQlC0TfYocPlupnI")
        cb.setOAuthAccessTokenSecret("Kw0gnSPE4AkWBMpJ8gNtir8izGcZxWpTW67CVDqkoEkkY")


        //xps 1
        //	         cb.setOAuthConsumerKey("DSlsEVL8jUhLn3YCMITQOE750")
        //   cb.setOAuthConsumerSecret("DJ9fpErgd906ZlBnhFJZpModXwMbKpylanxPT2qgTd9OsqE4Wt")
        //   cb.setOAuthAccessToken("3009667412-TBfLtoYtn9Eh7OZPWK4hEW5Vdt9PD7CHpzv0Nun")
        //   cb.setOAuthAccessTokenSecret("fTIri69QpNypdkCjTlKdAo4OWWv3AAqy9MHUnCAZ6h4Xc")
        //
        ////


        //xps 2
        //	  	         cb.setOAuthConsumerKey("2YgqSp46Y0OWU2iKfZBGE1Ajq")
        //   cb.setOAuthConsumerSecret("L9cDALuThICHpk8RPgAgAonRSxd5chsxftnVY8MnnrWV145OSN")
        //   cb.setOAuthAccessToken("3009667412-fibZytzxxFICr9inAk6qlq7KaaKeW2J0xRTcErt")
        //   cb.setOAuthAccessTokenSecret("e0G8HH00rB8H1HnAaGpVdmxgFkFtoki9PB3P2MBRuJd5y")
        //


        //xps3
        //	  	  	         cb.setOAuthConsumerKey("nWyhTKqCDDDUTiZJYfuYWQ2aU")
        //   cb.setOAuthConsumerSecret("odwEMi8FT79WTL81n4BGucbycdehJLU3FonMIb7ASuFBzziwxP")
        //   cb.setOAuthAccessToken("3009667412-alVaKaJZ3Uq0XDVgmnUtvsguHG4WjTZlhnSiTD6")
        //   cb.setOAuthAccessTokenSecret("iQ4bUWsEPrq4MiscETQrvkdWHbJ7K7sBS2S4mVzDYH99S")


        //   xps 4
        //       cb.setOAuthConsumerKey("i6O2g2YsPMIqBuXTv8JquKdoe")
        //cb.setOAuthConsumerSecret("O76xpD1NyG80LGmF6arOYbPmOYuHGydaOOaZCqYzRQh8dDR7iy")
        //cb.setOAuthAccessToken("2353305776-9dhnqFXGpSQcuy5uE7kSbUVvhYLI1lPPSnIipmv")
        //cb.setOAuthAccessTokenSecret("49AtW4nEXjnCLAKY9P5PkrsAvxdgT6PRZoW6LPS8ZdVya")


        //   xps 5
        //      cb.setOAuthConsumerKey("cXr5pXADI5agbqZDsydOVkbCA")
        //cb.setOAuthConsumerSecret("x8jOLgD3xv7aJ5F6P5s9f3Ftwr41T44dXCYFRexoQKDq9TLZWr")
        //cb.setOAuthAccessToken("3177229782-1jkAdL674cufncArLVy1I3Qh9iGnIxqHszicsQH")
        //cb.setOAuthAccessTokenSecret("VMJb3m9uH07AnExu1mbWeq4VNgYhkBInsedtLIe3EGS88")


        //   xps 6
        //      cb.setOAuthConsumerKey("svHRQ9SSpaGIJKfmJchO9s1Js")
        //cb.setOAuthConsumerSecret("KJOb0KvrMbUYBweJTUUiCzItfi5PE4MVe7ZpV6myDAeeduITAu")
        //cb.setOAuthAccessToken("3177229782-UdGttF0fSx7fXC9MY0Z5MBKWkuyWiE26Kdq9466")
        //cb.setOAuthAccessTokenSecret("0goIlue7V5NobHtOgvY6a7bIYzP27LRZ3jlcocaa4iiEX")


        //   xps 7
        //   cb.setOAuthConsumerKey("ROV5qc66Re8ssiLNheSA7qge8")
        //cb.setOAuthConsumerSecret("F4h5vYpZTDTA8AFhlhKHX11fyJHd8ZD4AzK1aRlYTAwaj7RTet")
        //cb.setOAuthAccessToken("3177229782-tensi5OEGyf3kpRJrZyzE6ZGYO4iYcseJ2XMVGi")
        //cb.setOAuthAccessTokenSecret("K7VqNAxu3sxQBipGB2Io14yyz5NPZq6GagAFNCMPXsoT8")


        //   xps 8
        //      cb.setOAuthConsumerKey("LK6iNb9g4rQLJVC4aHvYxLEP9")
        //   cb.setOAuthConsumerSecret("H5BUWkloeReaIXrHfTKoHNnATmA1ioSrNCTQ1NxMaBpxf4KRuk")
        //   cb.setOAuthAccessToken("3177229782-RYbqtGSGKR7NxKkULSf4VAstHfZTIB3QPQtcmfr")
        //   cb.setOAuthAccessTokenSecret("5jXtpEmta0ThbHQyVd9KGZso6YiRTUMjLF1bwauk0pWvI")

        //   xps 9
        //    	      cb.setOAuthConsumerKey("gCIpR3mqkV1W8n5jwyhs9Itsl")
        //    	   cb.setOAuthConsumerSecret("TFNC4srwmNdFiPifAjsHdyU9uSPgoYDs7cIlIFNr8jHSDJalAg")
        //    	   cb.setOAuthAccessToken("3177229782-QNKbkXhPTWuyMnvEXpI70ydLZg12BvlhZl5Es3M")
        //    	   cb.setOAuthAccessTokenSecret("EWw0w9LquKfj0LXFytUeOvYcYngMn87HXCvOFXWb32TS5")



        //  	   //   xps 10
        //     	      cb.setOAuthConsumerKey("V511mjI66LLG2ucvBEhHPVhwT")
        //     	   cb.setOAuthConsumerSecret("A5KmzcziNamgban88To6QH51ztclKeNV7RS61xovUHCdCRKzmR")
        //     	   cb.setOAuthAccessToken("3177229782-AO0aYsVcPVugmTNoTirCQXHKRv9lI88WJn8f7Ru")
        //     	   cb.setOAuthAccessTokenSecret("UXq9NpAdouUCmpj4CF3quhuBwWwdgVuOTcyYNsJyXnAUq")
        //


        println("Getting Tweets: ")

        ////////////////Right here the program really starts - Gets the user tweets////

        val twitter: Twitter = new TwitterFactory(cb.build()).getInstance()
        for (i <- 1 to 16) {//get the first i pages of 200 tweets (we expect i*200 tweets), max 3200 total
        val paging = new Paging(i, 200); //200 is the max # of tweets per page
        val statuses = twitter.getUserTimeline(userHandle, paging)
          if(statuses!=null){
            val u = statuses.get(0).getUser()
            val uCreatedAt = dateToString(u.getCreatedAt())
            for (status <- asScalaBuffer(statuses)) {
              pw.println(
                "@" + u.getScreenName() + "\t" +
                  c(u.getName()) + "\t" +
                  c(u.getId().toString) + "\t" +
                  c(u.getLocation()) + "\t" +
                  c(u.getFollowersCount().toString) + "\t" +
                  c(u.getUtcOffset().toString) + "\t" +
                  c(u.getTimeZone()) + "\t" +
                  c(uCreatedAt.toString) + "\t" +
                  c(u.getLang()) + "\n" +
                  c(dateToString(status.getCreatedAt())) + "\t" +
                  geoLocationToString(status.getGeoLocation()) + "\t" +
                  placeToString(status.getPlace()) + "\n" +
                  c(status.getText()))
              pw.flush()
            }
          }
          Thread.sleep(SLEEP)
        }
      }
    }
  }
}


