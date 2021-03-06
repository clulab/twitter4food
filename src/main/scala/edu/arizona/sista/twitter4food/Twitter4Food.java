package edu.arizona.sista.twitter4food;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import twitter4j.*;
import twitter4j.conf.ConfigurationBuilder;

import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

/**
 * User: mihais
 * Date: 10/2/13
 */
public class Twitter4Food {
  private static final int SLEEP_FOR_FOOD = 500;
  private static final int SLEEP_FOR_EMOTICONS = 2000;
  private static final int SLEEP = SLEEP_FOR_EMOTICONS;

  public static void main(String[] args) throws Exception {
    if(args.length != 2) {
      System.err.println("Usage: Twitter4Food query|sample <output file>");
      System.exit(1);
    }
    final String cmd = args[0];
    final String output = args[1];
    final PrintWriter pw = new PrintWriter(new FileOutputStream(output, true));

    Config config = ConfigFactory.load();
    final String keysFilePath = config.getString("oldKeys");
    final BufferedReader br = new BufferedReader(new FileReader(keysFilePath));
    Map<String, String> oa = new HashMap<>();
    String thisLine;
    while((thisLine = br.readLine()) != null) {
      String[] kv = thisLine.trim().split("\\t");
      oa.put(kv[0], kv[1]);
    }

    ConfigurationBuilder cb = new ConfigurationBuilder();
    cb.setDebugEnabled(false);
    if(cmd.equals("query")) {
      // app name: search4food
      cb.setOAuthConsumerKey(oa.get("QCK"));
      cb.setOAuthConsumerSecret(oa.get("QCS"));
      cb.setOAuthAccessToken(oa.get("QAT"));
      cb.setOAuthAccessTokenSecret(oa.get("QATS"));

    } else if(cmd.equals("sample")) {
      // app name: search4food-sample
      cb.setOAuthConsumerKey(oa.get("SCK"));
      cb.setOAuthConsumerSecret(oa.get("SCS"));
      cb.setOAuthAccessToken(oa.get("SAT"));
      cb.setOAuthAccessTokenSecret(oa.get("SATS"));
    }

    TwitterStream twitterStream = new TwitterStreamFactory(cb.build()).getInstance();

    StatusListener listener = new StatusListener() {

      @Override
      public void onException(Exception x) { x.printStackTrace(); }

      @Override
      public void onDeletionNotice(StatusDeletionNotice arg0) { }

      @Override
      public void onScrubGeo(long arg0, long arg1) { }

      @Override
      public void onStallWarning(StallWarning warning) {
        System.err.println("Received STALL warning: " + warning);
        // System.exit(1);
      }

      @Override
      public void onTrackLimitationNotice(int arg0) { }

      @Override
      public void onStatus(Status status) {
        User u = status.getUser();
        pw.println(
          "@" + u.getScreenName() + "\t" +
          c(u.getName()) + "\t" +
          c(Long.toString(u.getId())) + "\t" +
          c(u.getLocation()) + "\t" +
          c(Integer.toString(u.getFollowersCount())) + "\t" +
          c(Integer.toString(u.getUtcOffset())) + "\t" +
          c(u.getTimeZone()) + "\t" +
          c(u.getCreatedAt() != null ? u.getCreatedAt().toString() : null) + "\t" +
          c(u.getLang()) + "\n" +
          c(status.getCreatedAt() != null ? status.getCreatedAt().toString() : null) + "\t" +
          geoLocationToString(status.getGeoLocation()) + "\t" +
          placeToString(status.getPlace()) + "\n" +
          c(status.getText()));
        pw.flush();

        if(cmd.equals("sample")) {
          try {
            System.err.println("Sleeping for " + (SLEEP/1000.0) + " seconds...");
            Thread.sleep(SLEEP);
          } catch (InterruptedException e) {
            System.err.println("Could not sleep for some reasons...");
            e.printStackTrace();
          }
        }
      }

      private String placeToString(Place p) {
        if(p == null) return "NIL";
        StringBuilder os = new StringBuilder();
        os.append(c(p.getPlaceType()));
        os.append("/" + c(p.getFullName()));
        os.append("/" + c(p.getCountryCode()));
        os.append("/" + c(p.getBoundingBoxType()));
        GeoLocation [][] gs = p.getBoundingBoxCoordinates();
        if(gs != null) {
          for(int i = 0; i < gs.length; i ++) {
            for(int j = 0; j < gs[i].length; j ++) {
              os.append("/" + geoLocationToString(gs[i][j]));
            }
          }
        }
        return os.toString();
      }

      private String geoLocationToString(GeoLocation g) {
        if(g == null) return "NIL";
        return c(Double.toString(g.getLatitude())) + "|" + c(Double.toString(g.getLongitude()));
      }

      private String c(String s) {
        if(s == null) return "NIL";
        if(s.length() == 0) return "NIL";
        return s.replaceAll("[\\t\\n\\r]+", " ");
      }
    };
    twitterStream.addListener(listener);

    if(cmd.equals("query")) {
      FilterQuery fq = new FilterQuery();
      //String keywords[] = { "#dinner", "#lunch", "#breakfast", "#snack", "#brunch", "#supper", "#meal" };
      String keywords[] = {
        ":)", "=)", ":-)", ":]", "=]", ":-]", ":g", ":o)", ":D", "=D", ":-D", ":P", "=P", ":-P",
        ":(", "=(", ":-(", ":[", "=[", ":-[", ":{", ":-c", ":c}", "D:", "D=", ":S", ":/", "=/", ":-/", ":’(", ":_("
      };
      System.out.println("Following these hashtags:");
      for(int i = 0; i < keywords.length; i ++)
          System.out.println("\t" + keywords[i]);
      fq.track(keywords);
      twitterStream.filter(fq);
      Thread.sleep(SLEEP * 5);
    } else if(cmd.equals("sample")) {
      twitterStream.sample();
      Thread.sleep(SLEEP * 5);
    }

  }
}
