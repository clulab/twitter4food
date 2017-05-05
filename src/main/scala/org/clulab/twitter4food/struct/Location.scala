package org.clulab.twitter4food.struct

/**
  * A location a tweet, FourSquare checkin, etc. was made from
  * @param id unique identifier (numeric for tweets, alphanumeric for checkins)
  * @param lat latitude
  * @param lng longitude
  * @param user unique user id (Twitter id)
  * @param createdAt date/time user was at location
  * @param source "twitter", "foursquare", etc.
  * @param venues list of closest [[Venue]]s in order of closeness
  */
class Location(
    val id: String,
    val llat: Double,
    val llng: Double,
    val user: Long,
    val createdAt: java.util.Date,
    val source: String,
    val venues: Seq[Venue])
  extends LatLng(lat = llat, lng = llng){
  override def toString: String = {
    val venueText = venues.map(v => s"(${v.toString})").mkString(", ")
    s"$id\t$lat\t$lng\t$user\t$createdAt\t$source\t$venueText"
  }

  def copy(
    id: String = this.id,
    lat: Double = this.lat,
    lng: Double = this.lng,
    user: Long = this.user,
    createdAt: java.util.Date = this.createdAt,
    source: String = this.source,
    venues: Seq[Venue] = this.venues
  ): Location = new Location(id, lat, lng, user, createdAt, source, venues)
}


class Venue(
    val name: String,
    val types: Seq[String],
    val vlat: Double,
    val vlng: Double
) extends LatLng(lat = vlat, lng = vlng) {
  override def toString: String = s"$name; [${types.mkString(" : ")}]; $lat; $lng"
}


class LatLng(val lat: Double, val lng: Double) {

  // https://stackoverflow.com/questions/639695/how-to-convert-latitude-or-longitude-to-meters
  /**
    * Returns distance in meters between two [[LatLng]]s
    */
  def dist(that: LatLng): Double = {
    val R = 6378.137  // Radius of earth in km
    val dLat = that.lat * math.Pi / 180 - this.lat * math.Pi / 180
    val dLon = that.lng * math.Pi / 180 - this.lng * math.Pi / 180
    val a = math.sin(dLat / 2) * math.sin(dLat / 2) +
      math.cos(this.lat * math.Pi / 180) * math.cos(that.lat * math.Pi / 180) *
        math.sin(dLon / 2) * Math.sin(dLon / 2)
    val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    val d = R * c
    d * 1000  // km to meters
  }
}