package org.clulab.twitter4food.struct

class Location(
    id: String,
    lat: Double,
    lng: Double,
    user: Double,
    createdAt: java.util.Date,
    source: String,
    venues: Seq[(String, String)]) {
  override def toString: String = {
    val venueText = venues.map{ case (name, venueType) => s"($name, $venueType)" }.mkString(", ")
    s"$id\t$lat\t$lng\t$user\t$createdAt\t$source\t$venueText"
  }

  def copy(
    id: String = this.id,
    lat: Double = this.lat,
    lng: Double = this.lng,
    user: Double = this.user,
    createdAt: java.util.Date = this.createdAt,
    source: String = this.source,
    venues: Seq[(String, String)] = this.venues
  ): Location = new Location(id, lat, lng, user, createdAt, source, venues)
} 