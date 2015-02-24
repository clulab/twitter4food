package edu.arizona.sista.twitter4food

// http://stackoverflow.com/questions/12591457/scala-2-10-json-serialization-and-deserialization
import java.lang.reflect.{Type, ParameterizedType}
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.annotation.JsonInclude.Include
import java.text.SimpleDateFormat

object JacksonWrapper {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
  mapper.setSerializationInclusion(Include.NON_NULL)
  mapper.setDateFormat(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'"))
  mapper.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))

  def serialize(value: Any): String = {
    import java.io.StringWriter
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def deserialize[T: Manifest](value: String) : T =
    mapper.readValue(value, typeReference[T])

  private [this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private [this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) { m.erasure }
    else new ParameterizedType {
      def getRawType = m.erasure
      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray
      def getOwnerType = null
    }
  }
}
