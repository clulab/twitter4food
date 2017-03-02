package org.clulab.twitter4food.t2dm

import org.slf4j.LoggerFactory
import scala.io.Source
import com.typesafe.config.ConfigFactory
import sys.process._
import java.io.File
import java.io.FileWriter 

object ImageCaptioner {
    val logger = LoggerFactory.getLogger(this.getClass)
    
  /**
    *   http://t-satoshi.blogspot.com/2015/12/image-caption-generation-by-cnn-and-lstm.html (using this, https://github.com/apple2373/chainer-caption
    * 	To generate caption:
		*			~/anaconda2/bin/python sample_code_beam.py --rnn-model ./data/caption_en_model40.model 
		* 			 --cnn-model ./data/ResNet50.model --vocab 
		*				./data/MSCOCO/mscoco_caption_train2014_processed_dic.json --gpu -1 
		* 			--img ./sample_imgs/COCO_val2014_000000250790.jpg     
    *
    */
  def main (args: Array[String]): Unit = {
      
      val config = ConfigFactory.load
      val twitterImagePath = config.getString(s"classifiers.overweight.twitterImages")
      
      val pythonPath = config.getString(s"captioner.pythonPath")
      val pythonCmd =  config.getString(s"captioner.pythonCmd")
      val pythonParams = config.getString(s"captioner.pythonParams")
      val outputFile = config.getString(s"captioner.outputFile")
      
      logger.info(s"Python path: ${pythonPath}")
      logger.info(s"Python command: ${pythonCmd}")
      logger.info(s"Python parameters : ${pythonParams}")
      logger.info(s"Twitter Image Path: ${twitterImagePath}")
      logger.info(s"Captioner Output being written to ${outputFile}") 
            
      val userImageDir = new File(twitterImagePath)
      val users = userImageDir.listFiles()
      val outputFileWriter = new FileWriter(new File(outputFile))
      
      val pb = new me.tongfei.progressbar.ProgressBar("CaptionGenerator", users.size)
      pb.start
      
      val userResults = users.par.foreach { user =>
        val userImages = user.listFiles.filter( _.getAbsolutePath.endsWith(".jpg") ).map ( _.getAbsolutePath )
        val results = for(img <- userImages) yield {
          val cmd = s"${pythonPath} ${pythonCmd} ${pythonParams} ${img}"
          val cmdOut = cmd!!
          
          val res = cmdOut.split("<sos>").filterNot(_ == "").map(_.trim.filter(_ >= ' ')
              .replace(" <eos>", "\t")).map {x => 
                val y = x.split("\t")
                (y(0), y(1).toDouble)
              }
//          logger.info(s"$img\t${res.mkString(":")}")
          (img, res)
        }
        
        outputFileWriter.write("User:" + user.getName + "\n")
        results.foreach{ res =>
          outputFileWriter.write(s"${res._1}\t${res._2.mkString(":")}")
        }
        pb.step
      }
      
      pb.stop
      outputFileWriter.close
    }
}

// Some Notes if we need to programatically call the python code : Use Jython or jep (see link 2): 
//http://stackoverflow.com/questions/1119696/java-python-integration
//https://sushant-hiray.me/posts/python-in-scala-stack/
// build.sbt #https://mvnrepository.com/artifact/org.scijava/jep/2.4.1 <-- https://sushant-hiray.me/posts/python-in-scala-stack/
// build.sbt ###for including jama for jep --> https://github.com/openimaj/openimaj/issues/90