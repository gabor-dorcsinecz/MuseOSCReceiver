package com.zorgonout.museoscreceiver

import us.hebi.matlab.mat.format.Mat5
import us.hebi.matlab.mat.types.{MatFile, MatlabType}

import scala.util.Random

/*
  Write a standard EEG matlab file with the provided data
  Matlab read write examples:
    https://github.com/HebiRobotics/MFL
    https://github.com/HebiRobotics/MFL/blob/master/mfl-core/src/test/java/us/hebi/matlab/mat/tests/Mat5Examples.java
 */
object Export2Matlab {

  val numberOfTrials = 1
  val samplingRate = 220 //EEG signals are oversampled and then downsampled to yield an output sampling rate of 220 Hz ...  https://sites.google.com/a/interaxon.ca/muse-developer-site/muse-hardware

  val numberOfChannels = 4 //AF7, AF8, TP9, and TP10 with electrode Fpz utilized as the reference electrode.

  def createMatFile(dataPoints: Int): MatFile = {

    val data = Mat5
      .newMatrix(Array(numberOfChannels, dataPoints, numberOfTrials), MatlabType.Single)
    for (channel <- 0 until numberOfChannels) {
      for (measurement <- 0 until dataPoints) {
        data.setDouble(Array(channel,measurement,numberOfTrials-1), Random.nextDouble())
      }
    }

    val times = Mat5.newMatrix(1,dataPoints, MatlabType.Double)
    var calculatedTime = 0d
    for (time <- 0 until dataPoints) {
      times.setDouble(0,time, calculatedTime)
      calculatedTime += 1d/samplingRate
    }

    val struct = Mat5.newStruct(1, 1)
      .set("setname", Mat5.newString("GMan EEG Data"))
      .set("filename", Mat5.newString(""))
      .set("filepath", Mat5.newString(""))
      .set("subject", Mat5.newString(""))
      .set("group", Mat5.newString(""))
      .set("nbchan", Mat5.newScalar(numberOfChannels))
      .set("trials", Mat5.newScalar(numberOfTrials))
      .set("pnts", Mat5.newScalar(640))
      .set("srate", Mat5.newScalar(samplingRate))
      .set("times", times)
      .set("data", data)
    Mat5.newMatFile().addArray("EEG", struct)
  }

  def writeToFile(mat: MatFile, fileName: String): Unit =
    Mat5.writeToFile(mat, "GManEEG.mat")

}
