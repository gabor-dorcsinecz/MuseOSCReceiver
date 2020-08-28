package com.zorgonout.museoscreceiver

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers._
import us.hebi.matlab.mat.format.{Mat5, Mat5File}
import us.hebi.matlab.mat.types.MatFile

import scala.jdk.CollectionConverters._

class Export2MatlabSpec extends AnyWordSpec with should.Matchers {
  "this" should {
    "work" in {
      val sampleEEG = Mat5.readFromFile("/home/hu3b1188/work/Courses/NeuralSignalProcessing/MySolutions/sampleEEGdata.mat")
      traceMatFile(sampleEEG)
      //println("sampleEEG: " + )
    }
  }

  "it" should {
    "write" in {
      val matFile = Export2Matlab.createMatFile(640)
      val myEEG = Export2Matlab.writeToFile(matFile, "GManEEG.mat")
      val readEEG = Mat5.readFromFile("GManEEG.mat")
      traceMatFile(readEEG)
    }
  }

  def traceMatFile(file: Mat5File): Unit = {
    for (entry <- file.getEntries.asScala) {
      println(s"$entry              ${entry.getClass}")
    }
  }
}

/*
EEG (global) = 1x1 struct
    setname: 'EEG sample dataset'
    filename: ''
    filepath: ''
    subject: ''
    group: ''
    condition: ''
    session: []
    comments: 4x63 char
    nbchan: 64.0
    trials: 99.0
    pnts: 640.0
    srate: 256.0
    xmin: -1.0
    xmax: 1.49609375
    times: 1x640 double
    data: 64x640x99 single
    icaact: []
    icawinv: []
    icasphere: []
    icaweights: []
    icachansind: []
    chanlocs: 1x64 struct
        labels
        theta
        radius
        X
        Y
        Z
        sph_theta
        sph_phi
        sph_radius
        type
        urchan
    urchanlocs: []
    chaninfo: 1x1 struct
        shrink: []
        plotrad: []
        nosedir: '+X'
        filename: '/Users/mcohen1/Documents/MATLAB/eeglab/plugins/dipfit2.2/standard_BESA/standard-10-5-cap385.elp'
        icachansind: []
    ref: 1x2 double
    event: 1x311 struct
        type
        latency
        duration
        urevent
        epoch
    urevent: 1x3009 struct
        type
        latency
        duration
    eventdescription: 1x5 cell
    epoch: 1x99 struct
        event
        eventduration
        eventlatency
        eventtype
        eventurevent
    epochdescription: {}
    reject: 1x1 struct
        rejmanual: []
        rejjp: []
        rejjpE: []
        rejkurt: []
        rejkurtE: []
        rejmanualE: []
        rejthresh: []
        rejthreshE: []
        rejconst: []
        rejconstE: []
        rejfreq: []
        rejfreqE: []
        icarejjp: []
        icarejjpE: []
        icarejkurt: []
        icarejkurtE: []
        icarejmanual: []
        icarejmanualE: []
        icarejthresh: []
        icarejthreshE: []
        icarejconst: []
        icarejconstE: []
        icarejfreq: []
        icarejfreqE: []
        rejglobal: []
        rejglobalE: []
        rejmanualcol: 1x3 double
        rejthreshcol: 1x3 double
        rejconstcol: 1x3 double
        rejjpcol: 1x3 double
        rejkurtcol: 1x3 double
        rejfreqcol: 1x3 double
        disprej: {}
        threshold: 1x3 double
        threshentropy: 600.0
        threshkurtact: 600.0
        threshkurtdist: 600.0
        gcompreject: []
    stats: 1x1 struct
        jp: []
        jpE: []
        icajp: []
        icajpE: []
        kurt: []
        kurtE: []
        icakurt: []
        icakurtE: []
        compenta: []
        compentr: []
        compkurta: []
        compkurtr: []
        compkurtdist: []
    specdata: []
    specicaact: []
    splinefile: ''
    icasplinefile: []
    dipfit: []
    history: '
    pop_eegplot( EEG, 1, 1, 1);
    EEG.setname='TheNewWave';
    EEG = pop_rejepoch( EEG, find(EEG.reject.rejglobal), 0);
    EEG = eeg_checkset( EEG );'
    saved: 'no'
    etc: 1x1 struct
        icaweights_beforerms: 64x64 double
        icasphere_beforerms: 64x64 double
    spedata: []              class us.hebi.matlab.mat.types.MatFile$Entry



 */