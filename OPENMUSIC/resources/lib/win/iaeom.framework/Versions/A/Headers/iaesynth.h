/**
 *
 * @file IaeSynth.h
 * @author Norbert.Schnell@ircam.fr
 * 
 * @brief IMTR Audio Engine Synthesis class header file
 * 
 * Copyright (C) 2010 by IMTR IRCAM – Centre Pompidou, Paris, France.
 * All rights reserved.
 * 
 */
#ifndef _IAE_SYNTH_H_
#define _IAE_SYNTH_H_

#ifndef IAE_VERSION_MAJOR
#define IAE_VERSION_MAJOR 1
#endif

#ifndef IAE_VERSION_MINOR
#define IAE_VERSION_MINOR 1
#endif


#ifndef IAE_VERSION_RELEASE
#define IAE_VERSION_RELEASE 1
#endif

extern "C" {
#include <stdlib.h>
}

#include "IaeSynthCorpus.h"
#include <string>

/**
 * @mainpage
 * 
 * <h2>IMTR Audio Engine</h2>
 *
 * The IMTR audio engine (IAE) is an embeddable synthesis engine for content based audio processing.
 *
 * The current version of the engine provides the following features:
 * \li asynchronous granular synthesis
 * \li concatenative synthesis
 * \li synchronous granular synthesis (not yet implemented)
 *
 * IAE is based on the ZsaZsa overlap-add engine and the MuBu container.
 *
 */

// opaque forward declaration 
typedef struct ZsaZsaSt ZsaZsaT;

/** \defgroup IAE IMTR Audio Engine
 */

// follows ZsaZsaFilterModeE
enum IaeFilterModeE {
  IaeFilterOff = 0, /* has to be aligned with biquad filter types in biquad.h */
  IaeFilterBiquadLowpass,
  IaeFilterBiquadHighpass,
  IaeFilterBiquadBandpassConstantSkirt,
  IaeFilterBiquadBandpassConstantPeak,
  IaeFilterBiquadNotch,
  IaeFilterBiquadAllpass,
  IaeFilterBiquadPeaking,
  IaeFilterBiquadLowshelf,
  IaeFilterBiquadHighshelf,
  IaeFilterModeNum
};

/** 
 * @class IaeSynthSources
 * @ingroup IAE
 *
 * @brief IMTR audio engine
 *
 * The class provides the zsazsa-dependent sources management.
 *
 * @author Diemo Schwarz
 * 
 */
class IaeSynthSources : public IaeSynthCorpus
{
private:  
  int *sourcePtr;
  int *pendingSourcePtr;
  int *unusedSourcePtr;  
  
protected:
  virtual void updateSources(void);
  void swapSources(ZsaZsaT *zsazsa);
  void setSources(int *sourcePtr);
  void clearSources(void);

public:
  IaeSynthSources(void);
  ~IaeSynthSources(void);
  
private:
  // forbid copy constructor and assignment because of pointer members
  IaeSynthSources (IaeSynthSources &) {};
  IaeSynthSources &operator= (IaeSynthSources &) {};

public:
  /** Clears unused source tables.  */
  void clearUnusedSources(void);  
};



/** 
 * @class IaeSynth
 * @ingroup IAE
 *
 * @brief IMTR audio engine
 *
 * The class provides the granular synth engine.
 *
 * @author Norbert.Schnell@ircam.fr
 * 
 */
class IaeSynth : public IaeSynthSources
{
  
private:
  ZsaZsaT *zsazsa;

  double maxGrainDuration;
  double maxDelayDuration;
  double maxTransposition;
  
  int voiceIndex;
  int synthMode;
  
public:
  /**
   * Creates and initializes an instance of the audio engine.
   * @param numVoices number of synthesis voices
   * @param numOutputChannels number of audio output channels
   * @param maxGrainDuration maximum grain duration
   * @param maxTransposition maximum grain resampling (transposition) in cent
   */ 
  IaeSynth(int numVoices, int numOutputChannels, double maxGrainDuration, double maxDelayDuration, double maxTransposition);
  virtual ~IaeSynth(void);
  
private:
  // forbid copy constructor and assignment because of zsazsa pointer
  IaeSynth (IaeSynth &) {};
  IaeSynth &operator= (IaeSynth &) {};
  
public:
  /** clear loaded data */
  void clear();
  
  /**
   * Resets DSP buffers with new configuration values.
   * @param sampleRate audio sample rate
   * @param maxOuputSize maximum audio output vector size
   * @param grainParamsQueueSize number of grains that can be explicitly triggered between two proccess calls
   * @return whether the buffers have been reset
   *
   * Reset the DSP buffers of the engine when the configuration has changed in repect to the 
   * values passed to the constructor.
   */
  bool resetDspBuffers(double sampleRate, int maxOutputSize, int grainParamsQueueSize);  

  /**
   * Synthesizes a single segment/grain.
   * @return period
   */  
  virtual double synthSegment(int voiceIndex, int sourceIndex, int markerIndex, double position);
  
  /**
   * Calculates outputs for one DSP block (vector).
   * @param outputs arrary of non-interleaved audio output vectors
   * @param outputSize number of frames to be calculated (= size of output vectors)
   * @param numOutputs number of audio outputs (= number of output vectors)
   *
   * @return number of grains started in this block
   */
  int audioProcess(float **outputs, int outputSize, int numOutputs);

  /**
   * Returns duration of the audio track of the given source.
   * @param sourceIndex source index
   * @return duration
   */  
  double getSourceDuration(int sourceIndex) { return this->container->getTrackEndTime(sourceIndex, 0); };
  
  /**
   * Sets voice index for following parameter changes.
   */
  void setVoiceIndex(int index);

  /**
   * Sets source index for next grain/segment.
   */
  void setSourceIndex(int index);
  /**
   * Gets current source index.
   */
  int getSourceIndex(void);

  /**
   * Sets marker index for next grain/segment.
   */
  void setMarkerIndex(int index);
  
  /**
   * Sets whether a grain/segment is repeated.
   */
  void setEnableMarkers(bool flag);
  
  /**
   * Sets whether a grain/segment is not repeated.
   */
  void setRepeatMarkers(bool flag);
  
  /**
   * Enable/disable considering audio buffer and markers as cyclic.
   */  
  void setCyclic(bool enable);
	
  /**
   * Enables/disables micro timing (by sample interpolation).
   */
  void setMicroTiming(bool enable);
  
  /**
   * Enables/disables (unsegmented) grains are centered.
   */
  void setCenteredGrains(bool enable);
  
  /**
   * Set grain/segment scheduling advance.
   */  
  void setAdvance(double advance);
	
  /**
   * Set minimum and maximum of grain/segment period.
   */  
  void setPeriodMinMax(double min, double max);

  /**
   * Set synthesis mode (Granular, Segmented, Synchronous).
   */  
  void setSynthMode(int mode);

  /**
   * Get synthesis mode (Granular, Segmented, Synchronous).
   */  
  int getSynthMode()  { return synthMode; };

  /**
   * Enable/disable (quasi) periodic playing.
   */  
  void setPlay(bool flag);
	
  /**
   * Get whether current voice is playing.
   */  
  bool isPlaying();

  /**
   * Triggers grain or resets phase of grain period.
   */
  void trigger(void);
  
  /**
   * Sets grain position in source (audio file).
   * @param position grain position in msec
   * @param transitionTime grain position transition time in msec
   */
  void setPosition(double position, double transitionTime = 0.0);

  /**
   * Controls random variation of position.
   * @param msec amout of random variation in msec
   */
  void setPositionVar(double msec);

  /**
   * Sets grain period.
   * @param absolute grain period in msec
   * @param relative grain period as a factor of marker interspace
   */
  void setPeriod(double absolute, double relative = 0.0);

  /**
   * Controls random variation of period.
   * @param absolute amout of random variation in msec
   * @param relative amout of random variation as a factor of the current period
   */
  void setPeriodVar(double absolute, double relative = 0.0);

  /**
   * Sets grain duration.
   * @param absolute grain duration in msec
   * @param relative grain duration as a factor of period or marker interspace (< 0)
   */
  void setDuration(double absolute, double relative = 0.0);

  /**
   * Controls random variation of grain duration.
   * @param absolute amout of random variation in msec
   * @param relative amout of random variation as a factor of the current period (< 0)
   */
  void setDurationVar(double absolute, double relative = 0.0);

  /**
   * Sets grain Offset.
   * @param msec grain absolute in msec
   */
  void setOffset(double msec);
  
  /**
   * Sets attack time.
   * @param absolute attack time in msec
   * @param relative attack time as a factor of the current duration
   */
  void setAttack(double absolute, double relative = 0.0);

  /**
   * Sets release time.
   * @param absolute release time in msec
   * @param relative release time as a factor of the current duration
   */
  void setRelease(double absolute, double relative = 0.0);

  /**
   * Controls grain resampling.
   * @param cent amount of grain resampling (transposition) in cent
   */
  void setResampling(double cent);

  /**
   * Controls random variation of grain resampling.
   * @param cent amount of variation in cent
   */
  void setResamplingVar(double cent);  
  
  /**
   * Sets grain gain as linear factor.
   * @param gain linear amplitude factor
   */
  void setGain(double gain);
  
  /**
   * Sets grain level in dB.
   * @param db attenuation in dB
   */
  void setLevel(double db);
  
  /**
   * Controls random variation of grain level.
   * @param db amout of random variation in dB
   */
  void setLevelVar(double db);  
  
  /**
   * Set grain/segment scheduling delay.
   */  
  void setDelay(double delay);
	
  /**
   * Sets grain filter mode.
   * @param mode filter mode
   */
  void setFilterMode(enum IaeFilterModeE mode);
  
  /**
   * Sets grain filter cutoff/center frequency.
   * @param hz frequency in Hz
   */
  void setFilterFreq(double hz);
  
  /**
   * Controls random variation of grain filter cutoff/center frequency.
   * @param hz frequency variation in hz
   */
  void setFilterFreqVar(double hz);  
  
  /**
   * Sets grain filter Q-factor.
   * @param q Q-factor
   */
  void setFilterQ(double q);
  
  /**
   * Controls random variation of grain filter cutoff/center frequency.
   * @param q Q-factor variation as an absolute value
   */
  void setFilterQVar(double q);
  
  /**
   * Sets grain filter gain.
   * @param db gain in dB
   */
  void setFilterGain(double db);
  
  /**
   * Enables/disables duplicating grain channels (when less than output channels).
   * @param enable flag
   */
  void setDuplicateChannels(bool enable);
  
  /**
   * Sets output channel offset.
   */
  void setOutputChannelOffset(int offset);
  
  /**
   * Sets output gain for a given output channel.
   * @param index output channel index
   * @param gain linear gain factor
   */
  void setOutputChannelGain(int index, double gain);
  
  /**
   * Sets additional output delay for a given output channel.
   * @param index output channel index
   * @param delay delay gain factor
   */
  void setOutputChannelDelay(int index, double delay);
  
  /**
   * Sets output gains and delay by balance between two sucessive output channels.
   * @param center ?
   * @param delay maximum delay difference
   */
  void setOutputChannelBalance(double center, double delay = 0.0, double ampl = 1.0);
  
  /**
   * Empty method, overriden by the derived class that loads buffers
   * @param
   * @param the list of all the files to be loaded as buffers
   */
  virtual void setFiles(int len, std::string *fileList);
  
  /**
   * Resets (zero) output streams.
   */  
  void resetOutputs(void);
  
    /** 
   * return string with info about loaded tracks
   * @return static string, not thread safe!!!
   */
  char *infoPrintString ();  

  static const int majorVersion();
  static const int minorVersion();
  static const int releaseVersion();
};

#endif /*_IAE_SYNTH_H_*/
