/** @file IaeCuss.h
 * 
 *  @brief concatenative unit-selection synthesis for IMTR Audio Engine
 *
 *  @author Diemo Schwarz, Norbert Schnell
 *  Created 21.02.11.
 *  Copyright 2011 IRCAM-Centre Georges Pompidou, Paris, France. All rights reserved.
 */

#ifndef _IAE_CUSS_H_
#define _IAE_CUSS_H_

#ifdef __cplusplus
extern "C" {
#endif
  
#include "assert.h"
#include "stdlib.h"
#include "float.h"

#ifndef WIN32
#include <sys/time.h>
#else
#include <time.h>
#include <sys/timeb.h>

#endif

// opaque forward declaration of kdtree data structs
typedef struct _kdtree_struct		kdtree_t;
typedef struct _kdtree_object_struct	kdtree_object_t;

// enums in rta_kdtree.h are replaced by ints

#ifdef __cplusplus
}
#endif

#include <string.h>
#include "IaeSynth.h"


typedef enum {ErrNone, ErrEmptyDataTrack, ErrNoSources, ErrNoMatrixData, 
	      ErrMissingTrack, 
	      ErrDimensionMismatchWarning, ErrDimensionMismatchError
} UpdateKdTreeErrorE; //todo: replace by exceptions

/** scaling mode for selection */
typedef enum {KnnScalingOff,	//!< no scaling
	      KnnScalingMinMax, //!< scaling from 0..1 to min..max of descriptor
	      KnnScalingMeanStddev,	//!< scaling from -1..1 to -std..mean..std
	      KnnScalingNum} KnnScalingModeE;

/** type of statistics to return for getDescriptorStat */
typedef enum {KnnStatMin,	//!< minimum
	      KnnStatMax,	//!< maximum
	      KnnStatRange,	//!< range
	      KnnStatMean,	//!< arithmetic mean
	      KnnStatStddev,	//!< standard deviation
	      KnnStatNormStddev,	//!< normalized standard deviation (divided by mean)
	      KnnStatNum} KnnStatE;

/** random mode for selection */
typedef enum {KnnRandomUniform,	//!< just random
	      KnnRandomUrn,	//!< random without repetition
	      KnnRandomNum} KnnRandomModeEnum;


#define MAX_KD_DIM 32

class IndexUrn
{
public:
    IndexUrn (int maxsize);
    ~IndexUrn ();
private:
    // forbid copy constructor and assignment because of pointer members
    IndexUrn (IndexUrn &) {};
    IndexUrn &operator= (IndexUrn &) {};
public:
    void reset(int size);		// re-fill urn
    void reset(int size, int last);	// reset and remember last
    int  draw();			// draw one ball
    void print(const char *note);

private:
    int allocsize_;	// max size of urn
    int size_;		// size of urn
    int left_;		// balls left in urn
    int	*index_;	// indices still to be drawn

#ifdef DEBUG
    int last_;
#endif
};


/** @class IaeCuss
@ingroup IAE
 
@brief helper class to mange list of playing grains
*/

class AliveList
{
public:
  AliveList (int num) 
  {
    start_ = get_time_since(0);
    will_end_at_.resize(num);
  };
  
  bool is_playing (int i) 
  {
    return will_end_at_[i] > current_time();
  }

  void set_playing (int i, long length) 
  {
    will_end_at_[i] = current_time() + length;
    //todo: getSegmentDuration(0, i) ???
  }

  long will_play_until (int i)  {  return will_end_at_[i]; };
  long will_play_for   (int i)  {  return will_end_at_[i] - current_time(); };
  unsigned long current_time () { return get_time_since(start_); };
  
  static unsigned long get_time_since (unsigned long start)
  {
#ifndef WIN32
    struct timeval time; 
    gettimeofday(&time, NULL); 
    return (time.tv_sec * 1000) + (time.tv_usec / 1000) - start; 
#else
	struct _timeb timebuffer;
	_ftime(&timebuffer);
	return (timebuffer.time * 1000) + timebuffer.millitm - start;
#endif
  } 

private:
  std::vector<unsigned long>  will_end_at_;
  unsigned long               start_;
};

/** @class IaeCuss
    @ingroup IAE
 
@brief subclass of IaeSynth for concatenative unit-selection corpus-based synthesis
 
This class specialises IaeSynth (for granular synthesis) by unit
selection methods based on descriptors and the kd-tree search index.
 
It adds methods to: 
\li query descriptors loaded into the mubucontainer from sdif files
\li build the kdtree from the descriptor data
\li (set the list of descriptors to use for selection)
\li query the kdtree by giving a list of target descriptor values and their weights
\li set and configure different synthesis behaviours (fence, loop, no repeat)
\li control synthesis from the given target values and behaviour

*/

class IaeCuss : public IaeSynth 
{
public:
    /** constructor, extending IaeSynth constructor
	@param numOutputChannels number of audio output channels
	@param maxOutputSize maximum size of output vectors (block computation)
	@param sampleRate audio sample rate
	@param maxGrainDuration maximum grain duration
	@param maxTransposition maximum grain resampling (transposition) in cent
	@param maxdescr	max number of descriptors to use
	@param kmax	max number for k (selected units)
     */
    IaeCuss (int numVoices, int numOutputChannels,
             double maxGrainDuration, double maxDelayDuration, double maxTransposition,
             int maxdescr, int kmax);
 
    // forbid copy constructor and assignment
private:
    IaeCuss (const IaeCuss&);
    const IaeCuss& operator=(const IaeCuss&);
  
public:
    /** destructor */
    virtual ~IaeCuss ();
  
    /** clear loaded data */
    void clear();
    
    /** set data track to use for descriptor data by index 
	@return		track index or -1 if not found */
    void setDescriptorTrack (int index);

    /** set data track to use for descriptor data by track name 
	@return		track index or -1 if not found */
    int setDescriptorTrack (const char *name);
    
    /** set data track to use for descriptor data by SDIF selection 
	@param stream	stream ID
	@param fsig	frame signature
	@param msig	matrix signature
	@return		track index or -1 if not found */
    int setDescriptorTrackSdif (int stream, const char *fsig = NULL, const char *msig = NULL);

    /** set number of descriptors used */
    int setNumDescriptors (int n) { numdescr_ = n; }

    /** get number of descriptors used */
    int getNumDescriptors () { return numdescr_; }

    /** get descriptor name in loaded data
	id must be < getNumDescriptors() */
    const char *getDescriptorName (int id);	

    /** get number of (included) units */
    int getNumData () { return kdNumData_; }

    /** get buffer index of (included) unit i */
    int getSourceIndex (int i);

    /** get segment index within buffer of (included) unit i */
    int getSegmentIndex (int i);

    /** get time for unit \p index */
    double getDescriptorTime (int buffer, int index);
  
    /** get number of matrix rows for unit \p index in descriptor track */
    int getDescriptorNumRows (int buffer, int index);
  
    /** get descriptor data from descriptor track of buffer for unit index.
	outdata must have space for getNumDescriptors() elements 
	@return ok flag */
    bool getDescriptorData (int buffer, int index, float *outdata);

    /** get descriptor statistics data of current corpus
	outdata must have space for getNumDescriptors() elements
	@return ok flag */
    bool getDescriptorStat (KnnStatE which, float *outdata);

    /** conversion functions: normalised values to/from raw descriptor value */
    float convMinMaxToDescriptor  (int descrid, float valnorm);
    float convMeanStdToDescriptor (int descrid, float valmeanstd);
    float convDescriptorToMinMax  (int descrid, float valdescr);
    float convDescriptorToMeanStd (int descrid, float valdescr);

    /** set max number of returned units, k = 0 means all up to kmax */
    void setK (int k);

    /** set max distance of returned units (0 = infinite) */
    void setRadius (float r);

    /** set target value for one descriptor
	@param i	descriptor index
	@param v	target value, in descriptor units if scalingMode is None, 0..1 otherwise    */
    void setTarget (int i, float v);
    void setTarget (int len, float *v);

    /** set weight for one descriptor 
	@param i	descriptor index
	@param w	relative weight > 0, 0 = don't use this descriptor */
    void setWeight (int i, float w);
    void setWeight (int len, float *w);
    void setWeight (int len, float *w, float pad);

    /** MUST call after setTarget or setWeight to trigger on changed target 
        @return number of units found by kdtree (if target wasn't changed, returns zero although the last unit might be triggered)
     */
    int selectNew (bool autotrigger = true);

    /** use this to switch back to granular mode */
    void clearSelection() { numselected_ = 0; };

    /** get weight for one descriptor */
    float getWeight (int i);

    /** set scaling mode for target values */
    void  setScalingMode (KnnScalingModeE m) { scaling_mode_ = m; };
    /** get scaling mode */
    KnnScalingModeE getScalingMode () { return scaling_mode_; };

    /** set random mode for target values */
    void  setRandomMode (KnnRandomModeEnum m) { random_mode_ = m; };
    /** get random mode */
    KnnRandomModeEnum getRandomMode () { return random_mode_; };

    /* return true if given buffer is included in tree */
    int getIncluded (int idx) { return kdIncluded_[idx]; };

    /** rebuild kdtree 
	@param rebuildIncludedSources	if included changed    */
    UpdateKdTreeErrorE updateKdTree (int rebuildIncludedSources); // : exceptions

    /** Load an audio file in a new buffer
	Resizes included list, sets included flag to true.
	@param fileName audio file name
	@return number of frames loaded */
    int loadAudioFile(const char *fileName);

  int loadDescriptionFile(const char *fileName, const char *sdifSelection = NULL);

  int removeSource(int sourceIndex);
  int removeSource(const char * sourceName);

  int setMarkerTrack(int index);
  int setMarkerTrack(const char *name);
  int setMarkerTrackSdif(int streamId, const char *frameSignature, const char *matrixSignature);
  
  /** Synthesizes a single segment/grain by selection.

	Perform selection of current target (rescaled according to scaling mode) with current weights
	Set numselected_ to number of units selected.

	@return period */  
    virtual double synthSegment (int voiceIndex, int sourceIndex, int markerIndex, double position);

    /** query number of last selected units */
    int getNumSelected () { return numselected_; };

    /** query last selected unit buffer indices */
    int getSelectedSourceIndex  (int i);

    /** query last selected unit indices */
    int getSelectedSegmentIndex (int i);

    /** query distance to last selected unit */
    float getSelectedDistance (int i) { return distances_[i]; };

    /** switch buffers to use 
	Note: you must then call updateKdTree(1, -1, -1) */
    void setIncludeSource(int index, bool onoff);
    void setIncludeSource(int len, bool *onoff);
    void setIncludeSource(int len, int *onoff);

    /** switch buffers to use 	
	Note: you must then call updateKdTree(1, -1, -1) */
    void setIncludeAll(bool onoff);
	
    kdtree_t   *kdTree_;

private:
    const int   kmax_;		// limit for k (allocated space)
    int		maxdescr_;	//**< max number of descriptors to use for selection
    int		numdescr_;	//**< number of descriptors present in loaded data
    float      *target_;	//**< list of numdescr target descriptor values 
    float      *target_sc_;	//**< list of numdescr target scaled values
    float      *sigma_;		//**< list of numdescr inverse weight values
    int		k_;
    float	radius_;
    bool	target_changed_;	//**< target or weights changed
    bool	params_changed_;	//**< k or radius changed
    bool	selection_changed_;	//**< result set rewritten

    // selection result set
    int			numselected_;
    bool		newselected_;	// true if selection changed
    kdtree_object_t    *indices_;
    float	       *distances_;

    int		dMode_;
    int		mMode_;  
    int		kdDim_;

    int	       *kdIncluded_;
    MuBuTrackT**kdTracks_;
    float     **kdData_;
    int	       *kdSizes_;
    int	       *kdIndices_;
    int		kdNumIncluded_;
    
    float      *kdMean_;
    float      *kdStddev_;
    float      *kdNormStddev_;
    float      *kdMin_;
    float      *kdMax_;
    float      *kdRange_;
    int	        kdNumData_;
    double      kdUpdateTime_;

    KnnScalingModeE   scaling_mode_;
    KnnRandomModeEnum random_mode_;

    int		descriptorTrackIndex_;

    void resetKdTree ();
    int updateStats ();

#ifdef DEBUG
public:
#endif
    IndexUrn	     *urn_;
};

#endif /*_IAE_CUSS_H_*/


/** EMACS **
 * Local variables:
 * mode: c
 * c-basic-offset:2
 * End:
 */
