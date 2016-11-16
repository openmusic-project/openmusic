# 1 "iae-om-binding.h"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 325 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "iae-om-binding.h" 2

// max number of descriptors to use

// max k-nn query size


/***************************************************************
 *
 *  ISMM audio engine for OpenMusic
 *
 *  Here we oerride the virtual functions of the IAE 
 *  creating an OpenMusic version of the engine.
 *
 */
# 23 "iae-om-binding.h"
typedef struct _iaeom iaeom_t;

int iae_get_num_params ();
const char *iae_get_param_name (int i);

// create iae engine of nvoice voices, alloc buffers
iaeom_t *iae_new (int sr, int maxbuffersize, int nchan, int nvoice);

void iae_delete (iaeom_t *self);

// generate a total of nsamp samples of interleaved audio for numchannels
int iae_synth (iaeom_t *self, int nsamp, float **audioout, int nchan);

int iae_get_track_size(iaeom_t *self, int buffer, int track);




char *iae_info_get_string (iaeom_t *self, const char *arg);
int iae_read (iaeom_t *self, const char *audioFileName, const char *descriptionFileName);






  int iae_pipo_create (iaeom_t *self, const char *name);
// TODO: can be pipo chain
  int iae_pipo_clear (iaeom_t *self);

  // query attrs
  /** get number of parameters of created pipos */
  int iae_pipo_param_num (iaeom_t *self);
  /** get name of pipo parameter at index (of the form: "piponame.paramname") */
  const char *iae_pipo_param_get_name (iaeom_t *self, int paramindex);
  /** get description of pipo parameter at index */
  const char *iae_pipo_param_get_description (iaeom_t *self, int paramindex);
  /** get type of pipo parameter at index
      @return type according to enum Type { Undefined, Bool, Enum, Int, Float, Double, String }
   */
  int iae_pipo_param_get_type (iaeom_t *self, int paramindex);

  // query enum attr elements
  /** get number of enum elements of a pipo parameter of type 2 (Enum) */
  int iae_pipo_param_enum_get_num (iaeom_t *self, const char *enumattrname);
  /** get name of enum element at index \p elem of a pipo parameter of type 2 (Enum) */
  const char *iae_pipo_param_enum_get_element (iaeom_t *self, const char *enumattrname, int elem);

  // set pipo attr value
  int iae_pipo_param_set_int (iaeom_t *self, const char *attrname, int index, int value);
  int iae_pipo_param_set_float (iaeom_t *self, const char *attrname, int index, double value);
  int iae_pipo_param_set_string (iaeom_t *self, const char *attrname, int index, const char *value);

/** run current pipo on given buffer, set output descriptor track index
    @param buffer	buffer index
    @return		descriptor track index
 */
int iae_pipo_run (iaeom_t *self, int bufferindex);


/** get number of rows of descriptor data matrix at given unit index in descriptor track (set by iae_pipo_run()).
    @param buffer	buffer index
    @param index	data element index (unit/marker/frame index)
    @return		number of matrix rows
 */
int iae_get_descriptor_num_rows (iaeom_t *self, int buffer, int index);

/** get descriptor data from descriptor track of buffer for unit index.
    outdata must have space for getNumDescriptors() * iae_get_descriptor_num_rows() elements 

    @param buffer	buffer index
    @param index	data element index (unit/marker/frame index)
    @return time 
*/
double iae_get_descriptor_data (iaeom_t *self, int buffer, int index, float *outdata);

/** get descriptor statistics data of current corpus
    outdata must have space for getNumDescriptors() elements
    @return ok flag 
*/
void iae_get_descriptor_stat (iaeom_t *self, int which, float *outdata);


/** rebuild kdtree after loading or calculating descriptors
 */
bool iae_update_kdtree (iaeom_t *self, bool rebuild);


/*
 * selection
 */

void iae_trigger (iaeom_t *self);
int iae_select_new (iaeom_t *self, bool forcetrigger);
void iae_clear_selection (iaeom_t *self);
bool iae_set_IncludeAll (iaeom_t *self, bool flag);


/** conversion functions: normalised values to/from raw descriptor value */
float iae_conv_minmax_to_descriptor (iaeom_t *self, int descrid, float valnorm);
float iae_conv_meanstd_to_descriptor (iaeom_t *self, int descrid, float valmeanstd);
float iae_conv_descriptor_to_minmax (iaeom_t *self, int descrid, float valdescr);
float iae_conv_descriptor_to_meanstd (iaeom_t *self, int descrid, float valdescr);


/** conversion functions: marker position / index */
int iae_conv_position_to_marker (iaeom_t *self, int buffer, double position);
double iae_conv_marker_to_position (iaeom_t *self, int buffer, int marker);


// special, not with macros

void iae_set_SynthMode(iaeom_t *self, int mode) ;
void iae_set_Position (iaeom_t *self, double position, double time);
void iae_set_VoiceIndex (iaeom_t *self, int i);


//todo: params
void iae_set_FilterMode (iaeom_t *self, int mode);
void iae_set_RandomMode (iaeom_t *self, int mode);
void iae_set_ScalingMode (iaeom_t *self, int mode);


// deprecated: now set automatically on iae_read.  Left here for old scripting access
void iae_set_MarkerTrackSdif (iaeom_t *self, int streamid, char *fsig, char *msig);

// deprecated: now set automatically on iae_read.  Left here for old scripting access
void iae_set_DescriptorTrackSdif (iaeom_t *self, int streamid, char *fsig, char *msig);



//getters for ENUM params are explicitly written here:
bool iae_get_FilterMode (iaeom_t *self, int *mode);
bool iae_get_RandomMode (iaeom_t *self, int *mode);
bool iae_get_ScalingMode (iaeom_t *self, int *mode);


//maps definitions to use bool as int, since UNITY has problems accessing bools







///////////// macros generating parameter setters/getters
# 198 "iae-om-binding.h"
//not used anymore
//#define IAEPARAMib(NAME, D1, D2) 	IAEPARAMxx(NAME, int, bool, "%d, %d", D1, D2)
//#define IAEPARAMif(NAME, D1, D2) 	IAEPARAMxx(NAME, int, float, "%d, %f", D1, D2)
//#define IAEPARAMid(NAME, D1, D2) 	IAEPARAMxx(NAME, int, double, "%d, %f", D1, D2)
//#define IAEPARAMis(NAME, D1, D2) 	IAEPARAMxx(NAME, int, char *, "%d, %s", D1, D2)



//not used anymore
//#define IAEPARAMiss(NAME, D1, D2, D3) IAEPARAMxxx(NAME, int, char *, char *, "%d, %s, %s", D1, D2, D3)
# 236 "iae-om-binding.h"
//not working for string:  #define IAEQUERYas(NAME, GETLEN)	IAEQUERYax(NAME, const char *, "'%s'", GETLEN)
# 245 "iae-om-binding.h"
//for specials





// array get: for deprecated access by index generate accessor functions:

/// <summary>Return number of sources</summary>
int iae_get_NumSources (iaeom_t *self);

/// <summary>Return duration of the audio track of the given buffer</summary>
/// <param name="Index">Index of sound file buffer to query</param>
double iae_get_AudioDuration (iaeom_t *self, int p);

/// <summary>Return duration of segment starting with marker markerIndex.
/// If last marker, duration until end of audio is returned.
/// (Can be negative if audio duration is shorter than marker track) </summary>
/// <param name="source">Index of sound file buffer to query</param>
/// <param name="marker">Index of marker</param>
double iae_get_SegmentDuration (iaeom_t *self, int p1, int p2);

/// <summary>Return size of the marker track of given buffer</summary>
int iae_get_NumMarkers (iaeom_t *self, int p);

/// <summary> get number of descriptors used</summary>
int iae_get_NumDescriptors (iaeom_t *self);
/// <summary> get descriptor name in loaded data</summary>
const char * iae_get_DescriptorName (iaeom_t *self, int p);

/// <summary> query number of last selected units </summary>
int iae_get_NumSelected (iaeom_t *self);
/// <summary> query last selected unit buffer indices </summary>
int iae_get_SelectedSourceIndex (iaeom_t *self, int p);
/// <summary> query last selected unit indices </summary>
int iae_get_SelectedSegmentIndex (iaeom_t *self, int p);
/// <summary> query last selected distance </summary>
float iae_get_SelectedDistance (iaeom_t *self, int p);

/// <summary>Return if engine is granular or other</summary>
int iae_get_SynthMode (iaeom_t *self);


# 1 "../unity3Diae/iae-params.h" 1
/*
 *  iaesynth
 */

/*  S E T T E R S  and  G E T T E R S */

// IAEPARAMdd(Position, 0, 0) // is special!
void iae_set_PositionVar (iaeom_t *self, double p);
bool iae_get_PositionVar (iaeom_t *self, double *p);

/// <summary>Switch IAE periodic grain triggering on/off</summary>
//IAEPARAMb (Play, false)  //-------->MOVE TO END

/// <summary>Set IAE grain trigger period</summary>
/// <param name="PeriodAbs">in ms</param>
/// <param name="PeriodRel">relative to segment length</param>
void iae_set_Period (iaeom_t *self, double p1, double p2);
bool iae_get_Period (iaeom_t *self, double *p);

/// <summary>Set IAE period random variation</summary>
/// <param name="PeriodVarAbs">in ms</param>
/// <param name="PeriodVarRel">relative to segment length if markers are present, relative to period otherwise</param>
void iae_set_PeriodVar (iaeom_t *self, double p1, double p2);
bool iae_get_PeriodVar (iaeom_t *self, double *p);

/// <summary>Set IAE grain duration</summary>
/// <param name="DurationAbs">in ms</param>
/// <param name="DurationRel">relative to segment length if markers are present, relative to period otherwise</param>
void iae_set_Duration (iaeom_t *self, double p1, double p2);
bool iae_get_Duration (iaeom_t *self, double *p);

/// <summary>Set IAE duration random variation</summary>
/// <param name="DurationVarAbs">in ms</param>
/// <param name="DurationVarRel">relative to segment length if markers are present, relative to period otherwise</param>
void iae_set_DurationVar (iaeom_t *self, double p1, double p2);
bool iae_get_DurationVar (iaeom_t *self, double *p);

void iae_set_Attack (iaeom_t *self, double p1, double p2);
bool iae_get_Attack (iaeom_t *self, double *p);
void iae_set_Release (iaeom_t *self, double p1, double p2);
bool iae_get_Release (iaeom_t *self, double *p);
void iae_set_Resampling (iaeom_t *self, double p);
bool iae_get_Resampling (iaeom_t *self, double *p);
void iae_set_ResamplingVar (iaeom_t *self, double p);
bool iae_get_ResamplingVar (iaeom_t *self, double *p);
//IAEPARAMi(FilterMode, 0)	// enum
void iae_set_FilterFreq (iaeom_t *self, double p);
bool iae_get_FilterFreq (iaeom_t *self, double *p);
void iae_set_FilterFreqVar (iaeom_t *self, double p);
bool iae_get_FilterFreqVar (iaeom_t *self, double *p);
void iae_set_FilterQ (iaeom_t *self, double p);
bool iae_get_FilterQ (iaeom_t *self, double *p);
void iae_set_FilterQVar (iaeom_t *self, double p);
bool iae_get_FilterQVar (iaeom_t *self, double *p);
void iae_set_FilterGain (iaeom_t *self, double p);
bool iae_get_FilterGain (iaeom_t *self, double *p);
void iae_set_Gain (iaeom_t *self, double p);
bool iae_get_Gain (iaeom_t *self, double *p); // deprecated: linear gain (overwrites Level)
void iae_set_Level (iaeom_t *self, double p);
bool iae_get_Level (iaeom_t *self, double *p); // dB (overwrites Gain)
void iae_set_LevelVar (iaeom_t *self, double p);
bool iae_get_LevelVar (iaeom_t *self, double *p);


void iae_set_SourceIndex (iaeom_t *self, int p);
bool iae_get_SourceIndex (iaeom_t *self, int *p);
void iae_set_MarkerIndex (iaeom_t *self, int p);
bool iae_get_MarkerIndex (iaeom_t *self, int *p);
//IAEPARAMb(EnableMarkers, false) not exist

void iae_set_RepeatMarkers (iaeom_t *self, bool p);
bool iae_get_RepeatMarkers (iaeom_t *self, bool *p);
void iae_set_Cyclic (iaeom_t *self, bool p);
bool iae_get_Cyclic (iaeom_t *self, bool *p);
void iae_set_MicroTiming (iaeom_t *self, bool p);
bool iae_get_MicroTiming (iaeom_t *self, bool *p);
void iae_set_CenteredGrains (iaeom_t *self, bool p);
bool iae_get_CenteredGrains (iaeom_t *self, bool *p);

void iae_set_Advance (iaeom_t *self, double p);
bool iae_get_Advance (iaeom_t *self, double *p);
//IAEPARAMdd(PeriodMinMax, 0)
void iae_set_Offset (iaeom_t *self, double p);
bool iae_get_Offset (iaeom_t *self, double *p);
void iae_set_Delay (iaeom_t *self, double p);
bool iae_get_Delay (iaeom_t *self, double *p);

/// <summary> duplicate mono file to all channels (not just left) </summary>
void iae_set_DuplicateChannels (iaeom_t *self, bool p);
bool iae_get_DuplicateChannels (iaeom_t *self, bool *p);

// IAEPARAMi(OutputChannelOffset, 0) useful?
// IAEPARAMid(OutputChannelGain, 0, 0)
// IAEPARAMid(OutputChannelDelay, 0, 0)

// spatialisation: parameters overridden by scripts
void iae_set_OutputChannelBalance (iaeom_t *self, double p1, double p2, double p3);
bool iae_get_OutputChannelBalance (iaeom_t *self, double *p);



/*  G E T T E R S  for specials and not parameters*/




//special params, but not ENUM parameters [explicit in each file that includes iae-params.h]
bool iae_get_Position (iaeom_t *self, double *p);

//ENUM 
//IAEGETi(FilterMode)
//IAEGETi(VoiceIndex)


/// <summary>Return duration of the audio track of the given buffer</summary>
/// <param name="Index">Index of sound file buffer to query</param>
int iae_get_array_AudioDuration (iaeom_t *self, double *arr);

/// <summary>Return size of the marker track of given buffer</summary>
int iae_get_array_NumMarkers (iaeom_t *self, int *arr);




/*
 *  iaecuss  
 */

/*  S E T T E R S  and  G E T T E R S */

/// <summary> set data track to use for descriptor data by index </summary>
bool iae_get_DescriptorTrack (iaeom_t *self, int *p);

// set data track to use for descriptor data by track name 
//todo: special, since overloading lost in C: IAEPARAMs(DescriptorTrack, 0)

// set data track to use for descriptor data by SDIF selection
//too special: IAEPARAMiss(DescriptorTrackSdif, 0)

/// <summary> set max distance of returned units (0 = infinite) </summary>
void iae_set_Radius (iaeom_t *self, float p);
bool iae_get_Radius (iaeom_t *self, float *p);

/// <summary> set max number of returned units, k = 0 means all up to kmax</summary>
void iae_set_K (iaeom_t *self, int p);
bool iae_get_K (iaeom_t *self, int *p);

/// <summary> set target value for all descriptors </summary>
int iae_set_Target (iaeom_t *self, int len, float *p);
bool iae_get_Target (iaeom_t *self, float *p);

/// <summary> set weight for all descriptors </summary>
int iae_set_Weight (iaeom_t *self, int len, float *p);
bool iae_get_Weight (iaeom_t *self, float *p);

// set random mode for target values 
// IAEPARAMi(RandomMode, 0) // KnnRandomModeEnum

// scaling mode for target values
// IAEPARAMi(ScalingMode, 0) // KnnScalingModeE

/// <summary> set buffers to use </summary>
//IAEPARAMab(IncludeSource, false)
int iae_set_IncludeSource (iaeom_t *self, int len, int *p);
bool iae_get_IncludeSource (iaeom_t *self, int *p);



/*
 *  iaesynth---> Play must be at the end 
 */

/// <summary>Switch IAE periodic grain triggering on/off</summary>
void iae_set_Play (iaeom_t *self, bool p);
bool iae_get_Play (iaeom_t *self, bool *p);




/*  G E T T E R S  for specials and not parameters*/




//special params, but not ENUM parameters [explicit in each file that includes iae-params.h]
//ENUM
// IAEGETi(RandomMode)
// IAEGETi(ScalingMode)





/// <summary> get number of (included) units</summary>
int iae_get_NumData (iaeom_t *self);

/// <summary> get descriptor name in loaded data</summary>
//IAEQUERYas(DescriptorName, NumDescriptors)

/// <summary> query last selected unit buffer indices </summary>
int iae_get_array_SelectedSourceIndex (iaeom_t *self, int *arr);
/// <summary> query last selected unit indices </summary>
int iae_get_array_SelectedSegmentIndex (iaeom_t *self, int *arr);
/// <summary> query last selected distance </summary>
int iae_get_array_SelectedDistance (iaeom_t *self, float *arr);
# 288 "iae-om-binding.h" 2





/** EMACS **
 * Local variables:
 * mode: c
 * c-basic-offset:2
 * End:
 */
