
// max number of descriptors to use
#define MAX_DESCR 64
// max k-nn query size
#define MAX_K 63

/***************************************************************
 *
 *  ISMM audio engine for OpenMusic
 *
 *  Here we oerride the virtual functions of the IAE 
 *  creating an OpenMusic version of the engine.
 *
 */

#pragma mark -
#pragma mark ieaom data structure

#ifdef __cplusplus
extern "C" {	// C-binding for OM library
#endif

typedef struct _iaeom iaeom_t;

int iae_get_num_params ();
const char *iae_get_param_name (int i);

// create iae engine of nvoice voices, alloc buffers
iaeom_t *iae_new (int sr, int maxbuffersize, int nchan, int nvoice);
	
void iae_delete (iaeom_t *self);

// generate a total of nsamp samples of interleaved audio for numchannels
int iae_synth (iaeom_t *self, int nsamp, float **audioout, int nchan);

int iae_get_track_size(iaeom_t *self, int buffer, int track);

#pragma mark -
#pragma mark file methods	

char *iae_info_get_string (iaeom_t *self, const char *arg);
int iae_read (iaeom_t *self, const char *audioFileName, const char *descriptionFileName);

  

#pragma mark -
#pragma mark analysis methods

  int iae_pipo_create (iaeom_t *self, const char *name);	// TODO: can be pipo chain
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
int  iae_get_descriptor_num_rows (iaeom_t *self, int buffer, int index);

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
	
void iae_set_SynthMode(iaeom_t *self, int mode)	;
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
#define float_map float	
#define double_map double	
#define bool_map  bool
#define int_map  int
#define charp_map char*	
#define charp char*	
	
///////////// macros generating parameter setters/getters
#define IAEPARAMx(NAME, TYPE, FORMAT, DEFVAL)			\
  void iae_set_ ## NAME (iaeom_t *self, TYPE p);		\
  bool iae_get_ ## NAME (iaeom_t *self, TYPE ## _map *p);

#define IAEPARAMax(NAME, TYPE, FORMAT, DEFVAL)				\
  int iae_set_ ## NAME (iaeom_t *self, int len, TYPE *p);		\
  bool iae_get_ ## NAME (iaeom_t *self, TYPE *p);

#define IAEPARAMxx(NAME, TYPE1, TYPE2, FORMAT, DEFVAL1, DEFVAL2)	\
  void iae_set_ ## NAME (iaeom_t *self, TYPE1 p1, TYPE2 p2);		\
  bool iae_get_ ## NAME (iaeom_t *self, TYPE1 *p);	/*we assume that the types of the parameters are the same*/ 


#define IAEPARAMxxx(NAME, TYPE1, TYPE2, TYPE3, FORMAT, DEFVAL1, DEFVAL2, DEFVAL3)	\
  void iae_set_ ## NAME (iaeom_t *self, TYPE1 p1, TYPE2 p2, TYPE3 p3); \
  bool iae_get_ ## NAME (iaeom_t *self, TYPE1 *p);	/*we assume that the types of the parameters are the same*/


#define IAEPARAMi(NAME, DEFVAL)  	IAEPARAMx(NAME, int,    "%d", DEFVAL)
#define IAEPARAMb(NAME, DEFVAL)  	IAEPARAMx(NAME, bool,   "%d", DEFVAL)
#define IAEPARAMf(NAME, DEFVAL)  	IAEPARAMx(NAME, float,  "%f", DEFVAL)
#define IAEPARAMd(NAME, DEFVAL)  	IAEPARAMx(NAME, double, "%f", DEFVAL)
#define IAEPARAMs(NAME, DEFVAL)  	IAEPARAMx(NAME, charp, "%s", DEFVAL)

#define IAEPARAMaf(NAME, DEFVAL) 	IAEPARAMax(NAME, float,  "%f", DEFVAL)
#define IAEPARAMab(NAME, DEFVAL) 	IAEPARAMax(NAME, bool,   "%d", DEFVAL)
#define IAEPARAMai(NAME, DEFVAL) 	IAEPARAMax(NAME, int,   "%d", DEFVAL)
#define IAEPARAMad(NAME, DEFVAL) 	IAEPARAMax(NAME, double, "%f", DEFVAL)
#define IAEPARAMas(NAME, DEFVAL) 	IAEPARAMax(NAME, char *, "%s", DEFVAL)

//not used anymore
//#define IAEPARAMib(NAME, D1, D2) 	IAEPARAMxx(NAME, int, bool, "%d, %d", D1, D2)
//#define IAEPARAMif(NAME, D1, D2) 	IAEPARAMxx(NAME, int, float, "%d, %f", D1, D2)
//#define IAEPARAMid(NAME, D1, D2) 	IAEPARAMxx(NAME, int, double, "%d, %f", D1, D2)
//#define IAEPARAMis(NAME, D1, D2) 	IAEPARAMxx(NAME, int, char *, "%d, %s", D1, D2)

#define IAEPARAMdd(NAME, D1, D2) 	IAEPARAMxx(NAME, double, double, "%f, %f", D1, D2)
#define IAEPARAMddd(NAME, D1, D2, D3) IAEPARAMxxx(NAME, double, double, double, "%f, %f, %f", D1, D2, D3)
//not used anymore
//#define IAEPARAMiss(NAME, D1, D2, D3) IAEPARAMxxx(NAME, int, char *, char *, "%d, %s, %s", D1, D2, D3)

#define IAEQUERYx(NAME, TYPE1, FORMAT)		\
  TYPE1 iae_get_ ## NAME (iaeom_t *self);

#define IAEQUERYax(NAME, TYPE, FORMAT, GETLEN)			\
  int iae_get_array_ ## NAME (iaeom_t *self, TYPE *arr);

#define IAEQUERYxx(NAME, TYPE1, TYPE2, FORMAT1, FORMAT2, COPY)	\
  TYPE1 iae_get_ ## NAME (iaeom_t *self, TYPE2 p);

#define IAEQUERYxxx(NAME, TYPE1, TYPE2, TYPE3, FORMAT1, FORMAT2, FORMAT3, COPY)	\
  TYPE1 iae_get_ ## NAME (iaeom_t *self, TYPE2 p1, TYPE3 p2); 

#define IAEGETx(NAME, TYPE, FORMAT)			\
  bool iae_get_ ## NAME (iaeom_t *self, TYPE *p); 

	


#define IAEQUERY_BEGIN    
#define IAEQUERY_END	

#define IAEQUERYi(NAME)		IAEQUERYx(NAME, int, "%d")
#define IAEQUERYb(NAME)		IAEQUERYx(NAME, bool, "%d")

#define IAEQUERYai(NAME, GETLEN)	IAEQUERYax(NAME, int, "%d", GETLEN)
#define IAEQUERYaf(NAME, GETLEN) 	IAEQUERYax(NAME, float, "%f", GETLEN)
#define IAEQUERYad(NAME, GETLEN) 	IAEQUERYax(NAME, double, "%f", GETLEN) 
//not working for string:  #define IAEQUERYas(NAME, GETLEN)	IAEQUERYax(NAME, const char *, "'%s'", GETLEN)

#define IAEQUERYii(NAME)		IAEQUERYxx(NAME, int, int, "%d", "%d", )
#define IAEQUERYfi(NAME) 		IAEQUERYxx(NAME, float, int, "%f", "%d", )
#define IAEQUERYdi(NAME) 		IAEQUERYxx(NAME, double, int, "%f", "%d", )
#define IAEQUERYdd(NAME) 		IAEQUERYxx(NAME, double, double, "%f", "%f", )
#define IAEQUERYsi(NAME)		IAEQUERYxx(NAME, const char *, int, "'%s'", "%d", strdup)
#define IAEQUERYdii(NAME) 		IAEQUERYxxx(NAME, double, int, int, "%f", "%d", "%d", )

//for specials
#define IAEGETi(NAME)		IAEGETx(NAME, int, "%d")
#define IAEGETb(NAME)		IAEGETx(NAME, bool, "%d")
#define IAEGETd(NAME)		IAEGETx(NAME, double, "%f")


// array get: for deprecated access by index generate accessor functions:

/// <summary>Return number of sources</summary>
IAEQUERYi(NumSources)

/// <summary>Return duration of the audio track of the given buffer</summary>
/// <param name="Index">Index of sound file buffer to query</param>
IAEQUERYdi(AudioDuration)

/// <summary>Return duration of segment starting with marker markerIndex.
/// If last marker, duration until end of audio is returned.
/// (Can be negative if audio duration is shorter than marker track) </summary>
/// <param name="source">Index of sound file buffer to query</param>
/// <param name="marker">Index of marker</param>
IAEQUERYdii(SegmentDuration)

/// <summary>Return size of the marker track of given buffer</summary>
IAEQUERYii(NumMarkers)

/// <summary> get number of descriptors used</summary>
IAEQUERYi(NumDescriptors)
/// <summary> get descriptor name in loaded data</summary>
IAEQUERYsi(DescriptorName)
  
/// <summary> query number of last selected units </summary>
IAEQUERYi(NumSelected)
/// <summary> query last selected unit buffer indices </summary>
IAEQUERYii(SelectedSourceIndex)
/// <summary> query last selected unit indices </summary>
IAEQUERYii(SelectedSegmentIndex)
/// <summary> query last selected distance </summary>
IAEQUERYfi(SelectedDistance)

/// <summary>Return if engine is granular or other</summary>
IAEQUERYi(SynthMode)
  
#include "iae-params.h"

#ifdef __cplusplus
} // extern "C"
#endif

/** EMACS **
 * Local variables:
 * mode: c
 * c-basic-offset:2
 * End:
 */
