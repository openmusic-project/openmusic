/** idesc API: external API around IrcamDescriptor template library

draft 2		11.6.2012	Diemo Schwarz, Frederic Cornu
draft 1		 4.5.2012	Diemo Schwarz
    
based on "Requirements for an Ircamdescriptor Library API"
Diemo Schwarz, Norbert Schnell, 4.11.2011

based on rt-requirements mail by Diemo Schwarz to Carmine Cella, 17.9.2008

$Id: idesc.h 5047 2012-10-25 12:16:10Z cornu $
*/

#ifndef IDESC_H_
#define IDESC_H_ 1

#ifndef IDESC_REAL_TYPE
#error IDESC_REAL_TYPE must be set to either float or double
#endif

namespace idesc {

/** API class around IrcamDescriptor template library

The idesc class provides a template-free API around the
IrcamDescriptor template library.  It provides a several fixed sets of
instantaneous descriptors only, choosable via compilation options.

\section config Compile-Time Configuration

It can be compiled for IDESC_REAL_TYPE = float or double run-time signal and descriptor
data (certain initialisation data may be computed with double
precision).

It can be compiled with different groups of descriptors excluded
(harmonic descriptors, perceptual descriptors, spectral descriptors)

\section seq Calling Sequence

- clear_descriptors (not needed the first time)
- set_<parameters>, set_descriptor
- build_descriptors
- get_dimensions
- compute
- flush remaining samples: compute with samples == NULL

After flushing, build_descriptors must be called again.
Any set_<parameter> or set_descriptor only takes effect with build_descriptors.
 
*/

class idesc 
{
public:
    /** initializes the Pm2 library (only needed for harmonic descriptors) */
    static void init_library();

    /** deinitializes the Pm2 library */
    static void deinit_library();

    /** constructor of idesc class
	@param sr	sampling rate in Hz
	@param winsize	window size in seconds
	@param hopsize  hop size in seconds
    */
    idesc (double sr, double winsize, double hopsize);

    /** Returns number of available instantaneous descriptors */
    static int get_num_descriptors ();

    /** returns name of instantaneous descriptor 
	(dimensions can be queried with get_dimensions() only after building the computation graph)
    */
    static const char *get_descriptor_name (int descrid);

    /** returns true if descriptor has variable output dimensions
	(a dimension query with get_dimensions returns the max size)
    */
    static bool is_descriptor_varsize (int descrid);

    /** get number of variations of descriptor
	(according to configuration) */
    static int get_num_variations (int descrid);
    static int get_default_variation (int descrid);
    static const char *get_variation_name (int descrid, int varnum);

    /**group: parameters: global calls to set parameters ************/
    void set_maxfreq (double maxfreq);		// global max analysis frequency (usually sr/2)
    void set_window (const char *name);		// window name

    void set_ResampleTo (int param); 		// Downsampled sampling rate [Hz]
    void set_FFTPadding (int param); 		// Pad FFT
    void set_NumBands (int param); 		// Number of spectrum bands (mel)
    void set_NumReducedBands (int param); 	// Number of reduced spectrum bands ()
    void set_NumAutoCorrCoeffs (int param); 	// Number of auto correlation coefficients
    void set_NumHarmonics (int param); 		// Number of harmonics
    
    void set_NumMFCCs (int param); 		// Number of mel-frequency cepstral coefficients
    void set_MfccFftBandNormMax (bool param); 	// Normalize the mel bands
    void set_MfccRemoveEnergy (bool param); 	// Remove first coefficient
    
    void set_F0MaxFreq (float param); 		// Maximum harmonic frequency
    void set_F0Min (float param); 		// Minimum candidate frequency
    void set_F0Max (float param); 		// Maximum candidate frequency
    void set_F0AmpThreshold (float param); 	// Amplitude threshold

    void set_SpectralRolloffThreshold (float param); 		// Threshold for spectral rolloff as ratio of spectrum
    void set_HarmonicSpectralDeviationStopBand (int param); 	// Threshold for spectral rolloff in percent of spectrum
    void set_HarmonicSpectralRolloffThreshold (float param);	// Threshold for harmonic spectral rolloff as ratio of spectrum
    void set_PerceptualSpectralDeviationStopBand (int param);	// Threshold for spectral rolloff in percent of spectrum
    void set_PerceptualSpectralRolloffThreshold (float param);	// Threshold for perceptual spectral rolloff as ratio of spectrum


    /**group: band parameters 
       tbd: get_..._band_limit methods only available after graph build? ***********/

    void set_ChromaResolution (double res);
    void set_ChromaTuning (double reference_pitch_hz);
    void set_ChromaRange (double min_freq_hz, double max_freq_hz);

    /** get number of chroma bands */
    int  get_chroma_band_num ();

    /** get limits of chroma bands,
	bands must point to space that can hold 2 * num values (lo, hi interleaved)
	@return		n+1 band limits in Hz for n bands     
    */
    void get_chroma_band_limits (/*out*/ float *bands);

    /** set number and limits of frequency bands for mel for PerceptualModel
	@param numbands	number of bands
	@param bands	points to 2 * numbands interleaved values 
	tbd: fixed n = 24? 
	tbd: redundant with set_NumBands Number of spectrum bands (mel) ???    
    */
    void set_mel_band_limits (int numbands, float *bands);

    /** get number of mel bands */
    int  get_mel_band_num ();

    /** get limits of frequency bands for mel for PerceptualModel,
	bands must point to space that can hold 2 * num values (lo, hi interleaved) 
    */
    void get_mel_band_limits (/*out*/ float *bands);

    /** set number and limits of frequency bands for mel for BandReducedModel
	@param bands	points to 2 * num interleaved values 
	tbd: fixed n = 4? 
	tbd: redundant with set_NumReducedBands ???    
    */
    void set_reduced_band_limits (int num, float *bands);

    /** get number of reduced bands */
    int  get_reduced_band_num ();

    /** get limits of frequency bands for BandReducedModel
	(SpectralCrest and SpectralFlatness descriptors)
	can be empty if descriptors not used
	@return		n+1 band limits in Hz for n bands     
    */
    void get_reduced_band_limits (/*out*/ float *bands);
    

    /**group: analysis graph setup *********/

    /** clear descriptors set with set_descriptor */
    void clear_descriptors ();

    /** set list of descriptor and variation to compute one by one
	@param descrid	descriptor index as returned by get_descriptor_name
	@param varid	variation index as returned by get_num_variations

	@return	success (false if descriptor unknown or variation invalid or invalid calling sequence)
    */
    bool set_descriptor (int descrid, int varid);

    /** build analysis
	uses all parameters and list of descriptors from set_descriptor
	adds all dependencies
	computes sizes of all result descriptor vectors or matrices

	@return	success	(false if invalid calling sequence)
    */
    bool build_descriptors ();


    /**group: query after building calculation graph ***************/

    /** get vector size of descriptor 
	(N.B. size is the same for all variations)

	If the descriptor has variable output size (is_descriptor_varsize() returned true), returns the max vector size.

	@param descrid
	@return number of dimensions = size of result vector or -1 on error
    */
    int get_dimensions (int descrid);

    
    /**group: Calculation Phase ***************/

    /**	frame callback function type called at frame start and end
	@param frame_time_sec	the time of the center of the signal window in seconds
	@param obj		callback object as passed to compute function
    */
    typedef void (*frame_callback_t)  (double frame_time_sec, void* obj);

    /** callback function type that will be called for each available result
	@param descrid		descriptor index (as used for set_descriptor())
	@param variation	variation index (as used for set_descriptor())
	@param frame_time_sec	the time of the center of the signal window in seconds
	@param numval		number of values actually present (can be less than number of dimensions queried with get_dimensions() if descriptor has variable output size (see is_descriptor_varsize()))
	@param values		pointer to numval descriptor values
	@param obj		callback object as passed to compute function
    */
    typedef void (*output_callback_t) (int descrid, int varnum, int numval, IDESC_REAL_TYPE *values, void* obj);

    /** compute instantaneous audio descriptors
	framestart callback is called at frame start, 
	then for each descriptor calculated, <p>callback</p> is called by the compute method for each descriptor/variation requested by set_descriptor
	then frameend callback is called at frame end.	

	@param nsamp		number of samples
	@param samples		pointer to one frame of audio signal, NULL for flushing at end of buffer
				(After flushing, you must call build_descriptors again before calling compute.)
	@param callback		pointer to callback function passing descriptor data callbacks or NULL
	@param framestart	pointer to callback function called before data callbacks, or NULL
	@param frameend		pointer to callback function called after data callbacks, or NULL
	@param obj		passed to callbacks
	@return			number of frames produced or -1 on error
    */
    int compute (int nsamp, float *samples, output_callback_t callback, frame_callback_t framestart, frame_callback_t frameend, void *obj);

    /** double input version of computation function (native sample format is IDESC_REAL_TYPE) 
     */
    int compute (int nsamp, double *samples, output_callback_t callback, frame_callback_t framestart, frame_callback_t frameend, void *obj);

    ~idesc();

private:
  struct Impl;
  Impl* m_impl;

  idesc(const idesc&);
  idesc& operator=(const idesc&);
}; // end class idesc
} // end namesspace idesc

#endif // IDESC_H_

