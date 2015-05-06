(in-package :cl-user)

(defpackage "libsndfile"
  (:nicknames "SF")
  (:use common-lisp cffi))

(in-package :sf)

(pushnew :libsndfile *features*)

;;; MUST BE INSTALLED !
;;; or linked statically (in LibAudioStream / MacOS)

#+linux
(progn
  (defparameter *libsndfile* nil)

  (defun init-libsndfile ()
    (define-foreign-library libsndfile
      (t (:default "libsndfile")))
    (setf *libsndfile*
	  (handler-case (progn (use-foreign-library libsndfile) t)
	    (error () (progn (print (format nil "could not load foreign-library libsndfile"))
			     nil)))))
  (oa:om-add-init-func 'init-libsndfile))

#+win32
(define-foreign-library libsndfile
  (:darwin "libsndfile.dylib")
  #+win32(:unix (:or "cygsndfile-1.dll" "libsndfile.so.1" "libsndfile.so"))
  #+win32(t (:default "libsndfile-1"))
  )

#+win32
(use-foreign-library libsndfile)


;(defctype :long-long :pointer)

(defconstant SF_FORMAT_WAV #x010000)
(defconstant SF_FORMAT_AIFF #x020000)
(defconstant SF_FORMAT_AU #x030000)
(defconstant SF_FORMAT_RAW #x040000)
(defconstant SF_FORMAT_PAF #x050000)
(defconstant SF_FORMAT_SVX #x060000)
(defconstant SF_FORMAT_NIST #x070000)
(defconstant SF_FORMAT_VOC #x080000)
(defconstant SF_FORMAT_IRCAM #x0A0000)
(defconstant SF_FORMAT_W64 #x0B0000)
(defconstant SF_FORMAT_MAT4 #x0C0000)
(defconstant SF_FORMAT_MAT5 #x0D0000)
(defconstant SF_FORMAT_PVF #x0E0000)
(defconstant SF_FORMAT_XI #x0F0000)
(defconstant SF_FORMAT_HTK #x100000)
(defconstant SF_FORMAT_SDS #x110000)
(defconstant SF_FORMAT_AVR #x120000)
(defconstant SF_FORMAT_WAVEX #x130000)
(defconstant SF_FORMAT_SD2 #x160000)
(defconstant SF_FORMAT_FLAC #x170000) 
(defconstant SF_FORMAT_CAF #x180000)
(defconstant SF_FORMAT_OGG #x200000)
(defconstant SF_FORMAT_PCM_S8 #x0001)
(defconstant SF_FORMAT_PCM_16 #x0002)
(defconstant SF_FORMAT_PCM_24 #x0003)
(defconstant SF_FORMAT_PCM_32 #x0004)
(defconstant SF_FORMAT_PCM_U8 #x0005)
(defconstant SF_FORMAT_FLOAT #x0006)
(defconstant SF_FORMAT_DOUBLE #x0007)
(defconstant SF_FORMAT_ULAW #x0010)
(defconstant SF_FORMAT_ALAW #x0011)
(defconstant SF_FORMAT_IMA_ADPCM #x0012)
(defconstant SF_FORMAT_MS_ADPCM #x0013)
(defconstant SF_FORMAT_GSM610 #x0020)
(defconstant SF_FORMAT_VOX_ADPCM #x0021)
(defconstant SF_FORMAT_G721_32 #x0030)
(defconstant SF_FORMAT_G723_24 #x0031)
(defconstant SF_FORMAT_G723_40 #x0032)
(defconstant SF_FORMAT_DWVW_12 #x0040)
(defconstant SF_FORMAT_DWVW_16 #x0041)
(defconstant SF_FORMAT_DWVW_24 #x0042)
(defconstant SF_FORMAT_DWVW_N #x0043)
(defconstant SF_FORMAT_DPCM_8 #x0050)
(defconstant SF_FORMAT_DPCM_16 #x0051)
(defconstant SF_FORMAT_VORBIS #x0060)
(defconstant SF_ENDIAN_FILE #x00000000)
(defconstant SF_ENDIAN_LITTLE #x10000000)
(defconstant SF_ENDIAN_BIG #x20000000)
(defconstant SF_ENDIAN_CPU #x30000000)
(defconstant SF_FORMAT_SUBMASK #x0000FFFF)
(defconstant SF_FORMAT_TYPEMASK #x0FFF0000)
(defconstant SF_FORMAT_ENDMASK #x30000000)

(defconstant SFC_GET_LIB_VERSION #x1000)
(defconstant SFC_GET_LOG_INFO #x1001)
(defconstant SFC_GET_NORM_DOUBLE #x1010)
(defconstant SFC_GET_NORM_FLOAT #x1011)
(defconstant SFC_SET_NORM_DOUBLE #x1012)
(defconstant SFC_SET_NORM_FLOAT #x1013)
(defconstant SFC_SET_SCALE_FLOAT_INT_READ #x1014)
(defconstant SFC_GET_SIMPLE_FORMAT_COUNT #x1020)
(defconstant SFC_GET_SIMPLE_FORMAT #x1021)
(defconstant SFC_GET_FORMAT_INFO #x1028)
(defconstant SFC_GET_FORMAT_MAJOR_COUNT #x1030)
(defconstant SFC_GET_FORMAT_MAJOR #x1031)
(defconstant SFC_GET_FORMAT_SUBTYPE_COUNT #x1032)
(defconstant SFC_GET_FORMAT_SUBTYPE #x1033)
(defconstant SFC_CALC_SIGNAL_MAX #x1040)
(defconstant SFC_CALC_NORM_SIGNAL_MAX #x1041)
(defconstant SFC_CALC_MAX_ALL_CHANNELS #x1042)
(defconstant SFC_CALC_NORM_MAX_ALL_CHANNELS #x1043)
(defconstant SFC_GET_SIGNAL_MAX #x1044)
(defconstant SFC_GET_MAX_ALL_CHANNELS #x1045)
(defconstant SFC_SET_ADD_PEAK_CHUNK #x1050)
(defconstant SFC_UPDATE_HEADER_NOW #x1060)
(defconstant SFC_SET_UPDATE_HEADER_AUTO #x1061)
(defconstant SFC_FILE_TRUNCATE #x1080)
(defconstant SFC_SET_RAW_START_OFFSET #x1090)
(defconstant SFC_SET_DITHER_ON_WRITE #x10A0)
(defconstant SFC_SET_DITHER_ON_READ #x10A1)
(defconstant SFC_GET_DITHER_INFO_COUNT #x10A2)
(defconstant SFC_GET_DITHER_INFO #x10A3)
(defconstant SFC_GET_EMBED_FILE_INFO #x10B0)
(defconstant SFC_SET_CLIPPING #x10C0)
(defconstant SFC_GET_CLIPPING #x10C1)
(defconstant SFC_GET_INSTRUMENT #x10D0)
(defconstant SFC_SET_INSTRUMENT #x10D1)
(defconstant SFC_GET_LOOP_INFO #x10E0)
(defconstant SFC_GET_BROADCAST_INFO #x10F0)
(defconstant SFC_SET_BROADCAST_INFO #x10F1)
(defconstant SFC_TEST_IEEE_FLOAT_REPLACE #x6001)
(defconstant SFC_SET_ADD_DITHER_ON_WRITE #x1070)
(defconstant SFC_SET_ADD_DITHER_ON_READ #x1071)

(defconstant  SF_STR_TITLE #x01)
(defconstant  SF_STR_COPYRIGHT #x02)
(defconstant  SF_STR_SOFTWARE #x03)
(defconstant  SF_STR_ARTIST #x04)
(defconstant  SF_STR_COMMENT #x05)
(defconstant  SF_STR_DATE #x06)

(defconstant  SF_FALSE 0)
(defconstant  SF_TRUE 1)
(defconstant  SFM_READ #x10)
(defconstant  SFM_WRITE #x20)
(defconstant  SFM_RDWR #x30)

(defconstant  SF_ERR_NO_ERROR 0)
(defconstant  SF_ERR_UNRECOGNISED_FORMAT 1)
(defconstant  SF_ERR_SYSTEM 2)
(defconstant  SF_ERR_MALFORMED_FILE 3)
(defconstant   SF_ERR_UNSUPPORTED_ENCODING 4)

(defconstant  SF_COUNT_MAX #x7FFFFFFFFFFFFFFF)

(defcstruct SF_INFO
	(frames :long-long)
	(samplerate :int)
	(channels :int)
	(format :int)
	(sections :int)
	(seekable :int))

(defcstruct SF_FORMAT_INFO
	(format :int)
	(name :string)
	(extension :string))


(defconstant SFD_DEFAULT_LEVEL 0)
(defconstant SFD_CUSTOM_LEVEL #x40000000)
(defconstant SFD_NO_DITHER 500)
(defconstant SFD_WHITE 501)
(defconstant SFD_TRIANGULAR_PDF 502)

(defcstruct SF_DITHER_INFO
	(type :int)
	(level :double)
	(name :string))

(defcstruct SF_EMBED_FILE_INFO
	(offset :double)
	(length :double))

(defconstant SF_LOOP_NONE 800)
(defconstant SF_LOOP_FORWARD 0)
(defconstant SF_LOOP_BACKWARD 0)
(defconstant SF_LOOP_ALTERNATING 0)

(defcstruct SF_INSTRUMENT
	(gain :int)
	(basenote :char)
	(detune :char)
	(velocity_lo :char)
	(velocity_hi :char)
	(key_lo :char)
	(key_hi :char)
	(loop_count :int)
	(loops :pointer))

(defcstruct SF_INSTRUMENT_loops
	(mode :int)
	(start :unsigned-int)
	(end :unsigned-int)
	(count :unsigned-int))

(defcstruct SF_LOOP_INFO
	(time_sig_num :short)
	(time_sig_den :short)
	(loop_mode :int)
	(num_beats :int)
	(bpm :float)
	(root_key :int)
	(future :pointer))

(defcstruct SF_BROADCAST_INFO
	(description :pointer)
	(originator :pointer)
	(originator_reference :pointer)
	(origination_date :pointer)
	(origination_time :pointer)
	(time_reference_low :int)
	(time_reference_high :int)
	(version :short)
	(umid :pointer)
	(reserved :pointer)
	(coding_history_size :unsigned-int)
	(coding_history :pointer))

(defcstruct SF_VIRTUAL_IO
	(get_filelen :pointer)
	(seek :pointer)
	(read :pointer)
	(write :pointer)
	(tell :pointer))

(defcfun ("sf_open" sf_open) :pointer
  (path :string)
  (mode :int)
  (sfinfo :pointer))

(defcfun ("sf_open_fd" sf_open_fd) :pointer
  (fd :int)
  (mode :int)
  (sfinfo :pointer)
  (close_desc :int))

(defcfun ("sf_open_virtual" sf_open_virtual) :pointer
  (sfvirtual :pointer)
  (mode :int)
  (sfinfo :pointer)
  (user_data :pointer))

(defcfun ("sf_close" sf_close) :int
  (sndfile :pointer))

(defcfun ("sf_error" sf_error) :int
  (sndfile :pointer))

(defcfun ("sf_strerror" sf_strerror) :string
  (sndfile :pointer))

(defcfun ("sf_error_number" sf_error_number) :string
  (errnum :int))

(defcfun ("sf_perror" sf_perror) :int
  (sndfile :pointer))

(defcfun ("sf_error_str" sf_error_str) :int
  (sndfile :pointer)
  (str :string)
  (len :pointer))

(defcfun ("sf_command" sf_command) :int
  (sndfile :pointer)
  (command :int)
  (data :pointer)
  (datasize :int))

(defcfun ("sf_format_check" sf_format_check) :int
  (info :pointer))

(defcfun ("sf_seek" sf_seek) :double
  (sndfile :pointer)
  (frames :long-long)
  (whence :int))

(defcfun ("sf_set_string" sf_set_string) :int
  (sndfile :pointer)
  (str_type :int)
  (str :string))

(defcfun ("sf_get_string" sf_get_string) :string
  (sndfile :pointer)
  (str_type :int))

(defcfun ("sf_write_sync" sf_write_sync) :void
  (sndfile :pointer))

;;;============
(defcfun (sf-readf-float "sf_readf_float") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-read-float "sf_read_float") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-writef-float "sf_writef_float") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-write-float "sf_write_float") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-readf-int "sf_readf_int") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-read-int "sf_read_int") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-writef-int "sf_writef_int") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-write-int "sf_write_int") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-readf-short "sf_readf_short") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-read-short "sf_read_short") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-writef-short "sf_writef_short") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-write-short "sf_write_short") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))


;;; test double ?

(defcfun (sf-readf-double "sf_readf_double") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-read-double "sf_read_double") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-writef-double "sf_writef_double") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))

(defcfun (sf-write-double "sf_write_double") :long-long
  (sndfile :pointer)
  (ptr :pointer)
  (frames :long-long))





;#-linux
;(fli:define-foreign-function (sf-readf-float "sf_readf_float")
;    ((sndfile :pointer)
;     (ptr :pointer)
;     (frames :long-long))
;  :result-type :long-long)

