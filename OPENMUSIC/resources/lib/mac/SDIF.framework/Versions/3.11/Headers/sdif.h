/* $Id: sdif.h.in,v 1.3 2012/01/02 23:49:08 roebel Exp $
 *
 * IRCAM SDIF Library (http://www.ircam.fr/sdif)
 *
 * Copyright (C) 1998-2002 by IRCAM-Centre Georges Pompidou, Paris, France.
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * See file COPYING for further informations on licensing terms.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * For any information regarding this and other IRCAM software, please
 * send email to:
 *                            sdif@ircam.fr
 *
 *
 * This file contains types and declarations that form the 
 * externally visible API of the IRCAM SDIF library (http://www.ircam.fr/sdif).
 *
 *
 * $Log: sdif.h.in,v $
 * Revision 1.3  2012/01/02 23:49:08  roebel
 * Base selection of WIN32 specific implementation on definition of macros  WIN32 OR _WIN32. The latter being standard in
 * Visual C++ it is most important to have it.
 *
 * Revision 1.2  2011/06/11 23:23:10  roebel
 * Fixed a number of issues with the conditional include of stdint.h and sys/types.h in instaled include file
 * sdif.h.
 * Correctly use the types provided by these include files for definition of sdif types.
 *
 * Revision 1.1  2011/04/18 18:46:27  roebel
 * added sdif.h.in to cvs
 *
 * Revision 1.71  2011/04/12 20:17:33  roebel
 * Fixed large file support to properly work on linux as well.
 *
 * Revision 1.70  2011/04/12 14:18:18  roebel
 * Fixed Sdif[fF]GetPos and Sdif[fF]SetPos to correctly support large files (>2GB).
 *
 * Revision 1.69  2011/04/06 17:08:45  diemo
 * realloc query tree
 *
 * Revision 1.68  2009/10/19 17:01:28  diemo
 * added prototype for SdifFreeQueryTree
 *
 * Revision 1.67  2009/04/20 14:18:37  diemo
 * example for SdifSelectAddSignature
 * move SDIF_API out of generating macro, to avoid it in SdifSelect.c (error on windows compilation)
 *
 * Revision 1.66  2009/04/20 14:12:28  diemo
 * document two missing prototypes
 *
 * Revision 1.65  2009/04/17 16:51:10  diemo
 * clarified syntax in code-generating macro
 *
 * Revision 1.64  2009/01/07 16:31:55  diemo
 * access functions for integration in script languages for all fields of:
 * - name-value table list global struct SdifNameValuesLT (-> list of tables),
 * - name-value table SdifNameValueTableT (-> hash of entries),
 * - name-value entry SdifNameValueT
 * iterator to iterate over all entries in hash table
 *
 * Revision 1.63  2008/12/18 11:42:19  diemo
 * Improvements of the public API for scripting languages binding to libSDIF:
 * - added SdifHashTableGetNbData
 * - added access methods to struct elements of type definitions.
 *
 * Revision 1.62  2008/05/22 11:57:32  roebel
 * Added missing SDIF_API to function declaration in sdif.h.
 *
 * Revision 1.61  2008/03/07 19:18:30  roebel
 * Support compilation as static library on windows
 *
 * Revision 1.60  2008/01/22 00:54:53  roebel
 * Use stdint.h defined types if this header is available.
 *
 * Revision 1.59  2008/01/11 15:51:26  roebel
 * Use const char* for read only function arguments.
 *
 * Revision 1.58  2007/12/10 10:45:33  roebel
 * Use const char* for read only function arguments.
 *
 * Revision 1.57  2007/11/26 18:55:52  roebel
 * Finished DLL export/import handling, support gcc visibility as well.
 *
 * Revision 1.56  2006/08/03 13:51:17  borghesi
 * exported new symbols for windows compilation
 *
 * Revision 1.55  2006/06/21 15:40:00  schwarz
 * LAST commit on this repository before moving to sourceforge
 * (documentation stuff)
 *
 * Revision 1.54  2006/05/03 15:06:23  schwarz
 * add SDIF_API (for windows dll)
 * untabify
 *
 * Revision 1.53  2006/05/03 14:33:29  schwarz
 * correct doc and remove function sketch (is now implemented)
 *
 * Revision 1.52  2005/12/05 16:39:40  schwarz
 * export SdifSelectGetIntMask
 *
 * Revision 1.51  2005/05/24 09:32:39  roebel
 * Fixed last checkin comment which turned out to be the start of
 * a c-comment.
 * Synchronized the extended ErrorTagET with the new
 * table of error messages.
 *
 * Revision 1.50  2005/05/23 19:17:52  schwarz
 * - Sdiffread* / Sdiffwrite* functions with SdifFileT instead of FILE *

 *   -> eof error reporting makes more sense
 * - more cleanup of sdif.h, above functions are private in SdifRWLowLevel.h
 * - eEof becomes error 4 to be distinguishable from ascii chars
 * - SdifFScanNameValueLCurrNVT reimplemented for ascii only
 *
 * Revision 1.49  2005/05/23 17:52:52  schwarz
 * Unified error handling:
 * - SdifErrorEnum (global errors) integrated into SdifErrorTagET (file errors)
 * - no more SdifError.[ch], everything done by SdifErrMess.[ch]
 *
 * Revision 1.48  2005/05/20 21:13:54  roebel
 * corrected detection seekablility of sdif file.
 * files are not seekable only if they are pipes!
 *
 * Revision 1.47  2005/05/13 15:35:01  schwarz
 * make it possible that global errors from SdifError be passed through
 * the SdifErrMsg functions as file errors
 *
 * Revision 1.46  2005/04/19 15:30:13  schwarz
 * make sdifcpp compile again for easdif:
 * - removed deleted files from makefiles
 * - fixed some includes that were missing (but only for C++ compilation!?)
 *
 * Revision 1.45  2005/04/07 15:58:52  schwarz
 * removed unused SdifMr stuff
 *
 * Revision 1.44  2004/09/14 15:45:47  schwarz
 * SdifMinMaxT with double
 *
 * Revision 1.43  2004/09/13 13:06:27  schwarz
 * SdifReadSimple even simpler, SdifReadFile for full-scale callback reading.
 * Moving the functionality of querysdif into the library with SdifQuery,
 * result in SdifQueryTreeT.
 *
 * Revision 1.42  2004/09/09 18:02:00  schwarz
 * - Changed SdifMatrixDataT to something sensible that allows to read
 *   and store a whole matrix's data as one block into field CurrMtrxData
 *   of SdifFileT with SdifFReadMatrixData and accessed with the
 *   functions SdifFCurrMatrixData, SdifFCurrMatrixDataPointer, with
 *   automatic reallocation.
 * - SdifReadSimple: simple callback-based reading of an entire SDIF file.
 * - SdifListConcat function
 * - SdifIsAReservedChar return value changed to boolean flag, much clearer.
 * - SdifSelectAppendList function
 * - Removed unimplemented prototypes drafted in sdif/SdifHighLevel.h
 *
 * Revision 1.41  2004/07/22 14:47:55  bogaards
 * removed many global variables, moved some into the thread-safe SdifGlobals structure, added HAVE_PTHREAD define, reorganized the code for selection, made some arguments const, new version 3.8.6
 *
 * Revision 1.40  2004/06/14 15:56:29  schwarz
 * Padding mask, other constants?
 * More doc for sdif_foralltypes macros.
 *
 * Revision 1.39  2004/06/03 11:39:57  schwarz
 * added array swapping and binary signature reading functions.
 *
 * Revision 1.38  2004/01/09 11:29:24  schwarz
 * Removed declaration of SdifFGetFrameType and SdifFGetMatrixType from
 * public API because they are misleading.
 * Use the correct SdifTestFrameType and SdifTestMatrixType instead.
 *
 * Revision 1.37  2003/12/15 13:13:44  schwarz
 * Added SdifFileT based functions SdifFSetPos, SdifFGetPos around the
 * Sdiff* Macros, to be callable from OpenMusic.
 *
 * Revision 1.36  2003/11/18 18:14:01  roebel
 * Added alias for typo in SdifErrorTagE.
 *
 * Revision 1.35  2003/11/07 12:09:07  ellis
 * Added the declaration of of two functions in the header file
 * SdifFAllFrameTypeToSdifString and SdifFAllMatrixTypeToSdifString
 *
 * Revision 1.34  2003/10/14 10:10:18  schwarz
 * SdifMatrixTypeGetColumnName returns pointer to name of column at index.
 *
 * Revision 1.33  2003/08/06 15:08:11  schwarz
 * SdifSelectIntMask added for all integer selections, new functions:
 * - SdifSelectTestIntMask
 * - SdifFNumStreamsSelected, SdifFNumRowsSelected, SdifFNumColumnsSelected
 * - SdifFRowIsSelected, SdifFColumnIsSelected
 * int value/range had to be changed to SdifUInt4 for this
 *
 * SdifCalloc now does what it appears to do: clear memory
 * Finally removed obsolete functions (like SdifSkip...).
 *
 * Revision 1.32  2003/07/21 15:46:47  roebel
 * Removed C++ comment.
 *
 * Revision 1.31  2003/07/07 10:27:01  roebel
 * Added support for eInt1 and eUInt1 data types
 *
 * Revision 1.30  2003/06/06 10:25:40  schwarz
 * Added eReadWriteFile that eventually opens a file in read-write mode.
 *
 * Revision 1.29  2003/05/30 17:42:04  schwarz
 * Added SdifFGetMatrixType and SdifFGetFrameType.
 *
 * Revision 1.28  2003/05/27 16:08:49  schwarz
 * Added SdifFGetMatrixTypesTable and SdifFGetFrameTypesTable.
 * Documented other type access functions.
 *
 * Revision 1.27  2003/05/01 18:48:41  roebel
 * SdifStringToSignature takes now const char * as argument.
 * Added missing declaration for SdifSkipASCIIUntilfromSdifString.
 *
 * Revision 1.26  2003/04/18 17:42:49  schwarz
 * Removed last warning from swig.
 *
 * Revision 1.25  2003/04/18 16:03:09  schwarz
 * Made parseable by swig (no generating macros).
 * Removed double definitions, reordered others.
 *
 * Revision 1.24  2002/11/28 19:56:21  roebel
 * Fixed some const arguments.
 * Make SdifFtruncte return SDIF_FTRUNCATE_NOT_AVAILABLE if
 * this is the case.
 *
 * Revision 1.23  2002/11/27 17:53:24  roebel
 * Improved documentation.
 *
 * Revision 1.22  2002/09/20 14:34:41  schwarz
 * New functions:
 * - SdifParseSignatureList Parse comma-separated list of signatures
 * - SdifKillSelectElement  now public
 *
 * Revision 1.21  2002/09/17 09:51:18  schwarz
 * Added copyright.
 *
 * Revision 1.20  2002/08/28 14:05:31  schwarz
 * New function SdifFRewind.
 * More documentation for positioning functions and truncate.
 *
 * Revision 1.19  2002/08/27 10:51:30  schwarz
 * New file truncate function.
 * Comments for file positioning macros.
 * String append from const char *
 *
 * Revision 1.18  2002/08/13 10:52:56  schwarz
 * Add SdifFReadNextSelectedFrameHeader (from SdifHighLevel.h).
 *
 * Revision 1.17  2002/08/05 14:22:42  roebel
 * Added support to replace a selection.
 *
 * Revision 1.16  2002/06/18 13:56:23  ftissera
 * Move SdifFGetAllTypefromSdifString declaration from SdifFGet.h to sdif.h
 *
 * Revision 1.15  2002/05/24 19:40:43  ftissera
 * Change code to be compatible with C++
 * Add two handlers for error and warning.
 * Create two default handlers and set functions
 *
 * Revision 1.14  2001/07/19 14:24:33  lefevre
 * Macintosh Compilation
 *
 * Revision 1.13  2001/07/12  14:11:48  roebel
 * Added include file holding library version defines to the distribution.
 *
 * Revision 1.12  2001/05/04 18:09:53  schwarz
 * Added function SdifNameValuesLPutCurrNVTTranslate.
 *
 * Revision 1.11  2001/05/04 14:07:28  tisseran
 * Liitle fix:
 * - Change a c++ commentary in a c one (sdif.h line:1532)
 * - Change publihs rules of main Makefile, now name of asrc archive on www.ircam.fr/sdif/download
 * is SDIF-(version)-src.tar.gz
 *
 * Revision 1.10  2001/04/26 14:47:02  tisseran
 * Correct a stupid error in previous log (arrggghh DON'T USE C COMMENTARY in cvs log).
 * Correct Makefile.am, change cvstag rules:
 * cvstags:
 *     cvs -F tags $(CVSTAGS)
 * Add some explication in file ChangeLog
 *
 * Revision 1.9  2001/04/25 11:29:10  tisseran
 * Change sdif_foralltype macro to compile on MacOS X.
 * Old version: sdif_foralltypes sdif__foralltypes(macro,)
 * New version; sdif_foralltypes sdif__foralltypes(macro, empty_definition)
 * with
 * #define empty_definition // empty definition
 *
 * Change AUTHORS to reflect people working on SDIF library
 *
 * Revision 1.8  2000/12/07 13:01:39  roebel
 * Fixed wrong enum datatype declarations for backward compatibility
 *
 * Revision 1.7  2000/11/21 16:34:48  roebel
 * New SdifSignatureConst builds integer signature according to
 * endianess of machine. Multicharacter constants are no longer
 * supported by the library. Cleaned up sdif.h/SdifGlobals.h a bit.
 * Test for Multicharacter conversion is removed from configure.in.
 *
 * Revision 1.6  2000/11/21 14:51:34  schwarz
 * - sdif.h is now included by all sdif/Sdif*.c files.
 * - Removed all public typedefs, enums, structs, and defines from the
 *   individual sdif/Sdif*.h files, because they were duplicated in sdif.h.
 * - Todo: Do the same for the function prototypes, decide which types and
 *   prototypes really need to be exported.
 * - Preliminary new version of SdiffGetPos, SdiffSetPos.  They used the
 *   type fpos_t, which is no longer a long on RedHat 7 Linux.
 *
 * Revision 1.5  2000/11/16 12:20:17  lefevre
 * no message
 *
 * Revision 1.4  2000/11/16  12:02:22  lefevre
 * no message
 *
 * Revision 1.3  2000/11/15  14:53:22  lefevre
 * no message
 *
 * Revision 1.2  2000/10/27  20:03:18  roebel
 * autoconf merged back to main trunk
 *
 * Revision 1.1.2.2  2000/08/21  21:34:52  tisseran
 * *** empty log message ***
 *
 * Revision 1.1.2.1  2000/08/21  17:08:40  tisseran
 * *** empty log message ***
 *
 * Revision 1.1.2.1  2000/08/21  13:07:41  tisseran
 * *** empty log message ***
 *
 * $Date: 2012/01/02 23:49:08 $
 *
 */


#ifndef _SDIF_H
#define _SDIF_H 1

#include "sdif_version.h"

#ifdef __cplusplus
extern "C" {
#endif


static const char _sdif_h_cvs_revision_ [] = "$Id: sdif.h.in,v 1.3 2012/01/02 23:49:08 roebel Exp $";

  /* the following two #if are configured by autoconf/cmake
   * and reflect capabilities of the installing compiler.
   * You cannot use an installed sdif with a comiler that des not provide  
   * stdint.h and/or sys/types.h when the configurin compiler provided those
   */
  /* will be 0 if HAVE_STDINT_H is not set or set to 0, will be 10 if HAVE_STDINT_H is set */
#if 10
#  define HAVE_STDINT_HEADER 1
#  include <stdint.h>
#endif

  /* will be 0 if HAVE_SYS_TYPES_H  is not set or set to 0, will be 10 if HAVE_SYS_TYPES_H is set */
#if 10
#  include <sys/types.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <float.h>

#if defined (SDIF_IS_STATIC) || defined(EASDIF_IS_STATIC)
#  define SDIF_API
#else 
# ifdef WIN32
#   ifdef DO_EXPORT_SDIF
#     define SDIF_API __declspec(dllexport)
#   else
#     define SDIF_API __declspec(dllimport)
#   endif
#  else
#    if defined(__GNUC__) && defined( GCC_HAS_VISIBILITY)
#      define SDIF_API __attribute__ ((visibility("default")))
#    else
#      define SDIF_API
#    endif
#  endif
#endif


/* forward declaration of big SDIF file struct type */
typedef struct SdifFileS SdifFileT;


/* SdifHash.h */
typedef enum SdifHashIndexTypeE
{
  eHashChar,
  eHashInt4
} SdifHashIndexTypeET;


typedef union SdifHashIndexU SdifHashIndexUT;

union SdifHashIndexU
{
  char* Char[1]; /* tab of one pointer to fixe union size at 4 or 8 bytes */
  unsigned int  Int4;
} ;

typedef struct SdifHashNS SdifHashNT;

/* hash bin struct, containing linked list of entries */
struct SdifHashNS 
{
  SdifHashNT *Next;	/* pointer to next entry */
  SdifHashIndexUT Index;
  void* Data;
};


typedef struct SdifHashTableS SdifHashTableT;

struct SdifHashTableS
{
  SdifHashNT* *Table;		/* table of pointers to hash bins */
  unsigned int HashSize;	/* number of hash bins */
  SdifHashIndexTypeET IndexType;
  void (*Killer)(void *); 
  unsigned int NbOfData;	/* total number of entries */
} ;


typedef struct SdifHashTableIteratorS SdifHashTableIteratorT;

struct SdifHashTableIteratorS
{
  SdifHashTableT *HTable;	/* pointer to hash table		 */
  unsigned int    BinIndex;	/* index of current hash bin		 */
  SdifHashNT     *Entry;	/* pointer to current hash entry in list */
};



/*
//FUNCTION GROUP: File positioning
*/

/* SdifFile.c */

/*DOC:
  Rewind to start of file (before header!) 
  [return] 1 on success, 0 on error
*/
SDIF_API int SdifFRewind(SdifFileT *file);


#define SDIFFTRUNCATE_NOT_AVAILABLE -2
/*DOC:
  Truncate file at current position
  This function is only available on certain systems that have the
  ftruncate function in their system libraries. 
  [return] 1 on success, 0 for error (check errno),  
        MACRO SDIFFTRUNCATE_NOT_AVAILABLE if function is not available.
*/
SDIF_API int SdifFTruncate(SdifFileT *file);



/* Give documentation and fake prototype for positioning macros.
   Cocoon ignores the #if 0.
*/
#if 0

/*DOC:
  Get position in file.
  [return] file offset or -1 for error.
 */
int SdiffGetPos(SdifFileT *file, SdiffPosT *pos);

/*DOC:
  Set absolute position in file.
  SdiffPosT is a plattform dependent integer type that should correctly work with long files. 
  [Return] 0 on success, 
          -1 on error (errno is set, see fseek(3) for details)

  On Mac or Windows, seeking on a stream is always considered
  successful (return 0), even if no seek was done!
 */
int SdiffSetPos(SdifFileT *file, SdiffPosT *pos);

#endif  /* if 0 */

/* do not use off_t here, because the size of off_t may depend on compiler options
 * so that the interface becomes unstable. 
 * This should be a 64bit int type such that for all configurations and compiler options
 * a long file position can be stored here.
 */
typedef int64_t SdiffPosT;

/* support file positions with files larger than 2GB */
#if defined(_WIN32) || defined(_WIN32)
    /* on  windows, seeking on a stream is always considered
       successful (return 0)! */
/* these 2 are used only localy here to test whether the stream supports seeking */ 
#  define SdiffIsFile(f)       ((f)!=stdin && (f)!=stdout && (f)!=stderr)
#  define Sdiffftell(f)        (SdiffIsFile(f)  ?  _ftelli64(f)  :  0)
#  define SdiffGetPos(f,p)     ((*(p) = Sdiffftell(f)) == -1  ?  -1  :  0)
#  define SdiffSetPos(f,p)     (SdiffIsFile(f)  \
                                ?  _fseeki64(f, (*(p)), SEEK_SET)  :  0)
#else
#  define SdiffGetPos(f,p)     ((*(p) = ftello(f)) == -1  ?  -1  :  0)
#  define SdiffSetPos(f,p)     (fseeko(f, (*(p)), SEEK_SET))

/*
#   define SdiffGetPos(f,p)     ((*(p) = ftell(f)) == -1  ?  -1  :  0)
#   define SdiffSetPos(f,p)     (fseek(f, (long)(*(p)), SEEK_SET))
*/


/*DS: FORCE long fpos*/
/* ftell/fseek can be applied to stdin/out/err at least in a restricted manner
 * (same as fgetpos/fsetpos) so let's try */
/* don't use ftell any more because it does not support files larger 2GB
#   define SdiffPosT            long
#   define SdiffGetPos(f,p)     ((*(p) = ftell(f)) == -1  ?  -1  :  0)
#   define SdiffSetPos(f,p)     (fseek(f, (long)(*(p)), SEEK_SET))
*/
#endif



/*DOC:
  Get position in file.
  [return] file offset or -1 for error.
  SdiffPosT is a plattform dependent integer type that should correctly work with long files. 
 */
SDIF_API int SdifFGetPos(SdifFileT *file, SdiffPosT *pos);

/*DOC:
  Set absolute position in file.
  SdiffPosT is a plattform dependent integer type that should correctly work with long files. 
  [Return] 0 on success, 
          -1 on error (errno is set, see fseeko/_fseeki64(3) for details)

  On Mac or Windows, seeking on a stream is always considered
  successful (return 0), even if no seek was done!
 */
SDIF_API int SdifFSetPos(SdifFileT *file, SdiffPosT *pos);




/* SdifHard_OS.h */
#ifdef HAVE_STDINT_HEADER
typedef char           SdifChar;
typedef char           SdifInt1;
typedef int16_t        SdifInt2;
typedef int32_t        SdifInt4;
typedef unsigned char  SdifUInt1;
typedef uint16_t       SdifUInt2;
typedef uint32_t       SdifUInt4;
#else
typedef char           SdifChar;
typedef char           SdifInt1;
typedef short          SdifInt2;
typedef unsigned char  SdifUInt1;
typedef unsigned short SdifUInt2;
typedef int            SdifInt4;
typedef unsigned int   SdifUInt4;
#endif

typedef SdifUInt4      SdifSignature;
typedef float          SdifFloat4;
typedef double         SdifFloat8;

typedef enum SdifMachineE
{
  eUndefinedMachine,
  eBigEndian,
  eLittleEndian,
  eBigEndian64,
  eLittleEndian64,
  ePDPEndian
} SdifMachineET;

typedef enum SdifBinaryMode
{
  eBinaryModeUnknown,
  eBinaryModeWrite,
  eBinaryModeRead,
  eBinaryModeReadWrite,
  eBinaryModeStdInput,
  eBinaryModeStdOutput,
  eBinaryModeStdError
} SdifBinaryModeET ;

/* SdifGlobals.h */
/* DOC:

  Macro to generate an integer representation of the sequence of unsigned chars 
  for example :

  SdifSignature sig=SdifSignatureConst('A','B','C','D');

  Because integers are differently handled on little/big endian machines the
  signatures are swapped if read from a file to match internal format. */
  
#   define SdifSignatureConst(p1,p2,p3,p4) (((((unsigned int)(p1))&0xff)<<24)|((((unsigned int)(p2))&0xff)<<16)|((((unsigned int)(p3))&0xff)<<8)|(((unsigned int)(p4))&0xff))


#ifndef SWIG
typedef enum SdifSignatureE
{
  eSDIF = SdifSignatureConst('S','D','I','F'), /* SDIF header */
  e1NVT = SdifSignatureConst('1','N','V','T'), /* Name Value Table */
  e1TYP = SdifSignatureConst('1','T','Y','P'), /* TYPe declarations */
  e1MTD = SdifSignatureConst('1','M','T','D'), /* Matrix Type Declaration */
  e1FTD = SdifSignatureConst('1','F','T','D'), /* Frame Type Declaration */
  e1IDS = SdifSignatureConst('1','I','D','S'), /* ID Stream Table */
  eSDFC = SdifSignatureConst('S','D','F','C'), /* Start Data Frame Chunk (text files) */
  eENDC = SdifSignatureConst('E','N','D','C'), /* END Chunk (text files) */
  eENDF = SdifSignatureConst('E','N','D','F'), /* END File (text files) */
  eFORM = SdifSignatureConst('F','O','R','M'), /* FORM for IFF compatibility (obsolete ?) */
  eEmptySignature = SdifSignatureConst('\0','\0','\0','\0')
} SdifSignatureET;
#endif

typedef enum SdifModifModeE
{
  eNoModif,
  eCanModif
} SdifModifModeET;


/* DataTypeEnum

   On Matt Wright's visit at IRCAM June 1999, we defined a new
   encoding for the MatrixDataType field with the feature that the low
   order byte encodes the number of bytes taken by each matrix
   element.  

   Low order byte encodes the number of bytes 
   High order bytes come from this (extensible) enum:

        0 : Float
        1 : Signed integer
        2 : Unsigned integer
        3 : Text (UTF-8 when 1 byte)
        4 : arbitrary/void
*/
typedef enum SdifDataTypeE
{
  eText     = 0x0301,
  eChar     = 0x0301,
  eFloat4   = 0x0004,
  eFloat8   = 0x0008,
  eInt1     = 0x0101,
  eInt2     = 0x0102,
  eInt4     = 0x0104,
  eInt8     = 0x0108,
  eUInt1    = 0x0201,
  eUInt2    = 0x0202,
  eUInt4    = 0x0204,
  eUInt8    = 0x0208,
            
  eFloat4a  = 0x0001,   /* =  1 */    /* Backwards compatibility with old */
  eFloat4b  = 0x0020,   /* = 32 */    /* IRCAM versions < 3 of SDIF */
  eFloat8a  = 0x0002,   /* =  2 */    /* IN TEXT MODE ONLY! */
  eFloat8b  = 0x0040    /* = 64 */
} SdifDataTypeET;

/* SdifList.h */
typedef void (*KillerFT) (void *);

typedef struct SdifListNS SdifListNT;

struct SdifListNS 
{
  SdifListNT *Next;
  void* Data;
};


typedef struct SdifListNStockS SdifListNStockT;

struct SdifListNStockS
{
    SdifListNT*  StockList; /* list of arrays of nodes, the first node is used to chain arrays */
    unsigned int SizeOfOneStock; /* must be > 1 */
    unsigned int NbStock;

    unsigned int NbNodesUsedInCurrStock;

    SdifListNT* Trash; /* to recycle nodes */
};


/* lists management */

typedef struct SdifListS SdifListT;
typedef SdifListT       *SdifListP;

struct SdifListS
{
  /* fifo list */
  SdifListNT *Head;
  SdifListNT *Tail;
  SdifListNT *Curr;  /* pointer before the next */
  void (*Killer)(void *);  
  unsigned int NbData;
} ;

/* SdifNameValue.h */
typedef struct SdifNameValueS SdifNameValueT;
struct SdifNameValueS
{
  char *Name;
  char *Value;
} ;

typedef struct SdifNameValueTableS SdifNameValueTableT;
struct SdifNameValueTableS
{
    SdifHashTableT* NVHT;
    SdifUInt4       NumTable;
    SdifUInt4       StreamID;   /* id of stream the table belongs to */
} ;

typedef struct SdifNameValuesLS SdifNameValuesLT;
struct SdifNameValuesLS
{
    SdifListT*              NVTList;  /* list of SdifNameValueTableT */
    SdifNameValueTableT*    CurrNVT;
    SdifUInt4               HashSize;
};


/* SdifStreamID.h */

/*
// DATA GROUP:          Stream ID Table and Entries for 1IDS ASCII chunk
*/


/*DOC:
  Stream ID Table Entry */
typedef struct SdifStreamIDS SdifStreamIDT;
struct SdifStreamIDS
{
  SdifUInt4     NumID;
  char *Source;
  char *TreeWay; /* for the moment or to be general*/
} ;

/*DOC:
  Stream ID Table, holds SdifStreamIDT stream ID table entries */
typedef struct SdifStreamIDTableS SdifStreamIDTableT;
struct SdifStreamIDTableS
{
    SdifHashTableT* SIDHT;
    SdifUInt4       StreamID;
    SdifFloat8      Time;       /* always _SdifNoTime */
} ;


typedef struct SdifColumnDefS SdifColumnDefT;

struct SdifColumnDefS
{
  char *Name;
  SdifUInt4 Num;
} ;


/* SdifMatrixType.h */
typedef struct SdifMatrixTypeS SdifMatrixTypeT;

struct SdifMatrixTypeS
{
  SdifSignature     Signature;

  SdifMatrixTypeT*  MatrixTypePre;

  SdifListT*        ColumnUserList; /* List of columns added by user: 
                                       SdifMatrixTypeInsertTailColumn(MatrixTypeT *)
                                    */

  SdifUInt4       NbColumnDef; /* Number of columns created by user:
                                  SdifMatrixTypeInsertTailColumn(MatrixTypeT *)
                               */
  SdifModifModeET ModifMode;
};


/* SdifFrameType.h */
typedef struct SdifComponentS SdifComponentT;
struct SdifComponentS
{
  SdifSignature MtrxS;
  char *Name;
  SdifUInt4  Num;
} ;

typedef struct SdifFrameTypeS SdifFrameTypeT;
struct SdifFrameTypeS
{
  SdifSignature Signature;

  SdifFrameTypeT* FrameTypePre;

  SdifHashTableT *ComponentUseHT;
  SdifUInt4       NbComponentUse;

  SdifUInt4       NbComponent;
  SdifModifModeET ModifMode;
};


/* SdifMatrix.h */
typedef struct SdifMatrixHeaderS SdifMatrixHeaderT;

struct SdifMatrixHeaderS
{
  SdifSignature  Signature;
  SdifDataTypeET DataType; /* Low level data type */
  SdifUInt4      NbRow;
  SdifUInt4      NbCol;
} ;


typedef union DataTypeU DataTypeUT;

union DataTypeU
{
  SdifFloat4 *Float4;
  SdifFloat8 *Float8;
  SdifInt1   *Int1  ;
  SdifInt2   *Int2  ;
  SdifInt4   *Int4  ;
/*SdifInt8   *Int8  ;*/
  SdifUInt1  *UInt1 ;
  SdifUInt2  *UInt2 ;
  SdifUInt4  *UInt4 ;
/*SdifUInt8  *UInt8 ;*/
  SdifChar   *Char  ;
  void       *Void  ;   /* generic pointer */
} ;


typedef struct SdifOneRowS SdifOneRowT;

struct SdifOneRowS
{
  SdifDataTypeET DataType;
  SdifUInt4      NbData;
  DataTypeUT     Data;
  SdifUInt4      NbGranuleAlloc;
} ;

typedef struct SdifMatrixDataS SdifMatrixDataT;
struct SdifMatrixDataS
{
  SdifMatrixHeaderT *Header;
  int               ForeignHeader;  /* Header was not allocated by me */
  SdifUInt4         Size;       /* byte size of matrix on file */
  DataTypeUT        Data;       /* any type pointer to data */
  SdifUInt4         AllocSize;  /* allocated size of data in bytes */
};

/* SdifFrame.h */
typedef struct SdifFrameHeaderS SdifFrameHeaderT;
struct SdifFrameHeaderS
{
  SdifSignature Signature;
  SdifUInt4  Size;
  SdifUInt4  NbMatrix;
  SdifUInt4  NumID;
  SdifFloat8 Time;
} ;


typedef struct SdifFrameDataS SdifFrameDataT;
struct SdifFrameDataS
{
  SdifFrameHeaderT *Header;
  SdifMatrixDataT* *Matrix_s;
} ;



/* SdifTimePosition.h */
typedef struct SdifTimePositionS SdifTimePositionT;

struct SdifTimePositionS
{
  SdifFloat8    Time;
  SdiffPosT     Position;
};


typedef struct SdifTimePositionLS SdifTimePositionLT;

struct SdifTimePositionLS
{
    SdifListT*          TimePosList;
};


SDIF_API SdifTimePositionT* SdifCreateTimePosition(SdifFloat8 Time, SdiffPosT Position);
SDIF_API void               SdifKillTimePosition(void* TimePosition);

SDIF_API SdifTimePositionLT* SdifCreateTimePositionL(void);
SDIF_API void                SdifKillTimePositionL  (SdifTimePositionLT *TimePositionL);

SDIF_API SdifTimePositionLT* SdifTimePositionLPutTail(SdifTimePositionLT* TimePositionL,
                                             SdifFloat8 Time, SdiffPosT Position);
SDIF_API SdifTimePositionT*  SdifTimePositionLGetTail(SdifTimePositionLT* TimePositionL);



/* SdifSignatureTab.h */
typedef struct SdifSignatureTabS SdifSignatureTabT;
struct SdifSignatureTabS
{
  SdifUInt4 NbSignMax;
  SdifUInt4 NbSign;
  SdifSignature* Tab;
};


/* SdifSelect.h */

/* 
// DATA GROUP:  SDIF Selection
*/

/* tokens (numerical ids) for sdifspec separators */
typedef enum { sst_specsep, sst_stream, sst_frame, sst_matrix, sst_column, 
               sst_row,     sst_time,   sst_list,  sst_range,  sst_delta,
               sst_num  /* number of tokens */,    sst_norange = 0
} SdifSelectTokens;

/*DOC: 
  Selection element interface (returned by SdifGetNextSelection*):
  One basic data element value, with optional range.  
  The meaning of range is determined by rangetype: 

  [] 0          no range
  [] sst_range  range is value..range
  [] sst_delta  range is value-range..value+range
*/

typedef struct 
{
    SdifUInt4          value, range;
    SdifSelectTokens   rangetype; /* 0 for not present, sst_range, sst_delta */
} SdifSelectElementIntT;

typedef struct 
{
    double             value, range;
    SdifSelectTokens   rangetype; /* 0 for not present, sst_range, sst_delta */
} SdifSelectElementRealT;

/* no SdifSelectElementSignatureT or string range, since it makes no sense */



/*DOC:
  Internal: one value of different possible types in a selection
  element (the element list determines which type is actually used).  
*/
typedef union SdifSelectValueS 
{
    SdifUInt4      integer;
    double         real;
    char           *string;
    SdifSignature  signature;
} SdifSelectValueT;

/*DOC: 
  Selection element internal data structure:
  One basic data element, with optional <ul>
  <li> range (value is lower, range is upper bound) or 
  <li> delta (value-range is lower, value+range is upper bound)
  </ul>
*/
typedef struct SdifSelectElementS
{
    SdifSelectValueT value;
    SdifSelectValueT range;
    SdifSelectTokens rangetype; /* 0 for not present, sst_range, sst_delta */
} SdifSelectElementT, *SdifSelectElementP;

typedef struct SdifSelectIntMaskS
{
    SdifUInt4   num;            /* number of ints selected */
    SdifUInt4   max;            /* max given int, #elems in mask */
    int        *mask;           /* selection bit mask */
    int         openend;        /* are elems > max included? */
} SdifSelectIntMaskT, *SdifSelectIntMaskP;

/*DOC: 
  Holds a selection of what data to access in an SDIF file,
  parsed from a simple regular expression.  
*/
typedef struct
{
    char        *filename,      /* allocated / freed by 
                                   SdifInitSelection / SdifFreeSelection */
                *basename;      /* points into filename */
    SdifListP   stream, frame, matrix, column, row, time;

    SdifSelectIntMaskT streammask;
    SdifSelectIntMaskT rowmask;
    SdifSelectIntMaskT colmask;
} SdifSelectionT;

/* TODO: array of select elements
     struct { 
        SdifListP list; 
        SdifSelectElementT minmax; 
        SdifSelectIntMaskP mask;
     } elem [eSelNum];
     indexed by
     enum   { eTime, eStream, eFrame, eMatrix, eColumn, eRow, eSelNum }
   to use in all API functions instead of SdifListP.
*/



/* SdifErrMess.h */
typedef enum SdifErrorTagE
{
    eFalse   = 0,
    eUnknown = 0,
    eTrue    = 1,
    eNoError = 1,
    eTypeDataNotSupported,
    eNameLength,
    eEof,       /* 4 */
    eReDefined,
    eUnDefined,
    eSyntax,
    eBadTypesFile,
    eBadType,
    eBadHeader,
    eRecursiveDetect,
    eUnInterpreted,
    eOnlyOneChunkOf,
    eUserDefInFileYet,
    eBadMode,
    eBadStdFile,
    eReadWriteOnSameFile,
    eBadFormatVersion,
    eMtrxUsedYet,
    eMtrxNotInFrame,
/* from here on global errors that don't always have an SdifFileT attached */
    eGlobalError,
    eFreeNull = eGlobalError,
    eAllocFail,
    eArrayPosition,
    eFileNotFound,
    eInvalidPreType,
    eAffectationOrder,
    eNoModifErr,
    eNotInDataTypeUnion,
    eNotFound,
    eExistYet,
    eWordCut,
    eTokenLength
} SdifErrorTagET;


/*DOC:
  Level of Error */
typedef enum SdifErrorLevelE
{
        eFatal,
        eError,
        eWarning,
        eRemark,
        eNoLevel,
        eNumLevels      /* level count, must always be last */
} SdifErrorLevelET;


typedef struct SdifErrorS SdifErrorT;
struct SdifErrorS
{
        SdifErrorTagET          Tag;
        SdifErrorLevelET        Level;
        char*                   UserMess;
};

typedef struct SdifErrorLS SdifErrorLT;
struct SdifErrorLS
{
  SdifListT*    ErrorList;
  SdifFileT*    SdifF; /* only a link */
};



/*DOC:
  Exit function type (See SdifSetExitFunc). */
typedef void (*SdifExitFuncT) (void);

/*DOC:
 Exception function type (See SdifSetErrorFunc and SdifSetWarningFunc). */
typedef void (*SdifExceptionFuncT) (SdifErrorTagET   error_tag, 
                                    SdifErrorLevelET error_level, 
                                    char *error_message, 
                                    SdifFileT *error_file, 
                                    SdifErrorT *error_ptr, 
                                    char *source_file, int source_line);



/* SdifFileStruct.h */

/*
// DATA GROUP:  SDIF File Structure
*/

/*DOC:
  File mode argument for SdifFOpen. */
typedef enum SdifFileModeE
{
  eUnknownFileMode,     /* 0 */
  eWriteFile,
  eReadFile,
  eReadWriteFile,
  ePredefinedTypes,     /* 4 */

  eModeMask = 7,        /* get rid of flags */

  /* from here on we have flags that can be or'ed with the previous modes */
  eParseSelection = 8
} SdifFileModeET ;



enum SdifPassE
{
  eNotPass,
  eReadPass,
  eWritePass
};


/*
// DATA GROUP:          SDIF File Structure
*/

#define MaxUserData     10
/*DOC:
  THE SDIF File Structure! */
struct SdifFileS
{
  char               *Name;             /* Name of the file, can be "stdin, stdout, stderr */
  SdifFileModeET     Mode;              /* eWriteFile or eReadFile or ePredefinedTypes */
  int                isSeekable;        /* file is not pipe i/o */

  SdifUInt4          FormatVersion;     /* version of the SDIF format itself */
  SdifUInt4          TypesVersion;      /* version of the description type collection */

  SdifNameValuesLT   *NameValues;       /* DataBase of Names Values */
  SdifHashTableT     *MatrixTypesTable; /* DataBase of Matrix Types */
  SdifHashTableT     *FrameTypesTable;  /* DataBase of Frame Types */
/*  SdifHashTableT     *StreamIDsTable;    DataBase of Stream IDs */
  SdifStreamIDTableT *StreamIDsTable;   /* DataBase of Stream IDs */
  SdifTimePositionLT *TimePositions;    /* List of (Time, Position in file) */
  SdifSelectionT     *Selection;        /* default selection parsed from Name */

  FILE *Stream;                         /* Stream to read or to write */

  SdifSignature      CurrSignature;
  SdifFrameHeaderT   *CurrFramH;        /* Current Frame Header can be NULL */
  SdifMatrixHeaderT  *CurrMtrxH;        /* Current Matrix Header can be NULL */

  SdifFrameTypeT     *CurrFramT;
  SdifMatrixTypeT    *CurrMtrxT;
  SdifFloat8         PrevTime;
  SdifSignatureTabT  *MtrxUsed;

  SdifOneRowT        *CurrOneRow;
  /* Current OneRow allocated memory in function
   * of _SdifGranule, use SdifReInitOneRow(SdifOneRowT *OneRow, SdifDataTypeET DataType, SdifUInt4 NbData)
   * to assure NbData (=NbColumns) objects memory allocated
   */

  /* data pointer used by SdifFReadMatrixData, never uses the Header field */
  SdifMatrixDataT    *CurrMtrxData;

  size_t  FileSize;
  size_t  ChunkSize;

  SdiffPosT  CurrFramPos;
  SdiffPosT  StartChunkPos;
  SdiffPosT  Pos;
  
  SdifUInt2  TypeDefPass;
  SdifUInt2  StreamIDPass;

  char *TextStreamName;                 /* Name of the text file corresponding to the sdif file */
  FILE *TextStream;                     /* Stream text */

  SdifUInt4     ErrorCount [eNumLevels];/* Error count per level of severity */
  SdifErrorLT  *Errors;                 /* List of errors or warnings */

  int           NbUserData;             /* todo: hash table */
  void          *UserData [MaxUserData];
};      /* end struct SdifFileS */







/* SdifString.h */

typedef struct SdifStringS SdifStringT;
struct SdifStringS
{
  char   *str; 
  size_t TotalSize; /* Memory size allocated for str */
  size_t SizeW; /* Memory size actually used */
  int    NbCharRead; /* Number of char read */
};


/*DOC: 
  Test if file is an SDIF file.

  [] Returns:   0 if not an SDIF file (the first 4 chars are not "SDIF"),
                or file can not be opened, else 1.  

  Warning: This function doesn't work with stdio. */
SDIF_API int SdifCheckFileFormat (const char *name);


/*DOC: 
  Test if file contains frames of certain types.

  [in]  name    Filename + selection
        frames  Table of frame signatures to look for
  []    return  The first signature from frames found, or eEmptySignature if 
                no frames could be found (or if file is not SDIF).

  Warning: This function doesn't work with stdio. */
SDIF_API SdifSignature SdifCheckFileFramesTab (const char              *name, 
                                               const SdifSignatureTabT *frames);

/*DOC: 
  Test if file contains frames of certain types.

  [in]  name    Filename + selection
        frames  Array of frame signatures to look for, terminated with 
                eEmptySignature.
  []    return  The index in frames of the first signature found, or -1
                if no frames could be found (or if file is not SDIF).

  Warning: This function doesn't work with stdio. */
SDIF_API int  SdifCheckFileFramesIndex (const char              *name, 
                                        const SdifSignature     *frames);

/*DOC: 
  Test if file contains frames of certain types.

  [in]  in      open SDIF file
        frames  Table of frame signatures to look for
  [out] index   If the int pointer index is not NULL, it will receive
                the index in frames of the first signature found, or -1
                if no frames could be found (or if file is not SDIF).
  []    return  The first signature from frames found, or eEmptySignature if 
                no frames could be found (or if file is not SDIF).

  Warning: This function doesn't work with stdio. */
SDIF_API SdifSignature SdifCheckNextFrame (SdifFileT               *in, 
                                           const SdifSignatureTabT *frames,
                                           int                     *index);

/*DOC: 
  TODO: Test if file is an SDIF file (only when opening for read or
  append) and open it.

  [Return] NULL if not an SDIF file (the first 4 chars are not "SDIF"),
  or file can not be opened.  */
SDIF_API SdifFileT* SdifFTryOpen (const char *Name, SdifFileModeET Mode);


/*DOC: 
  Converti un fichier texte pseudo-SDIF de nom TextStreamName en un
  fichier SDIF binaire de non SdifF->Name. Le fichier doit avoir été
  ouvert en écriture (eWriteFile).  */
SDIF_API size_t SdifToText (SdifFileT *SdifF, char *TextStreamName);


/*#include "SdifFile.h"
 */


/*DOC:
  Switch output of error messages on stderr by _SdifFError on. 
*/
SDIF_API void   SdifEnableErrorOutput  (void);

/*DOC:
  Switch output of error messages on stderr by _SdifFError off. 
*/
SDIF_API void   SdifDisableErrorOutput (void);


/* global variables to control error output */
extern SDIF_API int              gSdifErrorOutputEnabled;
extern SDIF_API char            *SdifErrorFile;
extern SDIF_API int              SdifErrorLine;
extern SDIF_API FILE            *SdifStdErr;



/*DOC: 
  Lit 4 bytes, les considère comme une signature qui est placée dans
  SdifF->CurrSignature, incrémente NbCharRead du nombre de bytes lus
  et renvoie le dernier caractère lu convert en int (-1 si erreur).  */
SDIF_API int    SdifFGetSignature       (SdifFileT *SdifF, size_t *NbCharRead);


/*DOC: 
  Lit l'entête du fichier, c'est à dire 'SDIF' puis 4 bytes.  affiche
  un message en cas de non reconnaissance du format.  */
SDIF_API size_t SdifFReadGeneralHeader    (SdifFileT *SdifF);

SDIF_API size_t SdifFReadAllASCIIChunks   (SdifFileT *SdifF);

/*DOC: 
  Cette fonction lit une entête de matrice <strong>signature
  incluse</strong>.  Elle vérifie le type de matrice, le champ
  DataType. Toute les données se trouvent stockées dans
  SdifF->CurrMtrxH. La plupart de ses champs sont directement
  accessible par les fonctions indépendantes du mode d'ouverture du
  fichier.  <strong>Elle effectue une mise à jour de l'allocation
  mémoire de SdifF->CurrOneRow en fonction des paramètres de l'entête
  de matrice.</strong> Ainsi, on est normalement près pour lire chaque
  ligne de la matrice courrante.  

  @return       number of bytes read or 0 if error 
*/
SDIF_API size_t SdifFReadMatrixHeader     (SdifFileT *SdifF);

/*DOC: 
  Cette fonction permet de lire 1 ligne de matrice. Les données lues
  sont stockées dans SdifF->CurrOneRow (jusqu'à une prochaine lecture
  d'entête de matrice qui réinitialise ses paramètres).  */
SDIF_API size_t SdifFReadOneRow           (SdifFileT *SdifF);

/*DOC:
  skip one matrix row, when reading row by row with SdifFReadOneRow */
SDIF_API size_t SdifFSkipOneRow(SdifFileT *SdifF);


/*DOC: 
  Cette fonction lit l'entête d'un frame à partir de la taille et
  jusqu'au temps. Donc <strong>elle ne lit pas la signature</strong>
  mais donne à SdifF->CurrFramH->Signature la valeur de
  SdifF->CurrSignature.  La lecture doit se faire avant, avec
  SdifFGetSignature.  */
SDIF_API size_t SdifFReadFrameHeader      (SdifFileT *SdifF);

/*DOC: 
  Cette fonction permet de passer une matrice toute entière entête
  incluse. Elle est utile lorsque qu'un frame contient plus de
  matrices que le programme lecteur n'en connaît. Il peut ainsi les
  passer pour retomber sur un autre frame.  */
SDIF_API size_t SdifFSkipMatrix          (SdifFileT *SdifF);

/*DOC: 
  Cette fonction permet de passer une matrice mais après la lecture de
  l'entête. On s'en sert lorsque le type de matrice est mauvais,
  inconnu, non interprétable par le programme lecteur.

  Note:  The matrix padding is skipped also. */
SDIF_API size_t SdifFSkipMatrixData       (SdifFileT *SdifF);

/*DOC: 
  Cette fonction à le même sens que SdifSkipMatrixData mais pour les
  frames. Il faut donc pour l'utiliser avoir au préalable lu la
  signature et l'entête.  */
SDIF_API size_t SdifFSkipFrameData        (SdifFileT *SdifF);

/*DOC: 
  Cette fonction permet de lire le Padding en fin de matrice.
  l'utilisation classique de cette fonctin est:<br> 
  <code> SizeR =  SdifFReadPadding(SdifF, SdifFPaddingCalculate(SdifF->Stream, SizeR));</code><br> 
  où SizeR est la taille en bytes lue depuis le
  début de la matrice, c'est à dire NbRow*NbCol*DataWith. En réalité,
  pour que SdifFPaddingCalculate fonctionne, il est seulement
  nécessaire que SizeR soit le nombre de bytes qui s'épare la position
  actuelle dans le fichier et un byte, repère d'allignement sur 64
  bits.  */
SDIF_API size_t SdifFReadPadding          (SdifFileT *SdifF, size_t Padding);


/* skip given number of bytes, either by seeking or by reading bytes */
SDIF_API size_t SdifFSkip (SdifFileT *SdifF, size_t bytes);


/*DOC:
  Read and throw away <i>num</i> bytes from the file. */
SDIF_API size_t SdifFReadAndIgnore (SdifFileT *SdifF, size_t bytes);


/*DOC: 
  écrit sur le fichier 'SDIF' puis 4 bytes chunk size.  */
SDIF_API size_t  SdifFWriteGeneralHeader   (SdifFileT *SdifF);

/*DOC: 
  écrit tous les chunks ASCII. C'est à dire: les tables de names
  values, les types créés ou complétés, et les Stream ID. Il faut donc
  au préalable avoir rempli complétement les tables avant de la
  lancer. Cette fonction de peut donc pas être executer une 2nd fois
  durant une écriture.  */
SDIF_API size_t  SdifFWriteAllASCIIChunks  (SdifFileT *SdifF);


/*
//FUNCTION GROUP:       Writing Matrices
*/

/*DOC: 
  Après avoir donner une valeur à chaque champ de SdifF->CurrMtrxH
  gràce à la fonction SdifFSetCurrMatrixHeader, SdifFWriteMatrixHeader
  écrit toute l'entête de la matrice.  Cette fonction réalise aussi
  une mise à jour de SdifF->CurrOneRow, tant au niveau de l'allocation
  mémoire que du type de données.  */
SDIF_API size_t  SdifFWriteMatrixHeader    (SdifFileT *SdifF);

/*DOC: 
  Après avoir donner les valeurs à chaque case de SdifF->CurrOneRow à
  l'aide de SdifFSetCurrOneRow ou de SdifFSetCurrOneRowCol (suivant
  que l'on possède déjà un tableau flottant ou respectivement une
  méthode pour retrouver une valeur de colonne), SdifFWriteOneRow
  écrit 1 ligne de matrice suivant les paramètres de SdifF->CurrMtrxH.  */
SDIF_API size_t  SdifFWriteOneRow          (SdifFileT *SdifF);

/*DOC: 
  Write whole matrix data, (after having set the matrix header with 
  SdifFSetCurrMatrixHeader (file, matrixsig, datatype, nrow, ncol).
  Data points to nbrow * nbcol * SdifSizeofDataType (datatype) bytes in 
  row-major order.  Padding still has to be written.  */
SDIF_API size_t SdifFWriteMatrixData (SdifFileT *SdifF, void *Data);

/*DOC:
  Write whole matrix: header, data, and padding.
  Data points to NbRow * NbCol * SdifSizeofDataType (DataType) bytes in
  row-major order. */
SDIF_API size_t SdifFWriteMatrix (SdifFileT     *SdifF,
                                  SdifSignature  Signature,
                                  SdifDataTypeET DataType,
                                  SdifUInt4      NbRow,
                                  SdifUInt4      NbCol,
                                  void          *Data);

/*DOC:
  Write a matrix with datatype text (header, data, and padding).
  Data points to Length bytes(!) of UTF-8 encoded text.  Length
  includes the terminating '\0' character!!!  That is, to write a
  C-String, use SdifFWriteTextMatrix (f, sig, strlen (str) + 1, str);
  to include it. */
SDIF_API size_t SdifFWriteTextMatrix (SdifFileT     *SdifF,
                             SdifSignature  Signature,
                             SdifUInt4      Length,
                             char          *Data);

/*DOC: 
  TBI: Convert ASCII C-String to UTF-8 encoded string, returning
  length (including terminating null character). */
SDIF_API size_t SdifAsciiToUTF8 (char *ascii_in, char *utf8_out);

/*DOC: 
  Cette fonction permet en fin d'écriture de matrice d'ajouter le
  Padding nécessaire. Il faut cependant avoir la taille de ce
  Padding. On utilise SdifFPaddingCalculate(SdifF->Stream,
  SizeSinceAlignement) où SizeSinceAllignement est un
  <code>size_t</code> désignant le nombre de bytes qui sépare la
  position actuelle d'écriture avec une position connue où le fichier
  est aligné sur 64 bits (en général, c'est la taille de la matrice en
  cours d'écriture: NbRow*NbCol*DatWitdh).  */
SDIF_API size_t  SdifFWritePadding         (SdifFileT *SdifF, size_t Padding);


/*
//FUNCTION GROUP:       Writing Frames
*/

/*DOC: 
  Après avoir donner une valueur à chaque champ de SdifF->CurrFramH
  gràce à la fonction SdifFSetCurrFrameHeader, SdifFWriteFrameHeader
  écrit toute l'entête de frame.  Lorsque la taille est inconnue au
  moment de l'écriture, donner la valeur _SdifUnknownSize. Ensuite,
  compter le nombre de bytes écrit dans le frame et réaliser un
  SdifUpdateChunkSize avec la taille calculée.  */
SDIF_API size_t  SdifFWriteFrameHeader     (SdifFileT *SdifF);

/*DOC: 
  Execute un retour fichier de ChunkSize bytes et l'écrit, donc on
  écrase la taille du chunk ou du frame.  Dans le cas où le fichier
  est stderr ou stdout, l'action n'est pas réalisée.  */
SDIF_API void    SdifUpdateChunkSize       (SdifFileT *SdifF, size_t ChunkSize);

/*DOC: 
  Rewrite given frame size and number of matrices in frame header.
  Return -1 on error or if file is not seekable (stdout or stderr). */
SDIF_API int     SdifUpdateFrameHeader     (SdifFileT *SdifF, size_t ChunkSize, 
                                   SdifInt4 NumMatrix);

/*DOC:
  Write a whole frame containing one matrix: 
  frame header, matrix header, matrix data, and padding.
  Data points to NbRow * NbCol * SdifSizeofDataType (DataType) bytes in
  row-major order. 

  This function has the big advantage that the frame size is known in
  advance, so there's no need to rewind and update after the matrix
  has been written.  */
SDIF_API size_t SdifFWriteFrameAndOneMatrix (SdifFileT     *SdifF,
                                             SdifSignature  FrameSignature,
                                             SdifUInt4      NumID,
                                             SdifFloat8     Time,
                                             SdifSignature  MatrixSignature,
                                             SdifDataTypeET DataType,
                                             SdifUInt4      NbRow,
                                             SdifUInt4      NbCol,
                                             void          *Data);


/*DOC:
  Return (constant) size of frame header after signature and size field. 
  Use this to calculate the Size argument for SdifFSetCurrFrameHeader. */
SDIF_API size_t SdifSizeOfFrameHeader (void);

/*DOC:
  Return size of matrix (header, data, padding).
  Use this to calculate the Size argument for SdifFSetCurrFrameHeader. */
SDIF_API size_t SdifSizeOfMatrix (SdifDataTypeET DataType,
                                  SdifUInt4      NbRow,
                                  SdifUInt4      NbCol);

/*DOC:
  Write a text matrix using a string.
  Return number of bytes written.
*/
SDIF_API size_t SdifFWriteTextFrame(SdifFileT     *SdifF,
                                    SdifSignature FrameSignature,
                                    SdifUInt4     NumID,
                                    SdifFloat8    Time,
                                    SdifSignature MatrixSignature,
                                    char          *str,
                                    size_t        length);

/*DOC:
  Write a text matrix using a SdifString.
  Return number of bytes written.
*/
SDIF_API size_t SdifFWriteTextFrameSdifString(SdifFileT     *SdifF,
                                              SdifSignature FrameSignature,
                                              SdifUInt4     NumID,
                                              SdifFloat8    Time,
                                              SdifSignature MatrixSignature,
                                              SdifStringT   *SdifString);


/*
// FUNCTION GROUP:      Opening and Closing of Files
*/

/*DOC:
 */
SDIF_API SdifFileT* SdifFOpen                   (const char *Name, SdifFileModeET Mode);

SDIF_API SdifFileT*         SdifFOpenText                (SdifFileT *SdifF, const char* Name, SdifFileModeET Mode);

/*DOC:
 */
SDIF_API void      SdifFClose                   (SdifFileT *SdifF);

SDIF_API SdifFrameHeaderT*  SdifFCreateCurrFramH         (SdifFileT *SdifF, SdifSignature Signature);
SDIF_API SdifMatrixHeaderT* SdifFCreateCurrMtrxH         (SdifFileT *SdifF);
SDIF_API FILE*              SdifFGetFILE_SwitchVerbose   (SdifFileT *SdifF, int Verbose);
SDIF_API void               SdifTakeCodedPredefinedTypes (SdifFileT *SdifF);
SDIF_API void               SdifFLoadPredefinedTypes     (SdifFileT *SdifF, const char *TypesFileName);

extern SDIF_API int        gSdifInitialised;
extern SDIF_API SdifFileT *gSdifPredefinedTypes;



/*
// FUNCTION GROUP:      Init/Deinit of the Library
*/

/*DOC: 
  Initialise the SDIF library, providing a name for an optional additional
  file with type definitions or "".
  <b>This function has to be called once and only once per process 
  before any other call to the SDIF library.</b> */
SDIF_API void SdifGenInit (const char *PredefinedTypesFile); 

/*DOC:
  Initialise the SDIF library if it has not been initialised before.
  This function has to be called at least once, but can be called as
  many times as desired.  Especially useful for dynamic libraries.

  [in] PredefinedTypesFile:
        name for an optional additional file with type definitions or "". */
SDIF_API void SdifGenInitCond (const char *PredefinedTypesFile);

/*DOC:
  Deinitialise the SDIF library */
SDIF_API void SdifGenKill (void); 

/*DOC:
  Set function that will be called after a grave error has occurred.  
  Default is exit(). */
SDIF_API void SdifSetExitFunc (SdifExitFuncT func);

/*DOC:
  Set function that will be called after an error has occured.
  make an exception. */
SDIF_API void SdifSetErrorFunc (SdifExceptionFuncT func);

/*DOC:
  Set function that will be called after a warning has occured.
  make an exception. */
SDIF_API void SdifSetWarningFunc (SdifExceptionFuncT func);

/*DOC:
  Print version information to standard error. */
SDIF_API void SdifPrintVersion(void);


/*
// FUNCTION GROUP:      Current Header Access Functions
*/

/*DOC: 
  Permet de donner des valeurs à chaque champ de l'entête de frame
  temporaire de SdifF.<p> 

  Exemple:
  <code>SdifSetCurrFrameHeader(SdifF, '1FOB', _SdifUnknownSize, 3, streamid, 1.0);</code> */
SDIF_API SdifFrameHeaderT* SdifFSetCurrFrameHeader (SdifFileT *SdifF, 
                                                    SdifSignature Signature, 
                                                    SdifUInt4 Size,
                                                    SdifUInt4 NbMatrix, 
                                                    SdifUInt4 NumID, 
                                                    SdifFloat8 Time);

/*DOC: 
  Permet de donner des valeurs à chaque champ de l'entête de matice
  temporaire de SdifF.<p>

  Exemple:
  <code>SdifSetCurrMatrixHeader(SdifF, '1FOF', eFloat4, NbFofs, 7);</code> */
SDIF_API SdifMatrixHeaderT* SdifFSetCurrMatrixHeader (SdifFileT *SdifF, 
                                                      SdifSignature Signature,
                                                      SdifDataTypeET DataType, 
                                                      SdifUInt4 NbRow, 
                                                      SdifUInt4 NbCol);


/*DOC: 
  Recopie la mémoire pointée par Values en fonction de l'entête de
  matrice courante.<p> 

  Exemple:<br>
<pre>
  #define NbCols = 10;<br>

  float t[NbCols] = { 1., 2., 3., 4., 5., 6., 7., 8., 9., 0.};<br>

  SdifFSetCurrMatrixHeader(SdifF, 'mtrx', eFloat4, 1, NbCols);<br>
  SdifFSetCurrOneRow      (SdifF, (void*) t);<br>
</pre>

  On connait la taille de la mémoire à recopier par le type de donnée
  (ici: eFloat4) et le nombre de colonnes (ici: NbCols). Il faut que
  le type de donnée de la matrice courante corresponde avec la taille
  d'un élément de t. Si t est composé de float sur 4 bytes, alors on
  doit avoir eFloat4. Si t est composé de double float sur 8 bytes,
  alors c'est eFloat8.<br>

  En général, les données d'un programme ne se présente pas sous cette
  forme et il faut réaliser une transposition lors des transfert de
  Sdif à un programme. Le programme Diphone Ircam a un bon exemple de
  lecture avec transposition automatique, généralisée pour tout type
  de matrice. */
SDIF_API SdifOneRowT*  SdifFSetCurrOneRow       (SdifFileT *SdifF, void *Values);


/*DOC: 
  Permet de donner la valeur Value dans la ligbe de matrice temporaire
  de SdifF à la colonne numCol (0<numCol<=SdifF->CurrMtrxH->NbCol).  */
SDIF_API SdifOneRowT* SdifFSetCurrOneRowCol (SdifFileT *SdifF, SdifUInt4 numCol, SdifFloat8 Value);


/*DOC: 
  Recupère la valeur stockée à la colonne numCol de la ligne
  temporaire.  C'est un SdifFloat8 donc un double!!  */ 
SDIF_API SdifFloat8 SdifFCurrOneRowCol (SdifFileT *SdifF, SdifUInt4 numCol);


/*DOC: 
  Idem que la fonction précédente mais en utilisant le type de la
  matrice et le nom de la colonne.  */
SDIF_API SdifFloat8    SdifFCurrOneRowColName   (SdifFileT *SdifF, 
                                        SdifMatrixTypeT *MatrixType, 
                                        const char *NameCD);


/*DOC: 
  Renvoie la signature temporaire de Chunk ou de Frame.  */
SDIF_API SdifSignature SdifFCurrSignature       (SdifFileT *SdifF);


/*DOC: 
  Met à 0 tous les bits de la signature temporaire.  */
SDIF_API SdifSignature SdifFCleanCurrSignature  (SdifFileT *SdifF);

/*DOC: 
  Renvoie la signature temporaire du dernier Frame lu ou du prochain à
  écrire.  */
SDIF_API SdifSignature SdifFCurrFrameSignature  (SdifFileT *SdifF);

/*DOC: 
  Renvoie la signature temporaire de la dernier matrice lue ou de la
  prochaine à écrire.  */
SDIF_API SdifSignature SdifFCurrMatrixSignature (SdifFileT *SdifF);

/*DOC: 
  Renvoie SdifF->CurrMtrx->NbCol, nombre de colonnes de la matrice en
  cours de traitement.  */
SDIF_API SdifUInt4     SdifFCurrNbCol           (SdifFileT *SdifF);

/*DOC: 
  Renvoie SdifF->CurrMtrx->NbRow, nombre de lignes de la matrice en
  cours de traitement.  */
SDIF_API SdifUInt4     SdifFCurrNbRow           (SdifFileT *SdifF);

/*DOC: 
  Returns the data type of the current matrix. */
SDIF_API SdifDataTypeET SdifFCurrDataType (SdifFileT *SdifF);

/*DOC: 
  Renvoie SdifF->CurrFramH->NbMatrix, mombre de matrices du frame
  courant.  */
SDIF_API SdifUInt4     SdifFCurrNbMatrix        (SdifFileT *SdifF);

/*DOC: 
  Renvoie SdifF->CurrFramH->NumID, index de l'objet du frame courant.  */
SDIF_API SdifUInt4     SdifFCurrID              (SdifFileT *SdifF);

/*DOC: 
  Renvoie SdifF->CurrFramH->Time.  */
SDIF_API SdifFloat8    SdifFCurrTime            (SdifFileT *SdifF);




/*
// FUNCTION GROUP:      File Data Access Functions
*/

/*DOC: 
  Renvoie la ligne temporaire de SdifF.  */
SDIF_API SdifOneRowT*  SdifFCurrOneRow          (SdifFileT *SdifF);

/*DOC:
  Returns a pointer to the data of the current matrix row.  
  According to the matrix data type, it can be a pointer to float or double. */
SDIF_API void*        SdifFCurrOneRowData          (SdifFileT *SdifF);

/*DOC: 
  Return pointer to current matrix data structure, if read before with
  SdifFReadMatrixData. */
SDIF_API SdifMatrixDataT *SdifFCurrMatrixData (SdifFileT *file);

/*DOC: 
  Return pointer to current raw matrix data, if read before with
  SdifFReadMatrixData.  Data is specified by current matrix header */
SDIF_API void*        SdifFCurrMatrixDataPointer (SdifFileT *file);


/*DOC:
  Return list of NVTs for querying. 
  [] precondition NVTs have been read with SdifFReadAllASCIIChunks. */
SDIF_API SdifNameValuesLT *SdifFNameValueList (SdifFileT *file);

/*DOC:
  Return number of NVTs present.
  [] precondition NVTs have been read with SdifFReadAllASCIIChunks. */
SDIF_API int SdifFNameValueNum (SdifFileT *file);

/*DOC:
  Return the file's stream ID table, created automatically by SdifFOpen. */
SDIF_API SdifStreamIDTableT *SdifFStreamIDTable (SdifFileT *file);

/*DOC:
  Add user data, return index added */
SDIF_API int SdifFAddUserData (SdifFileT *file, void *data);

/*DOC:
  Get user data by index */
SDIF_API void *SdifFGetUserData (SdifFileT *file, int index);



SDIF_API SdifFileT*    SdifFReInitMtrxUsed (SdifFileT *SdifF);
SDIF_API SdifFileT*    SdifFPutInMtrxUsed  (SdifFileT *SdifF, SdifSignature Sign);
SDIF_API SdifSignature SdifFIsInMtrxUsed   (SdifFileT *SdifF, SdifSignature Sign);



/*
// FUNCTION GROUP:      Error flag for file
*/

/*DOC: 
  Return pointer to last error struct or NULL if no error present
  for this file. */
SDIF_API SdifErrorT*     SdifFLastError    (SdifFileT *SdifF);

/*DOC: 
  Return tag of last error or eNoError if no error present for this file. */
SDIF_API SdifErrorTagET  SdifFLastErrorTag (SdifFileT *SdifF);


#define _SdifFrameHeaderSize 16  /* (ID=4)+(size=4)+(time=8) */



SDIF_API SdifFrameHeaderT* SdifCreateFrameHeader(SdifSignature Signature,
                                               SdifUInt4 Size,
                                               SdifUInt4 NbMatrix,
                                               SdifUInt4 NumID,
                                               SdifFloat8 Time);

SDIF_API SdifFrameHeaderT* SdifCreateFrameHeaderEmpty(SdifSignature Signature);

SDIF_API void              SdifKillFrameHeader  (SdifFrameHeaderT *FrameHeader);

SDIF_API SdifFrameDataT* SdifCreateFrameData(SdifHashTableT *FrameTypesTable,
                                           SdifSignature FrameSignature,
                                           SdifUInt4 NumID,
                                           SdifFloat8 Time);

SDIF_API void            SdifKillFrameData   (SdifHashTableT *FrameTypesTable, SdifFrameDataT *FrameData);

SDIF_API SdifFrameDataT* SdifFrameDataPutNthMatrixData(SdifFrameDataT *FrameData, unsigned int NthMatrix,
                                                     SdifMatrixDataT *MatrixData);

SDIF_API SdifFrameDataT* SdifFrameDataPutComponentMatrixData(SdifHashTableT *FrameTypesTable,
                                                           SdifFrameDataT *FrameData,
                                                           char *CompoName, SdifMatrixDataT *MatrixData);

SDIF_API SdifMatrixDataT* SdifFrameDataGetNthMatrixData(SdifFrameDataT *FrameData, unsigned int NthMatrix);

SDIF_API SdifMatrixDataT* SdifFrameDataGetComponentMatrixData(SdifHashTableT *FrameTypesTable,
                                                            SdifFrameDataT *FrameData,
                                                     char *CompoName);



SDIF_API SdifComponentT* SdifCreateComponent (SdifSignature MtrxS, char *Name, SdifUInt4 Num);
SDIF_API void            SdifKillComponent   (SdifComponentT *Component);
SDIF_API SdifFrameTypeT* SdifCreateFrameType (SdifSignature FramS, SdifFrameTypeT *PredefinedFrameType);

SDIF_API void            SdifKillFrameType               (SdifFrameTypeT *FrameType);

/**
 * Get number of matrix components defined in frame type. 
 */
SdifUInt4 SdifFrameTypeGetNbComponents (SdifFrameTypeT *FrameType);

/** 
 * Access a frame type component definition by matrix component number (starting from 1).
 */
SDIF_API SdifComponentT* SdifFrameTypeGetNthComponent    (SdifFrameTypeT *FrameType, SdifUInt4 NumC);

/** 
 * Access a frame type component definition by matrix component signature 
 */
SDIF_API SdifComponentT* SdifFrameTypeGetComponent_MtrxS (SdifFrameTypeT *FrameType, SdifSignature MtrxS);

/** 
 * Access a frame type component definition by matrix component name 
 */
SDIF_API SdifComponentT* SdifFrameTypeGetComponent       (SdifFrameTypeT *FrameType, const char *NameC);

SDIF_API SdifFrameTypeT* SdifFrameTypePutComponent       (SdifFrameTypeT *FrameType, SdifSignature MtrxS, char *NameC);


/** Get matrix signature of frame component definition */
SdifSignature SdifFrameTypeGetComponentSignature (SdifComponentT *comp);

/** Get matrix role of frame component definition */
char *SdifFrameTypeGetComponentName (SdifComponentT *comp);


/**
 * Get frame type pointer from signature, given a frame type hash table.
 * Use SdifFGetFrameTypesTable to get this.
 */ 
SDIF_API SdifFrameTypeT* SdifGetFrameType       (SdifHashTableT *FrameTypeHT, SdifSignature FramS);

SDIF_API void            SdifPutFrameType       (SdifHashTableT *FrameTypeHT, SdifFrameTypeT *FrameType);
SDIF_API SdifUInt2       SdifExistUserFrameType (SdifHashTableT *FrameTypeHT);



/* set default if not overridden from makefile */
#ifndef _SdifFormatVersion
#define _SdifFormatVersion 3
#endif

#define _SdifTypesVersion  1


/* _SdifEnvVar : Environnement variable which contains the name
 * of the file which contains predefined types (the name contains the path).
 * _SdifEnvVar is used in SdifFile.c SdifGenInit, the user can
 * reference predefined types by this envvar name.
 */
#define _SdifEnvVar "SDIFTYPES"

/* Default predefined types : _SdifTypesFileName see SdifFile.c
 */

/* allocation constants
   TODO: should these be public? */
#define _SdifListNodeStockSize 0x400 /* 1024 */
#define _SdifGenHashSize         127 /* size of matrix/frame type table */
#define _SdifNameValueHashSize    31 /* size of hash table for NVTs */
#define _SdifUnknownSize  0xffffffff
#define _SdifGranule            1024 /* for OneRow allocation in bytes */
#define _SdifPadding               8
#define _SdifPaddingBitMask (_SdifPadding - 1)

#define _SdifFloat8Error  0xffffffff
#define _SdifNoTime       _Sdif_MIN_DOUBLE_     /* for header ASCII frames */
#define _SdifNoStreamID   0xfffffffe            /* -2 used for 1TYP */
#define _SdifAllStreamID  0xffffffff            /* -1 used for 1IDS */
#define _SdifUnknownUInt4 0xffffffff

/* CNMAT restriction: only one frame type per stream.  
   Therefore we have to use unique IDs for all 'header' frames. */
#define _SdifNVTStreamID  0xfffffffd            /* -3 used for 1NVT */
#define _SdifIDSStreamID  0xfffffffc            /* -4 unused */
#define _SdifTYPStreamID  0xfffffffb            /* -5 unused */


#define _SdifFloatEps  FLT_EPSILON

/* DataTypeEnum

   On Matt Wright's visit at IRCAM June 1999, we defined a new
   encoding for the MatrixDataType field with the feature that the low
   order byte encodes the number of bytes taken by each matrix
   element.  

   Low order byte encodes the number of bytes 
   High order bytes come from this (extensible) enum:

        0 : Float
        1 : Signed integer
        2 : Unsigned integer
        3 : Text (UTF-8 when 1 byte)
        4 : arbitrary/void
*/


#ifndef SWIG
/* #ifdef STDC_HEADERS */  /* Is the compiler ANSI? */

/* generate template for all types, 
   called by sdif_foralltypes and sdif_proto_foralltypes. */
#define sdif__foralltypes(macro, post)  macro(Float4)post \
                                        macro(Float8)post \
                                        macro(Int1  )post \
                                        macro(Int2  )post \
                                        macro(Int4  )post \
                                        macro(UInt1 )post \
                                        macro(UInt2 )post \
                                        macro(UInt4 )post \
                                        macro(Char  )post \
                                     /* macro(Int8  )post \
                                        macro(UInt8 )post \
                                      */

#define sdif_foralltypes_post_body    /* this is empty */
#define sdif_foralltypes_post_proto ; /* this is a semicolon */


/* generate template for all types */
#define sdif_foralltypes(macro)         \
        sdif__foralltypes(macro,sdif_foralltypes_post_body)


/* generate prototype template for all types */
#define sdif_proto_foralltypes(macro)   \
        sdif__foralltypes(macro,sdif_foralltypes_post_proto)

/* #endif */ /* STDC_HEADERS */
#endif /* SWIG */


#define _SdifStringLen 1024

extern SDIF_API char gSdifString[_SdifStringLen];
extern SDIF_API char gSdifString2[_SdifStringLen];
extern SDIF_API char gSdifErrorMess[_SdifStringLen];

#define _SdifNbMaxPrintSignature 8
extern SDIF_API char gSdifStringSignature[_SdifNbMaxPrintSignature][5];
extern SDIF_API int  CurrStringPosSignature;


/*
// FUNCTION GROUP:      utility functions
*/

/*DOC:
*/
SDIF_API char*     SdifSignatureToString(SdifSignature Signature);

/*DOC: 
  Compare two signatures, ignoring the first character which
  encodes the type version.  Note that comparison of full signatures
  can be done simply with '=='. 
*/
SDIF_API int     SdifSignatureCmpNoVersion(SdifSignature Signature1, SdifSignature Signature2);

/*DOC: 
  Returns size of SDIF data type in bytes
  (which is always the low-order byte).  
*/
SDIF_API SdifUInt4 SdifSizeofDataType (SdifDataTypeET DataType);

/*DOC: 
  Returns true if DataType is in the list of known data types.
*/
SDIF_API int SdifDataTypeKnown (SdifDataTypeET DataType);

/*DOC:
*/
SDIF_API size_t    SdifPaddingCalculate  (size_t NbBytes);

/*DOC:
*/
SDIF_API size_t    SdifFPaddingCalculate (FILE *f, size_t NbBytes);

/* (double f1) == (double f2) with _SdifFloatEps for error */
SDIF_API int SdifFloat8Equ(SdifFloat8 f1, SdifFloat8 f2);


#ifndef MIN
#define MIN(a,b)        ((a) < (b)  ?  (a)  :  (b))
#endif

#ifndef MAX
#define MAX(a,b)        ((a) > (b)  ?  (a)  :  (b))
#endif



/* SdifHard_OS.h */

/* _Sdif_MIN_DOUBLE_ tested on SGI, DEC alpha, PCWin95 as 0xffefffffffffffff
 * include may be limits.h (float.h is sure with VisualC++5 Win 95 or NT)
 */
#define _Sdif_MIN_DOUBLE_ (- DBL_MAX)


SDIF_API int       SdifStrLen  (const char *s);

/* returns 0 if strings are equal */
SDIF_API int       SdifStrCmp  (const char *s1, const char *s2);

/* returns true if strings are equal */
SDIF_API int       SdifStrEq(const char *s1, const char *s2);
SDIF_API int       SdifStrNCmp (const char *s1, const char *s2, unsigned int n);
SDIF_API char*     SdifStrNCpy (char *s1, const char *s2, unsigned int n);
SDIF_API char*     SdifCreateStrNCpy (const char* Source, size_t Size);
SDIF_API void      SdifKillStr (char* String);


SDIF_API void     SdifSetStdIOBinary (void);
SDIF_API FILE*    SdiffBinOpen       (const char * Name, SdifBinaryModeET Mode);
SDIF_API SdifInt4 SdiffBinClose      (FILE *f);



SDIF_API SdifHashTableT* SdifCreateHashTable(unsigned int HashSize, SdifHashIndexTypeET IndexType, void (*Killer)(void *));

SDIF_API void SdifMakeEmptyHashTable (SdifHashTableT* HTable);
SDIF_API void SdifKillHashTable      (SdifHashTableT* HTable);
SDIF_API unsigned int SdifHashTableGetNbData  (SdifHashTableT* HTable);

/*DOC:
  Allocate and initialise hash table iterator, return pointer to it */
SDIF_API SdifHashTableIteratorT* SdifCreateHashTableIterator (SdifHashTableT *HTable);
/*DOC:
  Deallocate hash table iterator created with SdifCreateHashTableIterator */
SDIF_API void SdifKillHashTableIterator (SdifHashTableIteratorT *iter);
/*DOC:
  Initialise hash table iterator given by pointer.
  [Returns] true if hash table has elements. */
SDIF_API int  SdifHashTableIteratorInitLoop (SdifHashTableIteratorT *iter, 
					     SdifHashTableT *HTable);

/*DOC:
  Test if iterator has more elements */
SDIF_API int  SdifHashTableIteratorIsNext (SdifHashTableIteratorT *iter);

/*DOC:
  Return current Data pointer and advance iterator */
SDIF_API void* SdifHashTableIteratorGetNext (SdifHashTableIteratorT *iter);



/******************  eHashChar ****************/

SDIF_API unsigned int SdifHashChar(const char* s, unsigned int nchar, unsigned int HashSize);

SDIF_API void*           SdifHashTableSearchChar(SdifHashTableT* HTable, const char *s, unsigned int nchar);
SDIF_API SdifHashTableT* SdifHashTablePutChar   (SdifHashTableT* HTable, const char *s, unsigned int nchar, void* Data);


/***************** eHashInt4 **********************/

SDIF_API unsigned int SdifHashInt4(unsigned int i, unsigned int HashSize);

SDIF_API void*           SdifHashTableSearchInt4(SdifHashTableT* HTable, unsigned int i);
SDIF_API SdifHashTableT* SdifHashTablePutInt4   (SdifHashTableT* HTable, const unsigned int i, void* Data);


/*************************** for all ***********************/

SDIF_API void*           SdifHashTableSearch (SdifHashTableT* HTable, void *ptr, unsigned int nobj);
SDIF_API SdifHashTableT* SdifHashTablePut    (SdifHashTableT* HTable, const void *ptr, unsigned int nobj, void* Data);




/*
//FUNCTION GROUP:  High-Level I/O Functions
*/

/*DOC:
  Definition of the callback function types, used for SdifReadSimple. 
  SdifOpenFileCallbackT returns flag if rest of file should be read.
*/
typedef int (*SdifOpenFileCallbackT)   (SdifFileT *file, void *userdata);
typedef int (*SdifCloseFileCallbackT)  (SdifFileT *file, void *userdata);
typedef int (*SdifFrameCallbackT)      (SdifFileT *file, void *userdata);
typedef int (*SdifMatrixCallbackT)     (SdifFileT *file, 
                                        int nummatrix,   void *userdata);
typedef int (*SdifMatrixDataCallbackT) (SdifFileT *file, 
                                        int nummatrix,   void *userdata);

/*DOC: 
  Reads an entire SDIF file, calling matrixfunc for each matrix in the
  SDIF selection taken from the filename.  Matrixfunc is called with
  the SDIF file pointer, the matrix count within the current frame,
  and the userdata unchanged. 

  no row/column selection yet!
  
  @return number of bytes read
*/
SDIF_API size_t SdifReadSimple (const char              *filename, 
                                SdifMatrixDataCallbackT  matrixfunc,
                                void                    *userdata);


SDIF_API size_t SdifReadFile   (const char              *filename, 
                                SdifOpenFileCallbackT    openfilefunc,
                                SdifFrameCallbackT       framefunc,
                                SdifMatrixCallbackT      matrixfunc,
                                SdifMatrixDataCallbackT  matrixdatafunc,
                                SdifCloseFileCallbackT   closefilefunc,
                                void                    *userdata);

/*DOC: 
  Reads matrix data and padding.  The data is stored in CurrMtrxData,
  for which the library will allocate enough space for the data of one
  matrix, accessible by SdifFCurrMatrixData().  
  
  @return       number of bytes read or 0 if error
                N.B. first of all that an error is signalled to the error callback
                set with SdifSetErrorFunc, and the library tries to exit via the 
                function set with SdifSetExitFunc.  So, if you have to check the
                return value, note that for matrices with 0 rows or 0 columns,
                a return value of 0 is correct.  
                You should thus check for this with:

  if (nread == 0  &&  (SdifFCurrNbRow(file) != 0  ||  SdifFCurrNbCol(file) != 0))
      --> read problem

  [Precondition:] 
  Matrix header must have been read with SdifFReadMatrixHeader.  
*/
SDIF_API size_t SdifFReadMatrixData   (SdifFileT *file);




/*
//FUNCTION GROUP:  Querying SDIF Files
*/

typedef struct
{ 
    double min, max;    /* use double even for int, doesn't harm */
} SdifMinMaxT;

/* two-level tree node for matrices in frames */
typedef struct SdifQueryTreeElemS
{
    /* common fields */
    SdifSignature sig;
    int           count;
    int           parent;/* -1 for frames, index to parent frame for matrices */

    /* frame fields */
    int           stream;
    SdifMinMaxT   time, nmatrix;

    /* matrix fields */
    SdifMinMaxT   ncol, nrow;

} SdifQueryTreeElemT;


/* SdifQueryTreeT counts occurence of signatures as frame or matrix under
   different parent frames. */
typedef struct
{
    int                 num;            /* number of elems used */
    int                 nummatrix;      /* number of leaf nodes */
    int                 current;        /* index of current frame */
    int                 allocated;      /* number of elems allocated */
    SdifQueryTreeElemT *elems;
    SdifMinMaxT         time;           /* frame times */
} SdifQueryTreeT;


/* allocate query tree, starting with max elements (will be reallocated dynamically) */
SDIF_API SdifQueryTreeT *SdifCreateQueryTree(int max);

/* clean all elements from tree */
SDIF_API SdifQueryTreeT *SdifInitQueryTree(SdifQueryTreeT *tree);

/* free memory for tree and its elements */
SDIF_API void SdifFreeQueryTree(SdifQueryTreeT *tree);

/* create summary of file's data in query tree, return bytesize of file */
SDIF_API size_t SdifQuery (const char            *filename, 
                           SdifOpenFileCallbackT  openfilefunc,
                /*out*/    SdifQueryTreeT        *tree);





#if 0   /* TBI */

/*
//FUNCTION GROUP: to be implemented / TBI
*/


/*DOC: 
  Write whole matrix, given as separate columns in array "columns" of
  pointer to "DataType".  Each columns [i], i = 0..NbCol-1, points to 
  NbRow * SdifSizeofDataType (DataType) bytes.  
  TBI 
*/
SdifFWriteMatrixColumns (SdifFileT     *file,
                         SdifSignature  Signature,
                         SdifDataTypeET DataType,
                         SdifUInt4      NbRow,
                         SdifUInt4      NbCol,
                         void          *columns []);


/*DOC: 
  Reads matrix header and data into memory allocated by the library,
  accessible by SdifFCurrMatrixData (). */
int SdifFReadMatrix (SdifFileT *file);

void *SdifGetColumn ();



/*
 * Error handling (sketch TBI)
 */

int /*bool*/ SdifFCheckStatus (SdifFileT *file)
{
  return (SdifLastError (file->ErrorList)) == NULL);
}


int /*bool*/ SdifFCheckStatusPrint (SdifFileT *file)
{
  SdifError err = SdifLastError (file->ErrorList));
  if (err != eNoError)
     print (SdifFsPrintFirstError (..., file, ...);
  return err == NULL;
}


/* --> test in SdifFReadGeneralHeader  (file) + SdifFReadAllASCIIChunks (file)
   if (!SdifFCheckStatus (file))
      SdifWarningAdd ("Followup error");
*/

#endif /* TBI */



/* stocks management */

SDIF_API void        SdifInitListNStock      (SdifListNStockT *Stock, unsigned int SizeOfOneStock);
SDIF_API void        SdifNewStock            (SdifListNStockT *Stock);
SDIF_API SdifListNT* SdifGetNewNodeFromTrash (SdifListNStockT *Stock);
SDIF_API SdifListNT* SdifGetNewNodeFromStock (SdifListNStockT *Stock);
SDIF_API SdifListNT* SdifGetNewNode          (SdifListNStockT *Stock);
SDIF_API void        SdifPutNodeInTrash      (SdifListNStockT *Stock, SdifListNT* OldNode);
SDIF_API SdifListNT* SdifKillListNStock      (SdifListNT* OldStock);
SDIF_API void        SdifListNStockMakeEmpty (SdifListNStockT *Stock);

/* global variable gSdifListNodeStock */

extern SDIF_API SdifListNStockT gSdifListNodeStock;
SDIF_API SdifListNStockT* SdifListNodeStock  (void);
SDIF_API void    SdifInitListNodeStock       (unsigned int SizeOfOneStock);
SDIF_API void    SdifDrainListNodeStock      (void);


/* nodes management */

SDIF_API SdifListNT* SdifCreateListNode  (SdifListNT *Next, void *Data);
SDIF_API SdifListNT* SdifKillListNode    (SdifListNT *Node, KillerFT Killer);



/* lists management */

SDIF_API SdifListT*  SdifCreateList      (KillerFT Killer);
SDIF_API SdifListT*  SdifKillListHead    (SdifListT* List);
SDIF_API SdifListT*  SdifKillListCurr    (SdifListT* List);
SDIF_API SdifListT*  SdifMakeEmptyList   (SdifListT* List);
SDIF_API void        SdifKillList        (SdifListT* List);

/*DOC:
  Init the function SdifListGetNext. 
  [Return] head of List. */
SDIF_API void*       SdifListGetHead     (SdifListT* List); 

SDIF_API void*       SdifListGetTail     (SdifListT* List);
SDIF_API int         SdifListIsNext      (SdifListT* List);
SDIF_API int         SdifListIsEmpty     (SdifListT* List);
SDIF_API unsigned int SdifListGetNbData  (SdifListT* List);

/*DOC:
  Init for function SdifListGetNext.
  [Returns] true if List has elements. */
SDIF_API int         SdifListInitLoop    (SdifListT* List);

/*DOC:
  Set Curr to Curr->Next and after return Curr->Data */
SDIF_API void*       SdifListGetNext     (SdifListT* List);

/*DOC:
  Only return Curr->Data. */
SDIF_API void*       SdifListGetCurr     (SdifListT* List);

SDIF_API SdifListT*  SdifListPutTail     (SdifListT* List, void *pData);
SDIF_API SdifListT*  SdifListPutHead     (SdifListT* List, void *pData);

/*DOC:
  append list b to list a 

  WARNING: This creates double references to the data! */
SDIF_API SdifListT *SdifListConcat(SdifListT *a, SdifListT *b);




SDIF_API SdifMatrixHeaderT* SdifCreateMatrixHeader    (SdifSignature Signature, 
                                              SdifDataTypeET DataType,
                                              SdifUInt4 NbRow, 
                                              SdifUInt4 NbCol);

SDIF_API SdifMatrixHeaderT* SdifCreateMatrixHeaderEmpty (void);
SDIF_API void               SdifKillMatrixHeader        (SdifMatrixHeaderT *MatrixHeader);


/*
 * OneRow class
 */

SDIF_API SdifOneRowT*       SdifCreateOneRow          (SdifDataTypeET DataType, SdifUInt4  NbGranuleAlloc);
SDIF_API SdifOneRowT*       SdifReInitOneRow          (SdifOneRowT *OneRow, SdifDataTypeET DataType, SdifUInt4 NbData);
SDIF_API void               SdifKillOneRow            (SdifOneRowT *OneRow);

/* row element access */

SDIF_API SdifOneRowT*       SdifOneRowPutValue        (SdifOneRowT *OneRow, SdifUInt4 numCol, SdifFloat8 Value);
SDIF_API SdifFloat8         SdifOneRowGetValue        (SdifOneRowT *OneRow, SdifUInt4 numCol);
SDIF_API SdifFloat8         SdifOneRowGetValueColName (SdifOneRowT *OneRow, SdifMatrixTypeT *MatrixType, char * NameCD);


/*
 * matrix data class 
 */

SDIF_API SdifMatrixDataT*   SdifCreateMatrixData      (SdifSignature Signature, 
                                              SdifDataTypeET DataType,
                                              SdifUInt4 NbRow, 
                                              SdifUInt4 NbCol);

SDIF_API void               SdifKillMatrixData        (SdifMatrixDataT *MatrixData);

/* see if there's enough space for data, if not, grow buffer */
SDIF_API int                SdifMatrixDataRealloc     (SdifMatrixDataT *data, 
                                              int newsize);

/* matrix data element access by index (starting from 1!) */

SDIF_API SdifMatrixDataT*   SdifMatrixDataPutValue    (SdifMatrixDataT *MatrixData,
                                              SdifUInt4  numRow, 
                                              SdifUInt4  numCol, 
                                              SdifFloat8 Value);

SDIF_API SdifFloat8         SdifMatrixDataGetValue    (SdifMatrixDataT *MatrixData,
                                              SdifUInt4  numRow, 
                                              SdifUInt4  numCol);

/* matrix data element access by column name */

SDIF_API SdifMatrixDataT *  SdifMatrixDataColNamePutValue (SdifHashTableT *MatrixTypesTable,
                                                  SdifMatrixDataT *MatrixData,
                                                  SdifUInt4  numRow,
                                                  char *ColName,
                                                  SdifFloat8 Value);

SDIF_API SdifFloat8         SdifMatrixDataColNameGetValue (SdifHashTableT *MatrixTypesTable,
                                                  SdifMatrixDataT *MatrixData,
                                                  SdifUInt4  numRow,
                                                  char *ColName);

SDIF_API void      SdifCopyMatrixDataToFloat4    (SdifMatrixDataT *data, 
                                                  SdifFloat4      *dest);


SDIF_API SdifColumnDefT*  SdifCreateColumnDef (const char *Name,  unsigned int Num);
SDIF_API void             SdifKillColumnDef   (void *ColumnDef);

/*DOC: 
  premet de créer un objet 'type de matrice'. Le premier argument
  est la signature de ce type. Le second est l'objet 'type de matrice'
  prédéfini dans SDIF.<p>
  
  <strong>Important: Tous les types de matrices ou de frames utilisés
  dans une instance de SdifFileT doivent être ajoutés aux tables de
  cette instance, de façon a créer le lien avec les types
  prédéfinis.</strong> L'hors de la lecture des entêtes avec les
  fonctions SdifFReadMatrixHeader et SdifFReadFrameHeader, cette mise
  à jour se fait automatiquement à l'aide des fonctions
  SdifTestMatrixType et SdifTestFrameType. */
SDIF_API SdifMatrixTypeT* SdifCreateMatrixType              (SdifSignature Signature,
                                                                           SdifMatrixTypeT *PredefinedMatrixType);
SDIF_API void             SdifKillMatrixType                (SdifMatrixTypeT *MatrixType);

/*DOC: 
  permet d'ajouter une colonne à un type (toujours la dernière
  colonne).  */
SDIF_API SdifMatrixTypeT* SdifMatrixTypeInsertTailColumnDef (SdifMatrixTypeT *MatrixType, const char *NameCD);

/*DOC: 
  Return number of columns defined for given matrix type. */
SdifUInt4 SdifMatrixTypeGetNbColumns (SdifMatrixTypeT *mtype);

/*DOC: 
  Get index (starting from 1) of the column given by NameCD (0 if not found) */
SDIF_API SdifUInt4        SdifMatrixTypeGetNumColumnDef     (SdifMatrixTypeT *MatrixType, const char *NameCD);

/*DOC: 
  Get definition of column from NameCD (NULL if not found) */
SDIF_API SdifColumnDefT*  SdifMatrixTypeGetColumnDef        (SdifMatrixTypeT *MatrixType, const char *NameCD);

/*DOC: 
  Get definition of column from index (starting from 1) (NULL if not found) */
SDIF_API SdifColumnDefT*  SdifMatrixTypeGetNthColumnDef     (SdifMatrixTypeT *MatrixType, SdifUInt4 NumCD);

/*DOC: 
  Return pointer to name of column at index, NULL if it doesn't exist. */
SDIF_API const char*  SdifMatrixTypeGetColumnName           (SdifMatrixTypeT *MatrixType, int index);


/*DOC: 
  renvoie le type de matrice en fonction de la Signature. Renvoie
  NULL si le type est introuvable. Attention, si Signature est la
  signature d'un type prédéfini,
  SdifGetMatrixType(SdifF->MatrixTypeTable,Signature) renvoie NULL si
  le lien avec entre SdifF et gSdifPredefinedType n'a pas été mis à
  jour.  

  Tip: use SdifFGetMatrixTypesTable to obtain the matrix types hash table.
*/
SDIF_API SdifMatrixTypeT* SdifGetMatrixType                 (SdifHashTableT *MatrixTypesTable, 
                                                    SdifSignature Signature);

/*DOC: 
  permet d'ajouter un type de matrice dans une table.  */
SDIF_API void             SdifPutMatrixType(SdifHashTableT *MatrixTypesTable, SdifMatrixTypeT* MatrixType);
SDIF_API SdifUInt2        SdifExistUserMatrixType(SdifHashTableT *MatrixTypesTable);

/*DOC:
  Remark:
         This function implements the new SDIF Specification (June 1999):
       Name Value Table, Matrix and Frame Type declaration, Stream ID declaration are
       defined in text matrix:
       1NVT 1NVT
       1TYP 1TYP
       1IDS 1IDS
  Get all types from a SdifStringT
*/
SDIF_API size_t SdifFGetAllTypefromSdifString (SdifFileT *SdifF, 
                                               SdifStringT *SdifString);



/**
 * Get table of matrix type definitions, 
 * useful for SdifGetMatrixType. 
 *
 * @ingroup types
 */
SDIF_API SdifHashTableT *SdifFGetMatrixTypesTable(SdifFileT *file);

/**
 * Get table of frame type definitions declare in this file's header only, 
 * useful for SdifGetFrameType. 
 *
 * @ingroup types
 */
SDIF_API SdifHashTableT *SdifFGetFrameTypesTable(SdifFileT *file);



/*
 * Memory allocation wrappers
 */

#define SdifMalloc(_type) (_type*) malloc(sizeof(_type))

#define SdifCalloc(_type, _nbobj) (_type*) calloc(_nbobj, sizeof(_type))

#define SdifRealloc(_ptr, _type, _nbobj) (_type*) realloc(_ptr, sizeof(_type) * _nbobj)

#define SdifFree(_ptr) free(_ptr)




/*
 * NameValue
 */


SDIF_API SdifNameValueT* SdifCreateNameValue(const char *Name,  const char *Value);
SDIF_API void            SdifKillNameValue(SdifNameValueT *NameValue);




/*
 * NameValueTable
 */

SDIF_API SdifNameValueTableT* SdifCreateNameValueTable(  SdifUInt4 StreamID, 
                                                SdifUInt4 HashSize, 
                                                SdifUInt4 NumTable);
SDIF_API void            SdifKillNameValueTable          (void* NVTable);
SDIF_API SdifNameValueT* SdifNameValueTableGetNV         (SdifNameValueTableT* NVTable, const char *Name);
SDIF_API SdifNameValueT* SdifNameValueTablePutNV         (SdifNameValueTableT* NVTable, const char *Name,  const char *Value);
SDIF_API SdifFloat8      SdifNameValueTableGetTime       (SdifNameValueTableT* NVTable);
SDIF_API SdifUInt4       SdifNameValueTableGetNumTable   (SdifNameValueTableT* NVTable);
SDIF_API SdifUInt4       SdifNameValueTableGetStreamID  (SdifNameValueTableT* NVTable);



/*
 * NameValueTableList
 */

SDIF_API SdifNameValuesLT*   SdifCreateNameValuesL       (SdifUInt4  HashSize);
SDIF_API void                SdifKillNameValuesL         (SdifNameValuesLT *NameValuesL);

/*DOC: 
  Cette fonction permet d'ajouter une nouvelle NVT dans la liste
  de tables passée par argument:
  <code>SdifNameValuesLNewHT(SdifF->NamefValues);</code><br>
  Attention, à l'ouverture de SdifF, il n'y a aucune table dans
  SdifF->NamefValues. Il faudra donc au moins en ajouter une pour
  pouvoir y mettre des NameValue.  */
SDIF_API SdifNameValuesLT*   SdifNameValuesLNewTable     (SdifNameValuesLT *NameValuesL, SdifUInt4 StreamID);

/*DOC: 
  Cette fonction permet de définir la nième NVT de la liste des
  tables comme NVT courante.  */
SDIF_API SdifNameValueTableT*SdifNameValuesLSetCurrNVT   (SdifNameValuesLT *NameValuesL, SdifUInt4 NumCurrNVT);


/*DOC:
  Kill current NVT from list of NVTs.  
  Warning: current nvt is no longer valid afterwards. 
           call SdifNameValuesLSetCurrNVT again */
SDIF_API void SdifNameValuesLKillCurrNVT(SdifNameValuesLT *NameValuesL);


/*DOC: 
  Cette fonction permet de récupérer une Name-Value de la liste
  des NVTs en passant le Name en argument.  Dans le cas ou Name est
  référencé dans plusieurs NVT, alors c'est la première NVT le
  contenant qui sera prise en compte.  Le pointeur retourné est de
  type SdifNameValueT qui contient deux champs: Name et Value.  */
SDIF_API SdifNameValueT*     SdifNameValuesLGet          (SdifNameValuesLT *NameValuesL, char *Name);

/*DOC: 
  Cette fonction réalise aussi une requête en fonction de Name
  mais uniquement dans la NVT courante.  */
SDIF_API SdifNameValueT*     SdifNameValuesLGetCurrNVT   (SdifNameValuesLT *NameValuesL, const char *Name);

/*DOC: 
  Cette fonction permet d'ajouter une NameValue à table courante
  qui est la dernière table créée ou celle définie en tant que table
  courante. Name et Value doivent être des chaines caractères ASCII
  sans espacements.  */
SDIF_API SdifNameValueT*     SdifNameValuesLPutCurrNVT   (SdifNameValuesLT *NameValuesL, const char *Name,  const char *Value);

/*DOC: 
  Add a Name-Value pair to the current Name-Value Table, while
  replacing reserved characters and spaces with underscores "_" 
  (using SdifStringToNV).  FYI: The strings are copied. */
SDIF_API SdifNameValueT*     SdifNameValuesLPutCurrNVTTranslate(SdifNameValuesLT *NameValuesL, const char *Name,  const char *Value);

SDIF_API SdifUInt2           SdifNameValuesLIsNotEmpty   (SdifNameValuesLT *NameValuesL);

/*DOC:
  Get generic SDIF list with SdifNameValueTableT nvt elements from nvt container struct nvtl */
SDIF_API SdifListT *         SdifNameValueTableList (SdifNameValuesLT *nvtl);

/*DOC:
  Get pointer to hash table of one nvt */
SDIF_API SdifHashTableT*     SdifNameValueTableGetHashTable (SdifNameValueTableT* NVTable);

/*DOC:
  Get name string of Name--Value entry */
SDIF_API char *SdifNameValueGetName (SdifNameValueT *nv);

/*DOC:
  Get value string of Name--Value entry */
SDIF_API char *SdifNameValueGetValue (SdifNameValueT *nv);



#define   M_1FQ0_Frequency  "Frequency"
#define   M_1FQ0_Mode       "Mode"
#define   M_1FQ0_Hit        "Hit"

#define   M_1FOF_Frequency  "Frequency"
#define   M_1FOF_Amplitude  "Amplitude"
#define   M_1FOF_BandWidth  "BandWidth"
#define   M_1FOF_Tex        "Tex"
#define   M_1FOF_DebAtt     "DebAtt"
#define   M_1FOF_Atten      "Atten"
#define   M_1FOF_Phase      "Phase"

#define   M_1CHA_Channel1   "Channel1"
#define   M_1CHA_Channel2   "Channel2"
#define   M_1CHA_Channel3   "Channel3"
#define   M_1CHA_Channel4   "Channel4"

#define   M_1RES_Frequency  "Frequency"
#define   M_1RES_Amplitude  "Amplitude"
#define   M_1RES_BandWidth  "BandWidth"
#define   M_1RES_Saliance   "Saliance"
#define   M_1RES_Correction "Correction"

#define   M_1DIS_Distribution    "Distribution"
#define   M_1DIS_Amplitude  "Amplitude"

SDIF_API SdifFrameTypeT* CreateF_1FOB(void);
SDIF_API SdifFrameTypeT* CreateF_1REB(void);
SDIF_API SdifFrameTypeT* CreateF_1NOI(void);
SDIF_API void SdifCreatePredefinedTypes(SdifHashTableT *MatrixTypesHT,
                                      SdifHashTableT *FrameTypesHT);





/*************** Matrix Type ***************/

SDIF_API void SdifPrintMatrixType(FILE *fw, SdifMatrixTypeT *MatrixType);
SDIF_API void SdifPrintAllMatrixType(FILE *fw, SdifFileT *SdifF);

/*************** Frame Type ***************/

SDIF_API void SdifPrintFrameType(FILE *fw, SdifFrameTypeT *FrameType);
SDIF_API void SdifPrintAllFrameType(FILE *fw, SdifFileT *SdifF);

/********** Matrix **********/

SDIF_API void SdifPrintMatrixHeader(FILE *f, SdifMatrixHeaderT *MatrixHeader);
SDIF_API void SdifPrintOneRow(FILE *f, SdifOneRowT *OneRow);
SDIF_API void SdifPrintMatrixRows(FILE* f, SdifMatrixDataT *MatrixData);

/********** Frame ***********/

SDIF_API void SdifPrintFrameHeader(FILE *f, SdifFrameHeaderT* FrameHeader);

/************ High ***********/

SDIF_API void SdifPrintAllType(FILE *fw, SdifFileT *SdifF);




/*DOC:
  Return true if c is a reserved char. 
*/
SDIF_API int SdifIsAReservedChar (char c);

/*DOC: 
  Convert str <strong>in place</strong> so that it doesn't
  contain any reserved chars (these become '.') or spaces (these
  become '_').

  [] returns str
*/
SDIF_API char *SdifStringToNV (/*in out*/ char *str);

/* SdiffGetString lit un fichier jusqu'a un caractere reserve, ne
   rempli s que des caracteres non-espacement, renvoie le caractere
   reserve, saute les premiers caracteres espacement lus.  Il y a
   erreur si fin de fichier ou si un caractere non-espacement et
   non-reseve est lu apres un caractere espacement.  ncMax est
   typiquement strlen(s)+1.  
*/
SDIF_API int SdiffGetString      (FILE* fr, char* s, size_t ncMax, size_t *NbCharRead);

/* retourne le caractere d'erreur */
SDIF_API int SdiffGetSignature   (FILE* fr, SdifSignature *Signature, size_t *NbCharRead);
/*DOC:
  Function return the signature in a SdifStringT
*/
SDIF_API int SdiffGetSignaturefromSdifString(SdifStringT *SdifString, SdifSignature *Signature);

SDIF_API int SdiffGetWordUntil   (FILE* fr, char* s, size_t ncMax, size_t *NbCharRead, const char *CharsEnd);
/*DOC:
  Function return the word until in a SdifStringT
*/
SDIF_API int SdiffGetWordUntilfromSdifString(SdifStringT *SdifString, char* s, size_t ncMax,const char *CharsEnd);

SDIF_API int SdiffGetStringUntil (FILE* fr, char* s, size_t ncMax, size_t *NbCharRead, const char *CharsEnd);
/*DOC:
  Function return the string until in a SdifStringT
 */
SDIF_API int SdiffGetStringUntilfromSdifString(SdifStringT *SdifString, char *s, size_t ncMax,
                                     const char *CharsEnd);

SDIF_API int SdiffGetStringWeakUntil(FILE* fr, char* s, size_t ncMax, size_t *NbCharRead, const char *CharsEnd);
/*DOC:
  Return the weak string until in a SdifStringT
*/
SDIF_API int SdiffGetStringWeakUntilfromSdifString(SdifStringT *SdifString, char* s,
                                          size_t ncMax, const char *CharsEnd);

SDIF_API int SdifSkipASCIIUntil  (FILE* fr, size_t *NbCharRead, char *CharsEnd);
SDIF_API int SdifSkipASCIIUntilfromSdifString  (SdifStringT *SdifString, size_t *NbCharRead, char *CharsEnd);


#if 0   /* for cocoon's eyes only */
/* scan nobj items of TYPE from stream, return number sucessfully read */
size_t SdiffScan_TYPE   (FILE *stream, Sdif_TYPE  *ptr, size_t nobj);
size_t SdiffScanFloat4  (FILE *stream, SdifFloat4 *ptr, size_t nobj);
size_t SdiffScanFloat8  (FILE *stream, SdifFloat8 *ptr, size_t nobj);
#endif

#ifndef SWIG    /* are we scanned by SWIG? */

/* generate function prototypes for all types TYPE for the 
   SdiffScan<TYPE> functions */

#define sdif_scanproto(type) \
SDIF_API size_t SdiffScan##type (FILE *stream, Sdif##type *ptr, size_t nobj)

sdif_proto_foralltypes (sdif_scanproto)

#endif /* SWIG */


/* Unsafe but optimized version of SdifStringToSignature:
   Exactly 4 chars are considered, so make sure *str has at least that many! 
   The str pointer MUST be word (at least 4 byte or so) aligned.
*/
SDIF_API SdifSignature _SdifStringToSignature (const char *str);

/*DOC:
  Convert a string to an SDIF signature (in proper endianness).
  str can point to any string position of any length.  
*/
SDIF_API SdifSignature SdifStringToSignature (const char *str);






/*DOC:
  Return pointer to start of filename component in path inPathFileName.
 */
SDIF_API char *SdifBaseName (const char* inPathFileName);


/* 
// FUNCTION GROUP:      Init/Deinit
 */

/* init module, called by SdifGenInit */
SDIF_API int SdifInitSelect (void);

/*DOC: 
  Allocate space for an sdif selection.
*/
SDIF_API SdifSelectionT *SdifCreateSelection (void);

/*DOC: 
*/
SDIF_API int SdifInitSelection (SdifSelectionT *sel, const char *filename, int namelen);

/*DOC: 
*/
SDIF_API int SdifFreeSelection (SdifSelectionT *sel);

/*DOC:
  Killer function for SdifKillList: free one SdifSelectElement 
*/
SDIF_API void SdifKillSelectElement (/*SdifSelectionT*/ void *victim);



/*
// FUNCTION GROUP:      Parse and Set Selection
*/


/*DOC: 
  Returns pointer to first char of select spec (starting with ::), 
  or NULL if not found.
*/
SDIF_API char *SdifSelectFindSelection (const char *filename);


/*DOC: 
*/
SDIF_API char *SdifGetFilenameAndSelection (/*in*/  const char *filename, 
                                            /*out*/ SdifSelectionT *sel);

/*DOC: 
  Replace current selection by new one given in first argument.
  The selection specification may contain all the parts of a filename
  based selection after the  selection indicator :: .
*/
SDIF_API void SdifReplaceSelection (/*in*/ const char* selectionstr,
                           /*out*/ SdifSelectionT *sel);


/*DOC: 
*/
SDIF_API void SdifPrintSelection (FILE *out, SdifSelectionT *sel, int options);



/*DOC:
  Parse comma-separated list of signatures into list of SdifSelectElementT
  [Return] true if ok 

  List has to be created before with
        list = SdifCreateList (SdifKillSelectElement)
*/
SDIF_API int SdifParseSignatureList (SdifListT *list, const char *str);



/*
// FUNCTION GROUP:      Add Selections to Element Lists
*/

/* Give documentation and fake prototype for _add... macro generated functions.
   Cocoon ignores the #if 0.
*/
#if 0

/*DOC:
  Create and add one value to selection element list.  There are four 
  functions generated automatically, with the meta type-variables _TYPE_ and 
  _datatype_:
  [] _TYPE_ is one of:  <br> Int, Real,   Signature,     String, for
  [] _datatype_ of:     <br> int, double, SdifSignature, char *, respectively.

  Example: to add a matrix signature XSIG to the selection in file, call
        SdifSelectAddSignature(file->Selection->matrixsig, SdifStringToSignature("XSIG"));
*/
void SdifSelectAdd_TYPE_ (SdifListT *List, _datatype_ Value);

/*DOC:
  Create and add one range to selection element list.  There are four 
  functions generated automatically, with the meta type-variables _TYPE_ and 
  _datatype_:
  [] _TYPE_ is one of:  <br> Int, Real,   Signature,     String, for
  [] _datatype_ of:     <br> int, double, SdifSignature, char *, respectively.

  Example: to add the time range (t1, t2) to the selection in file, call
        SdifSelectAddRealRange(file->Selection->time, t1, sst_range, t2);
*/
void SdifSelectAdd_TYPE_Range (SdifListT *List, 
                               _datatype_ Value, 
                               SdifSelectTokens Rt, 
                               _datatype_ Range);

#endif  /* if 0 */

/* two following macros generate the prototype declarations for any _type_: */

#define _addrangeproto(_name_, _type_, _field_) \
void SdifSelectAdd##_name_##Range (SdifListT *List, \
                               _type_ Value, SdifSelectTokens Rt, _type_ Range)

#define _addsimpleproto(_name_, _type_, _field_) \
void SdifSelectAdd##_name_ (SdifListT *List, _type_ Value)

#define _addproto(_name_, _type_, _field_) \
SDIF_API _addsimpleproto  (_name_, _type_, _field_); \
SDIF_API _addrangeproto   (_name_, _type_, _field_);

/* expand macros for each _type_ */
_addproto (Int,       int,              integer)
_addproto (Real,      double,           real)
_addproto (Signature, SdifSignature,    signature)
_addproto (String,    char *,           string)


/*DOC: 
  copy a selection list source, appending to an existing one dest
  the same but freshly allocated elements from source

  @return dest
*/
SDIF_API SdifListT *SdifSelectAppendList (SdifListT *dest, SdifListT *source);

/*DOC:
  convert list of int selections to mask 

  do this whenever elements have been added to an int selection list
*/
SDIF_API void SdifSelectGetIntMask (SdifListP list, SdifSelectIntMaskP mask);




/*
// FUNCTION GROUP:      Query parsed ranges (list of ranges).
*/

/*DOC:
  Query parsed ranges (list of ranges) for a selection element (one of
  the SdifListP lists in SdifSelectionT).  Init list traversal with
  SdifListInitLoop, then call SdifSelectGetNext<type>(list) until it
  returns 0.

  The number of selections in the list is SdifListGetNbData(list), if
  it is 0, or SdifListIsEmpty(list) is true, then there was no
  selection for that element.

  If force_range is 1, the out value is converted to a range in any
  case, with value <= range guaranteed.  
*/
SDIF_API int SdifSelectGetNextIntRange  (/*in*/  SdifListP list, 
                                /*out*/ SdifSelectElementIntT  *range, 
                                /*in*/  int force_range);

/*DOC: 
  See SdifSelectGetNextInt.
*/
SDIF_API int SdifSelectGetNextRealRange (/*in*/  SdifListP list, 
                                /*out*/ SdifSelectElementRealT *range, 
                                /*in*/  int force_range);

/*DOC: 
  Query list of parsed selection elements (one of the SdifListP
  lists in SdifSelectionT).  Init list traversal with
  SdifListInitLoop, then call SdifSelectGetNext<type>(list) until it
  returns 0.

  See also SdifSelectGetNextInt.  
*/
SDIF_API SdifSignature  SdifSelectGetNextSignature (/*in*/  SdifListP list);

/*DOC: 
  See SdifSelectGetNextSignature.
*/
SDIF_API char          *SdifSelectGetNextString    (/*in*/  SdifListP list);


/*DOC: 
  Return value of first selection (ignoring range).
*/
SDIF_API int            SdifSelectGetFirstInt       (SdifListP l, int defval);
SDIF_API double         SdifSelectGetFirstReal      (SdifListP l, double defval);
SDIF_API char          *SdifSelectGetFirstString    (SdifListP l, char *defval);
SDIF_API SdifSignature  SdifSelectGetFirstSignature (SdifListP l, SdifSignature defval);





/*
// FUNCTION GROUP:      Selection Testing Functions
*/

SDIF_API int SdifSelectTestIntMask (SdifSelectIntMaskT *mask, SdifUInt4 cand);

SDIF_API int SdifSelectTestIntRange  (SdifSelectElementT *elem, SdifUInt4 cand);
SDIF_API int SdifSelectTestRealRange (SdifSelectElementT *elem, double cand);

SDIF_API int SdifSelectTestInt (SdifListT *list, SdifUInt4 cand);
SDIF_API int SdifSelectTestReal (SdifListT *list, double cand);
SDIF_API int SdifSelectTestSignature (SdifListT *list, const SdifSignature cand);
SDIF_API int SdifSelectTestString (SdifListT *list, const char *cand);



/*
// FUNCTION GROUP:      Using a Selection in File I/O.
*/


/*DOC:
  Get number of selected streams in file selection, 0 for all  */
SDIF_API int SdifFNumStreamsSelected (SdifFileT *file);

/*DOC: 
  Get number of selected rows in file selection, or num rows in
  current matrix when all are selected.
  SdifFReadMatrixHeader must have been called before! */
SDIF_API int SdifFNumRowsSelected (SdifFileT *file);

/*DOC:
  Get number of selected columns in file selection, or num columns in
  current matrix when all are selected  
  SdifFReadMatrixHeader must have been called before! */
SDIF_API int SdifFNumColumnsSelected (SdifFileT *file);

/*DOC: 
  Read frame headers until a frame matching the file selection
  has been found or the end of the file has been reached.

  [Return] false if end of file was reached, true if data has been read. */
SDIF_API int SdifFReadNextSelectedFrameHeader (SdifFileT *f);



/*DOC: 
  Test the selection elements from sel applicable to frame FramH:
  time, stream, frame type. */
SDIF_API int SdifFrameIsSelected (SdifFrameHeaderT *FramH, SdifSelectionT *sel);

/*DOC:
  Test the selection elements from sel applicable to matrix MtrxH: 
  the matrix signature. */
SDIF_API int SdifMatrixIsSelected (SdifMatrixHeaderT *MtrxH, SdifSelectionT *sel);


/*DOC: 
  Test if the current frame header is in the file selection
  (automatically parsed from the filename).  
  Can be called after SdifFReadFrameHeader(). */
SDIF_API int SdifFCurrFrameIsSelected (SdifFileT *file);

/*DOC:
  Test if the current matrix header is in the file selection
  (automatically parsed from the filename).  
  Can be called after SdifFReadMatrixHeader(). */
SDIF_API int SdifFCurrMatrixIsSelected (SdifFileT *file);

/*DOC:
  Test file selection if a given row (starting from 1) is selected */
SDIF_API int SdifFRowIsSelected (SdifFileT *file, int row);

/*DOC:
  Test file selection if a given column (starting from 1) is selected */
SDIF_API int SdifFColumnIsSelected (SdifFileT *file, int col);




/*
// FUNCTION GROUP:      Handling of a Table of Signatures
*/

/*DOC:
  Create table for initially NbSignMax signatures. */
SDIF_API SdifSignatureTabT* SdifCreateSignatureTab (const SdifUInt4 NbSignMax);

/*DOC:
  Free signature table. */
SDIF_API void               SdifKillSignatureTab   (SdifSignatureTabT *SignTab);

/*DOC:
  Reallocate table to hold NewNbSignMax signatures. */
SDIF_API SdifSignatureTabT* SdifReAllocSignatureTab(SdifSignatureTabT *SignTab, 
                                           const SdifUInt4 NewNbSignMax);

/*DOC:
  Reallocate table to hold NewNbSignMax signatures and clear signatures. */
SDIF_API SdifSignatureTabT* SdifReInitSignatureTab (SdifSignatureTabT *SignTab, 
                                           const SdifUInt4 NewNbSignMax);

/*DOC:
  Add signature Sign, no overflow check. */
SDIF_API SdifSignatureTabT* SdifPutInSignatureTab  (SdifSignatureTabT *SignTab, 
                                           const SdifSignature Sign);

/*DOC:
  Add signature Sign, reallocate table if necessary. */
SDIF_API SdifSignatureTabT* SdifAddToSignatureTab  (SdifSignatureTabT *SignTab, 
                                           const SdifSignature Sign);

/*DOC:
  Get signature at position index.  
  Returns eEmptySignature if index out of bounds. */
SDIF_API SdifSignature      SdifGetFromSignatureTab(const SdifSignatureTabT* SignTab, 
                                           const int index);

/*DOC:
  Test if signature Sign is in table SignTab. 
  [] Returns Sign if yes, 0 (== eEmptySignature) if no. */
SDIF_API SdifSignature      SdifIsInSignatureTab   (const SdifSignatureTabT *SignTab, 
                                           const SdifSignature Sign);

/*DOC:
  Test if signature Sign is in table SignTab. 
  [] Returns index of Sign if yes, -1 if no. */
SDIF_API int                SdifFindInSignatureTab (const SdifSignatureTabT* SignTab, 
                                           const SdifSignature Sign);






/*
// DATA GROUP:          Stream ID Table and Entries for 1IDS ASCII chunk
*/


SDIF_API SdifStreamIDT* SdifCreateStreamID(SdifUInt4 NumID, char *Source, char *TreeWay);
SDIF_API void           SdifKillStreamID(SdifStreamIDT *StreamID);


/*DOC:
  Create a stream ID table.  <strong>The stream ID table of the SDIF
  file structure is created automatically by SdifFOpen().</strong> 
  It can be obtained by SdifFStreamIDTable(). */
SDIF_API SdifStreamIDTableT* SdifCreateStreamIDTable     (SdifUInt4 HashSize);

/*DOC:
  Deallocate a stream ID table.  <strong>The stream ID table of the SDIF
  file structure is killed automatically by SdifFClose.</strong>  
  It can be obtained by SdifFStreamIDTable. */
SDIF_API void                SdifKillStreamIDTable       (SdifStreamIDTableT *SIDTable);

/*DOC:
  Add an entry to a stream ID table.  The table will be written by
  SdifFWriteAllASCIIChunks.
  [in]  SIDTable pointer to stream ID table, e.g. obtained by SdifFStreamIDTable
  [in]  NumID   stream ID of the frames the stream ID table describes
  [in]  Source  Source identifier for the table (ex. "Chant")
  [in]  TreeWay Routing and parameters, separated by slashes
  [return]
                The stream ID table entry just created and added */
SDIF_API SdifStreamIDT*      SdifStreamIDTablePutSID     (SdifStreamIDTableT *SIDTable,
                                                 SdifUInt4           NumID, 
                                                 char               *Source, 
                                                 char               *TreeWay);

/*DOC:
  Retrieve an entry to a stream ID table.  The table has to have been
  read by SdifFReadAllASCIIChunks.

  [in]  SIDTable pointer to stream ID table, e.g. obtained by 
                 SdifFStreamIDTable
  [in]  NumID    stream ID of the frames the stream ID table describes
  [return]
                 pointer to stream ID table entry, or NULL if no entry for 
                 stream ID NumID exists. */
SDIF_API SdifStreamIDT*      SdifStreamIDTableGetSID     (SdifStreamIDTableT *SIDTable, 
                                                 SdifUInt4           NumID);

/*DOC:
  Return number of entries in stream ID table SIDTable */
SDIF_API SdifUInt4           SdifStreamIDTableGetNbData  (SdifStreamIDTableT *SIDTable);


/*DOC:
  Return stream ID field in stream ID table entry SID */
SDIF_API SdifUInt4           SdifStreamIDEntryGetSID     (SdifStreamIDT *SID);

/*DOC:
  Return source field in stream ID table entry SID */
SDIF_API char               *SdifStreamIDEntryGetSource  (SdifStreamIDT *SID);

/*DOC:
  Return "treeway" field in stream ID table entry SID */
SDIF_API char               *SdifStreamIDEntryGetTreeWay (SdifStreamIDT *SID);




/*
//FUNCTION GROUP: Sdif String Handling
*/

/* SdifString.h */

/* Function declaration */

/*DOC:
  Make a memory allocation for a SdifStringT structure.
*/
SDIF_API SdifStringT * SdifStringNew(void);


/*DOC:
  Free memory allocated for SdifString.
*/
SDIF_API void SdifStringFree(SdifStringT * SdifString);


/*DOC:
  Append a string to another one.
  Manage memory reallocation.
  [Return] a boolean for the succes of the function's call.
*/
SDIF_API int SdifStringAppend(SdifStringT * SdifString, const char *strToAppend);


/*DOC:
  Read the current char (= fgetc).
*/
SDIF_API int SdifStringGetC(SdifStringT * SdifString);


/*DOC:
  Equivalent of ungetc: put one character back into string, clear EOS condition
*/
SDIF_API int SdifStringUngetC(SdifStringT * SdifString);


/*DOC:
  Test the end of the string (= feof)
*/
SDIF_API int SdifStringIsEOS(SdifStringT *SdifString);



/*
typedef enum SdifInterpretationErrorE
{
  eTypeDataNotSupported= 300,
  eNameLength,
  eReDefined,
  eUnDefined,
  eSyntax,
  eRecursiveDetect,
  eBadTypesFile,
  eBadType,
  eBadHeader,
  eOnlyOneChunkOf,
  eUnInterpreted,
  eUserDefInFileYet,
  eBadMode,
  eBadStdFile,
  eBadNbData,
  eReadWriteOnSameFile
} SdifInterpretationErrorET;



void
SdifInterpretationError(SdifInterpretationErrorET Error, SdifFileT* SdifF, const void *ErrorMess);

#define _SdifFileMess(sdiff, error, mess) \
(SdifErrorFile = __FILE__, SdifErrorLine = __LINE__, SdifInterpretationError((error), (sdiff),(mess)))

*/

#define _SdifFileMess(sdiff, error, mess) 

/*DOC: 
  Cette fonction vérifie si le type de matrice est répertorié
  dans SdifF.<br> S'il ne l'est pas, alors elle vérifie si c'est un
  type prédéfinis. S'il est prédéfini, elle crée le lien de SdifF vers
  le type prédéfini. Sinon, elle envoie un message sur l'erreur
  standart.  */
SDIF_API SdifMatrixTypeT* SdifTestMatrixType (SdifFileT *SdifF, SdifSignature Signature);
SDIF_API SdifFrameTypeT*  SdifTestFrameType  (SdifFileT *SdifF, SdifSignature Signature);



SDIF_API int SdifFTestMatrixWithFrameHeader (SdifFileT* SdifF);
SDIF_API int SdifFTestDataType              (SdifFileT* SdifF);
SDIF_API int SdifFTestNbColumns             (SdifFileT* SdifF);
SDIF_API int SdifFTestNotEmptyMatrix        (SdifFileT* SdifF);
SDIF_API int SdifFTestMatrixHeader          (SdifFileT* SdifF);



SDIF_API SdifColumnDefT*  SdifTestColumnDef (SdifFileT *SdifF, SdifMatrixTypeT *MtrxT, const char *NameCD);
SDIF_API SdifComponentT*  SdifTestComponent (SdifFileT* SdifF, SdifFrameTypeT *FramT, const char *NameCD);

SDIF_API int SdifTestSignature            (SdifFileT *SdifF, int CharEnd, SdifSignature Signature, const char *Mess);
SDIF_API int SdifTestCharEnd              (SdifFileT *SdifF, int CharEnd, char MustBe,
                                           char *StringRead, int ErrCondition, const char *Mess);


SDIF_API int SdifTestMatrixTypeModifMode  (SdifFileT *SdifF, SdifMatrixTypeT *MatrixType);
SDIF_API int SdifTestFrameTypeModifMode   (SdifFileT *SdifF, SdifFrameTypeT *FrameType);



SDIF_API size_t SdifFTextConvMatrixData     (SdifFileT *SdifF);
SDIF_API size_t SdifFTextConvMatrix         (SdifFileT *SdifF);
SDIF_API size_t SdifFTextConvFrameData      (SdifFileT *SdifF);
SDIF_API size_t SdifFTextConvFrameHeader    (SdifFileT *SdifF);
SDIF_API size_t SdifFTextConvFrame          (SdifFileT *SdifF);
SDIF_API size_t SdifFTextConvAllFrame       (SdifFileT *SdifF);
SDIF_API size_t SdifFTextConvFramesChunk    (SdifFileT *SdifF);
SDIF_API size_t SdifFTextConv               (SdifFileT *SdifF);

/* upper level : open the text in read mode */

/*DOC: 
  Converti un fichier SDIF ouvert en lecture (eReadFile) en un fichier
  texte pseudo-SDIF de nom TextStreamName.  */
SDIF_API size_t SdifTextToSdif (SdifFileT *SdifF, char *TextStreamName);




/* SdifFPrint */

SDIF_API size_t SdifFPrintGeneralHeader      (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintNameValueLCurrNVT  (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintAllNameValueNVT    (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintAllType            (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintAllStreamID        (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintAllASCIIChunks     (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintMatrixHeader       (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintFrameHeader        (SdifFileT *SdifF);
SDIF_API size_t SdifFPrintOneRow             (SdifFileT *SdifF);

SDIF_API size_t SdifFPrintMatrixType         (SdifFileT *SdifF, SdifMatrixTypeT *MatrixType);
SDIF_API size_t SdifFPrintFrameType          (SdifFileT *SdifF, SdifFrameTypeT  *FrameType);

/* SdifFPut */

SDIF_API int SdifFAllFrameTypeToSdifString   (SdifFileT *SdifF, SdifStringT *SdifString);
SDIF_API int SdifFAllMatrixTypeToSdifString  (SdifFileT *SdifF, SdifStringT *SdifSTring);

#ifdef __cplusplus
}
#endif

#endif /* _SDIF_H */
