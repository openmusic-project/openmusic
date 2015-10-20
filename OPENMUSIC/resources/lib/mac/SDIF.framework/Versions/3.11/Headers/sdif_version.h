/* $Id: sdif_version.h.in,v 1.5 2003/05/24 00:08:45 roebel Exp $ -*-c-*-

   sdif_version.h.in generates sdif_version.h with the right 
   version defines taken from configure.in

   $Log: sdif_version.h.in,v $
   Revision 1.5  2003/05/24 00:08:45  roebel
   Renamed VERSION to SDIF_VERSION_STRING which is more specific and
   prevents name claches with other libraries that will include SDIF
   for example Easdif.

   Revision 1.4  2003/05/23 17:15:21  schwarz
   VERSION is still used in SdifFile, so this is necessary for
   non-configured builds like on Mac.

   Revision 1.3  2003/05/23 16:14:43  schwarz
   Added definition of total VERSION string.
*/


#ifndef _SDIF_VERSION_H
#define _SDIF_VERSION_H 1

/* major version of sdif library */
#define SDIF_VERSION_MAJOR   3 

/* minor version of sdif library */
#define SDIF_VERSION_MINOR   11 

/* release number of current version of sdif library */
#define SDIF_VERSION_RELEASE 5

/* current version of sdif library */
#define SDIF_VERSION_STRING  "3.11.5"

#define TOSTRING(x) #x

#endif
