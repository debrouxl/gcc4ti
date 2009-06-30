#ifndef __VERSION_H
#define __VERSION_H

#define __TIGCC__ 0
#define __TIGCC_MINOR__ 96
#define __TIGCC_SP__ 0
#define __TIGCC_BETA__ 10
#define __TIGCC_VERSION__ 96
#define __TIGCC_VERSION_STRING__ "TIGCC0.96Beta10"

#define __TIGCCLIB__ 2
#define __TIGCCLIB_MINOR__ 72
#define __TIGCCLIB_SP__ 0
#define __TIGCCLIB_VERSION__ 272
#define __TIGCCLIB_VERSION_STRING__ "LIB2.72"

#ifdef EMBED_VERSION_STRINGS
char __tigcc_version__[]=__TIGCC_VERSION_STRING__;
char __tigcclib_version__[]=__TIGCCLIB_VERSION_STRING__;
#endif

#endif
