#ifndef __CERT
#define __CERT

#include <default.h>

/* Begin Auto-Generated Part */
#define H_NULL 0
#define NULL ((void*)0)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
typedef struct{unsigned short Field;unsigned short HdrLen;unsigned long Len;void*Data;}CERT_FIELD;
typedef struct{void*Start,*Pos,*End;short EOFVal;}CFILE;
#define ceof _rom_call(short,(CFILE*),128)
#define cfindfield _rom_call(short,(CFILE*,short,CERT_FIELD*),12A)
#define cgetc _rom_call(unsigned char,(CFILE*),12B)
#define cgetcertrevno _rom_call(short,(__plong),2A0)
#define cgetflen _rom_call(unsigned long,(CFILE*,short),12D)
#define cgetfnl _rom_call(long,(CERT_FIELD*),12E)
#define cgetnl _rom_call(long,(CFILE*),12F)
#define cgetns _rom_call(short,(CFILE*),130)
#define cgetsn _rom_call(void,(char*),2A1)
#define copen _rom_call(void,(CFILE*,char*,long),132)
#define copensub _rom_call(void,(CFILE*,CERT_FIELD*),133)
#define cputhdr _rom_call(short,(CFILE*,short,short),134)
#define cputnl _rom_call(void,(CFILE*,long),135)
#define cputns _rom_call(void,(CFILE*,short),136)
#define cread _rom_call(short,(CFILE*,CERT_FIELD*),137)
#define ctell _rom_call(unsigned long,(CFILE*),138)
#define cwrite _rom_call(short,(CFILE*,CERT_FIELD*),139)
#if MIN_AMS>=200
#define CertificateMemory ((unsigned char*const)(_rom_call_addr(43E)))
#endif
/* End Auto-Generated Part */

#endif
