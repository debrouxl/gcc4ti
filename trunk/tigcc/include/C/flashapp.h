#ifndef __FLASHAPP
#define __FLASHAPP

#include <default.h>

/* Begin Auto-Generated Part */
#ifndef __HAVE_CFILE
#define __HAVE_CFILE
typedef struct{void*Start,*Pos,*End;short EOFVal;}CFILE;
#endif
#if MIN_AMS>=200
#define EV_quit ({__need_in_use_bit;_rom_call(void,(void),48E);})
#define OO_GetEndOfAllFlashApps _rom_call(unsigned char*,(void),3D2)
#define OO_GetFirstFlashAppSectorAddress _rom_call(unsigned char*,(CFILE*),4F3)
#define OO_GetFlashAppSize _rom_call(unsigned long,(short),477)
#endif
/* End Auto-Generated Part */

#endif
