#ifndef __FILES
#define __FILES

#include <default.h>

/* Begin Auto-Generated Part */
#ifndef __HAVE_FindOptions
#define __HAVE_FindOptions
enum FindOptions{FO_SINGLE_FOLDER=0x01,FO_RECURSE=0x02,FO_SKIP_TEMPS=0x04,FO_NOTEMPS=0x04,FO_RETURN_TWINS=0x08,FO_CKTWINS=0x08,FO_RETURN_FOLDER=0x10,FO_SKIP_COLLAPSE=0x20};
#endif
#ifndef __HAVE_HSym
#define __HAVE_HSym
typedef struct{HANDLE folder;unsigned short offset;}HSym;
#endif
#ifndef __HAVE_SYM_ENTRY
#define __HAVE_SYM_ENTRY
typedef struct{char name[8];unsigned short compat;union{unsigned short flags_n;struct{unsigned int busy:1,local:1,flag1_5:1,flag1_4:1,collapsed:1,twin:1,archived:1,in_view:1;unsigned int folder:1,overwritten:1,checked:1,hidden:1,locked:1,statvar:1,graph_ref_1:1,graph_ref_0:1;}bits;}flags;HANDLE handle;}SYM_ENTRY;
#endif
enum fileMode{FM_CLOSED=0,FM_READ=1,FM_WRITE=2,FM_APPEND=3,FM_READ_ACCESS=1,FM_WRITE_ACCESS=4};
typedef struct fileStruct{unsigned short dataLen;unsigned short dataPos;unsigned short bufSize;HANDLE dataH;unsigned char fileFlags,fileMode;unsigned short fileStatus;HSym hSym;char type[5];}FILES;
enum FileStatusEnum{FS_OK=0x0000,FS_EOF=0xFFFF,FS_ERROR=0xFFFE,FS_BAD_NAME=0xFFFD,FS_MEMORY=0xFFFC,FS_NOT_FOUND=0xFFFB};
#if MIN_AMS>=200
#define FAccess ({__need_in_use_bit;_rom_call(unsigned short,(const char*,short,const char*),3D4);})
#define FClose _rom_call(unsigned short,(FILES*),3D5)
#define FCreate ({__need_in_use_bit;_rom_call(unsigned short,(const char*,const char*),3D6);})
#define FDelete ({__need_in_use_bit;_rom_call(unsigned short,(const char*),3D7);})
#define FEof _rom_call(short,(FILES*),3D8)
#define FFindFirst _rom_call(SYM_ENTRY*,(short,const char*,const char*),3D9)
#define FFindNext _rom_call(SYM_ENTRY*,(void),3DA)
#define FGetC _rom_call(unsigned short,(FILES*),3DB)
#define FGetPos _rom_call(unsigned short,(FILES*),3DC)
#define FGetSize _rom_call(unsigned short,(FILES*),3DD)
#define FOpen ({__need_in_use_bit;_rom_call(unsigned short,(const char*,FILES*,short,const char*),3DE);})
#define FPutC _rom_call(unsigned short,(short,FILES*),3DF)
#define FRead _rom_call(unsigned short,(void*,short,FILES*),3E0)
#define FSetBufSize _rom_call(unsigned short,(FILES*,short),3E2)
#define FSetPos _rom_call(unsigned short,(FILES*,short),3E1)
#define FSetSize _rom_call(unsigned short,(FILES*,short),3E3)
#define FSetVer _rom_call(unsigned char,(FILES*,char),3E4)
#define FStatus _rom_call(unsigned short,(FILES*),3E5)
#define FType ({__need_in_use_bit;_rom_call(unsigned short,(const char*,char*),3E6);})
#define FWrite _rom_call(unsigned short,(const void*,short,FILES*),3E7)
#endif
/* End Auto-Generated Part */

#endif

