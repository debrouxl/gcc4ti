#ifndef __LINK
#define __LINK

#include <default.h>

/* Begin Auto-Generated Part */
#define NULL ((void*)0)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_ESQ
#define __HAVE_ESQ
typedef unsigned char ESQ;
#endif
#ifndef __HAVE_CESI
#define __HAVE_CESI
typedef const ESQ*CESI;
#endif
#ifndef __HAVE_ESI
#define __HAVE_ESI
typedef ESQ*ESI;
#endif
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
#ifndef __HAVE_SYM_STR
#define __HAVE_SYM_STR
typedef CESI SYM_STR;
#endif
typedef struct{unsigned short hVar;union{const void*pVar;struct{unsigned short FindFlags;unsigned short NameSym;}DirVars;}extra;const char*VarName;unsigned long VarSize;unsigned short Index;unsigned short Unknown;unsigned char VarType;unsigned char VarCompat;}LIO_CTX;
#define getcalc ({__need_in_use_bit;_rom_call(void,(SYM_STR),54);})
#define LIO_Get _rom_call(unsigned short,(LIO_CTX*),57)
#define LIO_GetMultiple ({__need_in_use_bit;_rom_call(short,(LIO_CTX*),59);})
#define LIO_Receive ({__need_in_use_bit;_rom_call(unsigned short,(LIO_CTX*,short,short),58);})
#define LIO_RecvData _rom_call(unsigned short,(void*,long,long),5B)
#define LIO_Send _rom_call(unsigned short,(LIO_CTX*,short),56)
#define LIO_SendData _rom_call(unsigned short,(const void*,long),5A)
#define LIO_SendProduct _rom_call(unsigned short,(LIO_CTX*,short),253)
#define OSCheckSilentLink _rom_call(short,(void),24A)
#define OSLinkClose _rom_call(void,(void),24E)
#define OSLinkCmd ({__need_in_use_bit;_rom_call(void,(short),24B);})
#define OSLinkOpen _rom_call(void,(void),24D)
#define flush_link OSLinkOpen
#define OSLinkReset _rom_call(void,(void),24C)
#define reset_link OSLinkReset
#define OSLinkTxQueueActive _rom_call(short,(void),252)
#define OSLinkTxQueueInquire _rom_call(unsigned short,(void),251)
#define tx_free OSLinkTxQueueInquire
#define OSReadLinkBlock _rom_call(unsigned short,(char*,short),24F)
#define receive OSReadLinkBlock
#define OSWriteLinkBlock _rom_call(short,(const char*,short),250)
#define transmit OSWriteLinkBlock
#define sendcalc ({__need_in_use_bit;_rom_call(unsigned short,(SYM_STR,short,short,unsigned char*),55);})
/* End Auto-Generated Part */

#endif

