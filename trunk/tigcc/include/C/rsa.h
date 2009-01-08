#ifndef __RSA
#define __RSA

#include <default.h>

/* Begin Auto-Generated Part */
typedef struct{unsigned char Len;unsigned char Data[];}BN;
typedef struct{unsigned long state[4];unsigned long count[2];unsigned char buffer[64];}MD5_CTX;
#define BN_power17Mod _rom_call(void,(BN*,const BN*,const BN*),122)
#define BN_powerMod _rom_call(void,(BN*,const BN*,const BN*,const BN*),123)
#define BN_prodMod _rom_call(void,(BN*,const BN*,const BN*),124)
#define cdecrypt _rom_call(void,(BN*,char*,long,BN*),127)
#define MD5Done _rom_call(void,(BN*,MD5_CTX*),257)
#define MD5Final _rom_call(void,(unsigned char*,MD5_CTX*),256)
#define MD5Init _rom_call(void,(MD5_CTX*),254)
#define MD5Update _rom_call(void,(MD5_CTX*,unsigned char*,long),255)
/* End Auto-Generated Part */

#endif
