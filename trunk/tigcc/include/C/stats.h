#ifndef __STATS
#define __STATS

#include <default.h>

/* Begin Auto-Generated Part */
enum RegModelTypes{RM_NONE,RM_MEDMED,RM_LIN,RM_LN,RM_EXP,RM_POWER,RM_QUAD,RM_CUBIC,RM_QUART,RM_LOGISTIC,RM_SIN};
#if MIN_AMS>=200
#define QstatRcl _rom_call(short,(void),40B)
#define statEnd _rom_call(void,(void),409)
#define statFree ({__need_in_use_bit;_rom_call(void,(void),40A);})
#define statStart ({__need_in_use_bit;_rom_call(void,(void),408);})
#if MIN_AMS>=204
#define RM_Type (*((unsigned char*)(_rom_call_addr(5DC))))
#endif
#endif
/* End Auto-Generated Part */

#endif
