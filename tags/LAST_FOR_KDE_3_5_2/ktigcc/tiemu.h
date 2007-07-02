#ifndef TIEMU_H__
#define TIEMU_H__

#include "tiemu_stub.h"

typedef enum {
    TIEMU_CALC_TI92 = 1,
    TIEMU_CALC_TI89 = 2,
    TIEMU_CALC_TI92p = 4,
    TIEMU_CALC_V200 = 8,
    TIEMU_CALC_TI89t = 16
} TiEmuCalcModels;

#endif
