#ifndef __MATH
#define __MATH

#include <default.h>

/* Begin Auto-Generated Part */
#define HALF_PI (1.570796326794897)
#define NAN (0./0.)
#define NEGATIVE_INF (1/NEGATIVE_ZERO)
#define NEGATIVE_ZERO (-POSITIVE_ZERO)
#define PI (3.141592653589793)
#define POSITIVE_INF (1/POSITIVE_ZERO)
#define POSITIVE_ZERO (1.e-8192*1.e-8192)
#define UNSIGNED_INF (1/UNSIGNED_ZERO)
#define UNSIGNED_ZERO (0.)
#define ZERO (0.)
#ifndef __HAVE_bcd
#define __HAVE_bcd
typedef struct{unsigned short exponent;unsigned long long mantissa;}bcd __attribute__((__may_alias__));
#endif
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#define abs(x) ({typeof(x) __x = (x); __x >= 0 ? __x : -__x;})
#define acos(x) _tios_float_1(F5,x,float)
#define acosh(x) _tios_float_1(288,x,float)
#define asin(x) _tios_float_1(F6,x,float)
#define asinh(x) _tios_float_1(287,x,float)
#define atan2(x,y) _tios_float_2(F8,x,y,float,float)
#define atan(x) _tios_float_1(F7,x,float)
#define atanh(x) _tios_float_1(289,x,float)
#define cacos _rom_call(void,(float,float,float*,float*),13A)
#define cacosh _rom_call(void,(float,float,float*,float*),13D)
#define casin _rom_call(void,(float,float,float*,float*),13B)
#define casinh _rom_call(void,(float,float,float*,float*),13E)
#define catan _rom_call(void,(float,float,float*,float*),13C)
#define catanh _rom_call(void,(float,float,float*,float*),13F)
#define ccos _rom_call(void,(float,float,float*,float*),140)
#define ccosh _rom_call(void,(float,float,float*,float*),143)
#define ceil(x) _tios_float_1(105,x,float)
#define cexp _rom_call(void,(float,float,float*,float*),149)
#define cln _rom_call(void,(float,float,float*,float*),147)
#define clog10 _rom_call(void,(float,float,float*,float*),148)
#define cos(x) _tios_float_1(F9,x,float)
#define cosh(x) _tios_float_1(FC,x,float)
#define csin _rom_call(void,(float,float,float*,float*),141)
#define csinh _rom_call(void,(float,float,float*,float*),144)
#define csqrt _rom_call(void,(float,float,float*,float*),146)
#define ctan _rom_call(void,(float,float,float*,float*),142)
#define ctanh _rom_call(void,(float,float,float*,float*),145)
#define exp(x) _tios_float_1(FF,x,float)
#define fabs(x) _tios_float_1(106,x,float)
#define floor(x) _tios_float_1(107,x,float)
#define fmod(x,y) _tios_float_2(108,x,y,float,float)
#define hypot(x,y) ({float __x=(x),__y=(y);sqrt(fadd(fmul((__x),(__x)),fmul((__y),(__y))));})
#ifndef __HAVE_labs
#define __HAVE_labs
long labs(long)__ATTR_GCC__;
#endif
#define ldexp10(x,e) ({float __f=(x);((bcd*)&__f)->exponent+=(e);__f;})
#define log(x) _tios_float_1(100,x,float)
#define log10(x) _tios_float_1(101,x,float)
#define modf(x,y) _tios_float_2(102,x,y,float,float*)
#define pow(x,y) _tios_float_2(103,x,y,float,float)
#define sin(x) _tios_float_1(FA,x,float)
#define sinh(x) _tios_float_1(FD,x,float)
#define sqrt(x) _tios_float_1(104,x,float)
#define tan(x) _tios_float_1(FB,x,float)
#define tanh(x) _tios_float_1(FE,x,float)
#if MIN_AMS>=101
#ifndef __HAVE_atof
#define __HAVE_atof
extern float atof(const char*)__ATTR_LIB_ASM__;
#endif
#define frexp10(x,y) _tios_float_2(2FB,x,y,float,__pshort)
#define is_inf _rom_call(short,(float),2FF)
#define is_nan _rom_call(short,(float),306)
#define is_nzero _rom_call(short,(float),300)
#define is_pzero _rom_call(short,(float),301)
#define is_sinf _rom_call(short,(float),302)
#define is_transfinite _rom_call(short,(float),303)
#define is_uinf_or_nan _rom_call(short,(float),304)
#define is_uzero _rom_call(short,(float),305)
#endif
/* End Auto-Generated Part */

#endif
