// Perform big number arithmetic through BN_prodMod

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 101           // Compile for AMS 1.01 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents
#define RETURN_VALUE          // Return a Value

#include <tigcclib.h>         // Include All Header Files

#define GetBignumArg(ap, bn) \
  ({unsigned char __n = *--(unsigned char*)(ap); \
  char *__p = (char*)(bn) + __n; \
  (bn)->Len = __n; \
  while (__n--) *__p-- = *--(ap); \
  (void)*(ap)--; })

void push_Bignum(BN *bn)
{
  unsigned m, n = bn->Len;
  char *p = (char*)bn;
  m = n;
  while (n--) push_quantum (*++p);
  push_quantum_pair (m, POSINT_TAG);
}

void _main(void)
{
  ESI argptr = top_estack;
  BN *a = malloc (256), *b = malloc (256), *c = malloc (256);
  GetBignumArg (argptr, a);
  GetBignumArg (argptr, b);
  GetBignumArg (argptr, c);
  while (GetArgType (top_estack) != END_TAG)  // Clean up arguments
    top_estack = next_expression_index (top_estack);
  top_estack--;
  BN_prodMod (a, b, c);
  push_Bignum (a);
  free (a); free (b); free (c);
}
