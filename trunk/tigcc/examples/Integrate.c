// Integrate the argument with respect to x and display it

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 101           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  ESI argptr = top_estack;
  HANDLE h;
  clrscr ();
  TRY
    push_END_TAG ();
    push_quantum (VAR_X_TAG);
    push_expr_quantum (argptr, INTEGRATE_TAG);
    NG_rationalESI (top_estack);
    h = display_statements (top_estack, 1, FALSE);
    printf ("The integral is: %s", (const char*) (HeapDeref (h)));
    HeapFree (h);
  ONERR
    printf ("Error!");
  ENDTRY
  ngetchx();
}
