// Calculate an integral and pretty print it

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 101           // Compile for AMS 1.01 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  TRY
    push_END_TAG ();
    push_quantum (VAR_X_TAG);
    push_parse_text ("1/((x-3)(x^2+4))");
    push_quantum (INTEGRATE_TAG);
    NG_rationalESI (top_estack);
    WinClr (DeskTop);
    Print2DExpr (Parse2DExpr (top_estack, FALSE), DeskTop, 0, 50);
  ONERR
    DrawStr (20, 20, "Error!", A_NORMAL);
  ENDTRY
  ngetchx ();
}
