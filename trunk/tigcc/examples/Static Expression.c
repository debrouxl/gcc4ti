// Evaluate and print a static expression

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  static unsigned char rpn[] = {END_TAG, VAR_X_TAG, 1, 1, POSINT_TAG,
    VAR_X_TAG, 3, 1, POSINT_TAG, SUB_TAG, 2, 1, POSINT_TAG, VAR_X_TAG,
    POW_TAG, 4, 1, POSINT_TAG, ADD_TAG, MUL_TAG, DIV_TAG, INTEGRATE_TAG};
  NG_rationalESI (rpn + sizeof (rpn) - 1);
  WinClr (DeskTop);
  Print2DExpr (Parse2DExpr (top_estack, FALSE), DeskTop, 0, 50);
  ngetchx ();
}
