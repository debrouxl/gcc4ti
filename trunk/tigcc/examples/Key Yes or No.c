// Check whether typed keys represent "Yes" or "No"
// in current language

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 200           // Compile for AMS 2.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  short k, a;
  ClrScr ();
  while ((k = GKeyIn (NULL, 0)) != 32)
    // While the user doesn't type SPACE
  {
    a = KeyYesOrNo(k);
    if (a == 0)
      DrawStr (0, 0, "FALSE  ", A_REPLACE);
    else if (a == 1)
      DrawStr (0, 0, "TRUE   ", A_REPLACE);
    else
      DrawStr (0, 0, "NEITHER", A_REPLACE);
  }
  DrawStr (0, 0, "SPACE  ", A_REPLACE);
  GKeyIn (NULL, 0);
}
