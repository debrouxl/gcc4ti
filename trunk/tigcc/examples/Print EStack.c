// Print bottom_estack and top_estack

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  ClrScr ();
  printf_xy (0, 40, "Bottom = 0x%lp", bottom_estack);
  printf_xy (0, 50, "Top = 0x%lp", top_estack);
  ngetchx ();
}
