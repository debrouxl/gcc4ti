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
  WINDOW wind;
  WIN_RECT winr = {20, 20, 80, 50};
  WinOpen (&wind, &winr, WF_SAVE_SCR | WF_TTY);
  WinActivate (&wind);
  WinFont (&wind, F_6x8);
  WinStr (&wind, "hello everyone");
  ngetchx ();
  WinClose (&wind);
}
