// Install two timers with counter variables

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

CALLBACK void Action1(void)
{
  static int Counter = 0;
  printf_xy (50, 50, "Counter1 = %d  ", ++Counter);
}

CALLBACK void Action2(void)
{
  static int Counter = 0;
  printf_xy (70, 70, "Counter2 = %d  ", ++Counter);
}

void _main(void)
{
  OSVRegisterTimer (1, 3, Action1);
  OSVRegisterTimer (2, 10, Action2);
  ngetchx ();
  OSVFreeTimer (1);
  OSVFreeTimer (2);
}
