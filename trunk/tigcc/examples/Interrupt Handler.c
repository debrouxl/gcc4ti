// Interrupt handler incrementing a counter

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

INT_HANDLER OldInt5 = NULL;
volatile int Counter = 0;

DEFINE_INT_HANDLER(MyInt5)
{
  Counter++;
  ExecuteHandler (OldInt5);
}

void _main(void)
{
  OldInt5 = GetIntVec (AUTO_INT_5);
  SetIntVec (AUTO_INT_5, MyInt5);
  while (!kbhit()) printf_xy (50, 50, "Counter = %d  ", Counter);
  SetIntVec (AUTO_INT_5, OldInt5);
  GKeyFlush ();
}
