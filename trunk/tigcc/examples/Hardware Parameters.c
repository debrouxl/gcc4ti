// Return the hardware parameter block as a list

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define RETURN_VALUE          // Return Pushed Expression
#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  const HARDWARE_PARM_BLOCK *hpb = FL_getHardwareParmBlock ();
  const unsigned long *curptr;
  push_END_TAG ();
  for (curptr = (const unsigned long *) &(hpb->hardwareID) + hpb->len / 4 - 1; (unsigned long) curptr > (unsigned long) hpb; curptr--)
    {
      push_quantum (*curptr);
      push_quantum (1);
      push_quantum (POSINT_TAG);
    }
  push_quantum (hpb->len);
  push_quantum (1);
  push_quantum (POSINT_TAG);
  push_LIST_TAG ();
}
