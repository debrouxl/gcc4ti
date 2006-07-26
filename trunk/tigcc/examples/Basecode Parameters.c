// Return the basecode parameter block as a list.
// Works only on AMS 2.04 and later (returns with an error message on AMS 2.03 and older).

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define SAVE_SCREEN           // Save/Restore LCD Contents
#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 204           // Compile for AMS 2.04 or higher
#define RETURN_VALUE          // Return Pushed Expression

#define NO_CALC_DETECT
#define NO_EXIT_SUPPORT

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  const BASECODE_PARM_BLOCK *bpb = EX_getBasecodeParmBlock ();

  push_END_TAG ();

  push_quantum (bpb->releaseDateDay);
  push_quantum (1);
  push_quantum (POSINT_TAG);

  push_quantum (bpb->releaseDateMonth);
  push_quantum (1);
  push_quantum (POSINT_TAG);

  push_quantum (bpb->releaseDateYear);
  push_quantum ((bpb->releaseDateYear)>>8);
  push_quantum (2);
  push_quantum (POSINT_TAG);

  push_quantum (bpb->releaseVersionMinor);
  push_quantum (1);
  push_quantum (POSINT_TAG);

  push_quantum (bpb->releaseVersionMajor);
  push_quantum (1);
  push_quantum (POSINT_TAG);

  push_quantum (bpb->len);
  push_quantum (1);
  push_quantum (POSINT_TAG);
  push_LIST_TAG ();
}
