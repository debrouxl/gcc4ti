// Sort a list of floating point values

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Comparison Function
CALLBACK short flt_comp(const void *a, const void *b)
{
  return fcmp (*(const float*)a, *(const float*)b);
}

// Main Function
void _main(void)
{
  float list[5] = {2.5, 3.18, 1.42, 4.0, 3.25};
  int i;
  clrscr ();
  qsort (list, 5, sizeof (float), flt_comp);
  for (i = 0; i < 5; i++)
    printf ("%f\n", list[i]);
  ngetchx ();
}
