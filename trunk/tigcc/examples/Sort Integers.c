// Sort a list of integer values

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Comparison Function
CALLBACK short int_comp(const void *a, const void *b)
{
  return fcmp (*(const short*)a, *(const short*)b);
}

// Main Function
void _main(void)
{
  short list[10] = {2, 9, 3, 6, 4, 2, 3, 3, 1, 5};
  int i;
  clrscr ();
  qsort (list, 10, sizeof (short), int_comp);
  for (i = 0; i < 10; i++)
    printf ("%d ", list[i]);
  ngetchx ();
}
