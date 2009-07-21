// Search integer values using a binary search.

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Comparison Function
CALLBACK short int_comp(const void *a, const void *b)
{
  return (*(const short*)a) - (*(const short*)b);
}

// Main Function
void _main(void)
{
  short list[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  short i;
  short font;
  void *p;

  font = FontGetSys();
  FontSetSys(F_4x6);
  clrscr ();
  for (i = -1; i < 11; i++) {
    p = bsearch (&i, list, sizeof(list)/sizeof(list[0]), sizeof (list[0]), int_comp);
    if (p == NULL) {
      printf ("%d not found\n", i);
    }
    else {
      printf ("%d is at index %lu\n", i, ((void *)p - (void *)list)/sizeof(list[0]));
    }
  }
  FontSetSys(font);
  ngetchx ();
}
