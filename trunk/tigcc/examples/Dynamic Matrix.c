#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

#define M 10
#define N 5

int (*A)[N] = NULL;

void _main (void)
{
  int i,j;
  A = calloc (M, sizeof (*A));     // <I>allocation</I>
  for (i = 0; i < M; i++)
    for (j = 0; j < N; j++)
      A[i][j] = i*j;               // fill 'A' with the multiplication table
  clrscr ();
  for (i = 0; i < M; i++)
    {
      for (j = 0; j < N; j++)
        printf ("%2d ", A[i][j]);  // print out the matrix
      printf ("\n");
    }
  free (A);                        // free the memory
  ngetchx();
}
