// Allocate memory as long as possible, then throw an error
// All allocated memory will be freed again!

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define ENABLE_ERROR_RETURN   // Enable Returning Errors to TIOS

#include <tigcclib.h>         // Include All Header Files

#define BLOCK_SIZE 1024

void AllocRecursively(void)
{
  void *ptr = malloc_throw (BLOCK_SIZE);
  TRY
    // Could do something with ptr here...
    AllocRecursively ();
    // Could still do something with ptr...
  FINALLY
    free (ptr);
  ENDFINAL
}

// Main Function
void _main(void)
{
  AllocRecursively ();
}
