// Multiply and expand the two polynoms passed to the program

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define RETURN_VALUE          // Return a value
#define ENABLE_ERROR_RETURN   // Enable returning errors
#define MIN_AMS 101           // Compile for AMS 1.01 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  push_quantum_pair (MUL_TAG, EXPAND_TAG);
  NG_rationalESI (top_estack);
}
