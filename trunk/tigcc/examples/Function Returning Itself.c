// A function returning itself

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define RETURN_VALUE          // Return pushed expression
#define MIN_AMS 101           // Compile for AMS 1.01 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  ESI argptr = top_estack;
  if (GetArgType (argptr) <= VAR_Q_TAG)  // it means that arg is a variable
                                         // see Tags to see why...
    push_expr_quantum (SYMSTR ("example"), USERFUNC_TAG);
  else
    {
      while (ESTACK (top_estack) != END_TAG)
        top_estack = next_expression_index (top_estack);
      top_estack--;
      push_string (SYMSTR ("blabla"));
    }
}
