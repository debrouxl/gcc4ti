// Graph a given function using the current graph settings

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 101           // Compile for AMS 1.01 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  TRY
    push_quantum (VAR_T_TAG);
    push_parse_text ("sin(t)+sin(2t)");
    cmd_graph (top_estack);
    ngetchx ();
    cmd_disphome ();
  ONERR
  ENDTRY
}
