// A simple popup menu example

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define RETURN_VALUE          // Return a Value
#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 101           // Compile for AMS 1.01 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  HANDLE handle = PopupNew ("EXAMPLE", 40);
  PopupAddText (handle, -1, "Option 1", 1);
  PopupAddText (handle, -1, "Option 2", 2);
  PopupAddText (handle, 0, "Submenu 3", 3);
  PopupAddText (handle, 0, "Submenu 4", 4);
  PopupAddText (handle, -1, "Option 5", 5);
  PopupAddText (handle, 3, "Suboption 3.1", 6);
  PopupAddText (handle, 3, "Suboption 3.2", 7);
  PopupAddText (handle, 3, "Suboption 3.3", 8);
  PopupAddText (handle, 4, "Suboption 4.1", 9);
  short result = PopupDo (handle, CENTER, CENTER, 0);
  push_longint (result);
  HeapFree (handle);
}
