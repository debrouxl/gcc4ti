// Display a simple dialog box and let the user enter a name

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  char buffer[27] = "Hello ";
    // 6 bytes "Hello ", max. 20 bytes name, 1 zero byte
  HANDLE handle = DialogNewSimple (140, 55);
  DialogAddTitle (handle, "EXAMPLE", BT_OK, BT_CANCEL);
  DialogAddText (handle, 3, 20, "Enter name (max. 20 chars)");
  DialogAddRequest (handle, 3, 30, "Your name", 6, 20, 14);
  if (DialogDo (handle, CENTER, CENTER, buffer, NULL) == KEY_ENTER)
    DlgMessage ("GREETINGS", buffer, BT_OK, BT_NONE);
  HeapFree (handle);
}
