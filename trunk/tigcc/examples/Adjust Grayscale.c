// Adjust grayscale quality using + and - keys.

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  int key, value = 0;
  if (!GrayOn ())
    return;
  GrayAdjust (value);
  memset (GetPlane (DARK_PLANE), 255, LCD_SIZE);  // Fill the dark plane and
  memset (GetPlane (LIGHT_PLANE), 0, LCD_SIZE);   //  clear the light plane
  while ((key=ngetchx ()) != KEY_ESC)
    {
      if (key== '+' && value < 127)
        value++;
      if (key== '-' && value > (TI89 ? -28 : 0))
        value--;
      GrayAdjust(value);
    }
  GrayOff ();
}
