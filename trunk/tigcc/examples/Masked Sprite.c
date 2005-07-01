// Display a masked sprite over an arbitrary background

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
  static const unsigned char sprite[] = {0xFF,0x81,0x81,0x81,0x81,0x81,0x81,0xFF};
  static const unsigned char imask[] = {(unsigned char)~0xFF,(unsigned char)~0xFF,
    (unsigned char)~0xFF,(unsigned char)~0xFF,(unsigned char)~0xFF,
    (unsigned char)~0xFF,(unsigned char)~0xFF,(unsigned char)~0xFF};
  int i;
  ClrScr ();
  for (i = 0; i <= LCD_WIDTH; i++)
    DrawLine (i, 0, i, LCD_HEIGHT, A_SHADE_NS);  // A simple background
  Sprite8 (30, 30, 8, imask, LCD_MEM, SPRT_AND);
  Sprite8 (30, 30, 8, sprite, LCD_MEM, SPRT_OR);
  ngetchx ();
}
