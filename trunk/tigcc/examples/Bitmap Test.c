// Retrieve and store a bitmap

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization

#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  SCR_RECT full_screen = {{0, 0, LCD_WIDTH - 1, LCD_HEIGHT - 1}};
  char buffer [BITMAP_HDR_SIZE + LCD_WIDTH*LCD_HEIGHT/8]; // or 2004 for a TI-89 and 3844 for a TI-92+/V200 if you like it more
  BitmapGet (&full_screen, buffer);          // store screen in buffer
  clrscr ();
  printf ("Press any key to\nrestore screen...");
  ngetchx ();
  BitmapPut (0, 0, buffer, &full_screen, A_REPLACE);
  ngetchx ();
}