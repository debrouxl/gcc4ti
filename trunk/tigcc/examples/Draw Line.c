// Example of a fast line-drawing routine

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Draws a line from (x1,y2) to (x2,y2).
void DrawLineFast(short x1, short y1, short x2, short y2)
{
  short x = x1, y = y1;
  short dx = abs (x2 - x1), dy = abs (y2 - y1);
  short ystep = (y1 < y2) ? 1 : -1, pystep = 30 * ystep;
  short mov = dx ? 0 : -1;
  unsigned char *ptr = (char*)LCD_MEM + 30 * y + (x >> 3);
  short mask = 1 << (~x & 7);
  if (x1 < x2)
    while (x != x2 || y != y2)
      {
        *ptr |= mask;
        if (mov < 0) y += ystep, ptr += pystep, mov += dx;
        else
          {
            mov -= dy;
            if (++x & 7) mask >>= 1;
            else ptr++, mask = 0x80;
          }
      }
  else
    while (x != x2 || y != y2)
      {
        *ptr |= mask;
        if (mov < 0) y += ystep, ptr += pystep, mov += dx;
        else
          {
            mov -= dy;
            if (x-- & 7) mask <<= 1;
            else ptr--, mask = 1;
          }
      }
}

// Main Function
void _main(void)
{
  DrawLineFast (10, 10, 60, 70);
  ngetchx ();
}
