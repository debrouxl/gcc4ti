// Call HeapShuffle and compare addresses

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 200           // Compile for AMS 2.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  unsigned char *p1, *p2, *p3, *p4, *p5, *p6;
  HANDLE h1, h2, h3;
  short f = FontGetSys ();
  unsigned char b[50];
  
  h1 = HeapAlloc (0x1000);
  h2 = HeapAlloc (0x1000);
  h3 = HeapAlloc (0x1000);
  
  if (h1 && h2 && h3)
    {
      ClrScr ();
      
      p1 = HeapDeref (h1);
      p3 = HeapDeref (h2);
      p5 = HLock (h3);
      HeapShuffle ();
      p2 = HeapDeref (h1);
      p4 = HeapDeref (h2);
      p6 = HeapDeref (h3);
      
      FontSetSys (F_4x6);
      DrawStr (0, 0, "Before/after shuffle:", A_NORMAL);
      
      sprintf (b, "Block 1 (unlocked): %lp %lp", p1, p2);
      DrawStr (0, 10, b, A_NORMAL);
      
      sprintf (b, "Block 2 (unlocked): %lp %lp", p3, p4);
      DrawStr (0, 20, b, A_NORMAL);
      
      sprintf (b, "Block 3 (locked): %lp %lp", p5, p6);
      DrawStr (0, 30, b, A_NORMAL);
      
      FontSetSys (f); // Restore previous font
      GKeyIn (NULL, 0);
    }
  
  HeapFree (h3);
  HeapFree (h2);
  HeapFree (h1);
}
