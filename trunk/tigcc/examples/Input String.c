// Custom string input example

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

// Custom String Input Function
void InputStr(char *buffer, unsigned short maxlen)
{
  SCR_STATE ss;
  short key;
  unsigned short i = 0;
  buffer[0] = 0;
  SaveScrState (&ss);
  do
    {
      MoveTo (ss.CurX, ss.CurY);
      printf ("%s_  ", buffer);
        // Note that two spaces are required only if the F_4x6 font is used
      key = ngetchx ();
      if (key >= ' ' && key <= '~' && i < maxlen)
        buffer[i++] = key;
      else if (key == KEY_BACKSPACE && i)
        i--;
      buffer[i] = 0;
    } while (key != KEY_ENTER);
}

// Main Function
void _main(void)
{
  char s[20];
  clrscr ();
  InputStr (s, 20);
  printf ("\n%s", s);
  ngetchx ();
}
