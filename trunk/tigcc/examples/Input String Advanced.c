// Custom string input example enabling the CHAR menu

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

short captured;

CALLBACK void CaptureHandler(EVENT *ev)
{
  if (ev->Type == CM_STRING)
    captured = *(ev->extra.pasteText);
}

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
      do
        {
          key = ngetchx ();
          if (key == KEY_CHAR && i < maxlen)
            {
              EVENT ev;
              captured = 0;
              ev.Type = CM_KEYPRESS;
              ev.extra.Key.Code = key;
              EV_captureEvents (CaptureHandler);
              EV_defaultHandler (&ev);
              EV_captureEvents (NULL);
            }
        } while (!captured);
      if (key == KEY_CHAR && i < maxlen)
        buffer[i++] = captured;
      if (key >= ' ' && key <= '~' && i < maxlen) buffer[i++] = key;
      if (key == KEY_BACKSPACE && i) i--;
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
