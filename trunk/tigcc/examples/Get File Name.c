// Open VAR-LINK dialog and let user select something

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

char VarBuffer[20] = "";

CALLBACK void VarLinkHandler (EVENT *ev)
{
  if (ev->Type == CM_HSTRING)
    {
      strncpy (VarBuffer, HeapDeref (ev->extra.hPasteText), 19);
      VarBuffer [19] = 0;
    }
  EV_defaultHandler (ev);
}

void VarLinkDialog (void)
{
  EVENT ev;
  EVENT_HANDLER OldHandler = EV_captureEvents (VarLinkHandler);
  memset (&ev, sizeof (ev), 0);
  ev.Type = CM_KEYPRESS;
  ev.extra.Key.Code = KEY_VARLNK;
  EV_defaultHandler (&ev);
  EV_captureEvents (OldHandler);
}

void _main(void)
{
  VarLinkDialog ();
  printf_xy (0, 50, "You picked: %s", VarBuffer);
  ngetchx ();
}
