// Display the catalog and let the user select something

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher

#include <tigcclib.h>         // Include All Header Files

const char *ptr;
HANDLE handle;

CALLBACK void Handler(EVENT *ev)
{
  if (ev->Type == CM_STRING)
    ptr = ev->extra.pasteText;
  else if (ev->Type == CM_HSTRING)
    handle = ev->extra.hPasteText;
  ER_throw (1);
}

void _main(void)
{
  EVENT ev;
  char buffer[100];
  ptr = NULL;
  handle = H_NULL;
  EV_captureEvents (Handler);
  CAT_dialog ();
  TRY
    EV_eventLoop ();
  ONERR
    EV_captureEvents (NULL);
  ENDTRY
  if (handle != H_NULL)
    ptr = HLock (handle);
  if (ptr != NULL)
  {
    sprintf (buffer, "You selected \"%s\".", ptr);
    ST_helpMsg (buffer);
  }
  else ST_helpMsg ("You pressed ESC.");
  if (handle != H_NULL)
    HeapFree (handle);
  ev.Type = CM_UNFOCUS;               // This is more due to some
  EV_sendEvent (AP_CURRENT, &ev);     // aesthetical reasons
}
