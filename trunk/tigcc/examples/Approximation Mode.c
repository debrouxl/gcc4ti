// Run Home Screen in approximation mode

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

volatile EVENT_HANDLER Old_Handler;

CALLBACK void Handler(EVENT *ev)
{
  if (ev->Type == CM_KEYPRESS)
    {
      if (ev->extra.Key.Code == KEY_DIAMOND + KEY_ENTER)
        ER_throw (1);
      if (ev->extra.Key.Code == KEY_ENTER)
        ev->extra.Key.Code = KEY_DIAMOND + KEY_ENTER;
    }
  /* Send the event to the default application handler,
     but be careful to avoid infinite recursion! */
  EV_captureEvents (Old_Handler);
  EV_sendEvent (AP_CURRENT, ev);
  Old_Handler = EV_captureEvents (Handler);
}

void _main(void)
{
  Old_Handler = EV_captureEvents (Handler);
  TRY
    EV_eventLoop ();
  ONERR
    EV_captureEvents (Old_Handler);
  ENDTRY
}
