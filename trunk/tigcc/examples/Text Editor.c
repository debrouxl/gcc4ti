// A simple text editor example

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

TEXT_EDIT te;

// Typical event handler for text editing
CALLBACK void EventHandler(EVENT *ev)
{
  if (ev->Type == CM_KEYPRESS && ev->extra.Key.Code == KEY_ESC)
    ER_throw (1);
  if (!TE_handleEvent (&te, ev))
    EV_defaultHandler (ev);
}

// Main Function
void _main(void)
{
  char *base_addr;
  SYM_ENTRY *sym = SymFindPtr (SYMSTR ("mytext"), 0);

  if (!sym) return;  // Exit if file not found...

  // First, you need to remove the garbage data at the begining of
  // the text variable, because the text editor expects raw data:

  base_addr = HeapDeref (sym->handle);
  memmove (base_addr, base_addr + 4, peek_w(base_addr));

  // Now, do the editing. This is straightforward...

  WinClr (DeskTop);
  TE_open (&te, DeskTop, MakeWinRect (0, 16, 159, 92), sym->handle, 1, 0, 7);
  CU_start ();
  EV_captureEvents (EventHandler);
  TRY
     EV_eventLoop ();
  ONERR
     EV_captureEvents (NULL);
  ENDTRY

  // Finally, you must transform raw editor data into the proper
  // format of the text variable. This is not so straightforward:

  base_addr = HeapDeref (HeapRealloc (sym->handle, te.CurSize + 10));
  memmove (base_addr + 4, base_addr, te.CurSize);
  poke_w (base_addr, te.CurSize + 4);
  poke_w (base_addr + 2, te.CursorOffset);
  poke (base_addr + te.CurSize + 4, 0);
  poke (base_addr + te.CurSize + 5, TEXT_TAG);
}
