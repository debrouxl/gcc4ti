#define SAVE_SCREEN

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#include <graph.h>
#include <kbd.h>

void _main(void)
{
  static WIN_RECT rect = {0, 0, 100, 14};
  ClrScr ();
  FontSetSys (F_8x10);
  DrawStr (3, 3, "Hello world!", A_NORMAL);
  DrawClipRect (&rect, ScrRect, A_NORMAL);
  ngetchx ();
}
