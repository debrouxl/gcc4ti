#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#include <graph.h>
#include <kbd.h>

void _main(void)
{
  LCD_BUFFER buffer;
  static WIN_RECT rect = {0, 0, 100, 14};
  LCD_save (buffer);
  ClrScr ();
  FontSetSys (F_8x10);
  DrawStr (3, 3, "Hello world!", A_NORMAL);
  DrawClipRect (&rect, ScrRect, A_NORMAL);
  ngetchx ();
  LCD_restore (buffer);
}
