// Grayscale test program for TIGCC

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 100
#define SAVE_SCREEN

#include <tigcclib.h>

void _main(void)
{
  if (!GrayOn ())
    return;
  GraySetAMSPlane (LIGHT_PLANE);
  ClrScr ();
  ScrRectFill (&(SCR_RECT){{20,20,40,40}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{80,20,100,40}}, ScrRect, A_NORMAL);
  GraySetAMSPlane (DARK_PLANE);
  ClrScr ();
  ScrRectFill (&(SCR_RECT){{50,20,70,40}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{80,20,100,40}}, ScrRect, A_NORMAL);
  ngetchx ();
  GrayOff ();
}
