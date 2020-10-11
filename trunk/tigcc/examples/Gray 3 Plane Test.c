// Grayscale test program for GCC4TI

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 100
#define SAVE_SCREEN

#include <tigcclib.h>

//    10   30   50   70   90   110  130   150
// 20           L         M         L+M
// 40
// 60 D         L+D       M+D       L+M+D
// 80
void _main(void)
{
  if (!Gray3POn (1))
    return;
  Gray3PSetAMSPlane (GRAY3P_LIGHT_PLANE);
  ClrScr ();
  ScrRectFill (&(SCR_RECT){{50,20,70,40}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{130,20,150,40}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{50,60,70,80}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{130,60,150,80}}, ScrRect, A_NORMAL);
  Gray3PSetAMSPlane (GRAY3P_MEDIUM_PLANE);
  ClrScr ();
  ScrRectFill (&(SCR_RECT){{90,20,110,40}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{130,20,150,40}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{90,60,110,80}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{130,60,150,80}}, ScrRect, A_NORMAL);
  Gray3PSetAMSPlane (GRAY3P_DARK_PLANE);
  ClrScr ();
  ScrRectFill (&(SCR_RECT){{10,60,30,80}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{50,60,70,80}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{90,60,110,80}}, ScrRect, A_NORMAL);
  ScrRectFill (&(SCR_RECT){{130,60,150,80}}, ScrRect, A_NORMAL);
  ngetchx ();
  GrayOff ();
}


