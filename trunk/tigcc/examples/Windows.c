// An example of using windows in TIGCC

// Compile for all calcs
#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 100

#include <kbd.h>
#include <wingraph.h>

void _main(void)
{
  WINDOW w1,w2,w3;
  WinOpen(&w1,&(WIN_RECT){20,20,80,50},WF_SAVE_SCR|WF_TTY);
  WinBackground(&w1,A_NORMAL);
  WinAttr(&w1,A_REVERSE);
  WinFont(&w1,F_8x10);
  WinActivate(&w1);
  WinClr(&w1);
  WinStr(&w1,"Hello world!");
  WinOpen(&w2,&(WIN_RECT){3,60,67,87},WF_SAVE_SCR|WF_ROUNDEDBORDER|WF_TITLE,"ROUNDED WORLD");
  WinFont(&w2,F_4x6);
  WinActivate(&w2);
  WinStrXY(&w2,4,4,"Again hello!");
  WinOpen(&w3,&(WIN_RECT){70,40,150,70},WF_SAVE_SCR);
  WinFont(&w3,F_4x6);
  WinActivate(&w3);
  WinStrXY(&w3,5,8,"Hello (third) world!");
  ngetchx();
  WinClose(&w3);
  ngetchx();
  WinClose(&w2);
  ngetchx();
  WinClose(&w1);
}
