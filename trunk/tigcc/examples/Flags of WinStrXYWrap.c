// Shows the effect of the flags WinStrXYWrap supports.

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define SAVE_SCREEN           // Save/Restore LCD Contents
#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define NO_CALC_DETECT
#define MIN_AMS 207           // Compile for AMS 2.07 or higher

#include <tigcclib.h>         // Include All Header Files

void _main(void)
{
  WINDOW w;
  
  WinOpen(&w,&(WIN_RECT){10,10,130,90},WF_SAVE_SCR);
  ClrScr();
  DrawClipRect(&(WIN_RECT){10,10,130,90},&(SCR_RECT){{0,0,239,127}},A_NORMAL|B_NORMAL);
  WinStrXYWrap(&w,10,0,"Trying a string,trying\x0A...",WWF_DRAW|WWF_WRAP_ON_COMMAS|WWF_WRAP_BACK_TO_ZERO);
  
  GKeyIn(NULL,0);
  ScrRectFill(&(SCR_RECT){{11,11,129,89}},&(SCR_RECT){{0,0,239,127}},A_REVERSE);
  WinStrXYWrap(&w,10,0,"Trying a string,trying\x0A...",WWF_DRAW|WWF_WRAP_ON_COMMAS);
  
  GKeyIn(NULL,0);
  ScrRectFill(&(SCR_RECT){{11,11,129,89}},&(SCR_RECT){{0,0,239,127}},A_REVERSE);
  WinStrXYWrap(&w,10,0,"Trying a string,trying\x0A...",WWF_DRAW);
  
  GKeyIn(NULL,0);
  WinClose(&w);
}
