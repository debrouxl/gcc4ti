#define SAVE_SCREEN         // this directive forces saving/restoring the screen

#define USE_TI89            // produce all types of files
#define USE_TI92PLUS
#define USE_V200

#include <stdio.h>          // standard ANSI C input/output support
#include <kbd.h>            // keyboard handling support, needed for ngetchx

void _main(void)            // main entry point is function _main
{
  clrscr ();                // clear the screen and reset print position
  printf ("Hello world!");  // do you know what this is?
  ngetchx ();               // wait for a keypress
}
