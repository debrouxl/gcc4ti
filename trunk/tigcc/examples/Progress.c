// Progress bar example for TIGCC

// Define this to display the progress bar in a window
//#define USE_WINDOW_PB

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define MIN_AMS 200           // Compile for AMS 2.00 or higher

#include <tigcclib.h>         // Include All Header Files

// Main Function
void _main(void)
{
  short j;

#ifdef USE_WINDOW_PB
  ST_PROGRESS_BAR spb = {NULL, {0, 0, 0, 0}, 0, 0, 100, 100, 0};
  WINDOW w;
  memcpy (&(spb.rect), ScrToWin (ScrRect), sizeof (WIN_RECT));
  spb.physwidth = spb.rect.x1 - spb.rect.x0 + 1;
  WinOpen (&w, &(spb.rect), WF_SAVE_SCR | WF_NOBORDER);
  spb.w = &w;
#else
  ST_PROGRESS_BAR spb;
  ST_progressBar (&spb, 0, 100); // Create the progress bar in spb. low=0, high=100.
                                 // It will be created in the status line.
#endif
  
  for (j = 0; j < 20; j++)
    {
      OSFreeTimer (USER_TIMER);
      OSRegisterTimer (USER_TIMER, 1);
      while (!OSTimerExpired (USER_TIMER)); // Wait a little...
      ST_progressIncrement (&spb, 1); // Increment the progress bar by 1/100.
    }
  ST_progressUpdate (&spb, 50); // Increment the progress bar up to 50/100.
  OSFreeTimer (USER_TIMER);
  OSRegisterTimer (USER_TIMER, 20);
  while (!OSTimerExpired (USER_TIMER)); // Wait for about 1 second...
  OSFreeTimer (USER_TIMER);
  ST_progressUpdate (&spb, 100); // Fill the progress bar entirely.
  
  GKeyIn (NULL, 0);
  ST_progressDismiss (&spb); // Remove the progress bar, redraw status line.
#ifdef USE_WINDOW_PB
  WinClose (&w);
#endif
}
