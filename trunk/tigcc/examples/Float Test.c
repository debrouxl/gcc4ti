#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define SAVE_SCREEN

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <kbd.h>

void _main(void)
{
  float a, b, c, d;
  char buffer[200];
  clrscr ();
  puts ("a=");
  a = atof (gets (buffer));
  puts ("b=");
  b = atof (gets (buffer));
  puts ("c=");
  c = atof (gets (buffer));
  if (is_nan (a) || is_nan (b) || is_nan (c)) return;
  d = b * b - 4. * a * c;
  if (d >= 0.)
    {
      float x1, x2;
      x1 = (-b + sqrt (d)) / (2. * a);
      x2 = (-b - sqrt (d)) / (2. * a);
      printf ("\nx1=%f\nx2=%f", x1, x2);
    }
  else
    {
      float re, im;
      re = -b / (2. * a);
      im = fabs (sqrt (-d) / (2. * a));
      printf ("\nx1=%f-%f*i\nx2=%f+%f*i", re, im, re, im);
    }
  ngetchx();
}
