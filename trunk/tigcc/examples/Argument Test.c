// An example of passing arguments to C program
// Try this program calling argtest(arg1,arg2,...)

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 100

#include <graph.h>
#include <printf.h>
#include <kbd.h>
#include <args.h>

void _main(void)
{
  ESI argptr;
  int argtype;
  long num;
  InitArgPtr (argptr);
  while ((argtype = GetArgType (argptr)) != END_TAG)
    {
      DrawStr (0, 30, "                          ", A_REPLACE);
      if (argtype == STR_TAG)
        DrawStr (0, 30, GetStrnArg (argptr), A_REPLACE);
      else if (argtype == POSINT_TAG || argtype == NEGINT_TAG)
        {
          num = GetIntArg (argptr);
          if (argtype == NEGINT_TAG)
            num = -num;
          printf_xy (0, 30, "%ld", num);
        }
      else
        {
          DrawStr (0, 30, "Wrong arg type!", A_REPLACE);
          ngetchx ();
          break;
        }
      ngetchx ();
    }
}
