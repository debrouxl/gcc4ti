// Add the first two symbolic expressions passed to the program

#define USE_TI89
#define USE_TI92P
#define USE_V200

#define RETURN_VALUE
#define OPTIMIZE_ROM_CALLS
#define ENABLE_ERROR_RETURN
#define MIN_AMS 202

#include <tigcclib.h>

void _main(void)
{
  if (ArgCount () != 2)
    ER_throw (ER_ARGUMENT); /* Argument error */
  ESI delete_end = top_estack, delete_begin = delete_end;
  while (GetArgType (delete_begin) != END_TAG)
    delete_begin = next_expression_index (delete_begin);
  delete_begin--;
  push_sum (EX_getArg (0), EX_getArg (1));
  delete_between (delete_begin, delete_end);
}
