// Launcher for program called "example"

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#include <tigcclib.h>

#define fatal(s) ({ST_showHelp (s); return;})

void _main (void)
{
  char *fptr, *cptr;
  unsigned short plen;
  SYM_ENTRY *SymPtr = DerefSym (SymFind (SYMSTR ("example")));
  HANDLE h;
  if (!SymPtr) fatal ("Program not found");
  h = SymPtr->handle;
  if (HeapGetLock (h))
    {
      cptr = fptr = HeapDeref (h);
      h = 0;
    }
  else
    {
      cptr = fptr = HLock (h);
    }
  plen = *(short*)(cptr) + 3;
  if (SymPtr->flags.bits.archived)
    {
      if (!(cptr = malloc (plen)))
        {
          if (h) HeapUnlock (h);
          fatal ("Out of memory");
        }
      memcpy (cptr, fptr, plen);
    }
  enter_ghost_space ();
  EX_patch (cptr + 0x40002, cptr + plen + 0x3FFFE);
  ASM_call (cptr + 0x40002);
  if (h) HeapUnlock (h);
  if (cptr != fptr) free (cptr);
}
