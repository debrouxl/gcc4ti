// Create a variable using functions from vat.h

#define USE_TI89              // Compile for TI-89
#define USE_TI92PLUS          // Compile for TI-92 Plus
#define USE_V200              // Compile for V200

#define OPTIMIZE_ROM_CALLS    // Use ROM Call Optimization
#define MIN_AMS 100           // Compile for AMS 1.00 or higher
#define SAVE_SCREEN           // Save/Restore LCD Contents

#include <tigcclib.h>         // Include All Header Files

HANDLE CreateFile (const char *FileName)
// Returns a handle, H_NULL in case of error
{
  HANDLE h;
  SYM_ENTRY *sym_entry;
  char str[30], *sptr = str;
  *sptr = 0; while ((*++sptr = *FileName++));
  if (!(h = HeapAlloc (HeapMax ()))) return H_NULL;
  if (!(sym_entry = DerefSym (SymAdd (sptr))))
    {
      HeapFree (h);
      return H_NULL;
    }
  *(long*) HeapDeref (sym_entry->handle = h) = 0x00010000;
  return h;
}

void AppendCharToFile (HANDLE h, unsigned char c)
{
  char *base = HeapDeref(h);
  unsigned short len = *(unsigned short*)base;
  if (len > HeapSize(h) - 10) return;
  *(unsigned short*)base = len + 1;
  base[len+2] = c;
}

void AppendBlockToFile (HANDLE h, void *addr, unsigned short len)
{
  unsigned short i;
  for (i = len; i; i--) AppendCharToFile (h, *((char*)addr)++);
}

void CloseFile (HANDLE h)
{
  AppendCharToFile (h,0); AppendCharToFile (h,0x2D);
  HeapUnlock (h);
  HeapRealloc (h, *(unsigned short*)HeapDeref(h) + 3);
}

void _main(void)
{
  static char s[] = "Hello world!";
  HANDLE h;
  h = CreateFile ("example");
  AppendBlockToFile (h, s, 12);
  CloseFile (h);
}
