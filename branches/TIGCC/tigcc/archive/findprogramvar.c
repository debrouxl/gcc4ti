/* This routine will return a pointer to the SYM_ENTRY of the running program,
or NULL in case it isn't found (e.g. if the program was exe-packed).
Note: Do not call anything which may cause a heap compression between when
this routine is called and when the pointer to it is used. Otherwise, the
pointer may become invalid, causing a crash or other random, unexpected
behavior. You can also use FolderOp to avoid this problem.

Contributed by Joel Thompson */

#include <vat.h>
#include <alloc.h>

__ATTR_LIB_C__ SYM_ENTRY *FindProgramVar (void)
{
  SYM_ENTRY *symptr;
  unsigned char *program_counter;

  asm volatile ("bsr 0f\n"
                "0:move.l (%%sp)+,%0"
                : "=g" (program_counter));
  // In case the program is in the ghost space, AND out the extra bit(s).
  ((unsigned long) program_counter) &= 0x3FFFF;

  for (symptr = SymFindFirst (NULL, FO_RECURSE); symptr; symptr = SymFindNext ())
  {
    HANDLE handle = symptr->handle;
    unsigned char *ptr = HeapDeref (symptr->handle);
    if ((program_counter >= ptr) && (program_counter < ptr + HeapSize (handle)))
      return symptr;
  }

  return NULL;
}
