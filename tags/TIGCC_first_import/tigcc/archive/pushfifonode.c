#include <homescr.h>
#include <error.h>

__ATTR_LIB_C__ __HS_pushEmptyFIFONode__type__ __get_HS_pushEmptyFIFONode(void)
{
  short *Ptr = (void*)HomePushEStack, *EndPtr = Ptr + 0x80;
  for (; (unsigned long)Ptr < (unsigned long)EndPtr; Ptr++)
    if (*Ptr == 0x4EBA) /* bsr */
      {
        Ptr++;
        void *Addr = (char*)Ptr + *Ptr;
        if (Addr != HS_newFIFONode)
          return Addr;
      }
  ER_throw (410);
}
