#include <compat.h>
#include <dll.h>
#include <alloc.h>
#include <vat.h>
#include <string.h>
#include <system.h>
#include <peekpoke.h>

/* These functions are in the same file, so that they
   can access their global variables in a pc-relative
   fashion. */

__ATTR_LIB_C__ short LoadDLL(const char *DLL_name, long ID, short major, short minor)
{
  SYM_ENTRY *entry;
  HANDLE h;
  unsigned char *bptr,*sptr;
  unsigned short len,offset=0,wrongver=0;
  unsigned long pc;
  unsigned long signature[]={__DLL_SIGNATURE,ID};
  asm volatile("bsr 0f; 0:move.l (%%sp)+,%0":"=g"(pc));
  if(HW_VERSION==2 && pc<0x40000) return DLL_NOTINGHOSTSPACE;
  if(__DLL_body_ptr) return DLL_ALREADYLOADED;
  entry=SymFindFirst(NULL,2);
  do
    {
      if(!strcmp(entry->name,DLL_name)&&entry->handle&&!entry->flags.bits.twin
        &&(entry->flags.bits.archived||!HeapGetLock(entry->handle)))
          {
            len=peek_w(bptr=HeapDeref(entry->handle))+2;
            if(!memcmp(bptr+len-5,"DLL\x00\xF8",5))
              {
                offset=0;
                for(sptr=bptr+2;(sptr<bptr+len-1)&&!offset;sptr+=2)
                  if(!memcmp(sptr,signature,8))
                    {
                      if((unsigned short)major!=((__DLL_interface_struct*)sptr)->major
                        ||(unsigned short)minor>((__DLL_interface_struct*)sptr)->minor)
                          wrongver=1;
                      else
                        offset=sptr-bptr,wrongver=0;
                    }
                  if(offset) break;
                }
          }
    } while((entry=SymFindNext()));
  if(wrongver) return DLL_WRONGVERSION;
  if(!entry) return DLL_NOTFOUND;
  if(!HeapLock(h=entry->handle)) return DLL_LOCKFAILED;
  if(!(__DLL_body_ptr=malloc(len=peek_w(bptr=HeapDeref(h)+2)+2))) 
    {
      HeapUnlock(h);
      return DLL_OUTOFMEM;
    }
  memcpy(__DLL_body_ptr,bptr,len);
  EX_patch((char*)__DLL_body_ptr+(HW_VERSION==2?0x40000:0)+2,(char*)__DLL_body_ptr+(HW_VERSION==2?0x40000:0)+len-1);
  __DLL_interface_ptr=(__DLL_interface_struct*)((char*)__DLL_body_ptr+offset-2);
  HeapUnlock(h);
  return DLL_OK;
}

__ATTR_LIB_C__ void UnloadDLL(void)
{
  if(!__DLL_body_ptr) return;
  free(__DLL_body_ptr);
  __DLL_body_ptr=0;
  __DLL_interface_ptr=0;
}

__DLL_interface_struct *__DLL_interface_ptr=0;
void *__DLL_body_ptr=0;
