#include <stdio.h>
#include <tigcclib.h>

#define DEFAULT_FILE_ALLOC_SIZE 128
#define DEFAULT_FILE_BUFF_INC DEFAULT_FILE_ALLOC_SIZE

__ATTR_LIB_C__ FILE *fopen(const char *name, const char *mode)
{
  char str[50],*fpos,*epos,*base=0,*sptr=str,chmode=mode[0];
  short bmode=(mode[1]=='b'||(mode[1]&&mode[2]=='b')),flags=0,ferr;
  FILE *f;
  HSym hsym;
  HANDLE handle=H_NULL;
  
  *sptr=0; while((*++sptr=*name++));
  
  hsym=SymFind(sptr);
  if(hsym.folder)
    {
      SYM_ENTRY *sym_entry=DerefSym(hsym);
      if((sym_entry->flags.flags_n&(SF_LOCKED|SF_OPEN|SF_ARCHIVED|SF_BUSY))&&strpbrk(mode,"wa+"))
        return NULL;
      handle=sym_entry->handle;
    }
  else
    {
      if(chmode=='r') return NULL;
    }

  if(!(f=malloc(sizeof(FILE)))) return NULL;
  
  if(chmode=='r'||chmode=='a')
    {
      flags=_F_READ;
      if(handle)
        {
          base=HLock(handle);
          f->alloc=HeapSize(handle);
        }
      else
        {
          chmode='w';
        }
    }
  
  if(chmode=='w')
    {
      SCR_STATE scr_state;
      flags=_F_WRIT;
      SaveScrState(&scr_state);
      hsym=SymAdd(sptr);
      ferr=!(hsym.folder);
      RestoreScrState(&scr_state);
      if(!ferr)
        {
          handle=HeapAlloc(DEFAULT_FILE_ALLOC_SIZE);
          ferr=!(DerefSym(hsym)->handle=handle);
        }
      if(ferr)
        {
          SymDel(sptr);
          free(f);
          return NULL;
        }
      
      f->alloc=DEFAULT_FILE_ALLOC_SIZE;
      
      base=HLock(handle);
      if(bmode) poke_w(base,0);
      else
        {
          poke_l(base,0x00050001);
          poke_l(base+4,0x2000E000);
        }
    }
  
  epos=base+peek_w(base)+(bmode?2:0);
  if(chmode=='a') flags=_F_WRIT,fpos=epos;
  else fpos=base+(bmode?2:5);
  if(epos==fpos) flags|=_F_EOF;
  if(mode[1]=='+'||mode[2]=='+') flags|=_F_RDWR;
  if(bmode) flags|=_F_BIN;
  f->handle=handle; f->base=base; f->flags=flags; f->fpos=fpos; f->unget=0; f->buffincrement=DEFAULT_FILE_BUFF_INC;
  return f;
}
