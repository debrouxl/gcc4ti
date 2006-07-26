#ifndef __DLL
#define __DLL

#include <default.h>

#define __DLL_SIGNATURE 0x444C4C20
typedef struct{unsigned long Signature,ID;unsigned short major,minor;void*jump_table[];}__DLL_interface_struct;
extern __DLL_interface_struct*__DLL_interface_ptr;
extern void*__DLL_body_ptr;

/* Begin Auto-Generated Part */
enum DLL_ErrorCodes{DLL_OK,DLL_NOTINGHOSTSPACE,DLL_NOTFOUND,DLL_LOCKFAILED,DLL_OUTOFMEM,DLL_ALREADYLOADED,DLL_WRONGVERSION};
#define _DLL_call_attr(type,args,attr,index) (*(type(*attr)args)_DLL_entry(index))
#define _DLL_call(type,args,index) (*(type(*)args)_DLL_entry(index))
#define _DLL_entry(index) (__DLL_interface_ptr->jump_table[index])
#define _DLL_glbvar(type,index) (*(_DLL_reference(type,index)))
#define _DLL_reference(type,index) ((type*const)_DLL_entry(index))
extern short LoadDLL(const char*,long,short,short)__ATTR_LIB_C__;
extern void LoadDLLThrow(const char*,long,short,short)__ATTR_LIB_C__;
extern void UnloadDLL(void)__ATTR_LIB_C__;
#define DLL_EXPORTS ,{
#define DLL_ID __DLL_interface_struct __DLL_interface __attribute__((__section__("_stl20")))={__DLL_SIGNATURE,
#define DLL_IMPLEMENTATION ,(void*)-1L}};
#define DLL_INTERFACE asm(".xdef _nostub_dll\n\t.xdef __ld_ignore_global_imports");
#define DLL_VERSION ,
/* End Auto-Generated Part */

#endif
