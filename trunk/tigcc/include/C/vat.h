#ifndef __VAT
#define __VAT

#include <default.h>

void *alloca(long)__ATTR_GCC__;
#define SYMSTR_CONST(s) ((SYM_STR)(("\0"s)+sizeof(s)))
#define VATSTR SYMSTR

#define __Folder_Del _rom_call(short,(const char*,short),66)

/* Begin Auto-Generated Part */
#define H_NULL 0
#define HS_NULL ((HSym){0,0})
#define NULL ((void*)0)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_ESQ
#define __HAVE_ESQ
typedef unsigned char ESQ;
#endif
#ifndef __HAVE_CESI
#define __HAVE_CESI
typedef const ESQ*CESI;
#endif
#ifndef __HAVE_ESI
#define __HAVE_ESI
typedef ESQ*ESI;
#endif
#ifndef __HAVE_GraphModes
#define __HAVE_GraphModes
enum GraphModes{GR_FUNC=1,GR_PAR=2,GR_POL=3,GR_SEQ=4,GR_3D=5,GR_DE=6};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_MULTI_EXPR
#define __HAVE_MULTI_EXPR
typedef struct{unsigned short Size;ESQ Expr[];}MULTI_EXPR;
#endif
#ifndef __HAVE_SYM_STR
#define __HAVE_SYM_STR
typedef CESI SYM_STR;
#endif
enum CompatFlags{CF_NONE=0,CF_CONVERT=1,CF_ENHANCED=2,CF_NEW=3};
enum ExtendedSysTypes{SEQ_INITC=7,DE_INITC=8,DE_FLDPIC=9,SOLVER_SYS_VARS=10,UNIT_VAR=11,C_COL=12,R_REGEQ=13,R_SYSVAR=14};
#ifndef __HAVE_FindOptions
#define __HAVE_FindOptions
enum FindOptions{FO_SINGLE_FOLDER=0x01,FO_RECURSE=0x02,FO_SKIP_TEMPS=0x04,FO_NOTEMPS=0x04,FO_RETURN_TWINS=0x08,FO_CKTWINS=0x08,FO_RETURN_FOLDER=0x10,FO_SKIP_COLLAPSE=0x20};
#endif
enum FolderOpFlags{FOP_UNLOCK=0,FOP_LOCK=1,FOP_ALL_FOLDERS=0x80};
enum FolderStats{MAIN_FOLDER=2,FOLDER_TABLE=3,NOT_FOUND=4,BAD_FOLDER=5};
#ifndef __HAVE_HSym
#define __HAVE_HSym
typedef struct{HANDLE folder;unsigned short offset;}HSym;
#endif
#define HSYM HSym
#ifndef __HAVE_SYM_ENTRY
#define __HAVE_SYM_ENTRY
typedef struct{char name[8];unsigned short compat;union{unsigned short flags_n;struct{unsigned int busy:1,local:1,flag1_5:1,flag1_4:1,collapsed:1,twin:1,archived:1,in_view:1;unsigned int folder:1,overwritten:1,checked:1,hidden:1,locked:1,statvar:1,graph_ref_1:1,graph_ref_0:1;}bits;}flags;HANDLE handle;}SYM_ENTRY;
#endif
enum SymFlags{SF_GREF1=0x0001,SF_GREF2=0x0002,SF_STATVAR=0x0004,SF_LOCKED=0x0008,SF_HIDDEN=0x0010,SF_OPEN=0x0010,SF_CHECKED=0x0020,SF_OVERWRITTEN=0x0040,SF_FOLDER=0x0080,SF_INVIEW=0x0100,SF_ARCHIVED=0x0200,SF_TWIN=0x0400,SF_COLLAPSED=0x0800,SF_LOCAL=0x4000,SF_BUSY=0x8000};
#ifndef __HAVE_SystemDataTypes
#define __HAVE_SystemDataTypes
enum SystemDataTypes{SDT_EXPR=0,SDT_LIST=1,SDT_MAT=2,SDT_FUNC=3,SDT_PRGM=4,SDT_PIC=5,SDT_STR=6,SDT_TEXT=7,SDT_GDB=8,SDT_DATA=9,SDT_FIG=10,SDT_MAC=11,SDT_OTH=12,SDT_SYS=13,SDT_ALL=14,SDT_ASM=15};
#endif
enum VarRecallFlags{VR_NO_SYS_VARS=0x01,VR_FUNC_NAME=0x02,VR_LINK=0x04};
enum VarStoreFlags{STOF_ESI=0x4000,STOF_ELEMENT=0x4001,STOF_NONE=0x4002,STOF_HESI=0x4003};
#define $(s) (SYMSTR_CONST(#s))
#define AddSymToFolder _rom_call(HSym,(SYM_STR,SYM_STR),70)
#define checkCurrent ({__need_in_use_bit;_rom_call(HSym,(SYM_STR,ESQ),121);})
#define CheckLinkLockFlag _rom_call(void,(const SYM_ENTRY*),7F)
#define CheckReservedName _rom_call(short,(SYM_STR),8B)
#define CheckSysFunc _rom_call(short,(const char*,__pushort),89)
#define ClearUserDef _rom_call(void,(HANDLE),7E)
#define DerefSym _rom_call(SYM_ENTRY*,(HSym),79)
#define EM_moveSymFromExtMem ({__need_in_use_bit;_rom_call(short,(SYM_STR,HSym),161);})
#define EM_moveSymToExtMem ({__need_in_use_bit;_rom_call(short,(SYM_STR,HSym),162);})
#define EM_twinSymFromExtMem ({__need_in_use_bit;_rom_call(HSym,(SYM_STR,HSym),166);})
#define EX_stoBCD ({__need_in_use_bit;_rom_call(void,(unsigned char*,float*),C0);})
extern SYM_ENTRY *FindProgramVar(void)__ATTR_LIB_C__;
#define FindSymInFolder _rom_call(HSym,(SYM_STR,const char*),71)
#define FolderAdd _rom_call(HANDLE,(SYM_STR),64)
#define FolderAddTemp _rom_call(SYM_STR,(void),73)
#define FolderClear(c) __Folder_Del((c),1)
#define FolderCount _rom_call(unsigned short,(const SYM_ENTRY*),6B)
#define FolderCur ({__need_in_use_bit;_rom_call(short,(SYM_STR,short),65);})
#define FolderCurTemp _rom_call(short,(SYM_STR),72)
#define FolderDel(c) __Folder_Del((c),0)
#define FolderDelAllTemp ({__need_in_use_bit;_rom_call(void,(short),75);})
#define FolderDelTemp ({__need_in_use_bit;_rom_call(void,(void),74);})
#define FolderFind _rom_call(short,(SYM_STR),67)
#define FolderGetCur _rom_call(void,(char*),68)
#define FolderOp _rom_call(short,(SYM_STR,short),69)
#define FolderRename _rom_call(short,(const char*,const char*),6A)
#define GetFuncPrgmBodyPtr _rom_call_hack(ESI,(ESI),43A,(((unsigned char*)_rom_call_addr(290)-52)),200)
#define HSymDel ({__need_in_use_bit;_rom_call(short,(HSym),5F);})
#define HSYMtoName _rom_call(short,(HSym,char*),7A)
#define IsMainFolderStr _rom_call(short,(const char*),77)
#define MakeHSym _rom_call(HSym,(HANDLE,const SYM_ENTRY*),282)
#define MakeHsym MakeHSym
#define partial_len _rom_call(unsigned long,(const char*,unsigned char*),11C)
#define QSysProtected _rom_call(short,(ESQ),88)
#define ResetSymFlags _rom_call(void,(short),8E)
#define SetOK ({__need_in_use_bit;_rom_call_hack(void,(short),456,((((unsigned char*)_rom_call_addr(1FC)-216))),200);})
#define StrToTokN _rom_call(ESI,(const char*,unsigned char*),7B)
#define SymAdd ({__need_in_use_bit;_rom_call(HSym,(SYM_STR),5C);})
#define SymAddMain ({__need_in_use_bit;_rom_call(HSym,(SYM_STR),5D);})
#define SymAddTwin ({__need_in_use_bit;_rom_call(HSym,(SYM_STR),27F);})
#define SymCmp _rom_call(short,(const char*,const char*),81)
#define SymCpy0 _rom_call(void,(char*,const char*),83)
#define SymCpy _rom_call(void,(char*,const char*),82)
#define SymDel ({__need_in_use_bit;_rom_call(short,(SYM_STR),5E);})
#define SymDelTwin _rom_call(short,(SYM_ENTRY*),280)
#define SymFind ({__need_in_use_bit;_rom_call(HSym,(SYM_STR),60);})
#define SymFindFirst _rom_call(SYM_ENTRY*,(SYM_STR,short),6C)
#define SymFindFolderName _rom_call(char*,(void),6F)
#define SymFindFoldername SymFindFolderName
#define SymFindHome _rom_call(HSym,(SYM_STR),62)
#define SymFindMain ({__need_in_use_bit;_rom_call(HSym,(SYM_STR),61);})
#define SymFindNext _rom_call(SYM_ENTRY*,(void),6D)
#define SymFindPrev _rom_call(SYM_ENTRY*,(void),6E)
#define SymFindPtr ({__need_in_use_bit;_rom_call(SYM_ENTRY*,(SYM_STR,short),283);})
#define SymMove ({__need_in_use_bit;_rom_call(short,(SYM_STR,SYM_STR),63);})
#define SYMSTR(s) ({register unsigned short __slen=_rom_call(unsigned long,(const char*),27E)(s);ESI __tempstr=alloca(__slen+2);__tempstr[0]=0;_rom_call(void*,(void*,const void*,long),26A)(__tempstr+1,(s),__slen+1);__tempstr+__slen+1;})
#define SymSysVar _rom_call(short,(const char*),8C)
#define TempFolderName _rom_call(SYM_STR,(short),76)
#define TokToStrN _rom_call(short,(unsigned char*,SYM_STR),7C)
#define ValidateSymName _rom_call(short,(const char*),84)
#define VarRecall ({__need_in_use_bit;_rom_call(HSym,(SYM_STR,short),85);})
#define VarStore ({__need_in_use_bit;_rom_call(HSym,(SYM_STR,short,short,...),86);})
#if MIN_AMS>=101
#define GetDataType _rom_call_hack(short,(CESI),435,((*(unsigned long*)((unsigned char*)_rom_call_addr(319)+134))),200)
#if MIN_AMS>=200
#define SmapTypeStrings _rom_call(const char*,(short),436)
#endif
#endif
/* End Auto-Generated Part */

#endif
