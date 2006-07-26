#ifndef __STDIO
#define __STDIO

#include <default.h>

/* Begin Auto-Generated Part */
#define EOF (-1)
#define NULL ((void*)0)
#define TMP_MAX 152587890625
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
#ifndef __HAVE_va_list
#define __HAVE_va_list
typedef void*va_list;
#endif
typedef struct{char*fpos;void*base;unsigned short handle;short flags;short unget;unsigned long alloc;unsigned short buffincrement;}FILE;
enum FileFlags{_F_READ=0x0001,_F_WRIT=0x0002,_F_RDWR=0x0003,_F_ERR=0x0010,_F_EOF=0x0020,_F_BIN=0x0040};
typedef unsigned long fpos_t;
enum SeekModes{SEEK_SET,SEEK_CUR,SEEK_END};
typedef CALLBACK void(*vcbprintf_Callback_t)(char c,void**param);
typedef CALLBACK short(*vcbscanf_get_Callback_t)(void*param);
typedef CALLBACK short(*vcbscanf_unget_Callback_t)(void*param);
extern void cbprintf(vcbprintf_Callback_t,void**,const char*,...)__ATTR_TIOS__;
extern short cbscanf(vcbscanf_get_Callback_t asm("a0"),vcbscanf_unget_Callback_t asm("a1"),void* asm("a2"),const char* asm("a3"),...)__ATTR_LIB_ASM__;
#define clearerr(f) ((void)(((f)->flags)&=~(_F_EOF|_F_ERR)))
extern void clrscr(void)__ATTR_LIB_ASM__;
extern short fclose(FILE*)__ATTR_LIB_C__;
#define feof(f) (((f)->flags)&_F_EOF)
#define ferror(f) (((f)->flags)&_F_ERR)
#define fflush(f) ((f)->unget=0)
extern short fgetc(FILE*)__ATTR_LIB_C__;
extern short fgetchar(void)__ATTR_LIB_ASM__;
#define fgetpos(f,p) (((long)((*(p)=ftell(f))))==EOF)
extern char *fgets(char*,short,FILE*)__ATTR_LIB_C__;
extern FILE *fopen(const char*,const char*)__ATTR_LIB_C__;
extern short fprintf(FILE*,const char*,...)__ATTR_TIOS__;
extern short fputc(short,FILE*)__ATTR_TIOS_CALLBACK__;
extern short fputchar(short)__ATTR_TIOS_CALLBACK__;
extern short fputs(const char*,FILE*)__ATTR_LIB_ASM__;
extern unsigned short fread(void*,short,short,FILE*)__ATTR_LIB_C__;
#define freopen(n,m,f) (fclose(f),(f)=fopen((n),(m)),(f))
#define fscanf(__file__,__format__...) cbscanf((vcbscanf_get_Callback_t)__fscanf_get,(vcbscanf_unget_Callback_t)__fscanf_unget,(void*)(__file__),__format__)
extern short fseek(FILE*,long,short)__ATTR_LIB_C__;
extern void fsetbufsize(short,FILE*)__ATTR_LIB_C__;
#define fsetpos(f,p) fseek((f),*(p),SEEK_SET)
extern long ftell(const FILE*)__ATTR_LIB_C__;
extern unsigned short fwrite(const void*,short,short,FILE*)__ATTR_LIB_C__;
#define getc fgetc
#define getchar fgetchar
extern char *gets(char* asm("a2"))__ATTR_LIB_ASM__;
extern char *getsn(char* asm("a2"),long asm("d3"))__ATTR_LIB_ASM__;
#define printf_xy(x,y,f...) ({char __s[200];_rom_call(short,(char*,const char*,...),53)(__s ,##f);_rom_call(void,(short,short,const char*,short),1A9)(x,y,__s,4);})
extern void printf(const char*,...)__ATTR_TIOS__;
#define putc fputc
#define putchar fputchar
extern void puts(const char*)__ATTR_LIB_ASM__;
#define remove unlink
extern short rename(const char*,const char*)__ATTR_LIB_C__;
#define rewind(f) ((void)({FILE*__file1__=(f);fseek(__file1__,0,SEEK_SET);__file1__->flags&=~_F_ERR;}))
#define scanf(__format__...) ({long __maxbuflen=(_rom_call(unsigned long,(void),9C))()-2;(__maxbuflen>0)?({char*__inputbuf=(_rom_call(void*,(long),A2))(__maxbuflen);getsn(__inputbuf,__maxbuflen);short __result=sscanf(__inputbuf,__format__);(_rom_call(void,(void*),A3))(__inputbuf);__result;}):0;})
#define sprintf _rom_call_attr(short,(char*,const char*,...),__attribute__((__format__(__printf__,2,3))),53)
#define sscanf(__buffer__,__format__...) ({__sscanf_string __param__={(__buffer__),0};cbscanf((vcbscanf_get_Callback_t)__sscanf_get,(vcbscanf_unget_Callback_t)__sscanf_unget,&__param__,__format__);})
#define strerror _rom_call(char*,(short),27D)
extern void strputchar(char,void**)__ATTR_TIOS_CALLBACK__;
extern char *tmpnam(char*)__ATTR_LIB_C__;
#define ungetc(c,f) ((f)->unget=((c)|0x8000))
extern short unlink(const char*)__ATTR_LIB_C__;
#define vcbprintf ({register long __a=32+(long)(_rom_call_addr(53));(__vcbprintf__type__)(__a+*(short*)__a);})
extern short vcbscanf(vcbscanf_get_Callback_t asm("a0"),vcbscanf_unget_Callback_t asm("a1"),void* asm("a2"),const char* asm("a3"),va_list asm("a4"))__ATTR_LIB_ASM__;
#define vfprintf(s,f,a) vcbprintf((vcbprintf_Callback_t)fputc,(void**)(s),(f),(a))
#define vfscanf(__file__,__format__,__arglist__) vcbscanf((vcbscanf_get_Callback_t)__fscanf_get,(vcbscanf_unget_Callback_t)__fscanf_unget,(void*)(__file__),(__format__),(__arglist__))
#define vprintf(f,a) vcbprintf((vcbprintf_Callback_t)fputchar,NULL,(f),(a))
#define vscanf(__format__,__arglist__) ({long __maxbuflen=(_rom_call(unsigned long,(void),9C))()-2;(__maxbuflen>0)?({char*__inputbuf=(_rom_call(void*,(long),A2))(__maxbuflen);getsn(__inputbuf,__maxbuflen);short __result=vsscanf(__inputbuf,(__format__),(__arglist__));(_rom_call(void,(void*),A3))(__inputbuf);__result;}):0;})
#define vsprintf(b,f,a) ((void)({void*__p=(b);vcbprintf((vcbprintf_Callback_t)strputchar,&__p,(f),(a));*(char*)__p=0;}))
#define vsscanf(__buffer__,__format__,__arglist__) ({__sscanf_string __param__={(__buffer__),0};vcbscanf((vcbscanf_get_Callback_t)__sscanf_get,(vcbscanf_unget_Callback_t)__sscanf_unget,&__param__,(__format__),(__arglist__));})
/* End Auto-Generated Part */

#define __FERROR(f) ({(f)->flags|=_F_ERR; return EOF;})

typedef void(*__vcbprintf__type__)(vcbprintf_Callback_t,void**,const char*,void*)__ATTR_TIOS__;
CALLBACK short __fscanf_get(FILE *param);
CALLBACK void __fscanf_unget(short c, FILE *param);
typedef struct {const char *buffer; unsigned short pos;} __sscanf_string;
CALLBACK short __sscanf_get(__sscanf_string *param);
CALLBACK void __sscanf_unget(short c, __sscanf_string *param);

#endif
