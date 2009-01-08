#ifndef __CTYPE_H
#define __CTYPE_H

#include <default.h>

/* NOTE: These macros use GNU C extensions for defining safe and "smart" */
/* macros, so they are not portable to other C dialects                  */

extern char _extalnum_list[];
extern char _extpunct_list[];

/* Begin Auto-Generated Part */
#define _tolower(c) ((c)+'a'-'A')
#define _toupper(c) ((c)+'A'-'a')
#define isalnum(c) ({register short __c=(c);(__c>='0'&&__c<='9')||(__c>='A'&&__c<='Z')||(__c>='a'&&__c<='z');})
#define isalpha(c) ({register short __c=(c);(__c>='A'&&__c<='Z')||(__c>='a'&&__c<='z');})
#define isascii(c) ((unsigned short)(c)<128)
#define iscntrl(c) ((unsigned short)(c)<14)
#define isdigit(c) ({register short __c=(c);__c>='0'&&__c<='9';})
#define isextalnum(c) ({register short __c=(c);(unsigned short)__c<256&&_extalnum_list[__c>>3]&(1<<(__c&7));})
#define isextlower(c) ({register short __c=(c);(__c>='a'&&__c<='z')||(__c>=224&&__c<=254&&__c!=247);})
#define isextpunct(c) ({register short __c=(c);(unsigned short)__c<256&&_extpunct_list[__c>>3]&(1<<(__c&7));})
#define isextupper(c) ({register short __c=(c);(__c>='A'&&__c<='Z')||(__c>=192&&__c<=222&&__c!=215);})
#define isfrgn(c) ({register short __c=(c);(__c>=128&&__c<148)||(__c==181||__c>=192)&&(__c<=255&&__c!=215&&__c!=247);)}
#define isfrgnalnum(c) ({register short __c=(c);(__c>=128&&__c<=148&&__c!=140)||__c==181||(__c>=192&&__c<=255&&__c!=215&&__c!=247);})
#define isfrgnlower(c) ({register short __c=(c);__c>=224&&__c<=254&&__c!=247;})
#define isfrgnupper(c) ({register short __c=(c);__c>=192&&__c<=222&&__c!=215;})
#define isgraph(c) ({register short __c=(c);__c==11||(__c>13&&__c<256&&__c!=32);})
#define isGreek(c) ({register short __c=(c);(__c>=128&&__c<=148)||__c==181;})
#define islower(c) ({register short __c=(c);__c>='a'&&__c<='z';})
#define isprint(c) ({register short __c=(c);__c==11||(__c>13&&__c<256);})
#define ispunct(c) ({register short __c=(c);__c>=33&&__c<=127&&!((__c>='0'&&__c<='9')||(__c>='A'&&__c<='Z')||(__c>='a'&&__c<='z'));})
#define isspace(c) ({register short __c=(c);(__c>=9&&__c<=13)||__c==32;})
#define isupper(c) ({register short __c=(c);__c>='A'&&__c<='Z';})
#define isxdigit(c) ({register short __c=(c);(__c>='0'&&__c<='9')||(__c>='A'&&__c<='F')||(__c>='a'&&__c<='f');})
#define toascii(c) ((c)&0x7F)
#define toextlower(c) ({register short __c=(c);((__c>='A'&&__c<='Z')||(__c>=192&&__c<=222&&__c!=215))?(__c+'a'-'A'):__c;})
#define toextupper(c) ({register short __c=(c);((__c>='a'&&__c<='z')||(__c>=224&&__c<=254&&__c!=247))?(__c+'A'-'a'):__c;})
#define tolower(c) ({register short __c=(c);(__c>='A'&&__c<='Z')?(__c+'a'-'A'):__c;})
#define toupper(c) ({register short __c=(c);(__c>='a'&&__c<='z')?(__c+'A'-'a'):__c;})
/* End Auto-Generated Part */

#endif
