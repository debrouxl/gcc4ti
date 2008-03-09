/*
  TIGCC Documentation Tools

  Copyright (C) 2002-2004 Sebastian Reichelt
  Copyright (C) 2008 Kevin Kofler

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#include <QString>
#include <QRegExp>
#include <QFile>
#include <QFileInfo>

#define LENGTH(s) (sizeof((s))-1)

static QString GetSecID(QString Ref)
{
  Ref.replace("\r\n", " ");
  Ref.replace('\r', ' ');
  Ref.replace('\n', ' ');
  Ref.replace(",, ", ",,");
  QString Result;
  if (Ref == "Invoking GCC,,GCC Command Options")
    Result = "comopts/comopts";
  else if (Ref == "Overall Options,,Options Controlling the Kind of Output" || Ref == "Overall Options,,Options Controlling the Kind of Output, gcc.info, Using GNU CC")
    Result = "comopts/SEC4";
  else if (Ref == "C Dialect Options,,Options Controlling C Dialect")
    Result = "comopts/SEC6";
  else if (Ref == "Warning Options,,Options to Request or Suppress Warnings")
    Result = "comopts/SEC8";
  else if (Ref == "Debugging Options,,Options for Debugging Your Program or GCC" || Ref == "Debugging Options,,Options for Debugging Your Program or @command{gcc}")
    Result = "comopts/SEC9";
  else if (Ref == "Optimize Options,,Options That Control Optimization")
    Result = "comopts/SEC10";
  else if (Ref == "Preprocessor Options,,Options Controlling the Preprocessor")
    Result = "comopts/SEC11";
  else if (Ref == "Assembler Options,,Passing Options to the Assembler")
    Result = "comopts/SEC12";
  else if (Ref == "Directory Options,,Options for Directory Search")
    Result = "comopts/SEC14";
  else if (Ref == "Spec Files,,Specifying subprocesses and the switches to pass to them")
    Result = "comopts/SEC15";
  else if (Ref == "M680x0 Options,,M680x0 Options")
    Result = "comopts/SEC16";
  else if (Ref == "Code Gen Options,,Options for Code Generation Conventions")
    Result = "comopts/SEC44";
  else if (Ref == "Environment Variables,,Environment Variables Affecting GCC")
    Result = "comopts/SEC45";
  else if (Ref == "CPP Overview")
    Result = "cpp/SEC2";
  else if (Ref == "Initial processing")
    Result = "cpp/SEC3";
  else if (Ref == "Tokenization")
    Result = "cpp/SEC3a";
  else if (Ref == "The preprocessing language")
    Result = "cpp/SEC3b";
  else if (Ref == "Header Files")
    Result = "cpp/SEC4";
  else if (Ref == "Include Syntax")
    Result = "cpp/SEC6";
  else if (Ref == "Include Operation")
    Result = "cpp/SEC7";
  else if (Ref == "Once-Only Headers")
    Result = "cpp/SEC8";
  else if (Ref == "Computed Includes")
    Result = "cpp/SEC8a";
  else if (Ref == "Wrapper Headers")
    Result = "cpp/SEC9";
  else if (Ref == "System Headers")
    Result = "cpp/SEC9a";
  else if (Ref == "Macros")
    Result = "cpp/SEC10";
  else if (Ref == "Object-like Macros")
    Result = "cpp/SEC11";
  else if (Ref == "Function-like Macros")
    Result = "cpp/SEC12";
  else if (Ref == "Macro Arguments")
    Result = "cpp/SEC12a";
  else if (Ref == "Variadic Macros")
    Result = "cpp/SEC13";
  else if (Ref == "Predefined Macros")
    Result = "cpp/SEC14";
  else if (Ref == "Standard Predefined Macros")
    Result = "cpp/SEC15";
  else if (Ref == "Common Predefined Macros")
    Result = "cpp/SEC15a";
  else if (Ref == "System-specific Predefined Macros")
    Result = "cpp/SEC16";
  else if (Ref == "Stringification")
    Result = "cpp/SEC17";
  else if (Ref == "Concatenation")
    Result = "cpp/SEC18";
  else if (Ref == "Undefining and Redefining Macros")
    Result = "cpp/SEC19";
  else if (Ref == "Directives Within Macro Arguments")
    Result = "cpp/SEC20";
  else if (Ref == "Macro Pitfalls")
    Result = "cpp/SEC22";
  else if (Ref == "Misnesting")
    Result = "cpp/SEC23";
  else if (Ref == "Operator Precedence Problems")
    Result = "cpp/SEC24";
  else if (Ref == "Swallowing the Semicolon")
    Result = "cpp/SEC25";
  else if (Ref == "Duplication of Side Effects")
    Result = "cpp/SEC26";
  else if (Ref == "Self-Referential Macros")
    Result = "cpp/SEC27";
  else if (Ref == "Argument Prescan")
    Result = "cpp/SEC28";
  else if (Ref == "Newlines in Arguments")
    Result = "cpp/SEC30";
  else if (Ref == "Conditionals")
    Result = "cpp/SEC31";
  else if (Ref == "Conditional Uses")
    Result = "cpp/SEC32";
  else if (Ref == "Conditional Syntax")
    Result = "cpp/SEC33";
  else if (Ref == "If")
    Result = "cpp/SEC34";
  else if (Ref == "Else")
    Result = "cpp/SEC35";
  else if (Ref == "Elif")
    Result = "cpp/SEC36";
  else if (Ref == "Deleted Code")
    Result = "cpp/SEC37";
  else if (Ref == "Ifdef")
    Result = "cpp/SEC38";
  else if (Ref == "Defined")
    Result = "cpp/SEC38a";
  else if (Ref == "Assertions")
    Result = "cpp/SEC39";
  else if (Ref == "Diagnostics")
    Result = "cpp/SEC40";
  else if (Ref == "Line Control")
    Result = "cpp/SEC41";
  else if (Ref == "Pragmas")
    Result = "cpp/SEC46";
  else if (Ref == "Other Directives")
    Result = "cpp/SEC42";
  else if (Ref == "Preprocessor Output")
    Result = "cpp/SEC43";
  else if (Ref == "Traditional Mode")
    Result = "cpp/SEC70";
  else if (Ref == "Traditional lexical analysis")
    Result = "cpp/SEC71";
  else if (Ref == "Traditional macros")
    Result = "cpp/SEC72";
  else if (Ref == "Traditional miscellany")
    Result = "cpp/SEC73";
  else if (Ref == "Traditional warnings")
    Result = "cpp/SEC74";
  else if (Ref == "Implementation Details")
    Result = "cpp/SEC80";
  else if (Ref == "Implementation-defined behavior")
    Result = "cpp/SEC81";
  else if (Ref == "Implementation limits")
    Result = "cpp/SEC82";
  else if (Ref == "Obsolete Features")
    Result = "cpp/SEC83";
  else if (Ref == "Obsolete once-only headers")
    Result = "cpp/SEC84";
  else if (Ref == "Miscellaneous obsolete features")
    Result = "cpp/SEC85";
  else if (Ref == "Differences from previous versions")
    Result = "cpp/SEC86";
  else if (Ref == "Invocation")
    Result = "cpp/SEC44";
  else if (Ref == "Environment Variables")
    Result = "cpp/SEC45";
  else if (Ref == "C Extensions")
    Result = "gnuexts/gnuexts";
  else if (Ref == "Statement Exprs")
    Result = "gnuexts/SEC63";
  else if (Ref == "Local Labels")
    Result = "gnuexts/SEC64";
  else if (Ref == "Labels as Values")
    Result = "gnuexts/SEC65";
  else if (Ref == "Nested Functions")
    Result = "gnuexts/SEC66";
  else if (Ref == "Constructing Calls")
    Result = "gnuexts/SEC67";
  else if (Ref == "Naming Types")
    Result = "gnuexts/SEC68";
  else if (Ref == "Typeof")
    Result = "gnuexts/SEC69";
  else if (Ref == "Lvalues")
    Result = "gnuexts/SEC70";
  else if (Ref == "Conditional Extensions")
    Result = "gnuexts/SEC71";
  else if (Ref == "Long Long")
    Result = "gnuexts/SEC72";
  else if (Ref == "Complex")
    Result = "gnuexts/SEC73";
  else if (Ref == "Hex Floats")
    Result = "gnuexts/SEC74";
  else if (Ref == "Zero Length")
    Result = "gnuexts/SEC75";
  else if (Ref == "Variable Length")
    Result = "gnuexts/SEC76";
  else if (Ref == "Variadic Macro Extensions")
    Result = "gnuexts/SEC77";
  else if (Ref == "Subscripting")
    Result = "gnuexts/SEC78";
  else if (Ref == "Pointer Arith")
    Result = "gnuexts/SEC79";
  else if (Ref == "Initializers")
    Result = "gnuexts/SEC80";
  else if (Ref == "Compound Literals")
    Result = "gnuexts/SEC81";
  else if (Ref == "Designated Inits")
    Result = "gnuexts/SEC82";
  else if (Ref == "Case Ranges")
    Result = "gnuexts/SEC83";
  else if (Ref == "Cast to Union")
    Result = "gnuexts/SEC84";
  else if (Ref == "Function Attributes")
    Result = "gnuexts/SEC85";
  else if (Ref == "Attribute Syntax")
    Result = "gnuexts/SEC85a";
  else if (Ref == "Function Prototypes")
    Result = "gnuexts/SEC86";
  else if (Ref == "C++ Comments")
    Result = "gnuexts/SEC87";
  else if (Ref == "Dollar Signs")
    Result = "gnuexts/SEC88";
  else if (Ref == "Character Escapes")
    Result = "gnuexts/SEC89";
  else if (Ref == "Alignment")
    Result = "gnuexts/SEC90";
  else if (Ref == "Variable Attributes")
    Result = "gnuexts/SEC91";
  else if (Ref == "Type Attributes")
    Result = "gnuexts/SEC92";
  else if (Ref == "Inline")
    Result = "gnuexts/SEC93";
  else if (Ref == "Extended Asm,,Assembler Instructions with C Expression Operands")
    Result = "gnuexts/SEC94";
  else if (Ref == "Asm Labels")
    Result = "gnuexts/SEC96";
  else if (Ref == "Explicit Reg Vars")
    Result = "gnuexts/SEC97";
  else if (Ref == "Global Reg Vars")
    Result = "gnuexts/SEC98";
  else if (Ref == "Local Reg Vars")
    Result = "gnuexts/SEC99";
  else if (Ref == "Alternate Keywords")
    Result = "gnuexts/SEC100";
  else if (Ref == "Incomplete Enums")
    Result = "gnuexts/SEC101";
  else if (Ref == "Function Names")
    Result = "gnuexts/SEC102";
  else if (Ref == "Return Address")
    Result = "gnuexts/SEC103";
  else if (Ref == "Other Builtins")
    Result = "gnuexts/SEC104";
  else if (Ref == "Escaped Newlines")
    Result = "gnuexts/SEC105";
  else if (Ref == "Multi-line Strings")
    Result = "gnuexts/SEC106";
  else if (Ref == "Mixed Declarations")
    Result = "gnuexts/SEC107";
  else if (Ref == "Unnamed Fields")
    Result = "gnuexts/SEC108";
  else if (Ref == "Volatiles")
    Result = "gnuexts/SEC109";
  else if (Ref == "Empty Structures")
    Result = "gnuexts/SEC111";
  else if (Ref == "Acknowledgements,,GNU Assembler Acknowledgements")
    Result = "gnuasm/acknowledge";
  else if (Ref == "Command Line,,Command Line")
    Result = "gnuasm/SEC9";
  else if (Ref == "Invoking,,Command-Line Options" || Ref == "Invoking,,Comand-Line Options")
    Result = "gnuasm/SEC10";
  else if (Ref == "Overview,,Overview")
    Result = "gnuasm/SEC11";
  else if (Ref == "M68K-Opts,,M680x0 Options")
    Result = "gnuasm/SEC12";
  else if (Ref == "a,,Enable Listings: @option{-a[cdhlns]}")
    Result = "gnuasm/SEC13";
  else if (Ref == "listing,,Configuring listing output: @option{--listing}")
    Result = "gnuasm/SEC14";
  else if (Ref == "o,,Name the Object File: @option{-o}")
    Result = "gnuasm/SEC15";
  else if (Ref == "W,,Control Warnings: @option{-W}, @option{--warn}, @option{--no-warn}, @option{--fatal-warnings}")
    Result = "gnuasm/SEC16";
  else if (Ref == "R,,Join Data and Text Sections: @option{-R}")
    Result = "gnuasm/SEC17";
  else if (Ref == "L,,Include Local Labels: @option{-L}")
    Result = "gnuasm/SEC18";
  else if (Ref == "traditional-format,,Compatible Output: @option{--traditional-format}")
    Result = "gnuasm/SEC19";
  else if (Ref == "M,,Assemble in MRI Compatibility Mode: @option{-M}")
    Result = "gnuasm/SEC19a";
  else if (Ref == "Input Files,,Input Files")
    Result = "gnuasm/SEC21";
  else if (Ref == "Input Files,,Filenames and Line-numbers")
    Result = "gnuasm/SEC22";
  else if (Ref == "Object,,Output (Object) File")
    Result = "gnuasm/SEC23";
  else if (Ref == "Errors,,Error and Warning Messages")
    Result = "gnuasm/SEC24";
  else if (Ref == "Syntax,,Syntax")
    Result = "gnuasm/SEC25";
  else if (Ref == "Preprocessing,,Preprocessing")
    Result = "gnuasm/SEC26";
  else if (Ref == "Whitespace,,Whitespace")
    Result = "gnuasm/SEC27";
  else if (Ref == "Comments,,Comments")
    Result = "gnuasm/SEC28";
  else if (Ref == "Symbol Intro,,Symbols")
    Result = "gnuasm/SEC29";
  else if (Ref == "Statements,,Statements")
    Result = "gnuasm/SEC30";
  else if (Ref == "Constants,,Constants")
    Result = "gnuasm/SEC31";
  else if (Ref == "Characters,,Character Constants")
    Result = "gnuasm/SEC32";
  else if (Ref == "Strings,,Strings" || Ref == "Strings")
    Result = "gnuasm/SEC33";
  else if (Ref == "Chars,,Characters")
    Result = "gnuasm/SEC34";
  else if (Ref == "Numbers,,Number Constants")
    Result = "gnuasm/SEC35";
  else if (Ref == "Integers,,Integers")
    Result = "gnuasm/SEC36";
  else if (Ref == "Bignums,,Bignums")
    Result = "gnuasm/SEC37";
  else if (Ref == "Flonums,,Flonums")
    Result = "gnuasm/SEC38";
  else if (Ref == "M68K-Syntax,,Syntax")
    Result = "gnuasm/SEC216";
  else if (Ref == "M68K-Moto-Syntax,,Motorola Syntax")
    Result = "gnuasm/SEC217";
  else if (Ref == "M68K-Branch,,Branch Improvement")
    Result = "gnuasm/SEC221";
  else if (Ref == "M68K-Chars,,Special Characters")
    Result = "gnuasm/SEC222";
  else if (Ref == "Sections,,Sections and Relocation" || Ref == "Secs Background,,Background")
    Result = "gnuasm/SEC39";
  else if (Ref == "Ld Sections,,Linker Sections")
    Result = "gnuasm/SEC41";
  else if (Ref == "As Sections,,Assembler Internal Sections")
    Result = "gnuasm/SEC42";
  else if (Ref == "Sub-Sections,,Sub-Sections")
    Result = "gnuasm/SEC43";
  else if (Ref == "bss,,bss Section")
    Result = "gnuasm/SEC44";
  else if (Ref == "Symbols,,Symbols" || Ref == "Symbols")
    Result = "gnuasm/SEC45";
  else if (Ref == "Labels,,Labels")
    Result = "gnuasm/SEC46";
  else if (Ref == "Setting Symbols,,Giving Symbols Other Values")
    Result = "gnuasm/SEC47";
  else if (Ref == "Symbol Names" || Ref == "Symbol Names,,Symbol Names")
    Result = "gnuasm/SEC48";
  else if (Ref == "Symbol Names,,Local Symbol Names")
    Result = "gnuasm/SEC48L";
  else if (Ref == "Symbol Names,,Dollar Local Labels")
    Result = "gnuasm/SEC48LD";
  else if (Ref == "Dot,,The Special Dot Symbol")
    Result = "gnuasm/SEC49";
  else if (Ref == "Symbol Attributes" || Ref == "Symbol Attributes,,Symbol Attributes")
    Result = "gnuasm/SEC50";
  else if (Ref == "Symbol Value,,Value")
    Result = "gnuasm/SEC51";
  else if (Ref == "Symbol Type,,Type")
    Result = "gnuasm/SEC52";
  else if (Ref == "COFF Symbols,,Symbol Attributes for COFF")
    Result = "gnuasm/SEC56";
  else if (Ref == "COFF Symbols,,Primary Attributes")
    Result = "gnuasm/SEC57";
  else if (Ref == "COFF Symbols,,Auxiliary Attributes")
    Result = "gnuasm/SEC58";
  else if (Ref == "Expressions" || Ref == "Expressions,,Expressions")
    Result = "gnuasm/SEC60";
  else if (Ref == "Empty Exprs,,Empty Expressions")
    Result = "gnuasm/SEC61";
  else if (Ref == "Integer Exprs,,Integer Expressions")
    Result = "gnuasm/SEC62";
  else if (Ref == "Arguments,,Arguments")
    Result = "gnuasm/SEC63";
  else if (Ref == "Operators,,Operators")
    Result = "gnuasm/SEC64";
  else if (Ref == "Prefix Ops,,Prefix Operator" || Ref == "Prefix Ops,,Prefix Operators")
    Result = "gnuasm/SEC65";
  else if (Ref == "Infix Ops,,Infix Operator" || Ref == "Infix Ops,,Infix Operators")
    Result = "gnuasm/SEC66";
  else if (Ref == "Pseudo Ops,,Assembler Directives")
    Result = "gnuasm/SEC67";
  else if (Ref.contains(".abort"))
    Result = "gnuasm/SEC68";
  else if (Ref.contains(".ABORT"))
    Result = "gnuasm/SEC69";
  else if (Ref.contains(".align"))
    Result = "gnuasm/SEC70";
  else if (Ref.contains(".app-file"))
    Result = "gnuasm/SEC71";
  else if (Ref.contains(".ascii"))
    Result = "gnuasm/SEC72";
  else if (Ref.contains(".asciz"))
    Result = "gnuasm/SEC73";
  else if (Ref.contains(".balign"))
    Result = "gnuasm/SEC74";
  else if (Ref.contains(".byte"))
    Result = "gnuasm/SEC75";
  else if (Ref.contains(".comm"))
    Result = "gnuasm/SEC76";
  else if (Ref.contains(".data"))
    Result = "gnuasm/SEC77";
  else if (Ref.contains(".def"))
    Result = "gnuasm/SEC78";
  else if (Ref.contains(".desc"))
    Result = "gnuasm/SEC79";
  else if (Ref.contains(".dim"))
    Result = "gnuasm/SEC80";
  else if (Ref.contains(".double"))
    Result = "gnuasm/SEC81";
  else if (Ref.contains(".eject"))
    Result = "gnuasm/SEC82";
  else if (Ref.contains(".elseif"))
    Result = "gnuasm/SEC83IF";
  else if (Ref.contains(".else"))
    Result = "gnuasm/SEC83";
  else if (Ref.contains(".endef"))
    Result = "gnuasm/SEC84";
  else if (Ref.contains(".endif"))
    Result = "gnuasm/SEC84IF";
  else if (Ref.contains(".endfunc"))
    Result = "gnuasm/SEC84FUNC";
  else if (Ref.contains(".endm"))
    Result = "gnuasm/SEC84M";
  else if (Ref.contains(".endr"))
    Result = "gnuasm/SEC84R";
  else if (Ref.contains(".end"))
    Result = "gnuasm/SEC83b";
  else if (Ref.contains(".equiv"))
    Result = "gnuasm/SEC87";
  else if (Ref.contains(".equ"))
    Result = "gnuasm/SEC86";
  else if (Ref.contains(".err"))
    Result = "gnuasm/SEC88";
  else if (Ref.contains(".even"))
    Result = "gnuasm/SEC70E";
  else if (Ref.contains(".exitm"))
    Result = "gnuasm/SEC85";
  else if (Ref.contains(".extern"))
    Result = "gnuasm/SEC89";
  else if (Ref.contains(".fail"))
    Result = "gnuasm/SEC89a";
  else if (Ref.contains(".file"))
    Result = "gnuasm/SEC90";
  else if (Ref.contains(".fill"))
    Result = "gnuasm/SEC91";
  else if (Ref.contains(".float"))
    Result = "gnuasm/SEC92";
  else if (Ref.contains(".func"))
    Result = "gnuasm/SEC92a";
  else if (Ref.contains(".globl") || Ref.contains(".global"))
    Result = "gnuasm/SEC93";
  else if (Ref.contains(".hword"))
    Result = "gnuasm/SEC94";
  else if (Ref.contains(".ident"))
    Result = "gnuasm/SEC95";
  else if (Ref.contains(".ifdef"))
    Result = "gnuasm/SEC96D";
  else if (Ref.contains(".ifndef"))
    Result = "gnuasm/SEC96N";
  else if (Ref.contains(".ifnotdef"))
    Result = "gnuasm/SEC96ND";
  else if (Ref.contains(".if ") || Ref.contains(".if}"))
    Result = "gnuasm/SEC96";
  else if (Ref.contains(".incbin"))
    Result = "gnuasm/SEC97B";
  else if (Ref.contains(".include"))
    Result = "gnuasm/SEC97";
  else if (Ref.contains(".int"))
    Result = "gnuasm/SEC98";
  else if (Ref.contains(".irpc"))
    Result = "gnuasm/SEC100";
  else if (Ref.contains(".irp"))
    Result = "gnuasm/SEC99";
  else if (Ref.contains(".lcomm"))
    Result = "gnuasm/SEC101";
  else if (Ref.contains(".lflags"))
    Result = "gnuasm/SEC102";
  else if (Ref.contains(".line"))
    Result = "gnuasm/SEC103";
  else if (Ref.contains(".ln"))
    Result = "gnuasm/SEC105";
  else if (Ref.contains(".list"))
    Result = "gnuasm/SEC107";
  else if (Ref.contains(".long"))
    Result = "gnuasm/SEC108";
  else if (Ref.contains(".macro"))
    Result = "gnuasm/SEC109";
  else if (Ref.contains(".mri"))
    Result = "gnuasm/SEC106";
  else if (Ref.contains(".nolist"))
    Result = "gnuasm/SEC110";
  else if (Ref.contains(".octa"))
    Result = "gnuasm/SEC111";
  else if (Ref.contains(".org"))
    Result = "gnuasm/SEC112";
  else if (Ref.contains(".p2align[wl]"))
    Result = "gnuasm/SEC113";
  else if (Ref.contains(".print"))
    Result = "gnuasm/SEC113a";
  else if (Ref.contains(".psize"))
    Result = "gnuasm/SEC114";
  else if (Ref.contains(".purgem"))
    Result = "gnuasm/SEC114a";
  else if (Ref.contains(".quad"))
    Result = "gnuasm/SEC115";
  else if (Ref.contains(".rept"))
    Result = "gnuasm/SEC116";
  else if (Ref.contains(".sbttl"))
    Result = "gnuasm/SEC117";
  else if (Ref.contains(".scl"))
    Result = "gnuasm/SEC118";
  else if (Ref == "Section,,COFF Version")
    Result = "gnuasm/SEC119";
  else if (Ref.contains(".set"))
    Result = "gnuasm/SEC120";
  else if (Ref.contains(".short"))
    Result = "gnuasm/SEC121";
  else if (Ref.contains(".single"))
    Result = "gnuasm/SEC122";
  else if (Ref == "Size,,COFF Version")
    Result = "gnuasm/SEC123";
  else if (Ref.contains(".sleb128"))
    Result = "gnuasm/SEC124";
  else if (Ref.contains(".skip"))
    Result = "gnuasm/SEC125";
  else if (Ref.contains(".space"))
    Result = "gnuasm/SEC126";
  else if (Ref.contains(".stabd"))
    Result = "gnuasm/SEC127";
  else if (Ref.contains(".stabn"))
    Result = "gnuasm/SEC127N";
  else if (Ref.contains(".stabs"))
    Result = "gnuasm/SEC127S";
  else if (Ref.contains(".string"))
    Result = "gnuasm/SEC128";
  else if (Ref.contains(".struct"))
    Result = "gnuasm/SEC128a";
  else if (Ref.contains(".symver"))
    Result = "gnuasm/SEC129";
  else if (Ref.contains(".tag"))
    Result = "gnuasm/SEC130";
  else if (Ref.contains(".text"))
    Result = "gnuasm/SEC131";
  else if (Ref.contains(".title"))
    Result = "gnuasm/SEC132";
  else if (Ref == "Type,,COFF Version")
    Result = "gnuasm/SEC133";
  else if (Ref.contains(".uleb128"))
    Result = "gnuasm/SEC134";
  else if (Ref.contains(".val"))
    Result = "gnuasm/SEC135";
  else if (Ref.contains(".vtable_entry"))
    Result = "gnuasm/SEC135a";
  else if (Ref.contains(".word"))
    Result = "gnuasm/SEC136";
  else if (Ref == "Bug Criteria,,Have You Found a Bug?")
    Result = "Bug Criteria,,Have You Found a Bug";
  else
    Result = Ref.replace('/', '_');
  if (Result.contains(':'))
    Result.truncate(Result.indexOf(':'));
  return Result;
}

static void PreProcessSource(QString &Source)
{
  Source.replace("@value{AS}", "as");
  Source.replace("@value{LD}", "ld");
  Source.replace("@value{GCC}", "gcc");
  Source.replace("@value{CPP}", "cpp");
  Source.replace("''", "\"");
  Source.replace("``", "\"");
  Source.replace(QString::fromLatin1("\xb4\xb4"), "\"");
}

static void PostProcessDest(QString &Dest)
{
  int P, I;
  Dest.replace("<BR><BR>\r\n<BR><BR>", "<BR><BR>");
  Dest.replace("\r\n\r\n<BR><BR>", "\r\n<BR><BR>");
  Dest.replace("<BR><BR>\r\n\r\n", "<BR><BR>\r\n");
  Dest.replace("<LI>\r\n</LI>\r\n", "");
  Dest.replace("<LI>\r\n<BR><BR>", "<LI>");
  Dest.replace("<BR><BR>\r\n</LI>\r\n</UL>", "</LI>\r\n</UL>");
  Dest.replace("<BR><BR>\r\n</LI>\r\n</OL>", "</LI>\r\n</OL>");
  Dest.replace("<BR><BR>\r\n<PRE>", "<PRE>");
  Dest.replace("<BR><BR>\r\n</PRE>", "</PRE>");
  Dest.replace("</PRE>\r\n<BR><BR>", "</PRE>");
  Dest.replace("\r\n\r\n</PRE>", "\r\n</PRE>");
  Dest.replace("<BR><BR>\r\n<UL", "<UL");
  Dest.replace("</UL>\r\n<BR><BR>", "</UL>");
  Dest.replace("<BR><BR>\r\n<OL", "<OL");
  Dest.replace("</OL>\r\n<BR><BR>", "</OL>");
  Dest.replace("<BR><BR>\r\n<DL", "<DL");
  Dest.replace("</DL>\r\n<BR><BR>", "</DL>");
  Dest.replace("<BR><BR>\r\n</DL>", "</DL>");
  Dest.replace("<DD><DT>", "<BR>");
  P = Dest.indexOf("</CODE></A>");
  while (P >= 0) {
    Dest[P + 2] = 'A';
    Dest[P + 3] = '>';
    Dest[P + 4] = '<';
    Dest[P + 5] = '/';
    Dest[P + 6] = 'C';
    Dest[P + 7] = 'O';
    Dest[P + 8] = 'D';
    Dest[P + 9] = 'E';
    I = P - 1;
    while (I >= 0 && Dest.mid(I, LENGTH("<CODE>")) != "<CODE>")
      I--;
    if (I >= 0) {
      Dest.remove(I, LENGTH("<CODE>"));
      I--;
      while (I >= 0 && Dest[I] != '<')
        I--;
      if (I >= 0)
        Dest.insert(I, "<CODE>");
    }
    P = Dest.indexOf("</CODE></A>");
  }
  Dest.replace(".   ", ".  ");
}

enum TListType {ltNone, ltNormal, ltColumns, ltFiles};

static QString FullContents, NewContents, CurFile, CurNode, CurSection, CurSectionTitle, CurSubFile;
static int P, SecStart, PreFormatted, SCase, CodeFormat;
static bool InsertedSeeAlso = false;

static void ProcessSecID(QString &SecID)
{
  if (SecID == "Overview" && CurFile == "cpp")
    SecID = "CPP Overview";
  else if (SecID == "Conditionals" && CurFile == "gnuexts")
    SecID = "Conditional Extensions";
  else if (SecID == "Variadic Macros" && CurFile == "gnuexts")
    SecID = "Variadic Macro Extensions";
  else if (SecID == "Pragmas" && CurFile == "gnuexts")
    SecID = "Pragma Extensions";
  else if (SecID == "Acknowledgements,,Acknowledgements" && CurFile == "gnuasm")
    SecID = "Acknowledgements,,GNU Assembler Acknowledgements";
}

static QString GetLinkTo(QString Ref)
{
  ProcessSecID(Ref);
  QString Result = GetSecID(Ref);
  if (Result.startsWith(CurFile + '/'))
    return "$$LINK(" + Result.mid((CurFile + '/').length()) + ')';
  else
    return "$$INFOLINK(" + Result + ')';
}

static void TrimHTMLWhiteSpace(QString &Str)
{
  bool Stop;
  do {
    Stop = true;
    Str = Str.trimmed();
    if (Str.length() >= 4) {
      if (Str.left(4).toUpper() == "<BR>") {
        Str.remove(0, 4);
        Stop = false;
      } else if (Str.left(3).toUpper() == "<P>") {
        Str.remove(0, 3);
        Stop = false;
      } else if (Str.right(4).toUpper() == "<BR>") {
        Str.chop(4);
        Stop = false;
      } else if (Str.right(4).toUpper() == "</P>") {
        Str.chop(4);
        Stop = false;
      }
    }
  } while (!Stop);
}

static void WriteFile(const QString &FileName, const QString &Contents)
{
  QFile file(FileName);
  if (!file.open(QIODevice::WriteOnly))
    qFatal("failed to open file '%s'", FileName.toLocal8Bit().data());
  QByteArray ba = Contents.toLatin1();
  if (file.write(ba) < ba.length())
    qFatal("failed to write to file '%s'", FileName.toLocal8Bit().data());
  file.close();
}

static void FinishSection()
{
  int I;
  QString SecID, TempContents, S;
  SecID = CurNode;
  if (!CurSection.isEmpty() && ((CurFile != "cpp" && CurFile != "gnuexts") || SecID == "Extended Asm"))
    SecID.append(",," + CurSection);
  ProcessSecID(SecID);
  SecID = GetSecID(SecID);
  if (!CurSubFile.isEmpty())
    SecID.append('_' + QString(CurSubFile).replace('/', " and "));
  TempContents = NewContents.mid(SecStart);
  PostProcessDest(TempContents);
  TrimHTMLWhiteSpace(TempContents);
  if (CurSectionTitle.contains("@code") && CurSectionTitle.contains('.')) {
    CurSectionTitle.remove(0, CurSectionTitle.indexOf('.'));
    I = CurSectionTitle.indexOf(QRegExp("[^A-Za-z_.0-9\\[\\]]"), 2);
    if (I >= 0)
      CurSectionTitle.truncate(I);
  }
  if (!TempContents.isEmpty()) {
    if (!CurSubFile.isEmpty())
      S = CurSubFile;
    else
      S = CurSectionTitle;
    WriteFile(SecID + ".hss", "[Main]\r\nTitle=" + S + "\r\n\r\n[Top]\r\n" + TempContents + "\r\n");
  }
  SecStart = NewContents.length();
}

static void StartSeeAlso()
{
  int I = NewContents.length() - 1;
  while (I >= 0 && NewContents[I].isSpace())
    I--;
  InsertedSeeAlso = true;
  if (I < 0 || NewContents[I] == '.' || NewContents[I] == '>')
    NewContents.append("See ");
  else if (QString(",;:([{").contains(NewContents[I]))
    NewContents.append("see ");
  else
    InsertedSeeAlso = false;
}

static void EndSeeAlso()
{
  if (InsertedSeeAlso && P < FullContents.length() && FullContents[P] == ',')
    P++;
  InsertedSeeAlso = false;
}

static QString LoadFile(const QString &FileName)
{
  QFile file(FileName);
  if (!file.open(QIODevice::ReadOnly))
    qFatal("failed to open file '%s'", FileName.toLocal8Bit().data());
  QString result = QString::fromLatin1(file.readAll());
  file.close();
  return result;
}

static QString HTMLizeChar(QChar Input)
{
  switch (Input.unicode()) {
    case '&':
      return "&amp;";
    case '<':
      return "&lt;";
    case '>':
      return "&gt;";
    case '\"':
      return "&quot;";
    default:
      return Input;
  }
}

static QString HTMLize(const QString &Input)
{
  QString Result = Input;
  Result.replace("&#", "$$$#");
  Result.replace("&", "&amp;");
  Result.replace("$$$#", "&#");
  Result.replace(QChar(0xB7), "&nbsp;");
  Result.replace('<', "&lt;");
  Result.replace('>', "&gt;");
  Result.replace('\"', "&quot;");
  return Result;
}

static void ConvertFromP(TListType ListType = ltNone)
{
  int I, J;
  QString Tag, Parameters, Quotes, S;
  while (P < FullContents.length()) {
    if (FullContents[P] == '}') {
      P++;
      break;
    } else if (FullContents[P] == '{') {
      P++;
      ConvertFromP(ListType);
    } else if (FullContents[P] == '\r') {
      while (!NewContents.isEmpty() && NewContents.endsWith(' '))
        NewContents.chop(1);
      if (PreFormatted > 0) {
        NewContents.append("\r\n");
        P++;
      } else {
        if (FullContents.mid(P, 2) == "\r\r" || FullContents.mid(P, 4) == "\r\n\r\n" || (FullContents.mid(P, 2) == "\r@" || FullContents.mid(P, 3) == "\r\n@") && (!FullContents.mid(P, 25).contains('{') && !FullContents.mid(P, 25).contains("if"))) {
          if (!NewContents.endsWith("<BR><BR>\r\n"))
            NewContents.append("\r\n<BR><BR>\r\n");
          while (P < FullContents.length() && (FullContents[P] == '\r' || FullContents[P] == '\n'))
            P++;
        } else {
          if (!NewContents.endsWith("\r\n"))
            NewContents.append("\r\n");
          P++;
        }
      }
    } else if (FullContents[P] == '@') {
      P++;
      if (P < FullContents.length()) {
        if (FullContents[P] >= 'A' && FullContents[P] <= 'Z'
            || FullContents[P] >= 'a' && FullContents[P] <= 'z'
            || FullContents[P] >= '0' && FullContents[P] <= '9') {
          Tag = QString();
          Parameters = QString();
          Quotes = QString();
          while (P < FullContents.length() && !QString("\r\n {").contains(FullContents[P])) {
            Tag.append(FullContents[P]);
            P++;
          }
          if (P < FullContents.length() && FullContents[P] == '{') {
            P++;
            if (Tag == QString('w')
                || Tag == QString('r')
                || Tag == "gccoptlist"
                || Tag == "key"
                || Tag == "value"
                || Tag == "copyrightnotice"
                || Tag == "covertexts"
                || Tag == "acronym")
              Tag = QString();
            else if (Tag == "sc") ;
            else if (Tag == "anchor") {
              Tag = QString();
              while (P < FullContents.length() && !QString("@{}").contains(FullContents[P]))
                P++;
            } else if (Tag == "copyright") {
              Tag = QString();
              NewContents.append("(c)");
            } else if (Tag == "TeX") {
              Tag = QString();
              NewContents.append("TeX");
            } else if (Tag == "equiv") {
              Tag = QString();
              NewContents.append("&lt;=&gt;");
            } else if (Tag == "expansion") {
              Tag = QString();
              NewContents.append("expands to");
            } else if (Tag == "error") {
              Tag = QString();
              NewContents.append("Error:");
            } else if (Tag == "minus") {
              Tag = QString();
              NewContents.append("<FONT FACE=\"Courier New\">-</FONT>");
            } else if ((Tag == "dots")) {
              Tag = QString();
              NewContents.append("...");
            } else if (Tag == "code" || Tag == "samp" || Tag == "command" || Tag == "file" || Tag == "math" || Tag == "env" || Tag == "kbd" || Tag == QString('t')) {
              if (FullContents.length() > P + 2 && FullContents[P] == '-' && ((FullContents[P + 1] >= 'A' && FullContents[P + 1] <= 'Z') || (FullContents[P + 1] >= 'a' && FullContents[P + 1] <= 'z') || (FullContents[P + 1] >= '0' && FullContents[P + 1] <= '9') || (FullContents[P + 1] == '-' && ((FullContents[P + 2] >= 'A' && FullContents[P + 2] <= 'Z') || (FullContents[P + 2] >= 'a' && FullContents[P + 2] <= 'z') || (FullContents[P + 2] >= '0' && FullContents[P + 2] <= '9'))))) {
                Tag = 'B';
                Quotes = '\'';
              } else
                Tag = "CODE";
            } else if (Tag == "strong" || Tag == QString('b'))
              Tag = 'B';
            else if (Tag == "emph" || Tag == "var" || Tag == QString('i'))
              Tag = 'I';
            else if (Tag == "dfn")
              Tag = 'U';
            else if (Tag == "option") {
              Tag = 'B';;
              Quotes = '\'';
            } else if (Tag == "cite") {
              Tag = QString();
              Quotes = '"';
            } else if (Tag == "ref" || Tag == "xref" || Tag == "pxref") {
              Tag = 'A';
              I = FullContents.mid(P, 255).indexOf('}');
              if (I >= 0) {
                while (FullContents.mid(P, I).count('{') > FullContents.mid(P, I).count('}'))
                  I++;
                Parameters = "HREF=\"" + GetLinkTo(FullContents.mid(P, I)) + '"';
                if ((FullContents.mid(P, 255).contains(",,") || FullContents.mid(P, 255).contains(",\r,") || FullContents.mid(P, 255).contains(",\r\n,")) && !FullContents.mid(P, FullContents.mid(P, 255).indexOf(',') + 1).contains('{')) {
                  FullContents.remove(P, FullContents.mid(P, 255).indexOf(',') + 1);
                  FullContents.remove(P, FullContents.mid(P, 255).indexOf(',') + 1);
                  while (FullContents[P].isSpace())
                   FullContents.remove(P, 1);
                }
                StartSeeAlso();
              }
            } else if (Tag == "uref") {
              Tag = 'A';
              I = FullContents.mid(P, 255).indexOf('}');
              if (I >= 0) {
                Parameters = "HREF=\"" + FullContents.mid(P, I) + '"';
                StartSeeAlso();
              }
            } else if (Tag == "email") {
              Tag = 'A';
              I = FullContents.mid(P, 255).indexOf('}');
              if (I >= 0)
                Parameters = "HREF=\"mailto:" + FullContents.mid(P, I).replace("@@", "@") + '"';
            } else if (Tag == "footnote") {
              Tag = QString();
              Quotes = '(';
              NewContents.append(' ');
            } else
              qFatal("Unsupported style \"%s\"", Tag.toLocal8Bit().data());
            if (!Tag.isEmpty() && Tag != "sc") {
              NewContents.append('<' + Tag);
              if (!Parameters.isEmpty())
                NewContents.append(' ' + Parameters);
              NewContents.append('>' + Quotes);
            }
            if (Tag == "sc")
              SCase++;
            else if (Tag == "CODE")
              CodeFormat++;
            ConvertFromP(ListType);
            if (Tag == "sc")
              SCase--;
            else if (Tag == "CODE")
              CodeFormat--;
            if (Quotes == QString('('))
              Quotes = ')';
            if (!Tag.isEmpty() && Tag != "sc")
              NewContents.append(Quotes + "</" + Tag + '>');
            if (Tag == QString('A'))
              EndSeeAlso();
          } else {
            while (P < FullContents.length() && FullContents[P] == ' ')
              P++;
            if (Tag != "center") {
              while (P < FullContents.length() && !(FullContents[P] == '\r' || FullContents[P] == '\n')) {
                Parameters = Parameters + FullContents[P];
                P++;
              }
              while (P < FullContents.length() && (FullContents[P] == '\r' || FullContents[P] == '\n'))
                P++;
              Parameters = Parameters.trimmed();
              if (Tag == "end") {
                P--;
                if (FullContents[P] == '\n')
                  P--;
                break;
              } else if (Tag == QString('c')
                         || Tag == "comment"
                         || Tag == "set"
                         || Tag == "setfilename"
                         || Tag == "settitle"
                         || Tag == "title"
                         || Tag == "subtitle"
                         || Tag == "cindex"
                         || Tag == "kindex"
                         || Tag == "opindex"
                         || Tag == "findex"
                         || Tag == "gol"
                         || Tag == "noindent"
                         || Tag == "need"
                         || Tag == "page"
                         || Tag == "setchapternewpage"
                         || Tag == "finalout"
                         || Tag == "printindex"
                         || Tag == "syncodeindex"
                         || Tag == "sp"
                         || Tag == "author"
                         || Tag == "vskip"
                         || Tag == "top"
                         || Tag == "refill"
                         || Tag == "lowersections"
                         || Tag == "raisesections"
                         || Tag == "unnumbered"
                         || Tag == "unnumberedsec"
                         || Tag == "contents"
                         || Tag == "defcodeindex"
                         || Tag == "dircategory"
                         || Tag == "tab"
                         || Tag == "bye"
                         || Tag == "insertcopying") ;
              else if (Tag == "node") {
                if (!(Parameters == "Ln" && CurNode == "Line")) {
                  FinishSection();
                  CurNode = Parameters;
                  CurSection = QString();
                  CurSectionTitle = Parameters;
                }
              } else if (Tag == "section" || Tag == "subsection" || Tag == "subsubsection" || Tag == "subheading" || Tag == "chapter") {
                if (!(CurNode == "Line" && Parameters.contains(".ln"))) {
                  FinishSection();
                  CurSection = Parameters;
                  CurSectionTitle = Parameters;
                }
              } else if (Tag == "ignore"
                         || Tag == "tex"
                         || Tag == "menu"
                         || Tag == "direntry"
                         || Tag == "copying"
                         || (Tag == "ifset" && (Parameters != "M68K" && Parameters != "M680X0" && Parameters != "COFF" && Parameters != "W16" && Parameters != "have-stabs" && Parameters != "cppmanual"))) {
                I = NewContents.length();
                ConvertFromP(ListType);
                NewContents.truncate(I);
              } else if (Tag == "group"
                         || Tag == "ifset"
                         || Tag == "ifclear"
                         || Tag == "ifinfo"
                         || Tag == "iftex"
                         || Tag == "ifnottex"
                         || Tag == "ifhtml"
                         || Tag == "ifnothtml"
                         || Tag == "macro"
                         || Tag == "titlepage"
                         || Tag == "quotation"
                         || Tag == "display"
                         || Tag == "detailmenu"
                         || Tag == "html")
                ConvertFromP(ListType);
              else if (Tag == "example"
                       || Tag == "smallexample"
                       || Tag == "multitable"
                       || Tag == "format") {
                NewContents.append("<PRE>");
                PreFormatted++;
                ConvertFromP(ListType);
                PreFormatted--;
                NewContents.append("</PRE>");
              } else if (Tag == "table" || Tag == "ftable" || Tag == "vtable") {
                if (CurFile == "gnuexts") {
                  S = NewContents;
                  I = SecStart;
                  SecStart = NewContents.length();
                  ConvertFromP(ltFiles);
                  FinishSection();
                  CurSubFile = QString();
                  SecStart = I;
                  NewContents = S;
                } else {
                  if (ListType == ltColumns)
                    NewContents.append("<BR><BR>");
                  NewContents.append("<DL>\r\n");
                  ConvertFromP(ltColumns);
                  NewContents.append("</DL>");
                  if (ListType == ltColumns)
                    NewContents.append("<BR>");
                }
              } else if (Tag == "itemize") {
                NewContents.append("<UL>\r\n<LI>\r\n");
                ConvertFromP(ltNormal);
                NewContents.append("</LI>\r\n</UL>");
              } else if (Tag == "enumerate") {
                NewContents.append("<OL");
                if (!Parameters.isEmpty()) {
                  if (Parameters[1] >= '0' && Parameters[1] <= '9')
                    NewContents.append(" START=\"" + Parameters + '"');
                  else
                    NewContents.append(" TYPE=\"" + Parameters + '"');
                }
                NewContents.append(">\r\n<LI>\r\n");
                ConvertFromP(ltNormal);
                NewContents.append("</LI>\r\n</OL>");
              } else if (Tag == "item" || Tag == "itemx") {
                switch (ListType) {
                  case ltNormal:
                    NewContents.append("</LI>\r\n<LI>\r\n");
                    FullContents.insert(P, Parameters + "\r\n");
                    break;
                  case ltColumns:
                    Parameters = HTMLize(Parameters);
                    I = Parameters.indexOf('@');
                    while (I >= 0) {
                      Parameters.remove(I, 1);
                      if (Parameters.length() > I && (Parameters[I] >= 'A' && Parameters[I] <= 'Z' || Parameters[I] >= 'a' && Parameters[I] <= 'z')) {
                        if (Parameters.mid(I).startsWith("var{")) {
                          Parameters.remove(I, LENGTH("var{"));
                          J = Parameters.mid(I).indexOf('}');
                          if (J >= 0) {
                            Parameters.remove(I + J, 1);
                            Parameters.insert(I + J, "</I>");
                            Parameters.insert(I, "<I>");
                          }
                        } else {
                          while (Parameters.length() > I && Parameters[I] != '{')
                            Parameters.remove(I, 1);
                          if (Parameters.length() >= I)
                            Parameters.remove(I, 1);
                          J = Parameters.mid(I).indexOf('}');
                          if (J >= 0)
                            Parameters.remove(I + J, 1);
                        }
                      }
                      I = Parameters.indexOf('@');
                    }
                    NewContents.append("<DT><B>" + Parameters + "</B>\r\n<DD>");
                    break;
                  case ltFiles:
                    if (P > SecStart + 5) {
                      if (!CurSubFile.isEmpty())
                        FinishSection();
                      CurSubFile = Parameters;
                      CurSubFile.remove("__builtin_");
                      CurSubFile.remove(' ');
                    }
                    break;
                  default:
                    Parameters.insert(P, FullContents);
                }
              } else if (Tag == "exdent")
                Parameters.insert(P, FullContents);
              else if (Tag == "deftypefn") {
                S = NewContents;
                I = SecStart;
                SecStart = NewContents.length();
                CurSubFile = Parameters;
                CurSubFile.remove("__builtin_");
                CurSubFile.remove(' ');
                ConvertFromP();
                FinishSection();
                CurSubFile = QString();
                SecStart = I;
                NewContents = S;
              } else if (Tag == "include") {
                if (QFile::exists(Parameters) || (Parameters != "asconfig.texi" && Parameters != "md.texi" && !Parameters.startsWith("c-"))) {
                  S = LoadFile(Parameters);
                  PreProcessSource(S);
                  FullContents.insert(P, S);
                }
              } else
                qFatal("Unsupported command \"%s\"", Tag.toLocal8Bit().data());
            }
          }
        } else {
          if (FullContents[P] != ':')
            NewContents.append(HTMLizeChar(FullContents[P]));
          P++;
        }
      }
    } else {
      if (FullContents[P] != '\n') {
        if (QString(" \r\t").contains(FullContents[P])) {
          if ((NewContents.endsWith("eg") || NewContents.endsWith("ie")) && (NewContents.length() > 2) && (QString(" \r\t([{").contains(NewContents[NewContents.length() - 3]))) {
            NewContents.insert(NewContents.length() - 1, '.');
            NewContents.append('.');
          }
        }
        if (SCase > 0)
          NewContents.append(HTMLizeChar(FullContents[P].toUpper()));
        else if (FullContents[P] == QChar(0xB4) || FullContents[P] == '`')
          NewContents.append('\'');
        else if (FullContents[P] == '-' && NewContents.endsWith("--")) {
          NewContents.append(' ');
          NewContents[NewContents.length() - 3] = ' ';
        } else if (FullContents[P] == ' ' && CodeFormat > 0)
          NewContents.append("&nbsp;");
        else
          NewContents.append(HTMLizeChar(FullContents[P]));
      }
      P++;
    }
  }
}

int main(int argc, char **argv)
{
  if (argc < 2) qFatal("Missing parameter");
  QString ParamStr1 = QString::fromLocal8Bit(argv[1]);
  if (ParamStr1.contains("invoke"))
    CurFile = "comopts";
  else if (ParamStr1.contains("as"))
    CurFile = "gnuasm";
  else if (ParamStr1.contains("cpp"))
    CurFile = "cpp";
  else if (ParamStr1.contains("extend"))
    CurFile = "gnuexts";
  else
    CurFile = QFileInfo(ParamStr1).completeBaseName();
  FullContents = LoadFile(ParamStr1);
  PreProcessSource(FullContents);
  NewContents = "<HTML><BODY>\r\n";
  CurNode = CurFile;
  CurSection = QString();
  CurSectionTitle = QString();
  CurSubFile = QString();
  P = 0;
  SecStart = NewContents.length();
  PreFormatted = 0;
  SCase = 0;
  CodeFormat = 0;
  ConvertFromP();
  if (P < FullContents.length())
    qFatal("Unexpected closing bracket");
  FinishSection();
  NewContents.append("\r\n</BODY></HTML>");
  PostProcessDest(NewContents);
  WriteFile(QFileInfo(ParamStr1).completeBaseName() + ".html", NewContents);

  return 0;
}
