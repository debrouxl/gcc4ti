
           TIGCC Cross Compiler for the TI-89 and TI-92 Plus v0.95
          =========================================================

Xavier Vassor       Xavier@tigcc.ticalc.org    (original linker and tigcc)
Zeljko Juric        Zeljko@tigcc.ticalc.org    (library and documentation)
Kevin Kofler        Kevin@tigcc.ticalc.org     (linker, fixes for A68k, GCC,
                                                GAS, library, ...)
Sebastian Reichelt  Sebastian@tigcc.ticalc.org (linker, IDE, GCC fixes)
Niklas Brunlid      Niklas@tigcc.ticalc.org    (bugfixes and additions)
Jean Canazzi        Jean@tigcc.ticalc.org      (first modification of GCC)
Philipp Winkler     p.winkler@atn.nu           (library help file conversion)

Original files Copyright (C) 1999-2004 Xavier Vassor, Niklas Brunlid,
                                       and Jean Canazzi
ld-tigcc Copyright (C) 2002-2004 Sebastian Reichelt and Kevin Kofler
Library & Documentation Copyright (C) 2000-2005 Zeljko Juric and contributors
IDE Copyright (C) 2000-2004 Sebastian Reichelt

Included programs with different licenses:
A68k Copyright (C) 1987-2004 Charlie Gibbs, David Ellsworth, Kevin Kofler,
                             Julien Muchembled, and Paul Froissart
TIGCC Tools Suite Copyright (C) 2000-2004 Thomas Nussbaumer
                                          (http://tict.ticalc.org/)

This archive contains the sources for...
- the GCC patches ('GCC' folder)
- the A68k assembler ('A68k' folder)
- the linker ('ld-tigcc' folder)
- the command-line compiler ('TIGCC' folder)
- the IDE ('IDE' folder)
- the link library ('FolderLink' folder)
- the setup program ('Setup' folder)
- the TIGCC tools used ('Tools' folder)
- the standard library archive ('Archive' folder)
- the documentation ('Doc' folder)
- various utilities


Portability
-----------

This source code can be compiled without modifications only in Windows. For
other environments (Unix/Linux, Mac, etc.), you will have to download the
TIGCC/UNIX sources from http://lpg.ticalc.org/prj_tigcc and follow the
instructions.


GCC
===

GCC has to be compiled under Cygwin (www.cygwin.com) or MinGW32
(www.mingw.org) if you want to use it in a Win32 environment (see
instructions below). To make it use TI's calling convention, implement
floating point support, etc., you have to patch a lot of files. Simply apply
the appropriate .diff files in this archive using the GNU 'patch' utility
('patch' found on Unix systems may also work).

A reduced version of the full GCC source code can be downloaded at:
http://tigcc.ticalc.org/sources/sources-gcc.tar.bz2
Likewise, a reduced version of GNU binutils can be obtained from:
http://tigcc.ticalc.org/sources/sources-binutils.tar.bz2
However, if possible, it is better to get the official files from
http://www.gnu.org/. There you can also get the very latest versions of these
programs.

The binutils patches actually perform two tasks. For as, ld, and ar, it
just contains some minor bug fixes when dealing with MC68000 (instead of
MC68020) code. But the much larger part is the implementation of the AmigaOS
target using files found at www.geekgadgets.org. In the modified form
presented here, it is only usable as an input target, but objcopy and objdump
are handled well. Note that the original implementation supports an AmigaOS
output target as well.

Currently there are apparently three different possible approaches to
compiling a GCC-based cross compiler under Windows:
1. Compile with MinGW (www.mingw.org), using the MSYS environment.
2. Compile with Cygwin (www.cygwin.com), for a Cygwin host. This requires
   the resulting cross compiler to use the file cygwin1.dll.
3. Compile with Cygwin using the '-mno-cygwin' switch for GCC, supplying all
   necessary header files and libraries from www.mingw.org (install by
   editing 'specs').

We are currently using the first option, using 'i386-pc-mingw32' as host.

A typical environment variable setup would be this (typed in on the MSYS
prompt):
export CFLAGS="-Os -s" LD="ld" AR="ar" RANLIB="ranlib" NM="nm" STRIP="strip"

Note that these depend on the way you choose to build the GNU tools.

A typical configuration and compilation would then look like this (on the
Cygwin prompt):
cd <destdir>
<srcdir>/configure --build=i386-pc-cygwin --host=i386-pc-mingw32 --target=m68k-coff --with-gnu-as --with-gnu-ld --disable-shared --enable-static --disable-multilib --disable-threads --disable-win32-registry --disable-nls
<create missing makefiles (see below)>
make

Do the same for binutils, except that you probably have to copy 'nm.exe' to
'i386-pc-mingw32-nm.exe' and 'strip.exe' to 'i386-pc-mingw32-strip.exe').

To compile objcopy, do the same as for the other tools from binutils, but
compile in a different directory, and insert '--enable-targets=m68k-amigaos'
in the 'configure' line. This way you create a tool which, given the option
'-O coff-m68k', can convert an AmigaOS object file to a COFF file.

In the size-reduced sources available from tigcc.ticalc.org, there are some
missing directories. This causes the configure script to create empty
makefiles in the destination directory. The 'make' utility rejects these
files, however, so you need to replace these with 'Makefile-empty' from this
archive.

We hope these instructions were clear enough, although they do not really
fall into the category of how to recompile TIGCC. In fact, the possibilities
might be much greater in future releases of GCC. We will be glad to assist
you in any way.


LD-TIGCC
========

ld-tigcc can be compiled with MinGW32 (www.mingw.org). Cygwin should work as
well.


TIGCC/IDE/FOLDERLINK/SETUP
==========================

IDE.exe and Setup.exe, and now also tigcc.exe, have to be compiled with
Borland Delphi 4 or later. To do this, make sure all files in the 'Search
Path Items' folder are available in your search path. Then install all custom
components from the 'Components' folder.

To compile tigcc.exe
--------------------

Make sure the above conditions are true. Then open TIGCC.dpr in Delphi and
compile it.

To compile IDE.exe
------------------

Make sure the above conditions are true. Then open IDE.dpr in Delphi and
compile it.

To compile FolderLink.dll
-------------------------

Make sure the above conditions are true. Then open FolderLink.dpr in Delphi
and compile it.

To compile Setup.exe
--------------------

Windows NT/2000:
You can compile Setup.exe without any additional files installed, and then
use my program SetupWizard.exe to upload files directly to Setup.exe. This is
untested, however, and might not work. If it doesn't, try the second method.

All OSs:
Use SetupWizard.exe to modify Files.res, which will be linked to Setup.exe.
It is also possible to automate the process of updating the setup resource or
executable file. For an example, see the file CreateRes.bat. You have to set
all paths correctly, and you will probably have to modify the batch file a
little. It is just meant as an example.
In order to collect and zip all source files which need to be updated
frequently, I created another batch file, UpdateSources.bat. However, before
using it, please set all paths correctly, and read through the file. You
should be familiar with batch files and environment variables.


DOC
===

First, modify the file HelpSystem.ini in the 'Programs' subfolder of the
'Doc' folder to reflect your actual folder names. It exists so that the
'Programs' folder can reside at any given place on the hard disk.
Then, you need to call all 'Update...' programs once.

UpdateInclude.exe and UpdateInfo.exe take one or more complete file name(s)
as parameters. The actual names of the files are discarded, and the remaining
folder name is the folder to be updated. If the first parameter is '/ALL' or
no parameters are specified, all folders are updated, and in case of
UpdateInclude.exe, a header file index is generated.

UpdateKeywords.exe and UpdateIndex.exe do not take any parameters.

To compile the documentation, download the HTML Help Workshop from
http://msdn.microsoft.com. Then compile the project in the 'System\CHMFiles'
folder.

Tools to make editing help system files more convenient are also included in
this distribution.

A tools to create help system files from the original GNU Texinfo documents
is included as well, but it might need a recompilation to work with future
versions of the documentation. Anyway, the .texi files should be in the
current directory, and you should specify one of them in the command line.
The appropriate subfolders need to exist (e.g. gnuexts).

If you need detailed information about the file formats, look at the
Readme.txt file in the documentation folder.


ARCHIVE
=======

Use the TIGCC IDE to compile the archive, then copy tigcc.a into the Lib
folder.

Alternatively you can use the command line compiler and its '-ar' switch.


TOOLS
=====

Please download the complete TIGCC Tools Suite from http://tict.ticalc.org/.
You can use the files from this package, but this is not recommended.
