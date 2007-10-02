
           TIGCC Cross Compiler for the TI-89 and TI-92 Plus v0.96 Beta 9
          ================================================================

Xavier Vassor       Xavier@tigcc.ticalc.org    (original linker and tigcc)
Zeljko Juric        Zeljko@tigcc.ticalc.org    (library and documentation)
Kevin Kofler        Kevin@tigcc.ticalc.org     (linker, fixes for A68k, GCC,
                                                GAS, library, ...)
Sebastian Reichelt  Sebastian@tigcc.ticalc.org (linker, IDE, GCC fixes)
Niklas Brunlid      Niklas@tigcc.ticalc.org    (bugfixes and additions)
Jean Canazzi        Jean@tigcc.ticalc.org      (first modification of GCC)
Philipp Winkler     p.winkler@atn.nu           (library help file conversion)

Original files Copyright (C) 1999-2005 Xavier Vassor, Niklas Brunlid,
                                       and Jean Canazzi
ld-tigcc Copyright (C) 2002-2007 Sebastian Reichelt and Kevin Kofler
Library & Documentation Copyright (C) 2000-2007 Zeljko Juric and contributors
IDE Copyright (C) 2000-2007 Sebastian Reichelt and contributors
TIGCC Tools Suite Copyright (C) 2000-2004 Thomas Nussbaumer
                                          (http://tict.ticalc.org/)

Included programs with different licenses:
A68k Copyright (C) 1987-2005 Charlie Gibbs, David Ellsworth, Kevin Kofler,
                             Julien Muchembled, and Paul Froissart

This archive contains the sources for...
- the GCC patches ('GCC' folder)
- the A68k assembler ('A68k' folder)
- the linker ('ld-tigcc' folder)
- the command-line compiler ('TIGCC' folder)
- the IDE ('IDE' folder)
- the tprbuilder ('tprbuilder' folder)
- the link library ('FolderLink' folder)
- the setup program ('Setup' folder)
- the ExePack compressor (from the TIGCC Tools Suite) ('ttpack' folder)
- the standard library archive ('Archive' folder)
- the ExePack launcher ('pstarter' folder)
- the documentation ('Doc' folder)
- various utilities


Portability
-----------

This source code can be compiled without modifications only in Windows. For
other environments (Unix/Linux, Mac OS X, etc.), you will have to download
the TIGCC/*nix sources from http://tigcc.ticalc.org/linux/ and follow the
instructions.


GCC
===

GCC has to be compiled under MinGW (www.mingw.org) if you want to use it
in a Win32 environment (see instructions below). Cygwin is no longer supported
by TIGCC. To make it use TI's calling convention, implement floating point
support, etc., you have to patch a lot of files. The GNU assembler (from
Binutils) also has to be patched and compiled with MinGW. Simply apply the
appropriate .diff files in this archive using the GNU 'patch' utility ('patch'
found on Unix systems may also work).

A reduced version of the full GCC source code can be downloaded at:
http://tigcc.ticalc.org/sources/gcc-4.0.2.tar.bz2
Likewise, a reduced version of GNU binutils can be obtained from:
http://tigcc.ticalc.org/sources/gas-2.16.1.tar.bz2
However, if possible, it is better to get the official files from
http://www.gnu.org/. There you can also get the very latest versions of these
programs, but it is recommended to use the exact versions our patch was tested
with, newer versions may or may not work and are definitely not supported.

The as (binutils) patches add support for linker optimization (all-relocs
mode, mergeable sections) and contain some minor bug fixes when dealing with
MC68000 (instead of MC68020) code.

The recommended approach (and the one we use) to compile TIGCC is to compile
with MinGW (www.mingw.org), using the MSYS environment. Compiling with Cygwin
(with or without the '-mno-cygwin' switch) has not been tested for ages.

A typical environment variable setup would be this (typed in on the MSYS
prompt):
export CFLAGS='-Os -s -fno-exceptions'
export C_INCLUDE_PATH=""
(The latter is necessary if you have GTK+ development packages installed, so
as not to let cc1.exe depend on iconv.dll.)

A typical configuration and compilation would then look like this (on the
Cygwin prompt):
cd <destdir>
<srcdir>/configure --target=m68k-coff --with-gnu-as --disable-nls --disable-multilib --disable-shared --enable-static --disable-threads --disable-win32-registry --disable-checking --disable-werror --disable-pch --disable-mudflap
<create missing makefiles (see below)>
make
<repeat the last 2 steps until you get cc1.exe and xgcc.exe (see below)>

Do the same for binutils, except that the configure line looks like this:
<srcdir>/configure --host=mingw32 --target=m68k-coff --disable-shared --enable-static --disable-multilib --disable-nls --disable-win32-registry
and the required executable is as-new.exe.

In the size-reduced sources available from tigcc.ticalc.org, there are some
missing directories. This causes the configure script to create empty
makefiles in the destination directory. The 'make' utility rejects these
files, however, so you need to replace these with 'Makefile-empty' from this
archive. Unfortunately, unlike previous versions of GCC and Binutils which
first configured everything, then allowed you to replace the makefiles, and
then compiled everything, newer versions configure subdirectories only as
they are built. Therefore, you often have to replace the makefiles when an
error occurs and then relaunch 'make'. The TIGCC/*nix build scripts automate
this.

We hope these instructions were clear enough, although they do not really
fall into the category of how to recompile TIGCC. In fact, the possibilities
might be much greater in future releases of GCC. We will be glad to assist
you in any way.


LD-TIGCC/TPRBUILDER/A68K/TTPACK
===============================

ld-tigcc, tprbuilder, A68k and ttpack can be compiled with MinGW32
(www.mingw.org). ttpack can also be compiled using LCC-Win32.


TIGCC/IDE
=========

IDE.exe and tigcc.exe, have to be compiled with Borland Delphi 6 or later. To
do this, make sure all files in the 'Search Path Items' folder are available
in your search path. Then install all custom components from the 'Components'
folder.

To compile tigcc.exe
--------------------

Make sure the above conditions are true. Then open TIGCC.dpr in Delphi and
compile it.

To compile IDE.exe
------------------

Make sure the above conditions are true. Then open IDE.dpr in Delphi and
compile it.


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

Alternatively you can use the tprbuilder (which calls the command line
compiler and its '-ar' switch).


LAUNCHER (PSTARTER)
===================

Use the TIGCC IDE to compile the project, making sure "Delete object
files on successful linking" is unchecked, then copy pstarter.o into the
Lib folder.


SETUP
=====

The setup wizard and the distribution archives can be built by running
makeall.bat in the 'setup' folder. The following external utilities (all
Open Source and free of charge) are needed for that to work:
* UPX: http://upx.sourceforge.net
* NSIS: http://nsis.sourceforge.net
* Info-ZIP: http://www.info-zip.org/pub/infozip/
* ClamWin: http://www.clamwin.com
You'll also have to adjust the paths in the batch files and in tigcc.nsi
properly for them to work on your system.
