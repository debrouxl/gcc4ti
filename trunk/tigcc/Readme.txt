
           GCC4TI Cross Compiler for the TI-89 and TI-92 Plus v0.96 Beta 10
          ================================================================

Xavier Vassor       Xavier@tigcc.ticalc.org    (original linker and tigcc)
Zeljko Juric        Zeljko@tigcc.ticalc.org    (library and documentation)
Kevin Kofler        Kevin@tigcc.ticalc.org     (linker, fixes for A68k, GCC,
                                                GAS, library, ...)
Sebastian Reichelt  Sebastian@tigcc.ticalc.org (linker, IDE, GCC fixes)
Niklas Brunlid      Niklas@tigcc.ticalc.org    (bugfixes and additions)
Jean Canazzi        Jean@tigcc.ticalc.org      (first modification of GCC)
Philipp Winkler     p.winkler@atn.nu           (library help file conversion)

Lionel Debroux      lionel_debroux@yahoo.fr  
Patrick Pelissier   patrick.pelissier@gmail.com
Manoel Trapier      godzil@godzil.net

Original files Copyright (C) 1999-2005 Xavier Vassor, Niklas Brunlid,
                                       and Jean Canazzi
ld-tigcc Copyright (C) 2002-2008 Sebastian Reichelt and Kevin Kofler
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
- the TI-68k Developer utilities (formerly TIGCC Tools Suite) ('tools' folder)
- the standard library archive ('Archive' folder)
- the ExePack launcher ('pstarter' folder)
- the documentation ('Doc' folder)
- various utilities


GCC / Binutils
==============

In order to generate Windows executables, gcc and as have to be (cross-)compiled
under MinGW / MSYS (www.mingw.org). Starting from version 0.96 Beta 10, GCC4TI
implements cross-compiling support in the *nix source distribution, so most
Windows executables can be compiled under *nix.

GCC4TI automatically patches gcc and as to make them support a number of
TI-68k/AMS peculiarities, and TIGCC/GCC4TI capabilities, such as (but not
limited to):
    * TI's calling convention;
    * TI's (SMAP II BCD) floating-point values and AMS-compatible calling
      convention;
    * add support for linker optimization (all-relocs mode, mergeable
      sections);
    * various minor bug fixes when dealing with MC68000 (instead of MC68020) code.

The *nix compilation procedure works under MSYS, with a small modification:
before running Install, type
export C_INCLUDE_PATH=""
(which is necessary if you have GTK+ development packages installed, so
as not to let cc1.exe depend on iconv.dll.)
in the MSYS command prompt.

NOTE1: the GCC4TI scripts can handle paths containing spaces, but the `configure`
in binutils 2.16.1 and gcc 4.1.2 cannot. Don't use spaces either in the path
where you decompress the GCC4TI source distribution or in the target PATH ($TIGCC).

NOTE2: when building in MSYS running under Windows XP, we saw NTVDM errors about
forbidden instructions. As far as we could see, these errors can be safely ignored.

We hope these instructions were clear enough, although they do not really
fall into the category of how to recompile GCC4TI. In fact, the possibilities
might be much greater in future releases of GCC. We will be glad to assist
you in any way.


LD-TIGCC / TPRBUILDER / A68K / TI-68k Developer Utilities
=========================================================

All of them can be (cross-)compiled with MinGW / MSYS.


TIGCC / TIGCC IDE
=================

IDE.exe and tigcc.exe have to be compiled with Borland Delphi 7 or later. To
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
'Doc' folder to reflect your actual folder names. It contains an example of
configuration. It exists so that the 'Programs' folder can reside at any
given place on the hard disk.
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

Use tprbuilder or the TIGCC IDE to compile the archive, then copy tigcc.a
into the lib folder.


LAUNCHER (PSTARTER)
===================

Use
tigcc -c -Wa,--defsym,ttunpack=1,-l pstarter.s -o pstarter.o
to compile the pstarter, then copy pstarter.o into the lib folder.


SETUP
=====

The setup wizard and the distribution archives can be built by running
makeall.bat in the 'setup' folder. The following external utilities (all
Open Source and free of charge) are needed for that to work:
* UPX: http://upx.sourceforge.net
* NSIS: http://nsis.sourceforge.net
* Info-ZIP or compatible: http://www.info-zip.org/pub/infozip/
* ClamWin (for some virus checking): http://www.clamwin.com
You'll also have to adjust the paths in the batch files and in tigcc.nsi
properly for them to work on your system.
