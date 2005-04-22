/* main.c: Main entry point for ld-tigcc, handling the command line input

   Copyright (C) 2002-2004 Sebastian Reichelt
   Copyright (C) 2004-2005 Kevin Kofler
   Copyright (C) 2004 Billy Charvet

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#include "generic.h"
#include "intrface.h"
#include "data.h"
#include "manip.h"
#include "constmrg.h"
#include "gcunused.h"
#include "reorder.h"
#include "formats/ar.h"
#include "import/import.h"
#include "import/imp_ar.h"
#include "export/export.h"
#include "special.h"

#ifdef ENABLE_DUMP
#include "dump.h"
#endif /* ENABLE_DUMP */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define RESULT_OK             0
#define RESULT_GENERAL_ERROR  1
#define RESULT_EXPORT_ERROR   2
#define RESULT_STRANGE_ERROR  3

// When compiling a DLL, the caller needs to be able to identify the version
// of the function prototypes.
#ifdef TARGET_DLL
EXP_GET_INTERFACE_VERSION ()
{
	return CURRENT_INTERFACE_VERSION;
}
#endif /* TARGET_DLL */

// Main entry point.
#ifdef TARGET_EMBEDDED
ERROR_FUNCTION ErrorFunction;
void Error_Internal (const char *FileName, const char *Text)
{
	ErrorFunction (FileName, Text, MT_ERROR);
}
void Warning_Internal (const char *FileName, const char *Text)
{
	ErrorFunction (FileName, Text, MT_WARNING);
}
EXP_LINK_FILES ()
{
#define OptInfo OptimizeInfo
#ifdef DATA_VAR_SUPPORT
#define DatVarInfo DataVarInfo
#endif /* DATA_VAR_SUPPORT */
	const char **CurFile;
	BOOLEAN IsArchive;
#else /* !TARGET_EMBEDDED */
int main (int ArgCount, const char **Args)
{
	OPTIMIZE_INFO _OptimizeInfo;
#define OptInfo (&_OptimizeInfo)
#ifdef DATA_VAR_SUPPORT
	DATA_VAR_INFO _DataVarInfo;
#define DatVarInfo (&_DataVarInfo)
#endif /* DATA_VAR_SUPPORT */
	int CurArg;
	BOOLEAN OmitBSSInitialization = FALSE;
#ifdef ENABLE_STATS
	BOOLEAN DisplayStats = FALSE;
#endif /* ENABLE_STATS */
#ifdef DATA_VAR_SUPPORT
	char DataVarString[MAX_NAME_LEN+1+MAX_NAME_LEN+1];
#endif /* DATA_VAR_SUPPORT */
#endif /* !TARGET_EMBEDDED */
	
#ifdef ENABLE_DUMP
#define DUMP_COUNT 9
	BOOLEAN Dump [DUMP_COUNT] = {[0 ... (DUMP_COUNT - 1)] = FALSE};
#define DoDump(DumpNumber) (_DoDump (DumpNumber, ""))
#define DoSpecialDump(DumpNumber,SpecialText) (_DoDump (DumpNumber, " " SpecialText))
#define _DoDump(DumpNumber,SpecialText) \
({	if (((DumpNumber) >= 0) && ((DumpNumber) < DUMP_COUNT) && (Dump [(DumpNumber)])) \
	{ \
		printf ("*** DUMP " #DumpNumber SpecialText " ***\n"); \
		DumpProgram (stdout, NULL, &Program); \
		printf ("\n"); \
	} })
#else /* !ENABLE_DUMP */
#define DoDump(DumpNumber) ((void) 0)
#define DoSpecialDump(DumpNumber,SpecialText) ((void) 0)
#endif /* !ENABLE_DUMP */
	
	int Result = RESULT_GENERAL_ERROR;
	
	PROGRAM Program;
	
	// Check the sizes of basic integer types.
	if (sizeof (I1) != 1 || sizeof (I2) != 2 || sizeof (I4) != 4 || sizeof (SI1) != 1 || sizeof (SI2) != 2 || sizeof (SI4) != 4 || sizeof (OFFSET) < sizeof (SI4))
	{
		Error (NULL, "Generic type size error!");
		return RESULT_STRANGE_ERROR;
	}
	
	// Initialize.
	memset (&Program, 0, sizeof (Program));
	Program.EntryPoint.SymbolName = "__entry_point";
#ifdef TARGET_EMBEDDED
	ErrorFunction = ErrorMessage;
	if (NativeMode)
		Program.Type = PT_NATIVE;
#else /* !TARGET_EMBEDDED */
	memset (&_OptimizeInfo, 0, sizeof (_OptimizeInfo));
#ifdef DATA_VAR_SUPPORT
	memset (&_DataVarInfo, 0, sizeof (_DataVarInfo));
#endif /* DATA_VAR_SUPPORT */
	memset (ProgramName, 0, MAX_NAME_LEN + 1);
	memset (ProgramFolder, 0, MAX_NAME_LEN + 1);
	strcpy (ProgramFolder, "main");
#endif /* !TARGET_EMBEDDED */
	Program.OptimizeInfo = OptInfo;
#ifdef DATA_VAR_SUPPORT
	Program.DataVarInfo = DatVarInfo;
#endif /* DATA_VAR_SUPPORT */
	
#ifdef TARGET_EMBEDDED
	if (Fargo)	
	{
#ifdef FARGO_SUPPORT
		Program.Type = PT_FARGO;
		Program.Calcs |= CALC_TI92;
		Warning (NULL, "Fargo support in TIGCC is experimental.");
#else /* !FARGO_SUPPORT */
		Error (NULL, "Fargo support is not compiled in.");
		goto Cleanup;
#endif /* !FARGO_SUPPORT */
	}
	
	if (FlashOS)	
	{
#ifdef FLASH_OS_SUPPORT
		Program.Type = PT_FLASH_OS;
		Warning (NULL, "Flash OS support in TIGCC is experimental.");
#else /* !FLASH_OS_SUPPORT */
		Error (NULL, "Flash OS support is not compiled in.");
		goto Cleanup;
#endif /* !FLASH_OS_SUPPORT */
	}
	
	CurFile = ObjectFiles;
	IsArchive = FALSE;
	while (CurFile && (*CurFile))
	{
		FILE *File = fopen (*CurFile, "rb");
		if (File)
		{
			SIZE Size;
			fseek (File, 0, SEEK_END);
			Size = ftell (File);
			rewind (File);
			{
				I1 *Data = malloc (Size);
				if (Data)
				{
					if (fread (Data, Size, 1, File) == 1)
					{
						if (IsArchive)
							AddArchiveFile (&Program, Data, Size, *CurFile);
						else
							ImportObjectFile (&Program, Data, Size, *CurFile);
					}
					else
						Error (*CurFile, "Unable to read file.");
					if (!IsArchive)
						free (Data);
				}
				else
					Error (*CurFile, "Not enough memory to load file.");
			}
			fclose (File);
		}
		else
			Error (*CurFile, "Unable to open file.");
		
		if ((!IsArchive) && (!(*(CurFile + 1))))
		{
			CurFile = ArchiveFiles;
			IsArchive = TRUE;
		}
		else
			CurFile++;
	}
#else /* !TARGET_EMBEDDED */
#include "main_opt.inc"
#endif /* !TARGET_EMBEDDED */

	if (IsEmpty (Program.Sections))
		Error (NULL, "Cannot create empty program.");
	else
	{
		DoDump (0);
		
		// Connect all relocs to the appropriate symbols, or convert them into
		// ROM/RAM calls. Also import objects from archives.
		// Report all unresolved references.
		if (ResolveRelocs (&Program, TRUE))
		{
			DoDump (1);
			
			// Merge all zero-data and uninitialized sections.
			Program.BSSSection = MergeAllSections (&Program, NULL, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, DI_NONE);
			
			// As a dirty trick, allow the caller to skip the BSS
			// initialization entirely.
			if (Program.BSSSection && OmitBSSInitialization)
				Program.BSSSection->Initialized = FALSE;
			
			// Extract, merge, and mark constructor and destructor sections.
			CreateSectionMarkers (&(Program.Constructors), MergeAllSections (&Program, NULL, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,  FALSE, TRUE, TRUE, DI_NONE));
			CreateSectionMarkers (&(Program.Destructors),  MergeAllSections (&Program, NULL, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,  TRUE, TRUE, DI_NONE));
				
#ifdef DEBUGGING_INFO_SUPPORT
			// If we want debugging information, merge all debugging information
			// sections of each type.
			{
				DebuggingInfoTypes i;
				for (i = 1; i < DI_LAST; i++)
				{
					Program.DebuggingInfoSection[i] = MergeAllSections (&Program, NULL, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, i + 1);
					if (Program.DebuggingInfoSection[i])
					{
						Program.DebuggingInfoSection[i]->Handled = TRUE;
						Program.HaveDebuggingInfo = TRUE;
					}
				}
			}
#endif /* DEBUGGING_INFO_SUPPORT */

#ifdef DATA_VAR_SUPPORT
			// If we want a separate data variable, merge all data
			// sections.
			if (DatVarInfo->Name)
			{
				if (OptInfo->RemoveUnused && (!(Program.Frozen)))
				{
					// Mark the section containing __main as referenced.
					MarkMainSection (&Program);
					// Remove unreferenced sections now, before constant merging
					// and section merging make it impossible.
					RemoveUnusedSections (&Program);
					// Reset the Referenced flags so we can do another GC pass
					// when the imports are done.
					ResetReferencedFlags (&Program);

#ifdef DEBUGGING_INFO_SUPPORT
					if (Program.HaveDebuggingInfo)
					{
						// Merge all unused sections into a .deleted section.
						Program.DebuggingInfoSection[0] = MergeAllSections (&Program, NULL, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 1);
						if (Program.DebuggingInfoSection[0])
							Program.DebuggingInfoSection[0]->Handled = TRUE;
					}
#endif /* DEBUGGING_INFO_SUPPORT */
					
					DoSpecialDump (1, "(early-cut)");
				}
				
				if (OptInfo->MergeConstants && (!(Program.Frozen)))
				{
					// Merge constants now, as we can't do it anymore after
					// the data variable has been built.
					MergeConstants (&Program);
					
					DoSpecialDump (1, "(const-merged)");
				}
				
				Program.DataSection = MergeAllSections (&Program, NULL, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, DI_NONE);
				// Mark the section as "handled" so it will not be merged
				// with code.
				if (Program.DataSection)
					Program.DataSection->Handled = TRUE;
				else
					Warning (NULL, "No data to put into external variable.");
			}
#endif /* DATA_VAR_SUPPORT */
			
			DoDump (2);
			
			// Create all global imports needed by this program.
			CreateSpecialGlobalImports (&Program);
			
			DoDump (3);
			
			// Resolve all remaining global imports. Usually, global imports
			// are processed directly, but if a symbol contains an inverted
			// condition, we have to wait until we really know that no such
			// global import exists.
			ResolveRemainingGlobalImports (&Program);
			
			// Resolve the relocs from the newly imported archive members.
			if (ResolveRelocs (&Program, TRUE))
			{
				// No more startup sections may be added.
				// So the program entry point is fixed from now on.
				{
					SECTION *FirstSection = GetFirst (Program.Sections);
					if (FirstSection)
					{
						Program.EntryPoint.Symbol = FirstSection->SectionSymbol;
						Program.EntryPoint.SymbolName = FirstSection->SectionSymbol->Name;
#ifdef FARGO_SUPPORT
						// Fargo programs use the location in front
						// of the two size bytes as the entry point.
						if (Program.Type == PT_FARGO)
							Program.EntryPoint.Offset -= 2;
#endif /* FARGO_SUPPORT */
					}
				}
				
				DoDump (4);
				
				// Now that all relocs have been resolved, there is no chance
				// that some previously unknown archive member will add new imports.
				CheckAllGlobalImports (&Program);
				
				if (OptInfo->OptimizeRelocs)
				{
					// Optimize relocs. This should not have any effect on the program.
					OptimizeRelocs (&Program);
					
					DoSpecialDump (4, "(optimized)");
				}
				
				if (OptInfo->RemoveUnused && (!(Program.Frozen)))
				{
					// Remove unreferenced sections.
					RemoveUnusedSections (&Program);
					
#ifdef DEBUGGING_INFO_SUPPORT
					if (Program.HaveDebuggingInfo)
					{
						// Merge all unused sections into a .deleted section.
						// Remove the section from early-cutting if we have one.
						if (Program.DebuggingInfoSection[0])
							Program.DebuggingInfoSection[0]->Handled = FALSE;
						Program.DebuggingInfoSection[0] = MergeAllSections (&Program, Program.DebuggingInfoSection[0], TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 1);
						if (Program.DebuggingInfoSection[0])
							Program.DebuggingInfoSection[0]->Handled = TRUE;
					}
#endif /* DEBUGGING_INFO_SUPPORT */

					DoSpecialDump (4, "(cut)");
				}
				
				if (!DatVarInfo->Name && OptInfo->MergeConstants && (!(Program.Frozen)))
				{
					// Merge constants.
					MergeConstants (&Program);
					
					DoSpecialDump (4, "(const-merged)");
				}
				
				if (OptInfo->ReorderSections && (!(Program.Frozen)))
				{
					// Reorder sections.
					ReorderSections (&Program);
					
					DoSpecialDump (4, "(reordered)");
				}
				
#ifdef FLASH_OS_SUPPORT
				if (Program.Type == PT_FLASH_OS)
				{
					// Flash OS export: merge startup and normal sections separately.
					// The resulting two parts are merged later, padding the first
					// part to the full 24 KB of the OS startup area (base 1)
					// + the 8 KB corresponding to the read protected FlashROM
					// area.
					// Thus, the startup sections end up in the OS startup area
					// (base 1) and the non-startup areas end up in the OS main
					// area (base 2), the big OS code part.
					
					// Merge all startup sections.
					MergeAllSections (&Program, NULL, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, DI_NONE);
					// Merge all normal sections.
					MergeAllSections (&Program, NULL, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, DI_NONE);
				}				
#endif /* FLASH_OS_SUPPORT */
				
				// Merge all initialized sections.
				Program.MainSection = MergeAllSections (&Program, NULL, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, DI_NONE);
				// Merge all unhandled sections.
				Program.MainSection = MergeAllSections (&Program, Program.MainSection, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, DI_NONE);
				
				// Record the size of the BSS section.
				if (Program.BSSSection)
					OptInfo->BSSSize = Program.BSSSection->Size;
				
				if (Program.MainSection && Program.Library && (Program.MainSection->StartupNumber > 0))
					Warning (Program.MainSection->FileName, "Library only contains program startup sections.");
				
				DoDump (5);
				
				// Fix the code (if that is still possible).
				FixCode (&Program);
				
				DoDump (6);
				
				// Resolve the relocs pointing to built-in symbols.
				Program.ResolveAllBuiltins = TRUE;
				// Resolve requested automatic insertions.
				if (ResolveRelocs (&Program, TRUE))
				{
					DoDump (7);
					
					// Do one more optimization pass for relocs to automatic insertions.
					FixCode (&Program);
					
					// Remove relocs where possible.
					if (FixupRelativeRelocs (&Program))
					{
						DoDump (8);
						
						if (Program.Calcs)
						{
							// Export the program to the appropriate files.
							if (ExportProgram (&Program, GetOutputFile, FinalizeOutputFile))
							{
								Result = RESULT_OK;
#ifndef TARGET_EMBEDDED
#ifdef ENABLE_STATS
#include "main_vbs.inc"
#endif /* ENABLE_STATS */
#endif /* !TARGET_EMBEDDED */
							}
							else
								Result = RESULT_EXPORT_ERROR;
						}
						else
							Error (NULL, "No target calculators specified.");
					}
				}
			}
		}
	}
	
Cleanup: ATTRIBUTE_UNUSED
	// Final Cleanup.
	FreeProgram (&Program);
	
	return Result;
}
