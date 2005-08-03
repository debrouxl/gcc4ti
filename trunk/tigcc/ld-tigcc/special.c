/* special.c: Routines to handle special characteristics of the linker

   Copyright (C) 2002-2004 Sebastian Reichelt
   Copyright (C) 2003-2005 Kevin Kofler
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

#include "special.h"

#include "integers.h"
#include "manip.h"
#include "insert/insert.h"

#include <string.h>
#include <stdlib.h>

// Try to translate an external symbol into a special-feature symbol used by the linker.
// If the symbol contains a number, it is written to the variable pointed to by Number.
SpecialExternalSymbolTypes TranslateSpecialExternalSymbol (PROGRAM *Program, char *SymbolName, void **Reference, OFFSET *Number)
{
	SpecialExternalSymbolTypes SymType = ST_NORMAL;
	char *SymNum = SymbolName, *Divider = NULL;
	int NumBase = 16;
	
	if (!(strncmp (SymbolName, "tiamsapi_", sizeof ("tiamsapi_") - 1)))
	{
		SymType = ST_ROM_CALL;
		SymNum += sizeof ("tiamsapi_") - 1;
		NumBase = 10;
	}
	if (!(strncmp (SymbolName, "_ROM_CALL_", sizeof ("_ROM_CALL_") - 1)))
	{
		SymType = ST_ROM_CALL;
		SymNum += sizeof ("_ROM_CALL_") - 1;
	}
	else if (!(strncmp (SymbolName, "_RAM_CALL_", sizeof ("_RAM_CALL_") - 1)))
	{
		SymType = ST_RAM_CALL;
		SymNum += sizeof ("_RAM_CALL_") - 1;
	}
	else if (!(strncmp (SymbolName, "_extraramaddr@", sizeof ("_extraramaddr@") - 1)))
	{
		SymType = ST_EXTRA_RAM;
		SymNum += sizeof ("_extraramaddr@") - 1;
	}
	else if (!(strncmp (SymbolName, "_extraramaddr__", sizeof ("_extraramaddr__") - 1)))
	{
		SymType = ST_EXTRA_RAM;
		SymNum += sizeof ("_extraramaddr__") - 1;
	}
	else if (((Divider = strchr (SymbolName, '@'))) || (((Divider = strstr (SymbolName, "__"))) && (SymbolName [0] != '_') && (SymbolName [0] != '.') && (strncmp (SymbolName, "L_", sizeof ("L_") - 1))))
	{
		if (*Divider == '@')
			SymNum = Divider + 1;
		else
			SymNum = Divider + 2;
		if (strlen (SymNum) == 4)
			SymType = ST_LIB_CALL;
	}
	
	if (SymType != ST_NORMAL)
	{
		// Get the number the symbol contains (in hex).
		if (Number)
		{
			char *Err;
			*Number = strtoul (SymNum, &Err, NumBase);
			// If the number has an error in it, revert to normal symbol.
			if (*Err)
				SymType = ST_NORMAL;
		}
	}
	
	if (SymType == ST_LIB_CALL)
	{
		// Cut SymbolName at the divider.
		*Divider = 0;
		// Get a reference to the library identified by SymbolName.
		if (Reference)
		{
			*Reference = GetLibrary (Program, SymbolName);
			if (!(*Reference))
				SymType = ST_NORMAL;
		}
	}
	
	return SymType;
}

// Handle the symbol if it is a special one.
// Returns TRUE if it was special; in this case it should not be used.
BOOLEAN HandleSpecialSymbol (PROGRAM *Program, const char *SymbolName)
{
	BOOLEAN SymbolNameMatches (const char *Name)
	{
		return (!(strcmp (SymbolName, Name)));
	}
	
	const char *Divider;
	
	// All special symbols start with an underscore.
	if (SymbolName [0] == '_')
	{
		SymbolName++;
		
		// The most important special symbol: __ref_all_xxx.
		// It adds xxx to the global imports.
		if (!(strncmp (SymbolName, "_ref_all_", sizeof ("_ref_all_") - 1)))
		{
			if (!(Program->IgnoreGlobalImports))
				AddGlobalImport (Program, SymbolName + sizeof ("_ref_all_") - 1);
		}
		else if (SymbolNameMatches ("tigcc_native"))
		{
			// Kernel is the default type. Everything else must be defined explicitly
			// and should therefore take precedence over _tigcc_native.
			if (Program->Type == PT_KERNEL)
				Program->Type = PT_NATIVE;
		}
		else if (SymbolNameMatches ("nostub"))
		{
			SECTION *FirstSection = GetFirst (Program->Sections);
			if (FirstSection && (!(FirstSection->StartupNumber)))
				FirstSection->Essential = TRUE;
			Program->Type = PT_NOSTUB;
		}
#ifdef NOSTUB_DLL_SUPPORT
		else if (SymbolNameMatches ("nostub_dll"))
		{
			SECTION *FirstSection = GetFirst (Program->Sections);
			if (FirstSection && (!(FirstSection->StartupNumber)))
				FirstSection->Essential = TRUE;
			Program->Type = PT_NOSTUB_DLL;
			Program->Library = TRUE;
		}
#endif /* NOSTUB_DLL_SUPPORT */
#ifdef FARGO_SUPPORT
		else if (SymbolNameMatches ("fargo"))
			Program->Type = PT_FARGO;
#endif /* FARGO_SUPPORT */
#ifdef FLASH_OS_SUPPORT
		else if (SymbolNameMatches ("flash_os"))
			Program->Type = PT_FLASH_OS;
#endif /* FLASH_OS_SUPPORT */
		else if (SymbolNameMatches ("library"))
		{
			Program->Library = TRUE;
#ifdef FARGO_SUPPORT
			// Under Fargo, _library is a normal symbol as well.
			if (Program->Type == PT_FARGO)
				return FALSE;
#endif /* FARGO_SUPPORT */
		}
		else if (SymbolNameMatches ("ti92"))
		{
			Program->Calcs |= CALC_TI92;
		}
		else if (SymbolNameMatches ("ti89"))
		{
			Program->Calcs |= CALC_TI89;
			Program->KernelFlags |= 0x02;
		}
		else if (SymbolNameMatches ("ti89ti"))
		{
			Program->Calcs |= CALC_TI89 | CALC_FLAG_TITANIUM;
			Program->KernelFlags |= 0x42;
		}
		else if (SymbolNameMatches ("ti92plus"))
		{
			Program->Calcs |= CALC_TI92PLUS;
			Program->KernelFlags |= 0x01;
		}
		else if (SymbolNameMatches ("v200"))
		{
			Program->Calcs |= CALC_V200;
			Program->KernelFlags |= 0x20;
		}
		else if (!(strncmp (SymbolName, "flag_", sizeof ("flag_") - 1)))
			Program->KernelFlags |= (1 << strtoul (SymbolName + sizeof ("flag_") - 1, NULL, 10));
		else if (!(strncmp (SymbolName, "version", sizeof ("version") - 1)))
			Program->Version = strtoul (SymbolName + sizeof ("version") - 1, NULL, 16);
		else if (!(strcmp (SymbolName, "_ld_use_fline_jumps")))
		{
			if (Program->OptimizeInfo)
				Program->OptimizeInfo->UseFLineJumps = TRUE;
		}
		else if (!(strcmp (SymbolName, "_ld_use_4byte_fline_jumps")))
		{
			if (Program->OptimizeInfo)
				Program->OptimizeInfo->Use4ByteFLineJumps = TRUE;
		}
		else if (!(strcmp (SymbolName, SYM_IGNORE_GLOBAL_IMPORTS + 1)))
			Program->IgnoreGlobalImports = TRUE;
		else if (SymbolNameMatches (SYM_OMIT_BSS_INIT + 1) || SymbolNameMatches (SYM_ALL_RELOCS + 1))
			// These are mostly file-local special symbols; they are handled
			// by the importing function.
			;
		else
			// Not a special symbol.
			return FALSE;
		
		// The symbol was handled here.
		return TRUE;
	}
	else if (((Divider = strstr (SymbolName, "@version"))) || (((Divider = strstr (SymbolName, "__version"))) && (SymbolName [0] != '_') && (SymbolName [0] != '.') && (strncmp (SymbolName, "L_", sizeof ("L_") - 1))))
	{
		// Required minimum version number for a library.
		const char *Version;
		VERSION VersionNumber;
		
		if (*Divider == '@')
			Version = Divider + sizeof ("@version") - 1;
		else
			Version = Divider + sizeof ("__version") - 1;
		VersionNumber = strtoul (Version, NULL, 16);
		
		if (VersionNumber)
		{
			if (Divider - SymbolName <= MAX_SYM_LEN)
			{
				char LibName[MAX_SYM_LEN+1];
				LIBRARY *Library;
				
				strncpy (LibName, SymbolName, Divider - SymbolName);
				LibName [Divider - SymbolName] = 0;
				Library = GetLibrary (Program, LibName);
				if (Library && (Library->Version < VersionNumber))
					Library->Version = VersionNumber;
			}
			
			return TRUE;
		}
	}
	
	return FALSE;
}

// Translate a section name into a startup section number.
// Returns 0 if the name does not represent a startup section.
OFFSET GetStartupSectionNumber (const char *SectionName, SIZE MaxLen)
{
	// Check if it looks like a startup section.
	if ((MaxLen > (SIZE) (sizeof ("_st") - 1)) && (!(strncmp (SectionName, "_st", sizeof ("_st") - 1))))
	{
		// Copy the section name to a temporary null-terminated location.
		char SecName[MaxLen+1];
		memset (SecName, 0, MaxLen + 1);
		strncpy (SecName, SectionName, MaxLen);
		// Check whether it is a library startup section.
		if (SecName [sizeof ("_st") - 1] == 'l')
		{
			// Library startup sections are special:
			// They may always be included, even if the file is not executable.
			// Therefore they get a negative number to distinguish them.
			OFFSET RealNum = strtoul (SecName + sizeof ("_stl") - 1, NULL, 10);
			return (RealNum ? RealNum - 10000 : 0);
		}
		else
			return (strtoul (SecName + sizeof ("_st") - 1, NULL, 10));
	}
	else
		return 0;
}

// Translate a symbol name into the number of an exported function, if the
// function is meant to be exported.
// Returns -1 if it is not an exported function.
OFFSET GetExportNumber (const char *SymbolName)
{
	const char *Divider;
	
	if (((Divider = strchr (SymbolName, '@'))) || (((Divider = strstr (SymbolName, "__"))) && (SymbolName [0] != '_') && (SymbolName [0] != '.') && (strncmp (SymbolName, "L_", sizeof ("L_") - 1))))
	{
		OFFSET Number;
		char *Err;
		
		if (*Divider == '@')
			Divider++;
		else
			Divider += 2;
		
		Number = strtoul (Divider, &Err, 16);
		if (*Err)
			return (-1);
		
		return Number;
	}
	else
		return (-1);
}

// Add all imports with names defined by this program, if they are needed.
BOOLEAN CreateSpecialGlobalImports (PROGRAM *Program)
{
	BOOLEAN Result = TRUE;
	SECTION *TempSection;
	
	if (Program->Type == PT_KERNEL)
	{
		if (Program->Library)
			Result = Result && AddGlobalImport (Program, "__kernel_library_header");
		else
			Result = Result && AddGlobalImport (Program, "__kernel_program_header");
	}
#ifdef FARGO_SUPPORT
	else if (Program->Type == PT_FARGO)
	{
		if (Program->Library)
			Result = Result && AddGlobalImport (Program, "__fargo_library_header");
		else
			Result = Result && AddGlobalImport (Program, "__fargo_program_header");
	}
#endif /* FARGO_SUPPORT */
#ifdef FLASH_OS_SUPPORT
	else if (Program->Type == PT_FLASH_OS)
		Result = Result && AddGlobalImport (Program, "__flash_os_header");
#endif /* FLASH_OS_SUPPORT */
	if (Program->Constructors.Start)
		Result = Result && AddGlobalImport (Program, "__handle_constructors");
	if (Program->Destructors.Start)
		Result = Result && AddGlobalImport (Program, "__handle_destructors");
	if (Program->BSSSection && Program->BSSSection->Initialized)
		Result = Result && AddGlobalImport (Program, "__initialize_bss");
	
	// Handle BSS section if any.
	if (Program->BSSSection && (!(Program->BSSSection->Handled)))
	{
		GLOBAL_IMPORT *Import = CreateGlobalImport (Program, "__handle_bss");
		if (Import)
		{
			// This is the import that will mark the BSS section as handled
			// if it succeeds.
			Program->BSSImport = Import;
			
			// Try to resolve it against available archives.
			ResolveGlobalImport (Program, Import);
			
			// The BSS section is simply merged into the rest if it is not
			// handled.
			Import->Succeeded = TRUE;
		}
		else
			Result = FALSE;
	}
	
	// Handle absolute relocs if there are any.
	for_each (TempSection, Program->Sections)
	{
		BOOLEAN Done = FALSE;
		RELOC *TempReloc;
		for_each (TempReloc, TempSection->Relocs)
		{
			if ((!(TempReloc->Relative || TempReloc->Target.Builtin || (TempReloc->Target.Symbol && (TempReloc->Target.Symbol->Parent->Handled)))))
			{
				GLOBAL_IMPORT *Import = AddGlobalImport (Program, "__handle_relocs");
				if (Import)
					// Relocs are either handled by the export format, or a
					// warning is emitted.
					Import->Succeeded = TRUE;
				else
					Result = FALSE;
				Done = TRUE;
				break;
			}
		}
		if (Done)
			break;
	}
	
	// Handle ROM calls if there are any.
	for_each (TempSection, Program->Sections)
	{
		if (!(IsEmpty (TempSection->ROMCalls)))
		{
			GLOBAL_IMPORT *Import = AddGlobalImport (Program, "__handle_rom_calls");
			if (Import)
				// ROM calls are either handled by the export format, or a
				// warning is emitted.
				Import->Succeeded = TRUE;
			else
				Result = FALSE;
			break;
		}
	}
	
	// Handle RAM calls if there are any.
	for_each (TempSection, Program->Sections)
	{
		if (!(IsEmpty (TempSection->RAMCalls)))
		{
			GLOBAL_IMPORT *Import = AddGlobalImport (Program, "__handle_ram_calls");
			if (Import)
				// RAM calls are either handled by the export format, or a
				// warning is emitted.
				Import->Succeeded = TRUE;
			else
				Result = FALSE;
			break;
		}
	}
	
	// Handle libraries if any libraries are referenced.
	if (!(IsEmpty (Program->Libraries)))
	{
		GLOBAL_IMPORT *Import = AddGlobalImport (Program, "__handle_libraries");
		if (Import)
			// Libraries are either handled by the export format, or a
			// warning is emitted.
			Import->Succeeded = TRUE;
		else
			Result = FALSE;
	}
	
	// Handle _nostub comments if any.
	if ((Program->Type == PT_NATIVE) || (Program->Type == PT_NOSTUB))
	{
		BOOLEAN NostubComments = FALSE;
		for_each (TempSection, Program->Sections)
		{
			SYMBOL *TempSymbol;
			for_each (TempSymbol, TempSection->Symbols)
			{
				if (TempSymbol->Exported && (!(strncmp (TempSymbol->Name, "_nostub_data__", sizeof ("_nostub_data__") - 1))))
				{
					OFFSET ExportNumber = GetExportNumber (TempSymbol->Name + sizeof ("_nostub_") - 1);
					if (ExportNumber >= 0)
					{
						NostubComments = TRUE;
						TempSection->Essential = TRUE;
						break;
					}
				}
			}
		}
		if (NostubComments)
			Result = Result && AddGlobalImport (Program, "__nostub_comment_header");
	}
	
#ifdef DATA_VAR_SUPPORT
	// Handle data variable if desired.
	if (Program->DataVarInfo->Name && Program->DataSection)
	{
		Result = Result && (AddGlobalImport (Program, "__handle_data_var"));
		if (Program->DataVarInfo->CreateCopy)
		{
			Result = Result && (AddGlobalImport (Program, "__data_var_create_copy"));
			if (Program->DataVarInfo->CopyOnlyIfArchived)
				Result = Result && (AddGlobalImport (Program, "__data_var_copy_if_archived"));
		}
	}
#endif /* DATA_VAR_SUPPORT */
	
	return Result;
}

// If the reloc or its relation point to a special ld-exported symbol,
// change it to the appropriate value if possible. FALSE is returned
// only if it is not a special symbol location, or if there was an error.
BOOLEAN ResolveSpecialSymbolReloc (RELOC *Reloc, BOOLEAN *TryAgain)
{
	// At first, resolve the relation, because if this is a built-in
	// number, we cannot write it into the section contents directly,
	// but we have to subtract it from FixedOffset.
	BOOLEAN Result = ResolveSpecialSymbolRelocRelation (Reloc, TryAgain);
	
	// Now resolve the target.
	return (ResolveSpecialSymbolRelocTarget (Reloc, TryAgain) ? Result : FALSE);
}
	
// If the reloc points to a special ld-exported symbol, change it to the
// appropriate value if possible. FALSE is returned only if it is not a
// special symbol reloc, or if there was an error.
BOOLEAN ResolveSpecialSymbolRelocTarget (RELOC *Reloc, BOOLEAN *TryAgain)
{
	BOOLEAN Result = FALSE;
	SECTION *Section = Reloc->Parent;
	LOCATION *Location = &(Reloc->Target);
	
	// If the relation is an unresolved reference to a builtin number,
	// do not even try to resolve the target. This would only cause
	// trouble, since we would need to handle it immediately if it
	// resolved to a number. There will be another call to this anyway
	// after the relation has finally been resolved.
	// Usually, we would at least need to set the Target.Builtin flag.
	// However, currently all tests which check for Target.Builtin also
	// check for Relation or Relative in general. If this ever changes,
	// this code will break.
	if (Reloc->Relation && Reloc->Relation->Builtin)
		return TRUE;
	
	if (ResolveSpecialSymbolLocation (Section, Location, TryAgain))
	{
		// If it has resolved to a number, write it into the section
		// contents.
		if (!(Location->Symbol || Location->SymbolName))
		{
			if (Reloc->Relation)
				Warning (GetFileName (Section, Reloc->Location), "Ignoring invalid negative reference to `%s' at 0x%lX.", Reloc->Relation->SymbolName, (long) Reloc->Location);
			
			if ((Reloc->Location >= 0) && (Reloc->Location + Reloc->Size <= Section->Size))
			{
				// Add the target offset (to support things such as "__ld_xxx+1").
				OFFSET NewValue = Location->Offset + Reloc->FixedOffset;
				
				if (Reloc->Relative)
					Warning (GetFileName (Section, Reloc->Location), "Invalid relative reference to built-in number `%s' at 0x%lX; changing to absolute.", Reloc->Target.SymbolName, (long) Reloc->Location);
				
				// Check if the section contents at the reloc are zero.
				if (!(IsZeroDataRange (Section, Reloc->Location, Reloc->Location + Reloc->Size)))
					Warning (GetFileName (Section, Reloc->Location), "Builtin reloc at 0x%lX to `%s' on nonzero section contents. Overlapping with another?", (long) Reloc->Location, Reloc->Target.SymbolName);
				
				// Resolve the reloc by writing the value into the section.
				Result = AddTI (Section->Data + Reloc->Location, Reloc->Size, NewValue, TRUE, TRUE);
				
				if (!Result)
					Error (GetFileName (Section, Reloc->Location), "Number `%s' (=%ld) too large for size %ld reloc at 0x%lX.", Reloc->Target.SymbolName, (long) NewValue, (long) Reloc->Size, (long) Reloc->Location);
			}
			else
			{
				Warning (GetFileName (Section, Reloc->Location), "Removing reloc at 0x%lX to `%s' outside of section.", (long) Reloc->Location, Reloc->Target.SymbolName);
				Result = TRUE;
			}
			
			FreeReloc (Reloc);
		}
		else
			Result = TRUE;
	}
	
	return Result;
}

// If the reloc's relation points to a special ld-exported symbol,
// change it to the appropriate value if possible. FALSE is returned
// only if it is not a special symbol location, or if there was an error.
BOOLEAN ResolveSpecialSymbolRelocRelation (RELOC *Reloc, BOOLEAN *TryAgain)
{
	BOOLEAN Result = FALSE;
	SECTION *Section = Reloc->Parent;
	LOCATION *Location = Reloc->Relation;
	
	if (Reloc->Relation)
	{
		// Try to resolve the relation.
		if (ResolveSpecialSymbolLocation (Section, Location, TryAgain))
		{
			// Check if it has really been resolved.
			BOOLEAN Resolved = (Location->Symbol != NULL);
			
			// If it has been resolved to a number, subtract it from
			// FixedOffset.
			if (!(Location->Symbol || Location->SymbolName))
			{
				Reloc->FixedOffset -= Location->Offset;
				FreeRelocRelation (Reloc);
				Resolved = TRUE;
			}
			
			Result = TRUE;
		}
	}
	
	return Result;
}

// If the location points to a special ld-exported symbol, change it to the
// appropriate value if possible. FALSE is returned only if it is not a
// special symbol location, or if there was an error.
// Warning: If the special symbol resolves to a number, Location->Symbol and
// Location->SymbolName will both be NULL, and Location->Offset will contain
// the number.
BOOLEAN ResolveSpecialSymbolLocation (SECTION *Section, LOCATION *Location, BOOLEAN *TryAgain)
{
	const char *SymName = Location->SymbolName;
	
	BOOLEAN SymNameMatches (const char *Name)
	{
		return (!(strcmp (SymName, Name)));
	}
	
	// Locations that are already resolved do not need any treatment.
	if (Location->Symbol)
		return FALSE;
	
	// All special symbols start with '_'.
	if (SymName && (SymName [0] == '_'))
	{
		PROGRAM *Program = Section->Parent;
		BOOLEAN SetToEntryPoint = FALSE;
		OFFSET NewValue = 0;
		
		SymName++;
		
		// All built-in symbols start with "__ld_".
		if (!(strncmp (SymName, SYMPF_BUILTIN + 1, sizeof (SYMPF_BUILTIN) - 2)))
		{
			SIZE SymNameLength;
			BOOLEAN HasValue = FALSE;
			SYMBOL *NewSymbol = NULL;
			OFFSET NewTargetOffset = 0;
			SECTION *TempSection;
			
			// Skip the "__ld_" prefix.
			SymName += sizeof (SYMPF_BUILTIN) - 2;
			SymNameLength = strlen (SymName);
			
			// Find (but do not resolve) calculator constants.
			if (IsCalcBuiltinLocation (Location))
				;
			// Resolve references to insertions.
			else if (!(strncmp (SymName, SYMPF_INSERT, sizeof (SYMPF_INSERT) - 1)))
			{
				if ((!(TempSection = Program->MainSection))
				 || (!(NewSymbol = HandleAutoInsertion (TempSection, Location->SymbolName))))
					return TRUE;
			}
			// Resolve "has_...s".
			else if ((!(strncmp (SymName, "has_", sizeof ("has_") - 1))) && (SymName [SymNameLength - 1] == 's'))
			{
				if (Program->ResolveAllBuiltins)
				{
					Program->Frozen = TRUE;
					if (GetBuiltinValue (Program, SymName + (sizeof ("has_") - 1), SymNameLength - 1 - (sizeof ("has_") - 1), &NewValue, -1))
						HasValue = TRUE;
				}
			}
			// Resolve "..._count".
			else if ((SymNameLength > ((SIZE) (sizeof ("_count") - 1))) && (!(strcmp (SymName + SymNameLength - (sizeof ("_count") - 1), "_count"))))
			{
				if (Program->ResolveAllBuiltins)
				{
					Program->Frozen = TRUE;
					if (GetBuiltinValue (Program, SymName, SymNameLength - (sizeof ("_count") - 1), &NewValue, 0))
						HasValue = TRUE;
				}
			}
			else if (SymNameMatches ("entry_point"))
				SetToEntryPoint = TRUE;
			else if (SymNameMatches ("entry_point_plus_0x8000"))
			{
				SetToEntryPoint = TRUE;
				NewValue = 0x8000;
			}
			else if (SymNameMatches ("constructors_start"))
			{
				if (!(NewSymbol = Program->Constructors.Start))
					return TRUE;
			}
			else if (SymNameMatches ("constructors_end"))
			{
				if (!(NewSymbol = Program->Constructors.End))
					return TRUE;
			}
			else if (SymNameMatches ("constructors_size"))
			{
				if (Program->Constructors.Start && Program->Constructors.End)
				{
					NewValue = Program->Constructors.End->Location - Program->Constructors.Start->Location;
					HasValue = TRUE;
				}
				else if (Program->ResolveAllBuiltins)
					HasValue = TRUE;
			}
			else if (SymNameMatches ("destructors_start"))
			{
				if (!(NewSymbol = Program->Destructors.Start))
					return TRUE;
			}
			else if (SymNameMatches ("destructors_end"))
			{
				if (!(NewSymbol = Program->Destructors.End))
					return TRUE;
			}
			else if (SymNameMatches ("destructors_size"))
			{
				if (Program->Destructors.Start && Program->Destructors.End)
				{
					NewValue = Program->Destructors.End->Location - Program->Destructors.Start->Location;
					HasValue = TRUE;
				}
				else if (Program->ResolveAllBuiltins)
					HasValue = TRUE;
			}
			else if (SymNameMatches ("data_start"))
			{
				if (Program->DataSection)
					NewSymbol = Program->DataSection->SectionSymbol;
				else
					return TRUE;
			}
			else if (SymNameMatches ("data_end"))
			{
				if (Program->DataSection)
				{
					NewSymbol = Program->DataSection->SectionSymbol;
					NewTargetOffset = Program->DataSection->Size;
				}
				else
					return TRUE;
			}
			else if (SymNameMatches ("data_size"))
			{
				if (Program->DataSection)
				{
					NewValue = Program->DataSection->Size;
					HasValue = TRUE;
				}
				else if (Program->ResolveAllBuiltins)
					HasValue = TRUE;
			}
			else if (SymNameMatches ("bss_start"))
			{
				if (Program->BSSSection)
					NewSymbol = Program->BSSSection->SectionSymbol;
				else
					return TRUE;
			}
			else if (SymNameMatches ("bss_end"))
			{
				if (Program->BSSSection)
				{
					NewSymbol = Program->BSSSection->SectionSymbol;
					NewTargetOffset = Program->BSSSection->Size;
				}
				else
					return TRUE;
			}
			else if (SymNameMatches ("bss_size"))
			{
				if (Program->BSSSection)
				{
					NewValue = Program->BSSSection->Size;
					HasValue = TRUE;
				}
				else if (Program->ResolveAllBuiltins)
					HasValue = TRUE;
			}
			else if (SymNameMatches ("file_version"))
			{
				if (Program->ResolveAllBuiltins || Program->Version)
				{
					NewValue = Program->Version;
					HasValue = TRUE;
				}
			}
			else if (SymNameMatches ("kernel_flags"))
			{
				if (Program->ResolveAllBuiltins)
				{
					NewValue = Program->KernelFlags;
					HasValue = TRUE;
				}
			}
			else if (SymNameMatches ("kernel_bss_table"))
			{
				if (Program->BSSSection)
				{
					strcpy ((char *) (Location->SymbolName), "__kernel_bss_table");
					if (TryAgain)
						*TryAgain = TRUE;
					// This prevents the section from being merged, and prevents
					// relocs to it from being emitted.
					Program->BSSSection->Handled = TRUE;
					return FALSE;
				}
				else if (Program->ResolveAllBuiltins)
					SetToEntryPoint = TRUE;
			}
			else if (SymNameMatches ("program_size"))
			{
				if (Program->ResolveAllBuiltins && Program->MainSection)
				{
					NewValue = Program->MainSection->Size;
					HasValue = TRUE;
				}
				else
					return TRUE;
			}
			else if (SymNameMatches ("kernel_export_table"))
			{
				BOOLEAN HasExports = FALSE;
				for_each (TempSection, Program->Sections)
				{
					SYMBOL *TempSymbol;
					for_each (TempSymbol, TempSection->Symbols)
					{
						if (TempSymbol->Exported)
						{
							OFFSET ExportNumber = GetExportNumber (TempSymbol->Name);
							if (ExportNumber >= 0)
							{
								HasExports = TRUE;
								TempSection->Essential = TRUE;
								break;
							}
						}
					}
				}
				
				if (HasExports)
				{
					strcpy ((char *) (Location->SymbolName), "__kernel_export_table");
					if (TryAgain)
						*TryAgain = TRUE;
					return FALSE;
				}
				else if (Program->ResolveAllBuiltins)
					SetToEntryPoint = TRUE;
			}
#ifdef DATA_VAR_SUPPORT
			else if (SymNameMatches ("data_var_name_end"))
			{
				// Point the reloc to the terminating zero byte of the name.
				if (Program->DataVarInfo->Name)
				{
					strcpy ((char *) (Location->SymbolName), "__data_var_name_start");
					Location->Offset += strlen (Program->DataVarInfo->Name) + 1;
					if (TryAgain)
						*TryAgain = TRUE;
					return FALSE;
				}
			}
#endif /* DATA_VAR_SUPPORT */
			else
				return FALSE;
			
			if (!SetToEntryPoint)
			{
				if (HasValue)
				{
					// Point the location to the new value.
					Location->Symbol = NULL;
					FreeLocationSymbolName (Section, Location);
					Location->Offset += NewValue;
				}
				else if (NewSymbol)
				{
					// Point the location to the new symbol.
					Location->Symbol = NewSymbol;
					FreeLocationSymbolName (Section, Location);
					Location->Offset += NewTargetOffset;
				}
				else
					Location->Builtin = TRUE;
				
				return TRUE;
			}
		}
		// If this is a reloc to a kernel-specific symbol, point it to the
		// entry point. The result of this is that the reloc's value
		// becomes 0 if the reloc was made up by something like
		// _exit-__kernel_entry_point.
		else if (SymNameMatches ("exit") || SymNameMatches ("comment") || SymNameMatches ("extraram") || SymNameMatches ("library"))
			SetToEntryPoint = TRUE;
		
		if (SetToEntryPoint)
		{
			if (Program->EntryPoint.Symbol)
			{
				Location->Symbol = Program->EntryPoint.Symbol;
				FreeLocationSymbolName (Section, Location);
				Location->Offset += Program->EntryPoint.Offset + NewValue;
			}
			
			return TRUE;
		}
	}
	
	return FALSE;
}

// Count the items for a specific built-in symbol, specified by SymName
// and SymNameLength. If TrueValue is nonzero, items are not counted,
// but NewValue is set to this value if at least one item was found. In
// that case, if Program->ResolveAllBuiltins is false, NewValue may be
// unchanged even though there are some items; you need to check back
// later when Program->ResolveAllBuiltins is true.
BOOLEAN GetBuiltinValue (PROGRAM *Program, const char *SymName, SIZE SymNameLength, OFFSET *NewValue, OFFSET TrueValue)
{
#define Count(n,op,code) \
({ \
	register OFFSET n__ = (n); \
	if (TrueValue) \
	{ \
		if (n__) \
		{ \
			*NewValue = TrueValue; \
			code; \
		} \
	} \
	else \
		*NewValue op n__; \
})
#define SetCounter(n) (Count ((n), =, (void) 0))
#define IncreaseCounter(n) (Count ((n), +=, break))
	
	BOOLEAN SymNameMatches (const char *Name)
	{
		return (!(strncmp (SymName, Name, SymNameLength)));
	}
	
	SECTION *TempSection;
	RELOC *TempReloc;
	SYMBOL *TempSymbol;
	
	if (SymNameMatches ("constructor"))
	{
		if (Program->Constructors.Start && Program->Constructors.End)
			SetCounter ((Program->Constructors.End->Location - Program->Constructors.Start->Location) >> 2);
	}
	else if (SymNameMatches ("destructor"))
	{
		if (Program->Destructors.Start && Program->Destructors.End)
			SetCounter ((Program->Destructors.End->Location - Program->Destructors.Start->Location) >> 2);
	}
	else if (SymNameMatches ("reloc"))
	{
		// Count all absolute relocs.
		// Relative relocs will either be resolved completely or
		// produce errors.
		for_each (TempSection, Program->Sections)
		{
			IncreaseCounter (TempSection->Relocs.EmittedCount);
			// Since relocs may be removed, if ResolveAllBuiltins
			// is false, do not handle TrueValue.
			if ((!TrueValue) || Program->ResolveAllBuiltins)
			{
				for_each (TempReloc, TempSection->Relocs)
					if (!(TempReloc->Relative || TempReloc->Target.Builtin || (TempReloc->Target.Symbol && (TempReloc->Target.Symbol->Parent->Handled))))
						IncreaseCounter (1);
			}
		}
	}
	else if (SymNameMatches ("data_ref"))
	{
		if (Program->DataSection)
		{
			// Count all absolute relocs to the data section.
			for_each (TempSection, Program->Sections)
				for_each (TempReloc, TempSection->Relocs)
					if (TempReloc->Target.Symbol && (TempReloc->Target.Symbol->Parent == Program->DataSection) && (!(TempReloc->Relative || TempReloc->Target.Builtin)))
						IncreaseCounter (1);
		}
	}
	else if (SymNameMatches ("bss_ref"))
	{
		if (Program->BSSSection)
		{
			// Count all absolute relocs to the BSS section.
			for_each (TempSection, Program->Sections)
				for_each (TempReloc, TempSection->Relocs)
					if (TempReloc->Target.Symbol && (TempReloc->Target.Symbol->Parent == Program->BSSSection) && (!(TempReloc->Relative || TempReloc->Target.Builtin)))
						IncreaseCounter (1);
		}
	}
	else if (SymNameMatches ("rom_call"))
	{
		for_each (TempSection, Program->Sections)
			IncreaseCounter (CountItems (TempSection->ROMCalls, ROM_CALL));
	}
	else if (SymNameMatches ("ram_call"))
	{
		for_each (TempSection, Program->Sections)
			IncreaseCounter (CountItems (TempSection->RAMCalls, RAM_CALL));
	}
	else if (SymNameMatches ("lib"))
		SetCounter (CountItems (Program->Libraries, LIBRARY));
	else if (SymNameMatches ("referenced_lib"))
		SetCounter (Program->Libraries.ReferencedCount);
	else if (SymNameMatches ("export"))
	{
		// The number of exports is equal to the highest export number + 1.
		for_each (TempSection, Program->Sections)
		{
			for_each (TempSymbol, TempSection->Symbols)
			{
				if (TempSymbol->Exported)
				{
					OFFSET ExportNumber = GetExportNumber (TempSymbol->Name);
					if ((ExportNumber >= 0) && (*NewValue < ExportNumber + 1))
						Count (ExportNumber + 1, =, break);
				}
			}
		}
	}
	else if (SymNameMatches ("nostub_comment"))
	{
		for_each (TempSection, Program->Sections)
		{
			for_each (TempSymbol, TempSection->Symbols)
			{
				if (TempSymbol->Exported && (!(strncmp (TempSymbol->Name, SYMPF_NOSTUB_DATA, sizeof (SYMPF_NOSTUB_DATA) - 1))))
				{
					OFFSET ExportNumber = GetExportNumber (TempSymbol->Name + (sizeof (SYMPF_NOSTUB_DATA_START) - 1));
					if (ExportNumber >= 0)
						IncreaseCounter (1);
				}
			}
		}
	}
	else
		return FALSE;
	
#undef IncreaseCounter
#undef SetCounter
#undef Count
	
	return TRUE;
}

// If the given symbol name belongs to a calculator-specific builtin
// symbol, return a pointer to the part of it that holds the values.
static const char *GetCalcBuiltinValues (const char *SymName)
{
	if (!(strncmp (SymName, SYMPF_BUILTIN_CALC_CONST, sizeof (SYMPF_BUILTIN_CALC_CONST) - 1)))
	{
		return (SymName + (sizeof (SYMPF_BUILTIN_CALC_CONST) - 1));
	}
	return NULL;
}

// Check if the given location points to a calculator-specific builtin
// symbol.
BOOLEAN IsCalcBuiltinLocation (const LOCATION *Location)
{
	return (Location->SymbolName && (GetCalcBuiltinValues (Location->SymbolName) || (!(strcmp (Location->SymbolName, SYM_BUILTIN_HARDWARE_ID)))));
}

// Return whether the reloc can be resolved to a calculator-specific value.
// ResolveSpecialSymbol or something related must have been called on the
// reloc at least once.
BOOLEAN IsPlainCalcBuiltin (const RELOC *Reloc)
{
	// To improve speed, check whether the target and relation (if it exists)
	// are builtin symbols.
	if (Reloc->Target.Builtin && ((!(Reloc->Relation)) || Reloc->Relation->Builtin))
	{
		// Check the target.
		if (IsCalcBuiltinLocation (&(Reloc->Target)))
		{
			// If there is a relation, check it.
			if (Reloc->Relation)
				return IsCalcBuiltinLocation (Reloc->Relation);
			// Otherwise, the reloc may not be relative, since that would
			// mean that it cannot be resolved to a number.
			else
				return (!(Reloc->Relative));
		}
	}
	
	return FALSE;
}

// If the location can be resolved to a calculator-specific value, get the
// value for the specified calculator.
// If IsCalcBuiltinLocation returned a positive result for this reloc, this
// function will not return a negative result.
BOOLEAN GetCalcBuiltinLocationValue (const LOCATION *Location, ProgramCalcs DestCalc, IMAX *Value)
{
	// Basic sanity checks.
	if (Value && Location->SymbolName)
	{
		// Special case: __ld_hardware_id
		if (!(strcmp (Location->SymbolName, SYM_BUILTIN_HARDWARE_ID)))
		{
			switch (DestCalc)
			{
				case CALC_TI89:
					*Value = 3;
					break;
					
				case CALC_TI89 | CALC_FLAG_TITANIUM:
					*Value = 9;
					break;
					
				case CALC_TI92PLUS:
					*Value = 1;
					break;
					
				case CALC_V200:
					*Value = 8;
					break;
					
				default:
					Warning (NULL, SYM_BUILTIN_HARDWARE_ID " not defined for this calculator.");
					*Value = 0;
			}
			*Value += Location->Offset;
			return TRUE;
		}
		else
		{
			// Get the part of the symbol name that holds the values,
			// separated by '_'.
			const char *Values = GetCalcBuiltinValues (Location->SymbolName);
			if (Values)
			{
				// AND out the calculator flags.
				DestCalc &= ~CALC_FLAG_TITANIUM;
				
				// While we still have at least one value left...
				while (Values)
				{
					// Get the end of the value string.
					const char *ValueEnd = strchr (Values, '_');
					SIZE ValueSize = ValueEnd ? (SIZE) (ValueEnd - Values) : (SIZE) (strlen (Values));
					
					// If this is the value that belongs to the current calculator,
					// extract the value and return.
					if (DestCalc == 1)
					{
						char *EndPtr = NULL;
						
						// Create a copy of the value, with a terminating zero byte.
						char ValueStr[ValueSize+1];
						strncpy (ValueStr, Values, ValueSize);
						ValueStr [ValueSize] = 0;
						
						// Convert this string into a number and return it.
						*Value = strtoul (ValueStr, &EndPtr, 0);
						if (EndPtr && *EndPtr)
							Warning (NULL, "Invalid number `%s' in `%s'.", ValueStr, Location->SymbolName);
						*Value += Location->Offset;
						
						return TRUE;
					}
					
					// Advance to the next value.
					Values = ValueEnd ? ValueEnd + 1 : NULL;
					// Advance to the next calculator.
					DestCalc >>= 1;
				}
				
				// No more values were found, but the function did not exit yet.
				Warning (NULL, "Calculator constant `%s' contains too few values.", Location->SymbolName);
				*Value = 0;
				
				// We have to return a positive result anyway because
				// IsCalcBuiltinLocation would as well.
				return TRUE;
			}
		}
	}
	
	return FALSE;
}

// If the reloc can be resolved to a calculator-specific value, get the
// value for the specified calculator.
// The return value is the same as for IsPlainCalcBuiltin.
BOOLEAN GetCalcBuiltinValue (const RELOC *Reloc, ProgramCalcs DestCalc, IMAX *Value)
{
	if (Value)
	{
		// Get the value for the target.
		if (GetCalcBuiltinLocationValue (&(Reloc->Target), DestCalc, Value))
		{
			// If there is a relation, subtract its value.
			if (Reloc->Relation)
			{
				IMAX RelationValue;
				if (!(GetCalcBuiltinLocationValue (Reloc->Relation, DestCalc, &RelationValue)))
					return FALSE;
				*Value -= RelationValue;
			}
			// Otherwise, the reloc may not be relative
			// (see IsPlainCalcBuiltin).
			else if (Reloc->Relative)
				return FALSE;
			
			*Value += Reloc->FixedOffset;
			return TRUE;
		}
	}
	
	return FALSE;
}

// If required by some special symbol(s) at the end of the section,
// modify the contents of the section.
// This can be used to insert special items such as relocation entries.
// MergedSection specifies the (usually large) part of the program that
// has already been merged.
// If necessary, MergedSection is frozen automatically.
BOOLEAN HandleSectionContents (SECTION *Section, SECTION *MergedSection)
{
	SYMBOL *Symbol;
	
	// Search the labels at the end of the secion to find special ones.
	for (Symbol = FindSymbolAtPos (Section, Section->Size, TRUE); Symbol; Symbol = GetNext (Symbol))
	{
		if (!(strncmp (Symbol->Name, SYMPF_BUILTIN_INSERT, sizeof (SYMPF_BUILTIN_INSERT) - 1)))
			return (HandleInsertion (Section, Symbol->Location, Symbol->Name + sizeof (SYMPF_BUILTIN_INSERT) - 1, MergedSection, FALSE));
	}
	
	return TRUE;
}

// Insert contents for an insertion specified by SymbolName, and return
// a symbol from where the insertion took place.
// If necessary, Section is frozen automatically.
SYMBOL *HandleAutoInsertion (SECTION *Section, const char *SymbolName)
{
	if (!(strncmp (SymbolName, SYMPF_BUILTIN_INSERT, sizeof (SYMPF_BUILTIN_INSERT) - 1)))
	{
		CreateSectionSegment (Section);
		
		// All insertions except compressed ones should be aligned on a 2-byte boundary.
		if ((!(strncmp (SymbolName, SYMPF_BUILTIN_INSERT_COMPRESSED, sizeof (SYMPF_BUILTIN_INSERT_COMPRESSED) - 1)))
		 || (!(strncmp (SymbolName, SYMPF_BUILTIN_INSERT_MLINK, sizeof (SYMPF_BUILTIN_INSERT_MLINK) - 1)))
		 || (!(strcmp (SymbolName, SYMPF_BUILTIN_INSERT "fargo021_relocs")))
		 || (!(strcmp (SymbolName, SYMPF_BUILTIN_INSERT "preos_compressed_tables")))
		 || PadSection (Section, 2))
		{
			// Create a new symbol at the end of the section.
			SYMBOL *Result = calloc (1, sizeof (SYMBOL));
			
			if (Result)
			{
				Result->Parent   = Section;
				Result->Location = Section->Size;
				strncpy (Result->Name, SymbolName, MAX_SYM_LEN);
				Result->Exported = TRUE;
				Append (Section->Symbols, Result);
				
				// Insert the data.
				if (AppendInsertionData (Section, SymbolName + sizeof (SYMPF_BUILTIN_INSERT) - 1, Section, TRUE))
					return Result;
			}
			else
				Error (NULL, "Out of memory.");
		}
	}
	
	return NULL;
}

// Handle an insertion by cutting the section off at the specified location
// and inserting the contents specified by the name, taking into account
// that MergedSection specifies the (usually large) part of the program
// that has already been merged.
BOOLEAN HandleInsertion (SECTION *Section, OFFSET Location, const char *Name, SECTION *MergedSection, BOOLEAN AlwaysTerminate)
{
	if (Location == Section->Size)
		return AppendInsertionData (Section, Name, MergedSection, AlwaysTerminate);
	else
		return TRUE;
}

// Append the data required by an insertion (specified by name) to the
// section specified by Section, taking into account that MergedSection
// specifies the (usually large) part of the program that has already
// been merged.
BOOLEAN AppendInsertionData (SECTION *Section, const char *Name, SECTION *MergedSection, BOOLEAN AlwaysTerminate)
{
	BOOLEAN NameMatches (const char *InsertionName)
	{
		return (!(strcmp (Name, InsertionName)));
	}
	
	PROGRAM *Program = Section->Parent;
	
#ifdef DATA_VAR_SUPPORT
	// Data variable name.
	if (NameMatches ("data_var_name"))
		return InsertDataVarName (Section);
	else
#endif /* DATA_VAR_SUPPORT */
	
	// Nostub-specific formats.
	if (NameMatches ("nostub_comments"))
		return InsertNostubComments (Section);
	
	// Kernel-specific formats.
	else if (NameMatches ("kernel_relocs"))
		return InsertKernelRelocs (Section, NULL);
	else if (NameMatches ("kernel_bss_refs"))
		return InsertKernelSectionRefs (Section, Program->BSSSection,  AlwaysTerminate);
	else if (NameMatches ("kernel_data_refs"))
		return InsertKernelSectionRefs (Section, Program->DataSection, AlwaysTerminate);
	else if (NameMatches ("kernel_rom_calls"))
		return InsertKernelROMCalls (Section);
	else if (NameMatches ("kernel_ram_calls"))
		return InsertKernelRAMCalls (Section);
	else if (NameMatches ("kernel_libs"))
		return InsertKernelLibraries (Section);
	else if (NameMatches ("kernel_exports"))
		return InsertKernelExports (Section, TRUE);
	
#ifdef FARGO_SUPPORT
	// Fargo-specific formats.
	else if (NameMatches ("fargo_exports"))
		return InsertKernelExports (Section, FALSE);
	else if (NameMatches ("fargo020_bss_refs"))
		return InsertKernelSectionRefs (Section, Program->BSSSection, TRUE);
	else if (NameMatches ("fargo020_libs"))
		return InsertFargo020Libraries (Section);
#endif /* FARGO_SUPPORT */
	
	// PreOS-specific formats.
	else if (NameMatches ("preos_compressed_tables"))
		return InsertPreOsCompressedTables (Section);
	
	// Other compressed formats.
	else
	{
		char *ReferenceName = malloc ((sizeof (SYMPF_BUILTIN) - 1) + strlen (Name) + sizeof ("_ref"));
		
		if (ReferenceName)
		{
			// Build the reference symbol name: "__ld_" Name "_ref".
			strcpy (ReferenceName, SYMPF_BUILTIN);
			strcat (ReferenceName, Name);
			strcat (ReferenceName, "_ref");
			
			{
				LOCATION Reference = {NULL, ReferenceName, 0, FALSE};
				
				// Try to find a reference symbol. If none is found, use
				// the program entry point.
				Section->Relocs.UnresolvedCount++;
				if (!(ResolveLocation (Program, Section, &Reference)))
				{
					FreeLocationSymbolName (Section, &Reference);
					Reference = Program->EntryPoint;
				}
				
#ifdef FARGO_SUPPORT
				// Fargo-specific formats.
				if (NameMatches ("fargo021_relocs"))
					return InsertCompressedRelocs (Section, NULL, MergedSection, &Reference);
				else if (NameMatches ("fargo021_bss_refs"))
					return InsertFargo021SectionRefs (Section, Program->BSSSection, MergedSection, &Reference);
				else if (NameMatches ("fargo021_libs"))
					return InsertFargo021Libraries (Section, MergedSection, &Reference);
#endif /* FARGO_SUPPORT */
				
				// Compressed relocation tables using our own format.
				else if (NameMatches ("compressed_relocs"))
					return InsertCompressedRelocs (Section, NULL, MergedSection, &Reference);
				else if (NameMatches ("compressed_bss_refs"))
					return InsertCompressedSectionRefs (Section, Program->BSSSection, MergedSection, &Reference);
				else if (NameMatches ("compressed_data_refs"))
					return InsertCompressedSectionRefs (Section, Program->DataSection, MergedSection, &Reference);
				else if (NameMatches ("compressed_rom_calls"))
					return InsertCompressedROMCalls (Section, MergedSection, &Reference);
				
				// Compressed relocation tables using our own mlink-style format.
				else if (NameMatches ("mlink_relocs"))
					return InsertMlinkRelocs (Section, NULL, MergedSection, &Reference);
				else if (NameMatches ("mlink_bss_refs"))
					return InsertMlinkSectionRefs (Section, Program->BSSSection, MergedSection, &Reference);
				else if (NameMatches ("mlink_data_refs"))
					return InsertMlinkSectionRefs (Section, Program->DataSection, MergedSection, &Reference);
				else if (NameMatches ("mlink_rom_calls"))
					return InsertMlinkROMCalls (Section, MergedSection, &Reference);
				
				else
					Warning (GetFileName (Section, Section->Size), "Unrecognized insertion `%s'.", Name);
			}
		}
		else
		{
			Error (NULL, "Out of memory.");
			return FALSE;
		}
	}
	
	return TRUE;
}
