/* special.h: Routines to handle special characteristics of the linker

   Copyright (C) 2002-2003 Sebastian Reichelt
   Copyright (C) 2003-2005 Kevin Kofler

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

#ifndef SPECIAL_H
#define SPECIAL_H

#include "generic.h"
#include "data.h"

// Special symbols processed in other files.
#define SYM_OMIT_BSS_INIT         "__ld_omit_bss_init"
#define SYM_ALL_RELOCS            "__ld_all_relocs"
#define SYM_IGNORE_GLOBAL_IMPORTS "__ld_ignore_global_imports"

// Operators for global imports.
#define SYMOP_NOT "NOT_"
#define SYMOP_AND "_AND_"

// Special symbol prefix for built-in symbols.
#define SYMPF_BUILTIN "__ld_"

// Special symbol prefixes for automatic insertions.
#define SYMPF_INSERT "insert_"
#define SYMPF_BUILTIN_INSERT SYMPF_BUILTIN SYMPF_INSERT

#define SYMPF_COMPRESSED "compressed_"
#define SYMPF_BUILTIN_INSERT_COMPRESSED SYMPF_BUILTIN_INSERT SYMPF_COMPRESSED

#define SYMPF_MLINK "mlink_"
#define SYMPF_BUILTIN_INSERT_MLINK SYMPF_BUILTIN_INSERT SYMPF_MLINK

#define SYMPF_CALC_CONST "calc_const_"
#define SYMPF_BUILTIN_CALC_CONST SYMPF_BUILTIN SYMPF_CALC_CONST

// Special symbol prefix for nostub comment data.
#define SYMPF_NOSTUB_DATA "_nostub_data__"
// Beginning of the nostub data prefix, so we can call GetExportNumber
// on nostub data symbols by skipping this part.
#define SYMPF_NOSTUB_DATA_START "_nostub_"

// Special symbol resolving to hardware ID.
#define SYM_BUILTIN_HARDWARE_ID SYMPF_BUILTIN "hardware_id"

// Try to translate an external symbol into a special-feature symbol used by the linker.
// If the symbol contains a number, it is written to the variable pointed to by Number.
typedef enum {ST_NORMAL = 0, ST_ROM_CALL, ST_RAM_CALL, ST_EXTRA_RAM, ST_LIB_CALL} SpecialExternalSymbolTypes;
SpecialExternalSymbolTypes TranslateSpecialExternalSymbol (PROGRAM *Program, char *SymbolName, void **Reference, OFFSET *Number);

// Handle the symbol if it is a special one.
// Returns TRUE if it was special; in this case it should not be used.
BOOLEAN HandleSpecialSymbol (PROGRAM *Program, const char *SymbolName);

// Translate a section name into a startup section number.
// Returns 0 if the name does not represent a startup section.
OFFSET GetStartupSectionNumber (const char *SectionName, SIZE MaxLen);

// Translate a symbol name into the number of an exported function, if the
// function is meant to be exported.
// Returns -1 if it is not an exported function.
OFFSET GetExportNumber (const char *SymbolName);

// Add all imports with names defined by this program, if they are needed.
// For example, they can add code to support constructors and destructors,
// BSS blocks, etc.
BOOLEAN CreateSpecialGlobalImports (PROGRAM *Program);

// If the reloc or its relation point to a special ld-exported symbol,
// change it to the appropriate value if possible. FALSE is returned
// only if it is not a special symbol reloc, or if there was an error.
BOOLEAN ResolveSpecialSymbolReloc (RELOC *Reloc, BOOLEAN *TryAgain);
// If the reloc points to a special ld-exported symbol, change it to the
// appropriate value if possible. FALSE is returned only if it is not a
// special symbol reloc, or if there was an error.
BOOLEAN ResolveSpecialSymbolRelocTarget (RELOC *Reloc, BOOLEAN *TryAgain);
// If the reloc's relation points to a special ld-exported symbol,
// change it to the appropriate value if possible. FALSE is returned
// only if it is not a special symbol location, or if there was an error.
BOOLEAN ResolveSpecialSymbolRelocRelation (RELOC *Reloc, BOOLEAN *TryAgain);
// If the location points to a special ld-exported symbol, change it to the
// appropriate value if possible. FALSE is returned only if it is not a
// special symbol location, or if there was an error.
// Warning: If the special symbol resolves to a number, Location->Symbol and
// Location->SymbolName will both be NULL, and Location->Offset will contain
// the number.
BOOLEAN ResolveSpecialSymbolLocation (SECTION *Section, LOCATION *Location, BOOLEAN *TryAgain);

// Count the items for a specific built-in symbol, specified by SymName
// and SymNameLength. If TrueValue is nonzero, items are not counted,
// but NewValue is set to this value if at least one item was found. In
// that case, if Program->ResolveAllBuiltins is false, NewValue may be
// unchanged even though there are some items; you need to check back
// later when Program->ResolveAllBuiltins is true.
BOOLEAN GetBuiltinValue (PROGRAM *Program, const char *SymName, SIZE SymNameLength, OFFSET *NewValue, OFFSET TrueValue);

// Check if the given location points to a calculator-specific builtin
// symbol.
BOOLEAN IsCalcBuiltinLocation (const LOCATION *Location);
// Return whether the reloc can be resolved to a calculator-specific value.
// ResolveSpecialSymbol or something related must have been called on the
// reloc at least once.
BOOLEAN IsPlainCalcBuiltin (const RELOC *Reloc);
// If the location can be resolved to a calculator-specific value, get the
// value for the specified calculator.
// If IsCalcBuiltinLocation returned a positive result for this reloc, this
// function will not return a negative result.
BOOLEAN GetCalcBuiltinLocationValue (const LOCATION *Location, ProgramCalcs DestCalc, IMAX *Value);
// If the reloc can be resolved to a calculator-specific value, get the
// value for the specified calculator.
// The return value is the same as for IsPlainCalcBuiltin.
BOOLEAN GetCalcBuiltinValue (const RELOC *Reloc, ProgramCalcs DestCalc, IMAX *Value);

// If required by some special symbol(s) in the section, modify the
// contents of the section.
// This can be used to insert special items such as relocation entries.
// MergedSection specifies the (usually large) part of the program that
// has already been merged.
// If necessary, MergedSection is frozen automatically.
BOOLEAN HandleSectionContents (SECTION *Section, SECTION *MergedSection);
// Insert contents for an insertion specified by SymbolName, and return
// a symbol from where the insertion took place.
// If necessary, MergedSection is frozen automatically.
SYMBOL *HandleAutoInsertion (SECTION *Section, const char *SymbolName);
// Handle an insertion by cutting the section off at the specified location
// and inserting the contents specified by the name, taking into account
// that MergedSection specifies the (usually large) part of the program
// that has already been merged.
BOOLEAN HandleInsertion (SECTION *Section, OFFSET Location, const char *Name, SECTION *MergedSection, BOOLEAN AlwaysTerminate);
// Append the data required by an insertion (specified by name) to the
// section specified by Section, taking into account that MergedSection
// specifies the (usually large) part of the program that has already
// been merged.
BOOLEAN AppendInsertionData (SECTION *Section, const char *Name, SECTION *MergedSection, BOOLEAN AlwaysTerminate);

#endif
