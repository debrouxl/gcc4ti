/* imp_amiga.c: Routines to import an AmigaOS-hunks file

   Copyright (C) 2003-2005 Kevin Kofler
   Copyright (C) 2003 Sebastian Reichelt

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

#include "imp_amig.h"

#ifdef AMIGAOS_SUPPORT

#include "../../formats/amigaos.h"

#include <stdlib.h>
#include <string.h>

// Import the exported symbols of an AmigaOS-hunks file.
BOOLEAN ArImportAmigaOSFile (OBJECT_FILE *ObjectFile)
{
	// Call this for a nice and clean failing exit.
#define Fail() ({return FALSE;})
#define FailWithError(ErrorMsg...) ({Error (FileName, ErrorMsg); Fail ();})
#define TestMem(Ptr) ({ if (!(Ptr)) {FailWithError ("Out of memory.");} })

	// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) ((Ptr) >= File && (Ptr) + sizeof (Type) <= File + FileSize)
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) {FailWithError ("Corrupt A68k object file.");} })

	// Get next big-endian longint. We will need this in many places here, since
	// unlike COFF, AmigaOS-hunks is a sequential format, not a pointer-based one.
#define GetNextTI4(Ptr) ({register TI4 __TI4;TestInFile(Ptr,TI4);__TI4=(*((TI4*)Ptr));Ptr+=4;ReadTI4(__TI4);})

	// Local Variables
	const I1 *File = ObjectFile->Data;
	SIZE FileSize = ObjectFile->Size;
	const char *FileName = ObjectFile->FileName;
	const I1 *ptr = File;
	I4 hunkSize;

	if (!File)
		return FALSE;
	
	// Read unit hunk
	if (GetNextTI4(ptr) != AMIGAOS_HUNK_UNIT)
		// This should already be trapped by IsAmigaOSFile, but just to make sure.
		FailWithError("Corrupt A68k object file (unit hunk missing).");
	hunkSize = GetNextTI4(ptr)<<2;
	TestInFile(ptr,I1[hunkSize]);
	ptr += hunkSize;

	while (ptr < (File + FileSize)) {
		I4 hunkType = GetNextTI4(ptr) & AMIGAOS_HUNK_TYPE_MASK;

		switch (hunkType) {
			case AMIGAOS_HUNK_UNIT:
				FailWithError("Corrupt A68k object file (duplicate unit hunk).");
				break;

			case AMIGAOS_HUNK_NAME:
			case AMIGAOS_HUNK_CODE:
			case AMIGAOS_HUNK_DATA:
				hunkSize = GetNextTI4(ptr)<<2;
				ptr += hunkSize;
				break;

			case AMIGAOS_HUNK_BSS:
				ptr += 4;
				break;

			case AMIGAOS_HUNK_RELOC_ABS2:
			case AMIGAOS_HUNK_RELOC_ABS4:
			case AMIGAOS_HUNK_RELOC_REL1:
			case AMIGAOS_HUNK_RELOC_REL2:
			case AMIGAOS_HUNK_RELOC_REL4:
				hunkSize = GetNextTI4(ptr)<<2;
				while (hunkSize) {
					ptr += hunkSize+4;
					hunkSize = GetNextTI4(ptr)<<2;
				}
				break;

#ifdef AMIGAOS_TIGCC_EXTENSIONS
			case AMIGAOS_HUNK_RELOC_ABS4_POSNEG:
			case AMIGAOS_HUNK_RELOC_ABS2_POSNEG:
			case AMIGAOS_HUNK_RELOC_ABS1_POSNEG:
				hunkSize = GetNextTI4(ptr)<<4;
				ptr += hunkSize;
				break;
#endif

			case AMIGAOS_HUNK_EXT:
				hunkSize = GetNextTI4(ptr);
				while (hunkSize) {
					// The most significant byte of the size longword encodes the symbol type.
					hunkType = hunkSize>>24;
					hunkSize = (hunkSize&0xffffffL)<<2;

					TestInFile(ptr,I1[hunkSize]);
					
					{
						char symName[hunkSize+1];
						strncpy(symName,ptr,hunkSize);
						symName[hunkSize] = 0;
						ptr += hunkSize;

						switch (hunkType) {
							case AMIGAOS_EXT_ABS:
							case AMIGAOS_EXT_DEF:
								ptr += 4;
								{
									SYMBOL *Symbol = calloc(1,sizeof(SYMBOL));
									TestMem(Symbol);
									Symbol->Parent = ObjectFile;
									strncpy(Symbol->Name,symName,MAX_SYM_LEN);
									Symbol->NameLength = strlen (Symbol->Name);
									Append(ObjectFile->Symbols,Symbol);
									ObjectFile->Parent->SymbolCount++;
								}
								break;

							case AMIGAOS_EXT_REF_ABS1:
							case AMIGAOS_EXT_REF_ABS2:
							case AMIGAOS_EXT_REF_ABS4:
							case AMIGAOS_EXT_REF_REL1:
							case AMIGAOS_EXT_REF_REL2:
							case AMIGAOS_EXT_REF_REL4:
								hunkSize = GetNextTI4(ptr)<<2;
								ptr += hunkSize;
								break;

							default:
								FailWithError("Unsupported AmigaOS symbol type `0x%lX'.",(unsigned long)hunkType);
								break;
						}
					}

					hunkSize = GetNextTI4(ptr);
				}
				break;

			case AMIGAOS_HUNK_SYMBOL:
				hunkSize = GetNextTI4(ptr)<<2;
				while (hunkSize) {
					ptr += hunkSize+4;
					hunkSize = GetNextTI4(ptr)<<2;
				}
				break;

			case AMIGAOS_HUNK_END:
				break;

			default:
				FailWithError("Unsupported AmigaOS hunk type `0x%lX'.",(unsigned long)hunkType);
				break;
		}
	}

	return TRUE; // success

#undef GetNextTI4
#undef TestInFile
#undef IsInFile
#undef TestMem
#undef FailWithError
#undef Fail
}

#endif /* AMIGAOS_SUPPORT */
