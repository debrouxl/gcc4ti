/* imp_amig.c: Routines to import an AmigaOS-hunks file

   Copyright (C) 2002-2005 Kevin Kofler

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

#include "../formats/amigaos.h"
#include "../manip.h"
#include "../special.h"
#include "../bincode/fix_m68k.h"

#include <stdlib.h>
#include <string.h>

// reallocate array, free it if reallocation failed, zero out the added size
// otherwise
static void *crealloc(void *array, SIZE oldSize, SIZE newSize)
{
	void *p = realloc(array, newSize);
	if (!p) {
		if (array) {
			free(array);
		}
		return NULL;
	}
	if (newSize > oldSize) {
		memset(((I1*)p)+oldSize, 0, newSize-oldSize);
	}
	return p;
}

// Defines by how much the reloc target hunks array is grown.
#define RELOC_TARGET_HUNKS_INCREMENT (5 * sizeof(SECTION*))

// Import an AmigaOS-hunks file into the internal data structures.
BOOLEAN ImportAmigaOSFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName)
{
	// Call this for a nice and clean failing exit.
#define Fail() ({if(currSectionData) free(currSectionData);if(hunkName) free(hunkName);if(symName) free(symName);if(relocTargetHunks) free(relocTargetHunks);return FALSE;})
#define FailWithError(ErrorMsg...) ({Error (FileName, ErrorMsg); Fail ();})
#define TestMem(Ptr) ({ if (!(Ptr)) {FailWithError ("Out of memory.");} })

	// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) ((Ptr) >= File && (Ptr) + sizeof (Type) <= File + FileSize)
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) {FailWithError ("Corrupt A68k object file.");} })

	// Get next big-endian longint. We will need this in many places here, since
	// unlike COFF, AmigaOS-hunks is a sequential format, not a pointer-based one.
#define GetNextTI4(Ptr) ({register TI4 __TI4;TestInFile(Ptr,TI4);__TI4=(*((TI4*)Ptr));Ptr+=4;ReadTI4(__TI4);})

	// Local Variables
	const I1 *ptr = File;
	I4 hunkSize;
	char *hunkName = NULL, *symName = NULL;
	SECTION *currSection = NULL;
	SYMBOL *currSymbol;
	I1 *currSectionData = NULL;
	OFFSET StartupNumber;
	BOOLEAN InitializeBSS = TRUE;
	BOOLEAN AllRelocs = FALSE;
	COUNT numRelocTargetHunks=0; // number of hunks which are candidates for reloc targets
	SECTION **relocTargetHunks=NULL;
	SIZE relocTargetHunksSize=0;
	const RELOC *relocHint=NULL;

	// Read unit hunk
	if (GetNextTI4(ptr) != AMIGAOS_HUNK_UNIT)
		// This should already be trapped by IsAmigaOSFile, but just to make sure.
		FailWithError("Corrupt A68k object file (unit hunk missing).");
	hunkSize = GetNextTI4(ptr)<<2;
	TestInFile(ptr,I1[hunkSize]);
	ptr += hunkSize;

	while (ptr < (File + FileSize)) {
		I4 hunkType = GetNextTI4(ptr);

		// Hunk flags:
		// AMIGAOS_HUNK_FLAG_ADVISORY:
		//	 A68k never generates this one
		if (hunkType & AMIGAOS_HUNK_FLAG_ADVISORY)
			FailWithError("Unsupported hunk flag `advisory'.");
		// AMIGAOS_HUNK_FLAG_CHIP,
		// AMIGAOS_HUNK_FLAG_FAST:
		//	 simply ignore those flags, they have no meaning on TI calculators
		hunkType &= AMIGAOS_HUNK_TYPE_MASK;

		switch (hunkType) {
			// beginning section - there should be only one of those, handled above
			case AMIGAOS_HUNK_UNIT:
				FailWithError("Corrupt A68k object file (duplicate unit hunk).");
				break;

			// name of the section to follow - temporarily store a pointer to it
			case AMIGAOS_HUNK_NAME:
				hunkSize = GetNextTI4(ptr)<<2;
				TestInFile(ptr,I1[hunkSize]);
				hunkName = malloc(hunkSize+1);
				TestMem(hunkName);
				strncpy(hunkName,ptr,hunkSize);
				hunkName[hunkSize]=0;
				ptr += hunkSize;
				break;

			case AMIGAOS_HUNK_CODE:
			case AMIGAOS_HUNK_DATA:
				// allocate space for at least one more reloc target hunk
				while ((SIZE) ((numRelocTargetHunks+1) * sizeof(SECTION*)) > relocTargetHunksSize) {
					relocTargetHunks = crealloc(relocTargetHunks, relocTargetHunksSize,
																			relocTargetHunksSize+RELOC_TARGET_HUNKS_INCREMENT);
					TestMem(relocTargetHunks);
					relocTargetHunksSize += RELOC_TARGET_HUNKS_INCREMENT;
				}

				// invent a section name if no reasonable one given
				if (!hunkName || !*hunkName || *hunkName==' ') {
					if (hunkName) {
						free(hunkName);
					}
					hunkName = malloc(sizeof(".data"));
					TestMem(hunkName);
					strcpy(hunkName,((hunkType == AMIGAOS_HUNK_CODE)?".text":".data"));
				}

				// determine whether it is a startup section
				// This is very important as we cannot omit it.
				StartupNumber = GetStartupSectionNumber (hunkName, strlen(hunkName));

				hunkSize = GetNextTI4(ptr)<<2;
				// omit empty sections to simplify the output
				if ((!StartupNumber) && (!hunkSize)) {
					free(hunkName);
					hunkName=NULL;
					break; // no data to skip - there is none!
				}

				// Section is not empty (or a startup section).
				// Try to allocate data for the section, if necessary.
				TestInFile(ptr,I1[hunkSize]);
				TestMem ((currSectionData = calloc (hunkSize, 1)));
				memcpy (currSectionData, ptr, hunkSize);
				ptr += hunkSize;
			
				// create a new section, initialize it, and append it to the list of sections.
				if (relocTargetHunks[numRelocTargetHunks]) {
					currSection = relocTargetHunks[numRelocTargetHunks];
				} else {
					currSection = calloc (1, sizeof (SECTION));
					TestMem (currSection);
					currSection->Parent = Program;
					currSection->FileName = FileName;
				}

				currSection->Data = currSectionData;
				currSection->Size = hunkSize;
				currSection->Code = (hunkType == AMIGAOS_HUNK_CODE);
				currSection->Initialized = TRUE;
				currSection->StartupNumber = StartupNumber;
				currSection->Constructors = (!(strcmp (hunkName, ".ctors")));
				currSection->Destructors = (!(strcmp (hunkName, ".dtors")));
				currSection->CanCutRanges = AllRelocs;

				InsertSection (Program, currSection);

				if (!(CreateSectionSymbol (currSection, hunkName)))
					Fail ();

				relocTargetHunks[numRelocTargetHunks++] = currSection;
				currSectionData = NULL; // don't free handles already in linked list!

				if (hunkType == AMIGAOS_HUNK_CODE && Program->OptimizeInfo->OptimizeNOPs)
					M68kRemoveTrailingNOP (currSection);

				free(hunkName);
				hunkName = NULL;
				break;

			case AMIGAOS_HUNK_BSS:
				// allocate space for at least one more reloc target hunk
				while ((SIZE) ((numRelocTargetHunks+1) * sizeof(SECTION*)) > relocTargetHunksSize) {
					relocTargetHunks = crealloc(relocTargetHunks, relocTargetHunksSize,
																			relocTargetHunksSize+RELOC_TARGET_HUNKS_INCREMENT);
					TestMem(relocTargetHunks);
					relocTargetHunksSize += RELOC_TARGET_HUNKS_INCREMENT;
				}

				// invent a section name if no reasonable one given
				if (!hunkName || !*hunkName || *hunkName==' ') {
					if (hunkName) {
						free(hunkName);
					}
					hunkName = malloc(sizeof(".bss"));
					TestMem(hunkName);
					strcpy(hunkName,".bss");
				}

				hunkSize = GetNextTI4(ptr)<<2;
				// omit empty sections to simplify the output
				if (!hunkSize) break; // no data to skip - there is none!

				// Section is not empty.
				// create a new section, initialize it, and append it to the list of sections.
				if (relocTargetHunks[numRelocTargetHunks]) {
					currSection = relocTargetHunks[numRelocTargetHunks];
				} else {
					currSection = calloc (1, sizeof (SECTION));
					TestMem (currSection);
					currSection->Parent = Program;
					currSection->FileName = FileName;
				}

				currSection->Initialized = InitializeBSS;
				currSection->Size = hunkSize;
				currSection->CanCutRanges = AllRelocs;

				Append (Program->Sections, currSection);

				if (!(CreateSectionSymbol (currSection, hunkName)))
					Fail ();

				relocTargetHunks[numRelocTargetHunks++] = currSection;

				free(hunkName);
				hunkName = NULL;
				break;

			case AMIGAOS_HUNK_RELOC_REL1:
			case AMIGAOS_HUNK_RELOC_REL2:
			case AMIGAOS_HUNK_RELOC_REL4:
			case AMIGAOS_HUNK_RELOC_ABS2:
			case AMIGAOS_HUNK_RELOC_ABS4:
				// make sure we have a section to put those relocs into
				if (!currSection)
					FailWithError("Relocation hunk (type `0x%lX') without context.",(long)hunkType);

				{
					SIZE relocSize = 0;
					BOOLEAN relative = FALSE;
					switch (hunkType) {
						case AMIGAOS_HUNK_RELOC_REL1:
							relative = TRUE;
							relocSize = 1;
							break;
						case AMIGAOS_HUNK_RELOC_REL2:
							relative = TRUE;
						case AMIGAOS_HUNK_RELOC_ABS2:
							relocSize = 2;
							break;
						case AMIGAOS_HUNK_RELOC_REL4:
							relative = TRUE;
						case AMIGAOS_HUNK_RELOC_ABS4:
							relocSize = 4;
							break;
					}

					hunkSize = GetNextTI4(ptr); // hunkSize is the number of relocs here.

					while (hunkSize) {
						OFFSET targetHunkNumber = GetNextTI4(ptr);
						SECTION *targetHunk;
						OFFSET i;

						// allocate space for at least one more reloc target hunk
						while ((SIZE) ((targetHunkNumber+1) * sizeof(SECTION*)) > relocTargetHunksSize) {
							relocTargetHunks = crealloc(relocTargetHunks, relocTargetHunksSize,
														relocTargetHunksSize+RELOC_TARGET_HUNKS_INCREMENT);
							TestMem(relocTargetHunks);
							relocTargetHunksSize += RELOC_TARGET_HUNKS_INCREMENT;
						}

						targetHunk = relocTargetHunks[targetHunkNumber];
						if (!targetHunk) {
							// create placeholder for section right now
							targetHunk = calloc (1, sizeof (SECTION));
							TestMem(targetHunk);
							// initialize the fields to something acceptable to the backend:
							// the dummy section is a 0-byte BSS section
							targetHunk->Parent = Program;
							targetHunk->FileName = FileName;

							// This will get overwritten when the real section will be read in.
							// Otherwise, the "targetHunk" section reference was invalid, hence
							// the name of the dummy symbol.
							if (!(CreateSectionSymbol (targetHunk, "(invalid AmigaOS target section)")))
								Fail ();

							relocTargetHunks[targetHunkNumber] = targetHunk;
						}

						for (i=0; i<(SIZE)hunkSize; i++) {
							RELOC *newReloc;
							I4 location = GetNextTI4(ptr);
							OFFSET targetOffset = 0;
							BOOLEAN unoptimizable = FALSE;
#ifdef AMIGAOS_TIGCC_EXTENSIONS
							unoptimizable = !!(location&AMIGAOS_RELOC_UNOPTIMIZABLE);
							location &= ~AMIGAOS_RELOC_UNOPTIMIZABLE;
#endif
							newReloc = calloc (1, sizeof (RELOC));
							TestMem (newReloc);
							newReloc->Parent = currSection;
							newReloc->Location = location;
							newReloc->Target.Symbol = targetHunk->SectionSymbol;
							newReloc->Target.SymbolName = newReloc->Target.Symbol->Name;
							newReloc->Size = relocSize;
							newReloc->Relative = relative;
							newReloc->Unoptimizable = unoptimizable;
							if ((OFFSET) (location+relocSize) <= currSection->Size) {
								if (currSection->Data) {
									targetOffset = ReadSTI(currSection->Data+location,relocSize);
									// Zero out the section contents.
									memset(currSection->Data+location,0,relocSize);
									// We have to guess the first bytes of the targetOffset. This is
									// the easiest way to do it.
									if (targetHunk != currSection) {
										OFFSET maxDistance = 1 << ((relocSize * 8) - 1);
										// If the section is in front of the current section,
										// set the reference location as close to the end as possible.
										if (targetHunkNumber < (numRelocTargetHunks - 1)) {
											location = targetHunk->Size - (maxDistance - 1);
										// Otherwise, set the reference location as close to the start as possible.
										} else {
											location = maxDistance;
										}
									}
									{
										SI4 difference = targetOffset - location;
										if (relocSize <= 1) {
											targetOffset = location + ((SI1) difference);
										} else if (relocSize <= 2) {
											targetOffset = location + ((SI2) difference);
										}
									}
								} else {
									Warning(FileName,"Adding reloc at 0x%lX in section `%s' without data to section `%s'.",(long)location,currSection->SectionSymbol->Name,newReloc->Target.SymbolName);
								}
							} else {
								Warning(FileName,"Invalid reloc location `0x%lX' in size 4 reloc table for origin section `%s' and target section `%s'",(long)location,currSection->SectionSymbol->Name,newReloc->Target.SymbolName);
							}

							// Apply architecture-specific fixes to the offset.
							newReloc->Target.Offset = ((currSection->Code && newReloc->Target.Symbol->Parent->Code) ? M68kFixTargetOffset (targetOffset, newReloc->Size, newReloc->Relative) : targetOffset);

							// Calculate the remaining part of the offset.
							newReloc->FixedOffset = targetOffset - newReloc->Target.Offset;

							// Put this reloc into the linked list.
							InsertReloc(currSection,newReloc);
						}

						hunkSize = GetNextTI4(ptr);
					}
				}
				break;

#ifdef AMIGAOS_TIGCC_EXTENSIONS
			case AMIGAOS_HUNK_RELOC_ABS1_POSNEG:
			case AMIGAOS_HUNK_RELOC_ABS2_POSNEG:
			case AMIGAOS_HUNK_RELOC_ABS4_POSNEG:
				// make sure we have a section to put those relocs into
				if (!currSection)
					FailWithError("Relocation hunk (type `0x%lX') without context.",(long)hunkType);

				{
					OFFSET i;
					SIZE relocSize = 0;

					switch (hunkType) {
						case AMIGAOS_HUNK_RELOC_ABS1_POSNEG:
							relocSize = 1;
							break;
						case AMIGAOS_HUNK_RELOC_ABS2_POSNEG:
							relocSize = 2;
							break;
						case AMIGAOS_HUNK_RELOC_ABS4_POSNEG:
							relocSize = 4;
							break;
					}

					hunkSize = GetNextTI4(ptr); // hunkSize is the number of relocs here.

					for (i=0; i<(SIZE)hunkSize; i++) {
						RELOC *newReloc;
						OFFSET targetHunkNumber = GetNextTI4(ptr);
						SECTION *targetHunk;
						I4 location;
						OFFSET targetOffset;
						BOOLEAN unoptimizable;

						// allocate space for at least one more reloc target hunk
						while ((SIZE) ((targetHunkNumber+1) * sizeof(SECTION*)) > relocTargetHunksSize) {
							relocTargetHunks = crealloc(relocTargetHunks, relocTargetHunksSize,
														relocTargetHunksSize+RELOC_TARGET_HUNKS_INCREMENT);
							TestMem(relocTargetHunks);
							relocTargetHunksSize += RELOC_TARGET_HUNKS_INCREMENT;
						}

						targetHunk = relocTargetHunks[targetHunkNumber];
						if (!targetHunk) {
							// create placeholder for section right now
							targetHunk = calloc (1, sizeof (SECTION));
							TestMem(targetHunk);
							// initialize the fields to something acceptable to the backend:
							// the dummy section is a 0-byte BSS section
							targetHunk->Parent = Program;
							targetHunk->FileName = FileName;

							// This will get overwritten when the real section will be read in.
							// Otherwise, the "targetHunk" section reference was invalid, hence
							// the name of the dummy symbol.
							if (!(CreateSectionSymbol (targetHunk, "(invalid AmigaOS target section)")))
								Fail ();

							relocTargetHunks[targetHunkNumber] = targetHunk;
						}

						location = GetNextTI4(ptr);
						unoptimizable = !!(location&AMIGAOS_RELOC_UNOPTIMIZABLE);
						location &= ~AMIGAOS_RELOC_UNOPTIMIZABLE;
						targetOffset = GetNextTI4(ptr);
						if (GetNextTI4(ptr)) { // negative reloc
							RELOC *PositiveReloc = NULL;

							// Find a matching positive reloc.
							PositiveReloc = FindMatchingReloc (currSection, location, relocSize, FALSE, NULL, relocHint);

							if (PositiveReloc) {
								LOCATION *relation = calloc (1, sizeof (LOCATION));
								TestMem (relation);
								relation->Symbol = targetHunk->SectionSymbol;
								relation->SymbolName = relation->Symbol->Name;
								relation->Offset = targetOffset;
								PositiveReloc->Relative = TRUE;
								PositiveReloc->Relation = relation;
								HandleLocation(PositiveReloc, relation);

								relocHint = PositiveReloc;
							} else {
								Warning (FileName, "Removing negative reloc at 0x%lX with no matching positive reloc.", (long) location);
							}
						} else { // positive reloc
							newReloc = calloc (1, sizeof (RELOC));
							TestMem (newReloc);
							newReloc->Parent = currSection;
							newReloc->Location = location;
							newReloc->Target.Symbol = targetHunk->SectionSymbol;
							newReloc->Target.SymbolName = newReloc->Target.Symbol->Name;
							newReloc->Target.Offset = targetOffset;
							newReloc->Size = relocSize;
							newReloc->Relative = FALSE;
							newReloc->Unoptimizable = unoptimizable;

							// Those hunks put the target offset in the relocation table. However, we have to
							// read a possible FixedOffset from the data stream.
							if ((OFFSET) (location+relocSize) <= currSection->Size) {
								if (currSection->Data) {
									newReloc->FixedOffset = ReadSTI(currSection->Data+location,relocSize);
									// Zero out the section contents.
									memset(currSection->Data+location,0,relocSize);
								} else {
									Warning(FileName,"Adding reloc at 0x%lX in section `%s' without data to section `%s'.",(long)location,currSection->SectionSymbol->Name,newReloc->Target.SymbolName);
								}
							} else {
								Warning(FileName,"Invalid reloc location `0x%lX' in extended size %ld reloc table for origin section `%s' and target section `%s'",(long)location,(long)relocSize,currSection->SectionSymbol->Name,newReloc->Target.SymbolName);
							}

							// Put this reloc into the linked list.
							InsertReloc(currSection,newReloc);
						}
					}
				}
				break;
#endif

			case AMIGAOS_HUNK_END:
				// Do nothing - we already skipped the hunk type, and that's all we need
				// to do here.
				break;

			case AMIGAOS_HUNK_EXT:
				// make sure we have a section to put those symbols into
				if (!currSection) 
					FailWithError("Symbol import/export hunk without context.");
				hunkSize = GetNextTI4(ptr);
				while (hunkSize) {
					// The most significant byte of the size longword encodes the symbol type.
					hunkType = hunkSize >> 24;
					hunkSize = (hunkSize&0xffffffL)<<2;

					TestInFile(ptr,I1[hunkSize]);
					symName = malloc(hunkSize+1);
					TestMem(symName);
					strncpy(symName,ptr,hunkSize);
					symName[hunkSize]=0;
					ptr+=hunkSize;

					switch (hunkType) {
						// Definitions:
						// Absolute definition: we cannot use those, but ignoring them
						// should not be fatal, so we will ignore them with a warning.
						case AMIGAOS_EXT_ABS:
							Warning(FileName,"Cannot handle absolute symbol `%s'.",symName);
							TestInFile(ptr,TI4);
							ptr+=4;
							break;

						// Standard definition (offset from a section):
						case AMIGAOS_EXT_DEF: {
							I4 location = GetNextTI4(ptr);
							if (HandleSpecialSymbol (Program, symName)) {
								// This is the best we can get without
								// adding too much extra code: Omit the
								// initialization for all following BSS
								// sections.
								if (!(strcmp(symName,SYM_OMIT_BSS_INIT))) {
									InitializeBSS=FALSE;
								}
								// __ld_all_relocs has to be exported from the
								// first section. So we'll set CanCutRanges to
								// TRUE for the section it appears in, and
								// the variable AllRelocs will handle the rest.
								else if (!(strcmp(symName,SYM_ALL_RELOCS))) {
									currSection->CanCutRanges = TRUE;
									AllRelocs = TRUE;
								}
								break;
							}
							currSymbol = calloc (1, sizeof (SYMBOL));
							TestMem(currSymbol);
							currSymbol->Parent = currSection;
							currSymbol->Location = location;
							strncpy(currSymbol->Name,symName,MAX_SYM_LEN);
							currSymbol->Exported = TRUE;
							InsertSymbol(currSection,currSymbol);
							break; }

						// References:
						case AMIGAOS_EXT_REF_ABS1:
						case AMIGAOS_EXT_REF_ABS2:
						case AMIGAOS_EXT_REF_ABS4:
						case AMIGAOS_EXT_REF_REL1:
						case AMIGAOS_EXT_REF_REL2:
						case AMIGAOS_EXT_REF_REL4: {
							// Those are actually relocs in ld-tigcc terms.
							OFFSET i;

							SIZE relocSize = 0;
							BOOLEAN relative = FALSE;
							switch (hunkType) {
								case AMIGAOS_EXT_REF_REL1:
									relative = TRUE;
								case AMIGAOS_EXT_REF_ABS1:
									relocSize = 1;
									break;
								case AMIGAOS_EXT_REF_REL2:
									relative = TRUE;
								case AMIGAOS_EXT_REF_ABS2:
									relocSize = 2;
									break;
								case AMIGAOS_EXT_REF_REL4:
									relative = TRUE;
								case AMIGAOS_EXT_REF_ABS4:
									relocSize = 4;
									break;
							}

							hunkSize = GetNextTI4(ptr); // hunkSize is the number of relocs here.

							for (i=0; i<(SIZE)hunkSize; i++) {
								RELOC *newReloc;
								I4 location = GetNextTI4(ptr);
								BOOLEAN unoptimizable = FALSE;
								char *newName = malloc(strlen(symName)+1);
								TestMem(newName);
								strcpy(newName,symName);
#ifdef AMIGAOS_TIGCC_EXTENSIONS
								unoptimizable = !!(location&AMIGAOS_RELOC_UNOPTIMIZABLE);
								location &= ~AMIGAOS_RELOC_UNOPTIMIZABLE;
#endif
								newReloc = calloc (1, sizeof (RELOC));
								TestMem(newReloc);
								newReloc->Parent = currSection;
								newReloc->Location = location;
								newReloc->Target.SymbolName = newName;
								newReloc->Size = relocSize;
								newReloc->Relative = relative;
								newReloc->Unoptimizable = unoptimizable;
								if ((OFFSET) (location+relocSize) <= currSection->Size) {
									if (currSection->Data) {
										newReloc->FixedOffset = ReadSTI(currSection->Data+location,relocSize);
										memset(currSection->Data+location,0,relocSize);
									} else {
										Warning(FileName,"Adding reloc at 0x%lX in section `%s' without data to symbol `%s'.",(long)location,currSection->SectionSymbol->Name,symName);
									}
								} else {
									Warning(FileName,"Invalid reloc location `0x%lX' in size %ld reference table for origin section `%s' and target symbol `%s'",(long)location,(long)relocSize,currSection->SectionSymbol->Name,symName);
								}

								// Put this reloc into the linked list
								InsertReloc(currSection,newReloc);
							}
							// The (unused) last copy of the symbol name will be freed below.
							break; }

						// Other symbol types are never used by A68k.
						default:
							FailWithError("Unsupported AmigaOS symbol type `0x%lX'.",(unsigned long)hunkType);
							break;
					}

					free(symName);
					symName = NULL; // Prevent symName from getting freed again.
					hunkSize = GetNextTI4(ptr);
				}
				break;

			case AMIGAOS_HUNK_SYMBOL:
				// debugging symbol table - contains local (non-exported) symbols,
				// which are in the object file for debugging purposes only

				// make sure we have a section to put those symbols into
				if (!currSection) 
					FailWithError("Symbol table hunk without context.");

				hunkSize = GetNextTI4(ptr)<<2;
				while (hunkSize) {
					SIZE symSize;
					I4 location;
					SYMBOL *symbol;
					BOOLEAN found=FALSE;

					TestInFile(ptr,I1[hunkSize]);
					symSize = (hunkSize<MAX_SYM_LEN)?hunkSize:MAX_SYM_LEN;

					for_each(symbol,currSection->Symbols) {
						if (!strncmp(symbol->Name,ptr,symSize) && !(ptr[symSize])) {
							found = TRUE;
							break;
						}
					}

					if (found) { // symbol already present - it was already defined as global
						ptr+=hunkSize;

						// skip location
						TestInFile(ptr,TI4);
						ptr+=4;
					} else {
						// copy symbol name
						currSymbol = calloc (1, sizeof (SYMBOL));
						TestMem(currSymbol);
						currSymbol->Parent = currSection;
						strncpy(currSymbol->Name,ptr,symSize);
						currSymbol->Name[symSize+1]=0;
						ptr+=hunkSize;
						// Do NOT handle special symbols specially here! Those symbols are
						// local and should NOT be interpreted by the linker.

						// copy location
						location = GetNextTI4(ptr);
						currSymbol->Location = location;

						//register symbol
						InsertSymbol(currSection,currSymbol);
					}

					hunkSize = GetNextTI4(ptr)<<2;
				}
				break;

			// The other hunk types are not well-documented or not applicable here, and A68k never generates them anyway.
			default:
				FailWithError("Unsupported AmigaOS hunk type `0x%lX'.",(unsigned long)hunkType);
				break;
		}
	}

	// free allocated memory
	if (hunkName) {
		Warning(FileName,"Hunk name (`%s') with no corresponding hunk.",hunkName);
		free(hunkName);
	}
	if (relocTargetHunks) {
		free(relocTargetHunks);
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
