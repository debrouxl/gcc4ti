/* main_opt.inc: Option parser for ld-tigcc command-line program

   Copyright (C) 2002-2004 Sebastian Reichelt
   Copyright (C) 2004 Billy Charvet
   Copyright (C) 2004-2006 Kevin Kofler

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

// This file is included by main.c.
{
	const char *Arg;
	
	BOOLEAN ArgMatches (const char *Name)
	{
		return (!(strcmp (Arg, Name)));
	}
	
	// For each argument...
	for (CurArg = 1; CurArg < ArgCount; CurArg++)
	{
		// Get pointer to arg.
		Arg = Args [CurArg];
		// Check whether it is empty.
		if (Arg && *Arg)
		{
			// Check whether it is an option.
			if (Arg [0] == '-')
			{
				// Skip this dash and possibly a second one.
				Arg++;
				if (Arg [0] == '-')
					Arg++;
				
				// Handle the option.
#ifdef ENABLE_HELP
				if (ArgMatches ("version"))
				{
					printf ("ld-tigcc Version " PROGRAM_VERSION_STRING "\n"
					        COPYRIGHT_NOTICE_STRING "\n"
					        "ld-tigcc is free software; see the source code for details.\n");
					goto Cleanup;
				}
				else if ((ArgMatches ("help")) || (ArgMatches ("h")))
				{
					printf ("Usage: ld-tigcc [options] <file> [<file> ...]\n"
					        "Options:\n"
					        "  -h --help               Display this message\n"
					        "     --version            Display version number\n"
#ifdef ENABLE_STATS
					        "  -v --verbose            Display program statistics\n"
#endif /* ENABLE_STATS */
#ifdef ENABLE_DUMP
					        "     --dump[n]            Display [n-th] dump of the program contents\n"
#endif /* ENABLE_DUMP */
					        "     --native             Link in TIGCC native mode\n"
#ifdef FLASH_OS_SUPPORT
					        "     --flash-os           Create (unsigned) Flash OS\n"
#endif /* FLASH_OS_SUPPORT */
#ifdef FARGO_SUPPORT
					        "     --fargo              Create Fargo II program\n"
#endif /* FARGO_SUPPORT */
					        "     --remove-unused      Remove unused sections\n"
					        "     --optimize-relocs    Update relocs to point to the closest symbol\n"
					        "     --optimize-code      Perform all MC68000 code optimizations (!)\n"
					        "       --optimize-nops      Remove format-specific NOP instructions\n"
					        "       --optimize-returns   Optimize function return sequences (!)\n"
					        "       --optimize-branches  Optimize branch instructions\n"
					        "       --optimize-moves     Optimize move/load/push instructions (!)\n"
					        "       --optimize-tests     Optimize compare/test instructions (!)\n"
					        "       --optimize-calcs     Optimize calculation instructions (!)\n"
					        "     --cut-ranges         Cut unneeded section ranges when optimizing (!)\n"
					        "                          (ignored for files not assembled in all-relocs mode)\n"
					        "     --reorder-sections   Reorder sections to make references shorter\n"
					        "     --merge-constants    Merge constants and strings to avoid duplication\n"
					        "     --omit-bss-init      Do not initialize BSS data to zero\n"
					        "     --outputbin          Create program image without PC header\n"
					        "     --outputbin-main-only\n"
					        "                          Create main file w/o PC header (but data file with)\n"
					        "  -o --output <file>      Write output to <file>.???\n"
					        "  -n --varname <[folder\\]name>\n"
					        "                          Name output variable <folder\\name>\n"
#ifdef DATA_VAR_SUPPORT
					        "  -d --data-var <[folder\\]name>\n"
					        "                          Use data variable named <folder\\name>\n"
					        "     --data-var-copy={never|archived|always}\n"
					        "                          Work on original data variable or copy\n"
#endif /* DATA_VAR_SUPPORT */
					        "<file> can be an archive or an object file.\n"
					        "Options marked with `(!)' are likely to cause errors in binary code.\n");
					goto Cleanup;
				}
				else
#endif /* ENABLE_HELP */
#ifdef ENABLE_STATS
				if ((ArgMatches ("verbose")) || (ArgMatches ("v")))
					DisplayStats = TRUE;
				else
#endif /* ENABLE_STATS */
#ifdef ENABLE_DUMP
				if (!(strncmp (Arg, "dump", sizeof ("dump") - 1)))
				{
					Arg += sizeof ("dump") - 1;
					if (*Arg)
					{
						int DumpIdx = atoi (Arg);
						if ((DumpIdx >= 0) && (DumpIdx < DUMP_COUNT))
							Dump [DumpIdx] = TRUE;
					}
					else
						memset (Dump, -1, sizeof (Dump));
				}
				else
#endif /* ENABLE_DUMP */
				if (ArgMatches ("native"))
					Program.Type = PT_NATIVE;
				else
#ifdef FLASH_OS_SUPPORT
				if (ArgMatches ("flash-os"))
				{
					Program.Type = PT_FLASH_OS;
					Warning (NULL, "Flash OS support in TIGCC is experimental.");
				}
				else
#endif /* FLASH_OS_SUPPORT */
#ifdef FARGO_SUPPORT
				if (ArgMatches ("fargo"))
				{
					Program.Type = PT_FARGO;
					Program.Calcs |= CALC_TI92;
					Warning (NULL, "Fargo support in TIGCC is experimental.");
				}
				else
#endif /* FARGO_SUPPORT */
				if (ArgMatches ("remove-unused"))
					OptInfo->RemoveUnused = TRUE;
				else if (ArgMatches ("optimize-relocs"))
					OptInfo->OptimizeRelocs = TRUE;
				else if (ArgMatches ("optimize-code"))
				{
					OptInfo->OptimizeNOPs     = TRUE;
					OptInfo->OptimizeReturns  = TRUE;
					OptInfo->OptimizeBranches = TRUE;
					OptInfo->OptimizeMoves    = TRUE;
					OptInfo->OptimizeTests    = TRUE;
					OptInfo->OptimizeCalcs    = TRUE;
				}
				else if (ArgMatches ("optimize-nops"))
					OptInfo->OptimizeNOPs = TRUE;
				else if (ArgMatches ("optimize-returns"))
					OptInfo->OptimizeReturns = TRUE;
				else if (ArgMatches ("optimize-branches"))
					OptInfo->OptimizeBranches = TRUE;
				else if (ArgMatches ("optimize-moves"))
					OptInfo->OptimizeMoves = TRUE;
				else if (ArgMatches ("optimize-tests"))
					OptInfo->OptimizeTests = TRUE;
				else if (ArgMatches ("optimize-calcs"))
					OptInfo->OptimizeCalcs = TRUE;
				else if (ArgMatches ("cut-ranges"))
					OptInfo->CutRanges = TRUE;
				else if (ArgMatches ("reorder-sections"))
					OptInfo->ReorderSections = TRUE;
				else if (ArgMatches ("merge-constants"))
					OptInfo->MergeConstants = TRUE;
				else if (ArgMatches ("omit-bss-init"))
					OmitBSSInitialization = TRUE;
				else if (ArgMatches ("outputbin"))
					OutputBin = TRUE;
				else if (ArgMatches ("outputbin-main-only"))
					OutputBinMainOnly = TRUE;
				else if ((ArgMatches ("output")) || (ArgMatches ("o")))
				{
					if ((++CurArg) < ArgCount)
					{
						DestFile = Args [CurArg];
						DestFileSize = strlen (DestFile);
						// Adjust the program's name if it isn't set yet.
						if (!(*ProgramName))
						{
							// Start at the last separator.
							const char *Name = strrchr (DestFile, '/');
							if (!Name)
								Name = strrchr (DestFile, '\\');
							if (Name)
								Name++;
							else
								Name = DestFile;
							// Copy into name field.
							strncpy (ProgramName, Name, MAX_NAME_LEN);
							{
								// Cut at first dot.
								char *S = strchr (ProgramName, '.');
								if (S)
									*S = 0;
								// Cut at first space.
								S = strchr (ProgramName, ' ');
								if (S)
									*S = 0;
								// Convert to lowercase.
								for (S = ProgramName; *S; S++)
									*S = tolower (*S);
							}
						}
					}
					else
						Error (NULL, "`--output' option must be followed by a name.");
				}
				else if ((ArgMatches ("varname")) || (ArgMatches ("n")))
				{
					if ((++CurArg) < ArgCount)
					{
						// Extract variable name.
						const char *Name = strchr (Args [CurArg], '/');
						if (!Name)
							Name = strchr (Args [CurArg], '\\');
						if (Name)
						{
							Name++;
							// Extract folder name.
							strncpy (ProgramFolder, Args [CurArg], MAX_NAME_LEN);
							{
								char *S;
								for (S = ProgramFolder; *S; S++)
								{
									if (*S == '/' || *S == '\\')
									{
										*S = 0;
										break;
									}
									else
										*S = tolower (*S);
								}
							}
						}
						else
							Name = Args [CurArg];
						strncpy (ProgramName, Name, MAX_NAME_LEN);
						{
							char *S;
							for (S = ProgramName; *S; S++)
								*S = tolower (*S);
						}
					}
					else
						Error (NULL, "`--varname' option must be followed by a name.");
				}
#ifdef DATA_VAR_SUPPORT
				else if (ArgMatches ("data-var") || ArgMatches ("d"))
				{
					if ((++CurArg) < ArgCount)
					{
						// Extract variable name.
						const char *Name = strchr (Args [CurArg], '/');
						if (!Name)
							Name = strchr (Args [CurArg], '\\');
						if (Name)
						{
							Name++;
							// Extract folder name.
							strncpy (DataFolder, Args [CurArg], MAX_NAME_LEN);
							{
								char *S;
								for (S = DataFolder; *S; S++)
								{
									if (*S == '/' || *S == '\\')
									{
										*S = 0;
										break;
									}
									else
										*S = tolower (*S);
								}
							}
							sprintf (DataVarString, "%s\\", DataFolder);
						}
						else
						{
							Name = Args [CurArg];
							*DataVarString = 0;
						}
						strncpy (DataName, Name, MAX_NAME_LEN);
						{
							char *S;
							for (S = DataName; *S; S++)
								*S = tolower (*S);
						}
						strcat (DataVarString, DataName);
						DatVarInfo->Name = DataVarString;
						DatVarInfo->CreateCopy = TRUE;
						DatVarInfo->CopyOnlyIfArchived = TRUE;
					}
					else
						Error (NULL, "`--data-var' option must be followed by a name.");
				}
				else if (!(strncmp (Arg, "data-var-copy=", sizeof ("data-var-copy=") - 1)))
				{
					Arg += sizeof ("data-var-copy=") - 1;
					if (ArgMatches ("always"))
					{
						DatVarInfo->CreateCopy = TRUE;
						DatVarInfo->CopyOnlyIfArchived = FALSE;
					}
					else if (ArgMatches ("archived"))
					{
						DatVarInfo->CreateCopy = TRUE;
						DatVarInfo->CopyOnlyIfArchived = TRUE;
					}
					else if (ArgMatches ("never"))
					{
						DatVarInfo->CreateCopy = FALSE;
						DatVarInfo->CopyOnlyIfArchived = FALSE;
					}
					else
						Error (NULL, "Unrecognized copying condition `%s'.", Arg);
				}
#endif /* DATA_VAR_SUPPORT */
				else
					Error (NULL, "Unrecognized option `%s'.", Args [CurArg]);
			}
			else
			{
				// Treat it as a file name.
				
				// Open the file and load it into memory.
				{
					FILE *File = fopen (Arg, "rb");
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
								BOOLEAN IsArchive = FALSE;
								
								if (fread (Data, Size, 1, File) == 1)
								{
									// Act differently depending on the
									// file type.
									IsArchive = IsArchiveFile (Data, Size);
									
									if (IsArchive)
									{
										// Add the file to the list of
										// available archives.
										AddArchiveFile (&Program, Data, Size, Arg);
									}
									else
									{
										// Adjust the destination file
										// name if it isn't set yet.
										if (!DestFile)
										{
											const char *DotPos, *SlashPos;
											// Assign the destination file name.
											DestFile = Arg;
											DestFileSize = strlen (Arg);
											// Cut it at the last dot.
											DotPos = strrchr (Arg, '.');
											if (DotPos)
											{
												SlashPos = strrchr (Arg, '/');
												if (!SlashPos)
													SlashPos = strrchr (Arg, '\\');
												if ((!SlashPos) || (DotPos > SlashPos))
													DestFileSize = DotPos - Arg;
											}
										}
										
										// Import the file.
										ImportObjectFile (&Program, Data, Size, Arg);
									}
								}
								else
									Error (Arg, "Unable to read file.");
								
								// Archives are not imported; they keep
								// their data until they are freed.
								if (!IsArchive)
									free (Data);
							}
							else
								Error (Arg, "Not enough memory to load file.");
						}
						fclose (File);
					}
					else
						Error (Arg, "Unable to open file.");
				}
			}
		}
	}
	
	// Adjust the program's name if it isn't set yet.
	if (DestFile && (!(*ProgramName)))
	{
		// Start at the last separator.
		const char *Name = strrchr (DestFile, '/');
		if (!Name)
			Name = strrchr (DestFile, '\\');
		if (Name)
			Name++;
		else
			Name = DestFile;
		// Copy into name field.
		strncpy (ProgramName, Name, MAX_NAME_LEN);
		{
			// Cut at first dot.
			char *S = strchr (ProgramName, '.');
			if (S)
				*S = 0;
			// Cut at first space.
			S = strchr (ProgramName, ' ');
			if (S)
				*S = 0;
			// Convert to lowercase.
			for (S = ProgramName; *S; S++)
				*S = tolower (*S);
		}
	}
}
