/* main_opt.inc: Option parser for ld-tigcc command-line program

   Copyright (C) 2002-2003 Sebastian Reichelt

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
				Arg++;
				if (Arg [0] == '-')
					Arg++;
				
				// Handle the option.
#ifdef ENABLE_HELP
				if (ArgMatches ("version"))
				{
					printf ("ar-tigcc Version " PROGRAM_VERSION_STRING "\n"
					        COPYRIGHT_NOTICE_STRING "\n"
					        "ar-tigcc is free software; see the source code for details.\n");
					goto Cleanup;
				}
				else if (ArgMatches ("help") || ArgMatches ("-h"))
				{
					printf ("Usage: ar-tigcc [options] <file> [<file> ...]\n"
					        "Options:\n"
					        "  -h --help           Display this message\n"
					        "     --version        Display version number\n"
#ifdef ENABLE_DUMP
					        "     --dump           Display small dump of the archive contents\n"
#endif /* ENABLE_DUMP */
					        "  -o --output <file>  Write output to <file>\n"
					        "  -rc/-qc <file>      Same as `-o' for compatibility with GNU ar\n"
					        "     --no-names       Do not include object file names in archive\n");
					goto Cleanup;
				}
				else
#endif /* ENABLE_HELP */
#ifdef ENABLE_DUMP
				if (ArgMatches ("dump"))
					Dump = TRUE;
				else
#endif /* ENABLE_DUMP */
				if (ArgMatches ("no-names"))
					NoNames = TRUE;
				else if (ArgMatches ("o") || ArgMatches ("output") || ArgMatches ("rc") || ArgMatches ("-qc"))
				{
					if ((++CurArg) < ArgCount)
						DestFile = Args [CurArg];
					else
						Error (NULL, "`--output' option must be followed by a name.");
				}
				else
					Error (NULL, "Unrecognized option `%s'.", Args [CurArg]);
			}
			else
			{
				// Treat it as a file name.
				
				// Open the file and load it into memory.
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
							if (fread (Data, Size, 1, File) == 1)
							{
								// Create a new object file inside the current archive.
								OBJECT_FILE *ObjectFile = calloc (1, sizeof (OBJECT_FILE));
								
								if (!ObjectFile)
								{
									Error (Arg, "Out of memory.");
									break;
								}
								
								ObjectFile->Parent = &Archive;
								ObjectFile->Data = Data;
								ObjectFile->Size = Size;
								ObjectFile->FileName = Arg;
								GetFileStats (Arg, ObjectFile->FileStats);
								Append (Archive.ObjectFiles, ObjectFile);
								
								// Update the statistics of the archive accordingly.
								if (StatFileIsNewer (ObjectFile->FileStats, Archive.FileStats))
									StatCopyAttributes (Archive.FileStats, ObjectFile->FileStats);
								
								// Try to import the object file's contents.
								ArImportObjectFile (ObjectFile);
							}
							else
								Error (Arg, "Unable to read file.");
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
