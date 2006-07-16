/* export.c: Routines for file exports

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

#include "export.h"

#ifdef TIOS_SUPPORT
#include "exp_tios.h"
#endif /* TIOS_SUPPORT */
#ifdef FLASH_OS_SUPPORT
#include "exp_os.h"
#endif /* FLASH_OS_SUPPORT */
#ifdef NOSTUB_DLL_SUPPORT
#include "exp_ndll.h"
#endif /* NOSTUB_DLL_SUPPORT */
#ifdef FARGO_SUPPORT
#include "exp_farg.h"
#endif /* FARGO_SUPPORT */
#ifdef DATA_VAR_SUPPORT
#include "exp_data.h"
#endif /* DATA_VAR_SUPPORT */
#ifdef DEBUGGING_INFO_SUPPORT
#include "exp_dbg.h"
#endif /* DEBUGGING_INFO_SUPPORT */

#include "../formats/tios.h"
#include "../formats/tiosupgd.h"

#include "../special.h"

#include <string.h>

typedef struct {
	OUTPUT_FILE_FUNCTION GetOutputFile;
	OUTPUT_FILE_FINALIZE_FUNCTION FinalizeOutputFile;
	GET_FILE_SIZE_FUNCTION GetFileSize;
	EXPORT_FILE_FUNCTION ExportFile, ExportDataFile;
	FileFormats FileFormat, DataFileFormat;
	unsigned int FileType, DataFileType;
	const char *Extension;
} EXPORT_STRUCT;

// Return the maximum file size allowed for a given format
// (without header and footer).
static SIZE GetMaxFileSize (FileFormats FileFormat, ProgramCalcs Calc)
{
	switch (FileFormat)
	{
		case FF_TIOS:
			return MAX_TIOS_FILE_SIZE;
		case FF_TIOS_UPGRADE:
			if (Calc == CALC_V200 || (Calc == (CALC_TI89 | CALC_FLAG_TITANIUM)))
				return MAX_TIOS_UPGRADE_FILE_SIZE_NEW;
			else
				return MAX_TIOS_UPGRADE_FILE_SIZE_OLD;
		default:
			return 0;
	}
}

// Export the internal data structures to an external file for the given
// calculator.
static BOOLEAN ExportProgramToFormat (const PROGRAM *Program, const EXPORT_STRUCT *ExportStruct, ProgramCalcs Calc)
{
	BOOLEAN Success;
	EXP_FILE File;
	SIZE Size, MaxSize;
	I4 EffectiveSize = 0;
	
	// Fill the output file struct with zeroes.
	memset (&File, 0, sizeof (File));
	
	// Get the file size needed to export the file.
	Size = ExportStruct->GetFileSize (Program);
	
	// A size of 0 indicates an error.
	if (Size <= 0)
		return FALSE;
	
	// Check for maximum file size.
	MaxSize = GetMaxFileSize (ExportStruct->FileFormat, Calc);
	
	if (Size > MaxSize)
	{
		Error (NULL, "Program size of %ld bytes exceeds maximum by %ld.", (long) Size, (long) (Size - MaxSize));
		return FALSE;
	}
	
	// Create an output file with the specified format.
	if (!(ExportStruct->GetOutputFile (&(File.File), Size, Calc, FR_MAIN, ExportStruct->FileFormat, ExportStruct->FileType, ExportStruct->Extension, !(Program->Library), &EffectiveSize)))
		return FALSE;
	
	// Export the program into the output file.
	Success = ExportStruct->ExportFile (Program, &File, Size, Calc);
	
	// Finalize and close the output file.
	if (ExportStruct->FinalizeOutputFile)
		ExportStruct->FinalizeOutputFile (&(File.File));
	
	if (Success)
	{
		if (Program->OptimizeInfo->ProgramSize < (SI4) EffectiveSize)
			Program->OptimizeInfo->ProgramSize = (SI4) EffectiveSize;
	}
	else
		return FALSE;
	
#ifdef DATA_VAR_SUPPORT
	// Write the data file, if any.
	if (Program->DataVarInfo->Name && Program->DataSection)
	{
		if (ExportStruct->DataFileFormat == FF_NONE)
		{
			Error (NULL, "Data variable is not supported in this file format.");
			return FALSE;
		}
		
		// Fill the output file struct with zeroes.
		memset (&File, 0, sizeof (File));
		
		// Get the file size needed to export the file.
		Size = GetDataFileSize (Program);
		
		// A size of 0 indicates an error.
		if (Size <= 0)
			return FALSE;
		
		MaxSize = GetMaxFileSize (ExportStruct->DataFileFormat, Calc);
		
		// Check for maximum file size.
		if (Size > MaxSize)
		{
			Error (NULL, "Data variable size of %ld bytes exceeds maximum by %ld.", (long) Size, (long) (MaxSize - Size));
			return FALSE;
		}
		// Create an output file with the specified format.
		if (!(ExportStruct->GetOutputFile (&(File.File), Size, Calc, FR_DATA, ExportStruct->DataFileFormat, ExportStruct->DataFileType, "DAT", FALSE, &EffectiveSize)))
			return FALSE;
		
		// Export the program into the output file.
		Success = ExportStruct->ExportDataFile (Program, &File, Size, Calc);
		
		// Finalize and close the output file.
		if (ExportStruct->FinalizeOutputFile)
			ExportStruct->FinalizeOutputFile (&(File.File));
		
		if (Success)
		{
			if (Program->OptimizeInfo->DataSize < (SI4) EffectiveSize)
				Program->OptimizeInfo->DataSize = (SI4) EffectiveSize;
		}
		else
			return FALSE;
	}
#endif /* DATA_VAR_SUPPORT */
	
	return TRUE;
}

// Export the internal data structures to external files, creating as many
// files as needed.
BOOLEAN ExportProgram (const PROGRAM *Program, OUTPUT_FILE_FUNCTION GetOutputFile, OUTPUT_FILE_FINALIZE_FUNCTION FinalizeOutputFile)
{
	EXPORT_STRUCT ExportStruct = {GetOutputFile, FinalizeOutputFile, NULL, NULL, NULL, FF_TIOS, FF_TIOS, TIOS_TAG_ASM, TIOS_TAG_OTH, NULL};
	
	if (!GetOutputFile)
		return FALSE;
	
#ifdef DEBUGGING_INFO_SUPPORT
	// If we have debugging information to export, do it.
	if (Program->HaveDebuggingInfo)
	{
			if (!ExportDebuggingInfo (Program, GetOutputFile, FinalizeOutputFile))
				return FALSE;
	}
#endif

	switch (Program->Type)
	{
#ifdef FLASH_OS_SUPPORT
		case PT_FLASH_OS:
			ExportStruct.GetFileSize    = GetFlashOSFileSize;
			ExportStruct.ExportFile     = ExportFlashOSFile;
			ExportStruct.FileFormat     = FF_TIOS_UPGRADE;
			ExportStruct.FileType       = 0; // Different types for Flash OSs do not exist.
			ExportStruct.DataFileFormat = FF_NONE;
			ExportStruct.DataFileType   = 0;
			break;
#endif /* FLASH_OS_SUPPORT */
		
#ifdef NOSTUB_DLL_SUPPORT
		case PT_NOSTUB_DLL:
			ExportStruct.GetFileSize = GetNostubDLLFileSize;
			ExportStruct.ExportFile  = ExportNostubDLLFile;
			ExportStruct.FileType    = TIOS_TAG_OTH;
			ExportStruct.Extension   = "DLL";
			break;
#endif /* NOSTUB_DLL_SUPPORT */
		
#ifdef FARGO_SUPPORT
		case PT_FARGO:
			ExportStruct.GetFileSize = GetFargoFileSize;
			ExportStruct.ExportFile  = ExportFargoFile;
			ExportStruct.FileType    = TIOS_TAG_PRGM;
			break;
#endif /* FARGO_SUPPORT */
		
		default:
#ifdef TIOS_SUPPORT
			ExportStruct.GetFileSize = GetTIOSFileSize;
			ExportStruct.ExportFile  = ExportTIOSFile;
#else /* !TIOS_SUPPORT */
			Error (NULL, "No default export target for this program type.");
			return FALSE;
#endif /* !TIOS_SUPPORT */
	}
	
#ifdef DATA_VAR_SUPPORT
	ExportStruct.ExportDataFile = ExportDataFile;
#endif /* DATA_VAR_SUPPORT */
	
	{
		ProgramCalcs CurCalc;
		
		// For all calculators...
		for (CurCalc = 1; CurCalc <= HIGHEST_CALC; CurCalc <<= 1)
		{
			if (Program->Calcs & CurCalc)
			{
				if (!(ExportProgramToFormat (Program, &ExportStruct, CurCalc)))
					return FALSE;
				
				if (Program->Type == PT_FLASH_OS && CurCalc == CALC_TI89 && (Program->Calcs & CALC_FLAG_TITANIUM))
				{
					if (!(ExportProgramToFormat (Program, &ExportStruct, CurCalc | CALC_FLAG_TITANIUM)))
						return FALSE;
				}
			}
		}
	}
	
	Program->OptimizeInfo->RelocCount += Program->OptimizeInfo->NativeRelocCount;
	
	return TRUE;
}

// Non-embedded file output functions as defined in intrface.h.
#ifndef TARGET_EMBEDDED

#include <stdio.h>

// Destination file name without extension.
const char *DestFile = NULL;
SIZE DestFileSize = 0;

// Folder and variable name for main file.
char ProgramFolder[MAX_NAME_LEN+1] = "main", ProgramName[MAX_NAME_LEN+1] = "program";
#ifdef DATA_VAR_SUPPORT
// Folder and variable name for data file.
char DataFolder[MAX_NAME_LEN+1] = "", DataName[MAX_NAME_LEN+1] = "data";
#endif /* DATA_VAR_SUPPORT */

// Specifies whether to output only the binary image of the program.
BOOLEAN OutputBin = FALSE;
// Specifies whether to output only the binary image of the main file.
BOOLEAN OutputBinMainOnly = FALSE;

// Maximum length of a file extension.
#define MAX_FILE_EXT_LEN 3

BOOLEAN GetOutputFile (INT_EXP_FILE *File, SIZE FileSize, unsigned int DestCalc, unsigned int FileRole, unsigned int FileFormat, unsigned int FileType, const char *Extension, BOOLEAN Executable ATTRIBUTE_UNUSED, I4 *EffectiveSize)
{
	// Keep some of the parameters for finalization.
	File->OutputBin  = OutputBin || (OutputBinMainOnly && FileRole == FR_MAIN);
	File->FileFormat = FileFormat;
	File->FileType   = FileType;
	File->Extension  = Extension;
	
	// Determine the destination file format and act accordingly.
	switch (FileFormat)
	{
#ifdef TIOS_FILE_SUPPORT
		case FF_TIOS:
			{
				const char *FolderName, *VarName;
				const char *FileExtCalc, *FileExtType;
				unsigned int LinkType;
				
				// Calculate the size needed for the two size bytes and the
				// tag.
				SIZE VarFileSize = 2 + FileSize + 1;
				
				// If the file has an extension, add the size needed for it.
				if (Extension)
					VarFileSize += 1 + strlen (Extension) + 1;
				
				// The variable size is what the user will see.
				*EffectiveSize = VarFileSize;
				
				// Determine the role of the file and set the folder and
				// variable names according to the user settings.
				switch (FileRole)
				{
					case FR_MAIN:
						FolderName = ProgramFolder;
						VarName = ProgramName;
						break;
#ifdef DATA_VAR_SUPPORT
					case FR_DATA:
						if (*DataFolder)
							FolderName = DataFolder;
						else
							FolderName = ProgramFolder;
						VarName = DataName;
						break;
#endif /* DATA_VAR_SUPPORT */
					default:
						Error (NULL, "Unknown file role.");
						return FALSE;
				}
				
				// Determine the destination calculator and set part of the
				// file extension.
				switch (DestCalc)
				{
					case CALC_TI92:
						FileExtCalc = "92";
						break;
					case CALC_TI89:
						FileExtCalc = "89";
						break;
					case CALC_TI92PLUS:
						FileExtCalc = "9x";
						break;
					case CALC_V200:
						FileExtCalc = "v2";
						break;
					default:
						Error (NULL, "Unknown calculator.");
						return FALSE;
				}
				
				// Determine the file type and set the other part of the file
				// extension.
				switch (FileType)
				{
					case TIOS_TAG_STR:
						FileExtType = "s";
						LinkType = TIOS_LINK_TYPE_STR;
						break;
					case TIOS_TAG_PRGM:
						FileExtType = "p";
						LinkType = TIOS_LINK_TYPE_PRGM;
						break;
					case TIOS_TAG_ASM:
						FileExtType = "z";
						LinkType = TIOS_LINK_TYPE_ASM;
						break;
					case TIOS_TAG_OTH:
						FileExtType = "y";
						LinkType = TIOS_LINK_TYPE_OTH;
						break;
					default:
						Error (NULL, "Unknown file type.");
						return FALSE;
				}
				
				{
					SIZE FileNameSize = DestFileSize;
					
					// Create a temporary file name string.
					char CurOutputFileName[FileNameSize+1+MAX_FILE_EXT_LEN+1];
					strncpy (CurOutputFileName, DestFile, FileNameSize);
					
					// Insert the dot into the file name.
					CurOutputFileName [FileNameSize++] = '.';
					
					// Insert the extension. Reverse in binary mode.
					if (File->OutputBin)
					{
						strcpy (CurOutputFileName + FileNameSize, FileExtType);
						FileNameSize += strlen (FileExtType);
						strcpy (CurOutputFileName + FileNameSize, FileExtCalc);
						FileNameSize += strlen (FileExtCalc);
					}
					else
					{
						strcpy (CurOutputFileName + FileNameSize, FileExtCalc);
						FileNameSize += strlen (FileExtCalc);
						strcpy (CurOutputFileName + FileNameSize, FileExtType);
						FileNameSize += strlen (FileExtType);
					}
					
					// Zero-terminate the string.
					CurOutputFileName [FileNameSize] = 0;
					
					// Open a file with the specified name.
					if (!(File->File = fopen (CurOutputFileName, "wb")))
					{
						Error (CurOutputFileName, "Could not open file for writing.");
						return FALSE;
					}
					
					// Write the host file header, if desired.
					if (!File->OutputBin)
					{
						TIOS_HOST_FILE_HEADER Header;
						
						memset (&Header, 0, sizeof (Header));
						
						// Write the signature.
						switch (DestCalc)
						{
							case CALC_TI92:
								strcpy (Header.Signature, "**TI92**");
								break;
							case CALC_TI89:
								strcpy (Header.Signature, "**TI89**");
								break;
							case CALC_TI92PLUS:
							case CALC_V200:
								strcpy (Header.Signature, "**TI92P*");
								break;
							default:
								Error (NULL, "Unknown calculator.");
								return FALSE;
						}
						
						// Write folder name.
						strncpy (Header.FolderName, FolderName, sizeof (Header.FolderName));
						
						// Write variable name.
						strncpy (Header.VarName, VarName, sizeof (Header.VarName));
						
						// Write link type.
						WriteHI1 (Header.LinkType, LinkType);
						
						// Write file size.
						WriteHI4 (Header.FileSize, VarFileSize + sizeof (TIOS_HOST_FILE_HEADER) + sizeof (TIOS_HOST_FILE_FOOTER));
						
						// Write reserved bytes.
						Header.Reserved1 [0] = 0x01;
						Header.Reserved2 [0] = 0x01;
						Header.Reserved2 [2] = 0x52;
						Header.Reserved4 [0] = 0xA5;
						Header.Reserved4 [1] = 0x5A;
						
						// Export the header.
						fwrite (&Header, sizeof (Header), 1, File->File);
					}
					
					// Write the on-calc file header.
					{
						TI2 EncVarFileSize;
						
						// Encode the embedded size into the target
						// endianness.
						WriteTI2 (EncVarFileSize, VarFileSize - 2);
						
						// Export the file size.
						fwrite (&EncVarFileSize, 2, 1, File->File);
						
						// Update the checksum, since this is still included
						// in the on-calc file image.
						File->CheckSum += ReadTI1 (EncVarFileSize.Hi) + ReadTI1 (EncVarFileSize.Lo);
					}
					
					return TRUE;
				}
			}
			break;
#endif /* TIOS_FILE_SUPPORT */
		
#ifdef TIOS_UPGRADE_FILE_SUPPORT
		case FF_TIOS_UPGRADE:
			*EffectiveSize = FileSize + sizeof (TIOS_UPGRADE_CALC_FOOTER);
			if (File->OutputBin)
			{
				const char *FileExtCalc;
				
				// Determine the destination calculator and set part of the
				// file extension.
				switch (DestCalc)
				{
					case CALC_TI92:
						FileExtCalc = "92";
						break;
					case CALC_TI89:
						FileExtCalc = "89";
						break;
					case CALC_TI89 | CALC_FLAG_TITANIUM:
						FileExtCalc = "89ti";
						break;
					case CALC_TI92PLUS:
						FileExtCalc = "9x";
						break;
					case CALC_V200:
						FileExtCalc = "v2";
						break;
					default:
						Error (NULL, "Unknown calculator.");
						return FALSE;
				}
				
				{
					SIZE FileNameSize = DestFileSize;
					
					// Create a temporary file name string.
					char CurOutputFileName[FileNameSize+1+4+1+sizeof("tib")];
					strncpy (CurOutputFileName, DestFile, FileNameSize);
					
					// Insert a dash into the file name.
					CurOutputFileName [FileNameSize++] = '-';
					
					// Insert the calculator model into the file name.
					strcpy (CurOutputFileName + FileNameSize, FileExtCalc);
					FileNameSize += strlen (FileExtCalc);
					
					// Insert the dot into the file name.
					CurOutputFileName [FileNameSize++] = '.';
					
					// Insert the .tib extension.
					strcpy (CurOutputFileName + FileNameSize, "tib");
					FileNameSize += sizeof ("tib") - 1;
					
					// Zero-terminate the string.
					CurOutputFileName [FileNameSize] = 0;
					
					// Open a file with the specified name.
					if (!(File->File = fopen (CurOutputFileName, "wb")))
					{
						Error (CurOutputFileName, "Could not open file for writing.");
						return FALSE;
					}
					return TRUE;
				}
			}
			else
				Error (NULL, "Support for `.??u' files is not implemented yet. Use `--outputbin'.");
			
			break;
#endif /* TIOS_UPGRADE_FILE_SUPPORT */
		
#ifdef DEBUGGING_INFO_SUPPORT
		case FF_GDB_COFF:
			*EffectiveSize = FileSize;
			{
				SIZE FileNameSize = DestFileSize+4;
					
				// Create a temporary file name string.
				char CurOutputFileName[FileNameSize+1];
				strncpy (CurOutputFileName, DestFile, DestFileSize);
					
				// Append ".dbg" to the file name.
				strcpy (CurOutputFileName + DestFileSize, ".dbg");
				
				// Zero-terminate the string.
				CurOutputFileName [FileNameSize] = 0;
				
				// Open a file with the specified name.
				if (!(File->File = fopen (CurOutputFileName, "wb")))
				{
					Error (CurOutputFileName, "Could not open file for writing.");
					return FALSE;
				}
				
				return TRUE;
			}
			break;
#endif /* DEBUGGING_INFO_SUPPORT */

		default:
			Error (NULL, "Unrecognized output file format.");
	}
	
	return FALSE;
}

void FinalizeOutputFile (INT_EXP_FILE *File)
{
	// Determine the destination file format and finish it.
	switch (File->FileFormat)
	{
#ifdef TIOS_FILE_SUPPORT
		case FF_TIOS:
			{
				// Write the on-calc file footer.
				{
					TI1 EncFileTag;
					
					// Export the possible file extension.
					if (File->Extension)
					{
						// Export the leading 0 byte.
						WriteTI1 (EncFileTag, 0);
						fwrite (&EncFileTag, 1, 1, File->File);
						
						// Export the extension string and terminating 0
						// byte.
						fwrite (File->Extension, 1, strlen (File->Extension) + 1, File->File);
						
						// Update the checksum.
						while (*(File->Extension))
							File->CheckSum += (*(File->Extension++));
					}
					
					// Encode the file tag.
					WriteTI1 (EncFileTag, File->FileType);
					
					// Export the file tag.
					fwrite (&EncFileTag, 1, 1, File->File);
					
					File->CheckSum += (I1) (File->FileType);
				}
				
				// Write the host file footer, if desired.
				if (!File->OutputBin)
				{
					TIOS_HOST_FILE_FOOTER Footer;
					
					memset (&Footer, 0, sizeof (Footer));
					
					// Write the checksum.
					WriteHI2 (Footer.CheckSum, File->CheckSum);
					
					// Export the footer.
					fwrite (&Footer, sizeof (Footer), 1, File->File);
				}
			}
			break;
#endif /* TIOS_FILE_SUPPORT */
		
#ifdef TIOS_UPGRADE_FILE_SUPPORT
		case FF_TIOS_UPGRADE:
			{
				// Write the on-calc upgrade file footer.
				{
					TIOS_UPGRADE_CALC_FOOTER Footer;
					
					// Since we do not have TI's private key, we cannot
					// write a real checksum and signature.
					memset (&Footer, 0, sizeof (Footer));
					
					// Encode the fixed data we know.
					WriteTI2 (Footer.SignatureHeader, 0x020D);
					WriteTI1 (Footer.SignatureType,     0x40);
					
					// Export the footer.
					fwrite (&Footer, sizeof (Footer), 1, File->File);
				}
			}
			break;
#endif /* TIOS_UPGRADE_FILE_SUPPORT */

#ifdef DEBUGGING_INFO_SUPPORT
		case FF_GDB_COFF:
			// Do nothing.
			break;
#endif /* DEBUGGING_INFO_SUPPORT */
	}
	
	// Close the file.
	fclose (File->File);
}

#endif /* !TARGET_EMBEDDED */
