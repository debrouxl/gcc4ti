/* exp_def.h: Definitions for file exports

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

#ifndef EXP_DEF_H
#define EXP_DEF_H

#include "../generic.h"
#include "../integers.h"
#include "../intrface.h"
#include "../data.h"

#ifdef TARGET_EMBEDDED

#include <string.h>
// This is the complete export file, created locally by ExportFile.
typedef struct {
	INT_EXP_FILE File;  // Internal part of the export file.
	OFFSET CurPos;      // Current writing position inside the file
} EXP_FILE;
#define ExportWrite(F,P,L,C) (memcpy ((F)->File.Data + (F)->CurPos, P, (L) * (C)), (F)->CurPos += (L) * (C))
#define ExportSeek(F,P) ((F)->CurPos = (P))
#define ExportTell(F) ((F)->CurPos)
#define ExportWriteTI1(F,D) ({ WriteTI1 (*((TI1 *) ((F)->File.Data + (F)->CurPos)), D); (F)->CurPos += 1; })
#define ExportWriteTI2(F,D) ({ WriteTI2 (*((TI2 *) ((F)->File.Data + (F)->CurPos)), D); (F)->CurPos += 2; })
#define ExportWriteTI4(F,D) ({ WriteTI4 (*((TI4 *) ((F)->File.Data + (F)->CurPos)), D); (F)->CurPos += 4; })
#define ExportWriteTI(F,D,L,AllowSigned,AllowUnsigned) ({ WriteTI (((F)->File.Data + (F)->CurPos), L, D, AllowSigned, AllowUnsigned); (F)->CurPos += (L); })

#else /* !TARGET_EMBEDDED */

#include <stdio.h>
// This is the complete export file, created locally by ExportFile.
typedef struct {
	INT_EXP_FILE File;  // Internal part of the export file.
} EXP_FILE;
#define ExportWrite(F,P,L,C) ({ I1 *__P = (I1 *) (P); OFFSET __CP; fwrite (__P, L, C, (F)->File.File); for (__CP = 0; __CP < (OFFSET) ((L) * (C)); __CP++) (F)->File.CheckSum += *(__P++); })
#define ExportSeek(F,P) (fseek ((F)->File.File, P, SEEK_SET))
#define ExportTell(F) (ftell ((F)->File.File))

#endif /* !TARGET_EMBEDDED */

#ifndef ExportWriteTI1
#define ExportWriteTI1(F,D) ({ TI1 __D; WriteTI1 (__D, D); ExportWrite (F, &__D, 1, 1); })
#endif

#ifndef ExportWriteTI2
#define ExportWriteTI2(F,D) ({ TI2 __D; WriteTI2 (__D, D); ExportWrite (F, &__D, 2, 1); })
#endif

#ifndef ExportWriteTI4
#define ExportWriteTI4(F,D) ({ TI4 __D; WriteTI4 (__D, D); ExportWrite (F, &__D, 4, 1); })
#endif

#ifndef ExportWriteTI
#define ExportWriteTI(F,D,L,AllowSigned,AllowUnsigned) ({ I1 __D[(L)]; WriteTI (__D, L, D, AllowSigned, AllowUnsigned); ExportWrite (F, __D, L, 1); })
#endif

// This is the format of a "Get...FileSize" function.
typedef SIZE (*GET_FILE_SIZE_FUNCTION) (const PROGRAM *Program);
// This is the format of a "Export...File" function.
typedef BOOLEAN (*EXPORT_FILE_FUNCTION) (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize, ProgramCalcs DestCalc);

// If the reloc can be resolved to a calculator-dependent builtin value,
// write the value into the data segment in the file which starts at
// DataStart.
BOOLEAN EmitCalcBuiltinValue (const RELOC *Reloc, ProgramCalcs DestCalc, EXP_FILE *File, SIZE FileSize, OFFSET DataStart);

#endif
