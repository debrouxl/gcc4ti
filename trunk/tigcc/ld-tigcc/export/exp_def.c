/* exp_def.c: File export utilities

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

#include "exp_def.h"

#include "../special.h"

// If the reloc can be resolved to a calculator-dependent builtin value,
// write the value into the data segment in the file which starts at
// DataStart.
BOOLEAN EmitCalcBuiltinValue (const RELOC *Reloc, ProgramCalcs DestCalc, EXP_FILE *File, SIZE FileSize, OFFSET DataStart)
{
	IMAX Value;
	
	if (GetCalcBuiltinValue (Reloc, DestCalc, &Value) && (Reloc->Location >= 0) && (Reloc->Location + Reloc->Size <= FileSize))
	{
		OFFSET CurPos = ExportTell (File);
		ExportSeek (File, DataStart + Reloc->Location);
		ExportWriteTI (File, Value, Reloc->Size, FALSE, TRUE);
		ExportSeek (File, CurPos);
		return TRUE;
	}
	
	return FALSE;
}
