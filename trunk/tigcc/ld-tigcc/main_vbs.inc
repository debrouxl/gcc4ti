/* main_vbs.inc: Display of statistics for ld-tigcc

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

// This file is included by main.c
{
	if (DisplayStats)
	{
		printf ("Target Calculators:\n");
		if (Program.Calcs & CALC_TI92)
			printf ("  TI-92\n");
		if (Program.Calcs & CALC_TI89)
			printf ("  TI-89\n");
		if (Program.Calcs & CALC_TI92PLUS)
			printf ("  TI-92 Plus\n");
		if (Program.Calcs & CALC_V200)
			printf ("  V200\n");
		printf ("Program Statistics:\n"
				"  Program Variable Name:                    %s\\%s\n"
				"  Program Variable Size:                    %ld Bytes\n",
				ProgramFolder, ProgramName, (long) OptInfo->ProgramSize);
		if (OptInfo->DataSize > 0)
		{
			printf ("  Data Variable Size:                       %ld Bytes\n",
					(long) OptInfo->DataSize);
		}
		if (OptInfo->BSSSize > 0)
		{
			printf ("  BSS Size:                                 %ld Bytes\n",
					(long) OptInfo->BSSSize);
		}
		printf ("  Absolute Relocs:                          %ld\n"
				"  Natively Emitted Relocs:                  %ld\n",
				(long) OptInfo->RelocCount, (long) OptInfo->NativeRelocCount);
		if (OptInfo->OptimizeBranchesResult > 0)
			printf (OptInfo->OptimizeBranches ? "  Relocs Removed by Branch Optimization:    %ld\n" : "  Relocs Removable by Branch Optimization:  %ld\n", (long) OptInfo->OptimizeBranchesResult);
		if (OptInfo->OptimizeMovesResult > 0)
			printf (OptInfo->OptimizeMoves ? "  Relocs Removed by Move Optimization:      %ld\n" : "  Relocs Removable by Move Optimization:    %ld\n", (long) OptInfo->OptimizeMovesResult);
		if (OptInfo->OptimizeTestsResult > 0)
			printf (OptInfo->OptimizeTests ? "  Relocs Removed by Test Optimization:      %ld\n" : "  Relocs Removable by Test Optimization:    %ld\n", (long) OptInfo->OptimizeTestsResult);
		if (OptInfo->OptimizeCalcsResult > 0)
			printf (OptInfo->OptimizeCalcs ? "  Relocs Removed by Calc Optimization:      %ld\n" : "  Relocs Removable by Calc Optimization:    %ld\n", (long) OptInfo->OptimizeCalcsResult);
		if (OptInfo->UseFLineJumpsResult > 0)
			printf (OptInfo->UseFLineJumps || OptInfo->Use4ByteFLineJumps ? "  Relocs Removed by F-Line Jumps:           %ld\n" : "  Relocs Removable by F-Line Jumps:         %ld\n", (long) OptInfo->UseFLineJumpsResult);
		if (OptInfo->CutRangesResult > 0)
			printf (OptInfo->CutRanges ? "  Space Saved by Range-Cutting:             %ld Bytes\n" : "  Estimated Space Savable by Range-Cutting: %ld Bytes\n", (long) OptInfo->CutRangesResult);
		if (OptInfo->NearAssemblyResult > 0)
			printf ("  Space Savable by Using GAS `-l' Switch:   %ld Bytes\n", (long) OptInfo->NearAssemblyResult);
	}
}
