/* Hey EMACS -*- linux-c -*- */
/* $Id: er_codes.c 2268 2006-11-06 17:18:51Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2005, Kevin Kofler
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*
    ER_throw support
*/

#include <stdio.h>
#include <stdlib.h>

/* This table is converted and hand-edited from the TIGCC error.h. */
static struct {unsigned int id; const char *name;} erthrow_table[] = {
  {0, "ER_OK"},
  {1, "ER_EXIT"},
  {2, "ER_STOP"},
  {3, "ER_OFF"},
  {4, "ER_PRGM_STOP"},
  {9, "ER_NO_MSG"},
  {10, "ER_FUNC_DID_NOT_RETURN_VALUE"},
  {20, "ER_TEST_NOT_TRUE_OR_FALSE"},
  {30, "ER_ARG_CANNOT_BE_FOLDER"},
  {40, "ER_ARGUMENT"},
  {50, "ER_ARG_MISMATCH"},
  {60, "ER_EXPECTED_BOOL_OR_AGG"},
  {70, "ER_ARG_MUST_BE_DECIMAL"},
  {80, "ER_ARG_MUST_BE_LABEL"},
  {90, "ER_ARGUMENT_MUST_BE_LIST"},
  {100, "ER_ARG_MUST_BE_MATRIX"},
  {110, "ER_ARG_MUST_BE_PIC"},
  {120, "ER_ARG_MUST_BE_PIC_OR_STR"},
  {130, "ER_ARG_MUST_BE_STRING"},
  {140, "ER_EXPECTED_VAR"},
  {150, "ER_ARG_MUST_BE_EMPTY_FOLDER"},
  {160, "ER_EXPECTED_ALGEBRAIC"},
  {161, "ER_ASAP_TOO_LONG"},
  {163, "ER_ATTRIBUTE_NOT_FOUND"},
  {165, "ER_BATT_LOW"},
  {170, "ER_BOUND"},
  {180, "ER_BREAK"},
  {185, "ER_CHECKSUM"},
  {190, "ER_CIRCULAR_DEFINITION"},
  {200, "ER_INVALID_SUCH_THAT"},
  {210, "ER_DATATYPE"},
  {220, "ER_DEPENDENT_LIMIT"},
  {225, "ER_DIFF_EQ_SETUP"},
  {230, "ER_DIMENSION"},
  {240, "ER_NON_CONFORMING_LISTS"},
  {250, "ER_DIVBY0"},
  {260, "ER_DOMAIN"},
  {270, "ER_DUPLICATE_VAR_NAME"},
  {280, "ER_ELSE_WITHOUT_IF"},
  {290, "ER_ENDTRY_WITHOUT_ELSE"},
  {295, "ER_EXCESSIVE_ITERATION"},
  {300, "ER_EXPECTED_2OR3_ELEMENTS"},
  {305, "ER_EXPIRED"},
  {307, "ER_APP_EXT_NOT_FOUND"},
  {308, "ER_APP_NOT_FOUND"},
  {310, "ER_INVALID_NSOLVE_ARG1"},
  {320, "ER_INVALID_SOLVE_ARG1"},
  {330, "ER_FOLDER"},
  {335, "ER_FUNCS_IN_DIFF_EQ"},
  {345, "ER_INCONSISTENT_UNITS"},
  {350, "ER_INVALID_SUBSCRIPT"},
  {360, "ER_INVALID_INDIR_STRING"},
  {380, "ER_INVALID_ANS"},
  {390, "ER_ILLEGAL_ASSIGNMENT"},
  {400, "ER_ILLEGAL_ASSIGNMENT_VALUE"},
  {405, "ER_INVALID_AXES"},
  {410, "ER_ILLEGAL_COMMAND"},
  {420, "ER_INVALID_FOLDER_NAME"},
  {430, "ER_GRAPH_MODE"},
  {435, "ER_INVALID_GUESS"},
  {440, "ER_INVALID_IMPLIED_MULT"},
  {450, "ER_ILLEGAL_IN_FUNC"},
  {460, "ER_ILLEGAL_IN_CUSTOM"},
  {470, "ER_ILLEGAL_IN_DIALOG"},
  {480, "ER_ILLEGAL_IN_TOOLBAR"},
  {490, "ER_CANNOT_EXIT_FROM_TRY"},
  {500, "ER_INVALID_LABEL"},
  {510, "ER_INVALID_LIST_OR_MATRIX"},
  {520, "ER_INVAL_OUTSIDE_TB_CM"},
  {530, "ER_INVAL_OUTSIDE_DG_TB_CM"},
  {540, "ER_INVALID_OUTSIDE_DIALOG"},
  {550, "ER_MUST_BE_IN_PRGM_OR_FUNC"},
  {560, "ER_EXIT_NOT_IN_LOOP"},
  {570, "ER_INVALID_PATHNAME"},
  {575, "ER_INVALID_POLAR_COMPLEX"},
  {580, "ER_ILLEGAL_PRGM_REF"},
  {590, "ER_INVALID_SYNTAX_BLOCK"},
  {600, "ER_INVALID_TABLE"},
  {605, "ER_INVALID_USE_OF_UNITS"},
  {610, "ER_INVALID_LOCAL_DECLARATION"},
  {620, "ER_EXPECTED_VAR_OR_FUNC"},
  {630, "ER_INVALID_VAR_REF"},
  {640, "ER_INVALID_VECTOR_SYNTAX"},
  {650, "ER_LINK_IO"},
  {665, "ER_MAT_NOT_DIAGONALIZABLE"},
  {670, "ER_MEMORY"},
  {673, "ER_STACK_VIO"},
  {680, "ER_EXPECTED_LPAR"},
  {690, "ER_EXPECTED_RPAR"},
  {700, "ER_EXPECTED_DOUBLE_QUOTE"},
  {710, "ER_EXPECTED_RIGHT_BRACKET"},
  {720, "ER_EXPECTED_RIGHT_BRACE"},
  {730, "ER_INVALID_BLOCK_STRUCTURE"},
  {740, "ER_MISSING_THEN"},
  {750, "ER_NOT_FUNC_OR_PRGM"},
  {765, "ER_NO_FUNCS_SEL"},
  {780, "ER_NO_SOLUTION"},
  {790, "ER_NON_ALGEBRAIC_VARIABLE"},
  {800, "ER_UNREAL_RESULT"},
  {810, "ER_MEMORY_DML"},
  {830, "ER_OVERFLOW"},
  {840, "ER_STAT_PLOT"},
  {850, "ER_PRGM_NOT_FOUND"},
  {860, "ER_RECURSION_TOO_DEEP"},
  {870, "ER_RESERVED"},
  {875, "ER_ROM_ROUTINE_NOT_AVAILABLE"},
  {880, "ER_SEQUENCE_SETUP"},
  {885, "ER_SIGNATURE_ERR"},
  {890, "ER_SINGULARMAT"},
  {895, "ER_SLOPE_FIELD_FUNCS"},
  {900, "ER_EMPTY_GROUP_NOT_VALID"},
  {910, "ER_SYNTAX"},
  {930, "ER_TOO_FEW_ARGS"},
  {940, "ER_TOO_MANY_ARGS"},
  {950, "ER_TOO_MANY_SUBSCRIPTS"},
  {955, "ER_TOO_MANY_UNDEFINED"},
  {960, "ER_UNDEFINED_VAR"},
  {965, "ER_UNLICENSED"},
  {970, "ER_VAR_IN_USE"},
  {980, "ER_PROTECTED"},
  {990, "ER_NAME_TOO_LONG"},
  {1000, "ER_RANGE"},
  {1010, "ER_ZOOM"},
  {1020, "ER_ILLEGAL_TAG"},
  {1030, "ER_MEM_VIO"},
  {2048, NULL}
};

const char* ercodes_get_name(unsigned id)
{
	if(id >= 2048)
		return "Unknown ERROR code";
	else
	{
		unsigned n = sizeof(erthrow_table)/sizeof(*erthrow_table), lb = 0, ub = n-1;
		while (lb < ub - 1)	
		{
			unsigned mid = (lb + ub) >> 1;
			if (erthrow_table[mid].id <= id)
				lb = mid;
			else
				ub = mid;
		}
		if (id != erthrow_table[lb].id)
		{
			static char buffer[100];
			sprintf(buffer, "%s + %u", erthrow_table[lb].name,
			        id - erthrow_table[lb].id);
			return buffer;
		}
		else
			return erthrow_table[lb].name;
	}
}

