/* Disassemble support for GDB.

   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

   Copyright 2007 Kevin Kofler

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include <stdint.h>
#include <limits.h>
#include <stdarg.h>

#include "sysdep.h"
#include "disasm.h"
#include "dis-asm.h"
#include "romcalls.h"
#include "main.h"

/* Disassemble functions.
   FIXME: We should get rid of all the duplicate code in gdb that does
   the same thing: disassemble_command() and the gdbtk variation. */

/* (TiEmu 20050429 Kevin Kofler) */
static int
sprintf_disasm (void *stream, const char *format, ...)
{
  int result;
  va_list args;
  va_start (args, format);
  result = vsprintf ((char *)stream + strlen((char *)stream), format, args);
  va_end (args);
  return result;
}

static void
strcat_disasm (const char *format, void *stream)
{
  strcat ((char *)stream, format);
}

static void
print_address_1 (int32_t reladdr, uint32_t absaddr, int flags, void *stream,
                 void (*fprintf_f) (void *, const char *, ...),
                 void (*fputs_f) (const char *, void *))
{
  int rcid;
  const char *rcname;
  PrintAddress (reladdr, absaddr, flags, stream, fprintf_f, fputs_f);
  if (((rcid = romcalls_is_addr (absaddr)) != -1)
      && ((rcname = romcalls_get_name (rcid)) != NULL))
    fprintf_f (stream, " /* tios::%s */", rcname);
}

static void
dis_asm_sprint_address (bfd_vma addr, struct disassemble_info *info)
{
  print_address_1 (addr, info->target, info->flags, info->stream,
                   (void (*) (void *, const char *, ...)) sprintf_disasm, strcat_disasm);
}

struct disassemble_info
gdb_disassemble_info (unsigned char *mem_buf, char *output_buf)
{
  struct disassemble_info di;
  init_disassemble_info (&di, output_buf, sprintf_disasm);
  di.buffer = mem_buf;
  di.buffer_length = UINT_MAX;
  di.print_address_func = dis_asm_sprint_address;
  di.flavour = bfd_target_unknown_flavour;
  di.arch = bfd_arch_m68k;
  di.mach = bfd_mach_m68000;
  di.endian = BFD_ENDIAN_BIG;
  disassemble_init_for_target (&di);
  return di;
}

uint32_t Dasm68000(unsigned char *mem_buf, char *output_buf, uint32_t addr)
{
  struct disassemble_info di = gdb_disassemble_info(mem_buf, output_buf);
  uint32_t offset;

  *output_buf = 0;
  di.buffer_vma = addr;
  offset = print_insn_m68k(addr, &di);
  return offset;
}
