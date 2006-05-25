/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
** You should not define a constructor or destructor in this file.
** Instead, write your code in functions called init() and destroy().
** These will automatically be called by the form's constructor and
** destructor.
*****************************************************************************/

/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2006 Kevin Kofler
   Copyright (C) 2006 Joey Adams

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#include <kurl.h>
#include "tpr.h" //tpr.h relies on kurl.h

extern tprLibOpts libopts;

void ProgramOptions::ImportSettings(void)
{
  //Tab: Calculator
  if (libopts.use_ti89)
    TI89->toggle();
  if (libopts.use_ti92p)
    TI92Plus->toggle();
  if (libopts.use_v200)
    V200->toggle();
  if (libopts.opt_calc_consts)
    OptimizeCalcConsts->toggle();
  //Tab: Operating System
  if (libopts.use_preos)
    PreOS->toggle();
  else if (libopts.use_kernel)
    RecentDoorsCompatibleKernels->toggle();
  else
    AnyNoKernel->toggle();
  if (libopts.use_minams)
    CMinimumAMSVersion->toggle();
  MinimumAMSVersion->setText(QString("%1.%2").arg(libopts.minams/100).arg(libopts.minams%100));
  if (libopts.unofficial_os)
    UnofficialOSSupport->toggle();
  //Tab: Reloc Format
  if (libopts.reloc_format==RT_KERNEL)
    RelocKernel->toggle();
  else if (libopts.reloc_format==RT_COMPRESSED)
    RelocCompressed->toggle();
  else if (libopts.reloc_format==RT_MLINK)
    RelocMlink->toggle();
  else
    RelocAMS->toggle();
  if (libopts.use_fline_jumps)
    UseFLineJumps->toggle();
  if (libopts.use_4b_fline_jumps)
    Use4ByteFLineJumps->toggle();
  if (libopts.rom_call_format==RT_KERNEL)
    ROMCallKernel->toggle();
  else if (libopts.rom_call_format==RT_COMPRESSED)
    ROMCallCompressed->toggle();
  else if (libopts.rom_call_format==RT_MLINK)
    ROMCallMlink->toggle();
  else if (libopts.rom_call_format==RT_FLINE)
    ROMCallFLine->toggle();
  else
    ROMCallDirect->toggle();
  if (libopts.opt_rom_calls)
    OptimizeROMCalls->toggle();
  if (libopts.use_internal_fline_emu)
    UseInternalFLineEmulator->toggle();
  //Tab: BSS/Data Format
  if (libopts.bss_ref_format==RT_KERNEL)
    BSSKernel->toggle();
  else if (libopts.bss_ref_format==RT_COMPRESSED)
    BSSCompressed->toggle();
  else if (libopts.bss_ref_format==RT_MLINK)
    BSSMlink->toggle();
  else
    BSSMerge->toggle();
  if (libopts.data_ref_format==RT_COMPRESSED)
    BSSDataCompressed->toggle();
  else if (libopts.data_ref_format==RT_MLINK)
    BSSDataMlink->toggle();
  else
    BSSDataKernel->toggle();
  //Tab: Home Screen
  if (libopts.use_return_value)
    HomeCustomValue->toggle();
  else
    HomeDone->toggle();
  if (libopts.enable_error_return)
    EnableReturningErrors->toggle();
  if (libopts.save_screen)
    SaveScreen->toggle();
}