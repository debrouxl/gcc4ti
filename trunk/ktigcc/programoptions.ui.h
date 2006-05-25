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
    TI89->setChecked(TRUE);
  if (libopts.use_ti92p)
    TI92Plus->setChecked(TRUE);
  if (libopts.use_v200)
    V200->setChecked(TRUE);
  if (libopts.opt_calc_consts)
    OptimizeCalcConsts->setChecked(TRUE);
  //Tab: Operating System
  if (libopts.use_preos)
    PreOS->setChecked(TRUE);
  else if (libopts.use_kernel)
    RecentDoorsCompatibleKernels->setChecked(TRUE);
  else
    AnyNoKernel->setChecked(TRUE);
  if (libopts.use_minams)
    CMinimumAMSVersion->setChecked(TRUE);
  MinimumAMSVersion->setText(QString("%1.%2%3").arg(libopts.minams/100).arg((libopts.minams/10)%10).arg(libopts.minams%10));
  if (libopts.unofficial_os)
    UnofficialOSSupport->setChecked(TRUE);
  //Tab: Reloc Format
  if (libopts.reloc_format==RT_KERNEL)
    RelocKernel->setChecked(TRUE);
  else if (libopts.reloc_format==RT_COMPRESSED)
    RelocCompressed->setChecked(TRUE);
  else if (libopts.reloc_format==RT_MLINK)
    RelocMlink->setChecked(TRUE);
  else
    RelocAMS->setChecked(TRUE);
  if (libopts.use_fline_jumps)
    UseFLineJumps->setChecked(TRUE);
  if (libopts.use_4b_fline_jumps)
    Use4ByteFLineJumps->setChecked(TRUE);
  if (libopts.rom_call_format==RT_KERNEL)
    ROMCallKernel->setChecked(TRUE);
  else if (libopts.rom_call_format==RT_COMPRESSED)
    ROMCallCompressed->setChecked(TRUE);
  else if (libopts.rom_call_format==RT_MLINK)
    ROMCallMlink->setChecked(TRUE);
  else if (libopts.rom_call_format==RT_FLINE)
    ROMCallFLine->setChecked(TRUE);
  else
    ROMCallDirect->setChecked(TRUE);
  if (libopts.opt_rom_calls)
    OptimizeROMCalls->setChecked(TRUE);
  if (libopts.use_internal_fline_emu)
    UseInternalFLineEmulator->setChecked(TRUE);
  //Tab: BSS/Data Format
  if (libopts.bss_ref_format==RT_KERNEL)
    BSSKernel->setChecked(TRUE);
  else if (libopts.bss_ref_format==RT_COMPRESSED)
    BSSCompressed->setChecked(TRUE);
  else if (libopts.bss_ref_format==RT_MLINK)
    BSSMlink->setChecked(TRUE);
  else
    BSSMerge->setChecked(TRUE);
  if (libopts.data_ref_format==RT_COMPRESSED)
    BSSDataCompressed->setChecked(TRUE);
  else if (libopts.data_ref_format==RT_MLINK)
    BSSDataMlink->setChecked(TRUE);
  else
    BSSDataKernel->setChecked(TRUE);
  //Tab: Home Screen
  if (libopts.use_return_value)
    HomeCustomValue->setChecked(TRUE);
  else
    HomeDone->setChecked(TRUE);
  if (libopts.enable_error_return)
    EnableReturningErrors->setChecked(TRUE);
  if (libopts.save_screen)
    SaveScreen->setChecked(TRUE);
}
