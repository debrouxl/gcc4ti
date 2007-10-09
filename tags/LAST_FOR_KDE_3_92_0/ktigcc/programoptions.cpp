/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2007 Kevin Kofler
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

#include "programoptions.h"

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <QApplication>
#include <QEvent>
#include <QEventLoop>
#include <QMouseEvent>
#include <QAssistantClient>
#include <QButtonGroup>
#include <kpushbutton.h> 
#include "ktigcc.h"
#include "tpr.h"

ProgramOptions::ProgramOptions(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  buttonClose->setGuiItem(KStandardGuiItem::Close);
  // Create button groups
  QButtonGroup *group;
  //Tab: Reloc Format
  group=new QButtonGroup(this);
  group->addButton(RelocAMS);
  group->addButton(RelocKernel);
  group->addButton(RelocCompressed);
  group->addButton(RelocMlink);
  group=new QButtonGroup(this);
  group->addButton(ROMCallDirect);
  group->addButton(ROMCallKernel);
  group->addButton(ROMCallCompressed);
  group->addButton(ROMCallMlink);
  group->addButton(ROMCallFLine);
  //Tab: BSS/Data Format
  group=new QButtonGroup(this);
  group->addButton(BSSMerge);
  group->addButton(BSSKernel);
  group->addButton(BSSCompressed);
  group->addButton(BSSMlink);
  group=new QButtonGroup(this);
  group->addButton(DataVarKernel);
  group->addButton(DataVarCompressed);
  group->addButton(DataVarMlink);
}

ProgramOptions::~ProgramOptions()
{
}

void ProgramOptions::ImportSettings()
{
  //Tab: Calculator
  TI89->setChecked(libopts.use_ti89);
  TI92Plus->setChecked(libopts.use_ti92p);
  V200->setChecked(libopts.use_v200);
  OptimizeCalcConsts->setChecked(libopts.opt_calc_consts);
  //Tab: Operating System
  if (libopts.use_preos)
    PreOS->setChecked(TRUE);
  else if (libopts.use_kernel)
    UseKernel->setChecked(TRUE);
  else
    Nostub->setChecked(TRUE);
  CMinimumAMSVersion->setChecked(libopts.use_minams);
  MinimumAMSVersion->setText(QString("%1.%2%3").arg(libopts.minams/100).arg((libopts.minams/10)%10).arg(libopts.minams%10));
  MinimumAMSVersion->setEnabled(libopts.use_minams);
  UnofficialOSSupport->setChecked(libopts.unofficial_os);
  //Tab: Reloc Format
  if (libopts.reloc_format==RT_KERNEL)
    RelocKernel->setChecked(TRUE);
  else if (libopts.reloc_format==RT_COMPRESSED)
    RelocCompressed->setChecked(TRUE);
  else if (libopts.reloc_format==RT_MLINK)
    RelocMlink->setChecked(TRUE);
  else
    RelocAMS->setChecked(TRUE);
  UseFLineJumps->setChecked(libopts.use_fline_jumps);
  Use4ByteFLineJumps->setChecked(libopts.use_4b_fline_jumps);
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
  OptimizeROMCalls->setChecked(libopts.opt_rom_calls);
  UseInternalFLineEmulator->setChecked(libopts.use_internal_fline_emu);
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
    DataVarCompressed->setChecked(TRUE);
  else if (libopts.data_ref_format==RT_MLINK)
    DataVarMlink->setChecked(TRUE);
  else
    DataVarKernel->setChecked(TRUE);
  //Tab: Home Screen
  if (libopts.use_return_value)
    HomeCustomValue->setChecked(TRUE);
  else
    HomeDone->setChecked(TRUE);
  EnableReturningErrors->setChecked(libopts.enable_error_return);
  SaveScreen->setChecked(libopts.save_screen);
}

void ProgramOptions::ExportSettings()
{
  //Tab: Calculator
  libopts.use_ti89=TI89->isChecked();
  libopts.use_ti92p=TI92Plus->isChecked();
  libopts.use_v200=V200->isChecked();
  libopts.opt_calc_consts=OptimizeCalcConsts->isChecked();
  //Tab: Operating System
  libopts.use_preos=PreOS->isChecked();
  libopts.use_kernel=UseKernel->isChecked();
  libopts.use_minams=CMinimumAMSVersion->isChecked();
  QString minams=MinimumAMSVersion->text();
  libopts.minams=minams.section('.',0,0).toUInt()*100+minams.section('.',1,1).toUInt();
  libopts.unofficial_os=UnofficialOSSupport->isChecked();
  //Tab: Reloc Format
  if (RelocKernel->isChecked())
    libopts.reloc_format=RT_KERNEL;
  else if (RelocCompressed->isChecked())
    libopts.reloc_format=RT_COMPRESSED;
  else if (RelocMlink->isChecked())
    libopts.reloc_format=RT_MLINK;
  else
    libopts.reloc_format=RT_AMS;
  libopts.use_fline_jumps=UseFLineJumps->isChecked();
  libopts.use_4b_fline_jumps=Use4ByteFLineJumps->isChecked();
  if (ROMCallKernel->isChecked())
    libopts.rom_call_format=RT_KERNEL;
  else if (ROMCallCompressed->isChecked())
    libopts.rom_call_format=RT_COMPRESSED;
  else if (ROMCallMlink->isChecked())
    libopts.rom_call_format=RT_MLINK;
  else if (ROMCallFLine->isChecked())
    libopts.rom_call_format=RT_FLINE;
  else
    libopts.rom_call_format=RT_DIRECT;
  libopts.opt_rom_calls=OptimizeROMCalls->isChecked();
  libopts.use_internal_fline_emu=UseInternalFLineEmulator->isChecked();
  //Tab: BSS/Data Format
  if (BSSKernel->isChecked())
    libopts.bss_ref_format=RT_KERNEL;
  else if (BSSCompressed->isChecked())
    libopts.bss_ref_format=RT_COMPRESSED;
  else if (BSSMlink->isChecked())
    libopts.bss_ref_format=RT_MLINK;
  else
    libopts.bss_ref_format=RT_NONE;
  if (DataVarCompressed->isChecked())
    libopts.data_ref_format=RT_COMPRESSED;
  else if (DataVarMlink->isChecked())
    libopts.data_ref_format=RT_MLINK;
  else
    libopts.data_ref_format=RT_KERNEL;
  //Tab: Home Screen
  libopts.use_return_value=HomeCustomValue->isChecked();
  libopts.enable_error_return=EnableReturningErrors->isChecked();
  libopts.save_screen=SaveScreen->isChecked();
}

void ProgramOptions::CMinimumAMSVersion_toggled(bool on)
{
  MinimumAMSVersion->setEnabled(on);
}

void ProgramOptions::CalcCheckbox_toggled(bool on __attribute__((unused)))
{
  static bool inEvent=FALSE;
  if (!inEvent) {
    inEvent=TRUE;
    unsigned numCalcs=TI89->isChecked()+TI92Plus->isChecked()+V200->isChecked();
    // OPTIMIZE_CALC_CONSTS makes no sense for only 1 calculator (calculator
    // constants are already optimized for that calculator at compile-time in
    // that case, no need to do it at link time).
    // Moreover, we can't enable it for old projects (no calculator checked,
    // calculators selected through int _ti89 and such in the code).
    OptimizeCalcConsts->setEnabled(numCalcs>=2);
    if (numCalcs<2) OptimizeCalcConsts->setChecked(numCalcs);
    inEvent=FALSE;
  }
}

void ProgramOptions::KernelRadiobutton_toggled(bool on __attribute__((unused)))
{
  static bool inEvent=FALSE;
  if (!inEvent) {
    inEvent=TRUE;
    bool nostub=Nostub->isChecked();
    bool kernel=UseKernel->isChecked();
    bool preos=PreOS->isChecked();
    // SAVE_SCREEN is currently always on in kernel mode.
    SaveScreen->setEnabled(nostub);
    if (!nostub) SaveScreen->setChecked(TRUE);
    if (kernel) {
      // Kernel mode, enable kernel reloc types.
      RelocKernel->setEnabled(TRUE);
      ROMCallKernel->setEnabled(TRUE);
      BSSKernel->setEnabled(TRUE);
      // Default to them if an unsupported reloc type was selected.
      if (RelocAMS->isChecked() || RelocCompressed->isChecked()
          || RelocMlink->isChecked())
        RelocKernel->setChecked(TRUE);
      if (ROMCallDirect->isChecked() || ROMCallCompressed->isChecked()
          || ROMCallMlink->isChecked())
        ROMCallKernel->setChecked(TRUE);
      if (BSSCompressed->isChecked() || BSSMlink->isChecked())
        BSSKernel->setChecked(TRUE);
      // Now disable the unsupported types.
      RelocAMS->setEnabled(FALSE);
      RelocCompressed->setEnabled(FALSE);
      RelocMlink->setEnabled(FALSE);
      ROMCallDirect->setEnabled(FALSE);
      ROMCallCompressed->setEnabled(FALSE);
      ROMCallMlink->setEnabled(FALSE);
      BSSCompressed->setEnabled(FALSE);
      BSSMlink->setEnabled(FALSE);
    } else if (preos) {
      // Kernel with compressed reloc tables mode, enable compressed reloc types.
      RelocCompressed->setEnabled(TRUE);
      ROMCallCompressed->setEnabled(TRUE);
      BSSCompressed->setEnabled(TRUE);
      // Default to them if an unsupported reloc type was selected.
      if (RelocAMS->isChecked() || RelocKernel->isChecked()
          || RelocMlink->isChecked())
        RelocCompressed->setChecked(TRUE);
      if (ROMCallDirect->isChecked() || ROMCallKernel->isChecked()
          || ROMCallMlink->isChecked())
        ROMCallCompressed->setChecked(TRUE);
      if (BSSKernel->isChecked() || BSSMlink->isChecked())
        BSSCompressed->setChecked(TRUE);
      // Now disable the unsupported types.
      RelocAMS->setEnabled(FALSE);
      RelocKernel->setEnabled(FALSE);
      RelocMlink->setEnabled(FALSE);
      ROMCallDirect->setEnabled(FALSE);
      ROMCallKernel->setEnabled(FALSE);
      ROMCallMlink->setEnabled(FALSE);
      BSSKernel->setEnabled(FALSE);
      BSSMlink->setEnabled(FALSE);
    } else {
      // In _nostub mode, all reloc types are allowed.
      RelocAMS->setEnabled(TRUE);
      RelocKernel->setEnabled(TRUE);
      RelocCompressed->setEnabled(TRUE);
      RelocMlink->setEnabled(TRUE);
      ROMCallDirect->setEnabled(TRUE);
      ROMCallKernel->setEnabled(TRUE);
      ROMCallCompressed->setEnabled(TRUE);
      ROMCallMlink->setEnabled(TRUE);
      BSSKernel->setEnabled(TRUE);
      BSSCompressed->setEnabled(TRUE);
      BSSMlink->setEnabled(TRUE);
    }
    RelocSettings_toggled(0);
    inEvent=FALSE;
  }
}

void ProgramOptions::RelocSettings_toggled(bool on __attribute__((unused)))
{
  static bool inEvent=FALSE;
  if (!inEvent) {
    inEvent=TRUE;
    // 4-byte F-Line jumps only make sense if F-Line jumps are enabled
    if (UseFLineJumps->isChecked())
      Use4ByteFLineJumps->setEnabled(TRUE);
    else {
      Use4ByteFLineJumps->setEnabled(FALSE);
      Use4ByteFLineJumps->setChecked(FALSE);
    }
    if (Use4ByteFLineJumps->isChecked()) {
      // 4-byte F-Line jumps require the internal emulator
      UseInternalFLineEmulator->setEnabled(FALSE);
      UseInternalFLineEmulator->setChecked(TRUE);
    } else if (UseFLineJumps->isChecked() || ROMCallFLine->isChecked()) {
      // using F-Line instructions where the internal emulator can be used
      UseInternalFLineEmulator->setEnabled(TRUE);
    } else {
      // no F-Line instructions to emulate, internally or otherwise
      UseInternalFLineEmulator->setEnabled(FALSE);
      UseInternalFLineEmulator->setChecked(FALSE);
    }
    // OPTIMIZE_ROM_CALLS only allowed with direct or F-Line ROM_CALLs in _nostub mode
    if (Nostub->isChecked() && (ROMCallDirect->isChecked() || ROMCallFLine->isChecked()))
      OptimizeROMCalls->setEnabled(TRUE);
    else {
      OptimizeROMCalls->setEnabled(FALSE);
      OptimizeROMCalls->setChecked(FALSE);
    }
    inEvent=FALSE;
  }
}

void ProgramOptions::mousePressEvent( QMouseEvent * e )
{
  if (e->button()==Qt::RightButton) {
    QWidget *widgetUnderCursor=childAt(e->pos());
    if (!widgetUnderCursor) return;
    QString toolTip=widgetUnderCursor->toolTip();
    if (toolTip.isEmpty()) return;
    QString docFile=lookup_doc_keyword(toolTip);
    if (docFile.isEmpty()) return;
    force_qt_assistant_page(1);
    // wait for Qt Assistant to actually open
    while (!assistant->isOpen())
      QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,1000);
    assistant->showPage(QString(tigcc_base)+QString("/doc/html/")+docFile);
  }
}


void ProgramOptions::languageChange()
{
  retranslateUi(this);
}

