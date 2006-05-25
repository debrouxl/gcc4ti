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

#include <qaccel.h>
#include <kmessagebox.h>
#include <kurl.h>
#include "tpr.h"
#include "programoptions.h"

extern tprSettings settings;

ProgramOptions *programoptions;
//the program options subdialog is created at initialization of a ProjectOptions and remains existant until the ProjectOptions is destroyed.

void ProjectOptions::init()
{
  //Toggle controls to match settings.
  ImportSettings();
  //Create the Program Options dialog and toggle its controls
  programoptions=new ProgramOptions(this);
  programoptions->ImportSettings();
  //Update stuff
  CheckOncalcNames();
  UpdateVisibilities();
}

void ProjectOptions::destroy()
{
  delete(programoptions);
  if (result()!=QDialog::Accepted)
    return;
  //Save settings
  ExportSettings();
}

void ProjectOptions::ImportSettings(void)
{
  Boolean isregular;
  //Tab: General
  OncalcVariableName_1->setText(settings.data_var);
  if (settings.copy_data_var)
  {
    if (settings.copy_data_var_arc)
      CreateCopyIfArchived->setChecked(TRUE);
    else
      CreateCopyAlways->setChecked(TRUE);
  }
  OncalcVariableName_2->setText(settings.pack_name);
  isregular=TRUE;
  if (settings.fargo)
    FargoProgram->setChecked(TRUE),isregular=FALSE;
  if (settings.flash_os)
    FlashOperatingSystem->setChecked(TRUE),isregular=FALSE;
  if (settings.archive)
    FunctionArchive->setChecked(TRUE),isregular=FALSE;
  if (isregular)
  {
    if (settings.use_data_var)
    {
      ExternalDataVariable->setChecked(TRUE);
      LOncalcVariableName_1->setEnabled(TRUE);
      OncalcVariableName_1->setEnabled(TRUE);
      LCreateCopyNever->setEnabled(TRUE);
      CreateCopyNever->setEnabled(TRUE);
      CreateCopyIfArchived->setEnabled(TRUE);
      CreateCopyAlways->setEnabled(TRUE);
    }
    if (settings.pack)
    {
      CompressProgram->setChecked(TRUE);
      LOncalcVariableName_2->setEnabled(TRUE);
      OncalcVariableName_2->setEnabled(TRUE);
    }
  }
  else
  {
    ExternalDataVariable->setEnabled(FALSE);
    CompressProgram->setEnabled(FALSE);
  }
  //Tab: Compilation
  GCCSwitches->setText(settings.cc_switches);
  AsSwitches->setText(settings.as_switches);
  A68kSwitches->setText(settings.a68k_switches);
  if (settings.debug_info)
    GenerateDebugInformation->setChecked(TRUE);
  //Tab: Linking
  if (settings.optimize_nops)
    NOPs->setChecked(TRUE);
  if (settings.optimize_returns)
    ReturnSequences->setChecked(TRUE);
  if (settings.optimize_branches)
    Branches->setChecked(TRUE);
  if (settings.optimize_moves)
    MoveLoadPushInstructions->setChecked(TRUE);
  if (settings.optimize_tests)
    TestCompareInstructions->setChecked(TRUE);
  if (settings.optimize_calcs)
    CalculationInstructions->setChecked(TRUE);
  if (settings.remove_unused)
    RemoveUnusedSections->setChecked(TRUE);
  if (settings.reorder_sections)
    ReorderSections->setChecked(TRUE);
  if (settings.cut_ranges)
    CutUnusedRanges->setChecked(TRUE);
  if (settings.merge_constants)
    MergeConstants->setChecked(TRUE);
  if (settings.std_lib)
    LinkAgainstStandardLibrary->setChecked(TRUE);
  if (settings.initialize_bss)
    InitializeBSSSection->setChecked(TRUE);
  if (settings.outputbin)
    OutputVariableImageWithoutWrapper->setChecked(TRUE);
  //Tab: Post-Build
  CallAfterBuilding->setText(settings.post_build);
  Parameters->setText(settings.cmd_line);
}

#define ExportCheckbox(checkwidget,settingvariable) if (checkwidget->isOn()) \
settings.settingvariable=TRUE; \
else \
settings.settingvariable=FALSE;

void ProjectOptions::ExportSettings(void)
{
  //Tab: General
  settings.fargo=FALSE;
  settings.flash_os=FALSE;
  settings.archive=FALSE;
  settings.use_data_var=FALSE;
  settings.pack=FALSE;
  if (FargoProgram->isOn())
    settings.fargo=TRUE;
  else if (FlashOperatingSystem->isOn())
    settings.flash_os=TRUE;
  else if (FunctionArchive->isOn())
    settings.archive=TRUE;
  else if (RegularProgram->isOn()) //the original TIGCC IDE automatically unchecked External data variable and Compress program when you toggled away from Regular Program, so we will only consider these two if Regular Program is toggled
  {
    if (ExternalDataVariable->isOn())
      settings.use_data_var=TRUE;
    if (CompressProgram->isOn())
      settings.pack=TRUE;
  }
  //Subselections for External data variable and Compress program are always preserved.
  settings.data_var=OncalcVariableName_1->text();
  settings.copy_data_var_arc=FALSE;
  if (CreateCopyNever->isOn())
    settings.copy_data_var=FALSE;
  else
  {
    settings.copy_data_var=TRUE;
    if (CreateCopyIfArchived->isOn())
      settings.copy_data_var_arc=TRUE;
  }
  settings.pack_name=OncalcVariableName_2->text();
  //Tab: Compilation
  settings.cc_switches=GCCSwitches->text();
  settings.as_switches=AsSwitches->text();
  settings.a68k_switches=A68kSwitches->text();
  ExportCheckbox(GenerateDebugInformation,debug_info);
  //Tab: Linking
  ExportCheckbox(NOPs,optimize_nops);
  ExportCheckbox(ReturnSequences,optimize_returns);
  ExportCheckbox(Branches,optimize_branches);
  ExportCheckbox(MoveLoadPushInstructions,optimize_moves);
  ExportCheckbox(TestCompareInstructions,optimize_tests);
  ExportCheckbox(CalculationInstructions,optimize_calcs);
  ExportCheckbox(RemoveUnusedSections,remove_unused);
  ExportCheckbox(ReorderSections,reorder_sections);
  ExportCheckbox(CutUnusedRanges,cut_ranges);
  ExportCheckbox(MergeConstants,merge_constants);
  ExportCheckbox(LinkAgainstStandardLibrary,std_lib);
  ExportCheckbox(InitializeBSSSection,initialize_bss);
  ExportCheckbox(OutputVariableImageWithoutWrapper,outputbin);
  //Tab: Post-Build
  settings.post_build=CallAfterBuilding->text();
  settings.cmd_line=Parameters->text();
}
#undef ExportCheckbox

void ProjectOptions::RegularProgramToggle()
{
  int state;
  buttonOk->setAccel(0);
  buttonOk->setText("OK");
  state=RegularProgram->isOn()?TRUE:FALSE;
  if (!state)
  {
    if (ExternalDataVariable->isOn())
      ExternalDataVariable->setChecked(TRUE);
    if (CompressProgram->isOn())
      CompressProgram->setChecked(TRUE);
  }
  ExternalDataVariable->setEnabled(state);
  CompressProgram->setEnabled(state);
  ExternalDataVariableToggle();
  CompressProgramToggle();
}


void ProjectOptions::ExternalDataVariableToggle()
{
  int state;
  state=ExternalDataVariable->isOn()?TRUE:FALSE;
  LOncalcVariableName_1->setEnabled(state);
  OncalcVariableName_1->setEnabled(state);
  LCreateCopyNever->setEnabled(state);
  CreateCopyNever->setEnabled(state);
  CreateCopyIfArchived->setEnabled(state);
  CreateCopyAlways->setEnabled(state);
  CheckOncalcNames();
}


void ProjectOptions::CompressProgramToggle()
{
  int state;
  state=CompressProgram->isOn()?TRUE:FALSE;
  LOncalcVariableName_2->setEnabled(state);
  OncalcVariableName_2->setEnabled(state);
  CheckOncalcNames();
}


void ProjectOptions::CheckOncalcNames()
{
  const QString &edvname=OncalcVariableName_1->text();
  const QString &compname=OncalcVariableName_2->text();
  Boolean edvon=OncalcVariableName_1->isEnabled();
  Boolean compon=OncalcVariableName_2->isEnabled();
  if ((edvon&&edvname.isEmpty()) || (compon&&compname.isEmpty()) || (edvon&&compon&&!edvname.compare(compname)))
    buttonOk->setEnabled(FALSE);
  else
    buttonOk->setEnabled(TRUE);
}


void ProjectOptions::UpdateVisibilities()
{
  Boolean regularprogram=RegularProgram->isOn();
  Boolean functionarchive=FunctionArchive->isOn();
  if (regularprogram)
    ProgramOptionsButton->show();
  else
    ProgramOptionsButton->hide();
  PO_TabWidget->setTabEnabled(PO_TabWidget->page(2),!functionarchive);
  PO_TabWidget->setTabEnabled(PO_TabWidget->page(3),!functionarchive);
  
}

void ProjectOptions::ProgramOptionsFunc()
{
  programoptions->exec();
}
