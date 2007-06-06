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

#include "projectoptions.h"

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <QButtonGroup>
#include <kmessagebox.h>
#include <kfiledialog.h>
#include <kshell.h>
#include <kurl.h>
#include <kpushbutton.h> 
#include "ktigcc.h"
#include "tpr.h"
#include "programoptions.h"

ProgramOptions *programoptions;
//the program options subdialog is created at initialization of a ProjectOptions and remains existant until the ProjectOptions is destroyed.

ProjectOptions::ProjectOptions(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  buttonOk->setGuiItem(KStandardGuiItem::Ok);
  buttonCancel->setGuiItem(KStandardGuiItem::Cancel);
  if (!have_fargo) FargoProgram->hide();
  if (!have_flashos) FlashOperatingSystem->hide();
  //Create the Program Options dialog
  programoptions=new ProgramOptions(this);
  //Toggle controls to match settings.
  ImportSettings();
  //Update stuff
  CheckOncalcNames();
  UpdateVisibilities();
}

ProjectOptions::~ProjectOptions()
{
  //Save settings
  if (result()==QDialog::Accepted)
    ExportSettings();
  delete(programoptions);
}

void ProjectOptions::ImportSettings(void)
{
  Boolean isregular;
  //Tab: General
  QButtonGroup *group=new QButtonGroup(this);
  group->addButton(CreateCopyNever);
  group->addButton(CreateCopyIfArchived);
  group->addButton(CreateCopyAlways);
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
    RegularProgram->setChecked(TRUE);
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
  GenerateDebugInformation->setChecked(settings.debug_info);
  //Tab: Linking
  NOPs->setChecked(settings.optimize_nops);
  ReturnSequences->setChecked(settings.optimize_returns);
  Branches->setChecked(settings.optimize_branches);
  MoveLoadPushInstructions->setChecked(settings.optimize_moves);
  TestCompareInstructions->setChecked(settings.optimize_tests);
  CalculationInstructions->setChecked(settings.optimize_calcs);
  RemoveUnusedSections->setChecked(settings.remove_unused);
  ReorderSections->setChecked(settings.reorder_sections);
  CutUnusedRanges->setChecked(settings.cut_ranges);
  MergeConstants->setChecked(settings.merge_constants);
  LinkAgainstStandardLibrary->setChecked(settings.std_lib);
  InitializeBSSSection->setChecked(settings.initialize_bss);
  OutputVariableImageWithoutWrapper->setChecked(settings.outputbin);
  //Tab: Post-Build
  CallAfterBuilding->setText(settings.post_build);
  Parameters->setText(settings.cmd_line);
  programoptions->ImportSettings();
}

void ProjectOptions::ExportSettings(void)
{
  //Tab: General
  settings.fargo=FALSE;
  settings.flash_os=FALSE;
  settings.archive=FALSE;
  settings.use_data_var=FALSE;
  settings.pack=FALSE;
  if (FargoProgram->isChecked())
    settings.fargo=TRUE;
  else if (FlashOperatingSystem->isChecked())
    settings.flash_os=TRUE;
  else if (FunctionArchive->isChecked())
    settings.archive=TRUE;
  else if (RegularProgram->isChecked()) //the original TIGCC IDE automatically unchecked External data variable and Compress program when you toggled away from Regular Program, so we will only consider these two if Regular Program is toggled
  {
    if (ExternalDataVariable->isChecked())
      settings.use_data_var=TRUE;
    if (CompressProgram->isChecked())
      settings.pack=TRUE;
  }
  //Subselections for External data variable and Compress program are always preserved.
  settings.data_var=OncalcVariableName_1->text();
  settings.copy_data_var_arc=FALSE;
  if (CreateCopyNever->isChecked())
    settings.copy_data_var=FALSE;
  else
  {
    settings.copy_data_var=TRUE;
    if (CreateCopyIfArchived->isChecked())
      settings.copy_data_var_arc=TRUE;
  }
  settings.pack_name=OncalcVariableName_2->text();
  //Tab: Compilation
  settings.cc_switches=GCCSwitches->text();
  settings.as_switches=AsSwitches->text();
  settings.a68k_switches=A68kSwitches->text();
  settings.debug_info=GenerateDebugInformation->isChecked();
  //Tab: Linking
  settings.optimize_nops=NOPs->isChecked();
  settings.optimize_returns=ReturnSequences->isChecked();
  settings.optimize_branches=Branches->isChecked();
  settings.optimize_moves=MoveLoadPushInstructions->isChecked();
  settings.optimize_tests=TestCompareInstructions->isChecked();
  settings.optimize_calcs=CalculationInstructions->isChecked();
  settings.remove_unused=RemoveUnusedSections->isChecked();
  settings.reorder_sections=ReorderSections->isChecked();
  settings.cut_ranges=CutUnusedRanges->isChecked();
  settings.merge_constants=MergeConstants->isChecked();
  settings.std_lib=LinkAgainstStandardLibrary->isChecked();
  settings.initialize_bss=InitializeBSSSection->isChecked();
  settings.outputbin=OutputVariableImageWithoutWrapper->isChecked();
  //Tab: Post-Build
  settings.post_build=CallAfterBuilding->text();
  settings.cmd_line=Parameters->text();
  programoptions->ExportSettings();
}

void ProjectOptions::RegularProgram_toggled(bool state)
{
  if (!state)
  {
    ExternalDataVariable->setChecked(FALSE);
    CompressProgram->setChecked(FALSE);
    ExternalDataVariable_toggled(FALSE);
    CompressProgram_toggled(FALSE);
  }
  ExternalDataVariable->setEnabled(state);
  CompressProgram->setEnabled(state);
}


void ProjectOptions::ExternalDataVariable_toggled(bool state)
{
  LOncalcVariableName_1->setEnabled(state);
  OncalcVariableName_1->setEnabled(state);
  LCreateCopyNever->setEnabled(state);
  CreateCopyNever->setEnabled(state);
  CreateCopyIfArchived->setEnabled(state);
  CreateCopyAlways->setEnabled(state);
  CheckOncalcNames();
}


void ProjectOptions::CompressProgram_toggled(bool state)
{
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
  Boolean regularprogram=RegularProgram->isChecked();
  Boolean functionarchive=FunctionArchive->isChecked();
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


void ProjectOptions::browseButton_clicked()
{
  QString ret=KFileDialog::getOpenFileName(KUrl("/usr/bin"),
    "application/x-executable application/x-executable-script",this,
    "Choose executable");
  if (!ret.isEmpty())
    CallAfterBuilding->setText(KShell::quoteArg(ret)+" \"($TI89File)\" \"($TI92PlusFile)\" \"($V200File)\"");
}

void ProjectOptions::languageChange()
{
  retranslateUi(this);
}

