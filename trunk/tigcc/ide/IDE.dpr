{
  TIGCC IDE

  Copyright (C) 2000-2004 Sebastian Reichelt
  Copyright (C) 2004 Frédéric Bour

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
}

program IDE;

uses
  Forms,
  Controls,
  VersionUnit in 'VersionUnit.pas',
	SourceFileUnit in 'SourceFileUnit.pas',
  MasterUnit in 'MasterUnit.pas',
	ParsingUnit in 'ParsingUnit.pas',
  ProcessUnit in 'ProcessUnit.pas',
  ToolsListUnit in 'ToolsListUnit.pas',
  MainUnit in 'MainUnit.pas' {MainForm},
  ProjectOptionsUnit in 'ProjectOptionsUnit.pas' {ProjectOptionsForm},
  PreferencesUnit in 'PreferencesUnit.pas' {PreferencesForm},
	AboutUnit in 'AboutUnit.pas' {AboutForm},
  ProgramOptionsUnit in 'ProgramOptionsUnit.pas' {ProgramOptionsForm},
	StyleSelectionUnit in 'StyleSelectionUnit.pas' {StyleSelectionForm},
  ColorsUnit in 'ColorsUnit.pas' {MultipleColorsForm},
  CustomStyleUnit in 'CustomStyleUnit.pas' {CustomStyleForm},
  WordListUnit in 'WordListUnit.pas' {WordListForm},
  StartupScreenUnit in 'StartupScreenUnit.pas' {StartupScreenForm},
  SourceFileWinUnit in 'SourceFileWinUnit.pas' {SourceFileForm},
	FunctionsWinUnit in 'FunctionsWinUnit.pas' {FunctionsForm},
  OpenFileStatusUnit in 'OpenFileStatusUnit.pas' {OpenFileStatusForm},
	NewsUnit in 'NewsUnit.pas' {NewsForm},
  SendProgressUnit in 'SendProgressUnit.pas' {SendProgressForm},
  ToolsUnit in 'ToolsUnit.pas' {ToolsForm},
	ToolPropertiesUnit in 'ToolPropertiesUnit.pas' {ToolPropertiesForm},
	ProgramOutputUnit in 'ProgramOutputUnit.pas' {ProgramOutputForm},
	VTIStartUnit in 'VTIStartUnit.pas' {VTIStartForm},
	TIEmuStartUnit in 'TIEmuStartUnit.pas' {TIEmuStartForm},
{$IFDEF CODINGEXT}
	CodeCompletion in 'CodeCompletion\CodeCompletion.pas',
	CompletionForm in 'CodeCompletion\CompletionForm.pas' {CompForm},
	HtFormatting in 'CodeCompletion\HtFormatting.pas',
	PrefFrame in 'CodeCompletion\PrefFrame.pas' {CodingExt: TFrame},
	TmpltForm in 'CodeCompletion\TmpltForm.pas' {TemplateForm},
	uBatch in 'CodeCompletion\Editor\uBatch.pas' {BatchEdit},
	uEditor in 'CodeCompletion\Editor\uEditor.pas' {CEditorForm},
	uHSFParser in 'CodeCompletion\Editor\uHSFParser.pas',
{$ENDIF}
	LinkDLLUnit in 'LinkDLLUnit.pas',
	FolderUnit in 'FolderUnit.pas',
	LinkUnit in 'LinkUnit.pas',
	CalcUnit in 'CalcUnit.pas',
	HTMLHelpUnit in '..\components\HTMLHelpUnit.pas';

{$R *.RES}

begin
	Application.Initialize;
	Application.Title := 'TIGCC IDE';
	Screen.Cursor := crAppStart;
	StartupScreenForm := TStartupScreenForm.Create (Application);
	StartupScreenForm.Show;
	StartupScreenForm.Update;
	StartupScreenForm.DisplayText := 'Creating Main Window...';
	Application.CreateForm(TMainForm, MainForm);
{$IFDEF CODINGEXT}
	Application.CreateForm(TCompForm, CompForm);
	Application.CreateForm(TTemplateForm, TemplateForm);
{$ENDIF}
	StartupScreenForm.Free;
	StartupScreenForm := nil;
	Screen.Cursor := crDefault;
	Application.Run;
end.
