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
	CalcUnit in 'CalcUnit.pas';

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
