{
  TIGCC IDE

  Copyright (C) 2000-2004 Sebastian Reichelt
  Copyright (C) 2005 Fréderic Bour
  Copyright (C) 2005-2006 Kevin Kofler

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

unit MainUnit;

interface

uses
	MasterUnit, SourceFileUnit, FolderUnit, ToolsListUnit, LinkDLLUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Menus, StdCtrls, ExtCtrls, ImgList, ComCtrls, Buttons, ActnList, ToolWin,
	Printers, SourceEditUnit, HTMLHelpUnit, MemoComponentUnit, ScktComp,
	ComObj, ActiveX, TiEmuOLELib_TLB;

type
  TPanel = class(ExtCtrls.TPanel)
  protected
    procedure Paint; override;
  end;

	TMainForm = class(TForm)
		ProjectTree: TTreeView;
		LittleIcons: TImageList;
		TreeEditorSplitter: TSplitter;
		MainMenu: TMainMenu;
		MainMenuFile: TMenuItem;
		MenuFileNew: TMenuItem;
		Project1: TMenuItem;
		AssemblerSource1: TMenuItem;
		Open1: TMenuItem;
		Save1: TMenuItem;
		SaveAs1: TMenuItem;
		N1: TMenuItem;
		Exit1: TMenuItem;
		CSourceFile1: TMenuItem;
		MainMenuEdit: TMenuItem;
		Undo1: TMenuItem;
		N2: TMenuItem;
		Delete1: TMenuItem;
		Cut1: TMenuItem;
		Copy1: TMenuItem;
		Paste1: TMenuItem;
		AddFiles1: TMenuItem;
		MenuCompileLine: TMenuItem;
		MainMenuProject: TMenuItem;
		Compile1: TMenuItem;
		Link1: TMenuItem;
		N4: TMenuItem;
		Options1: TMenuItem;
		AddFileDlg: TOpenDialog;
		N5: TMenuItem;
		Preferences1: TMenuItem;
		N6: TMenuItem;
		HeaderFile1: TMenuItem;
		SourceFilePopup: TPopupMenu;
		Compile2: TMenuItem;
		N7: TMenuItem;
		Remove1: TMenuItem;
		Delete2: TMenuItem;
		Splitter2: TSplitter;
		ErrWinPanel: TPanel;
		ErrorList: TListView;
		Save2: TMenuItem;
		SaveAs2: TMenuItem;
		N8: TMenuItem;
		MainMenuHelp: TMenuItem;
		Build1: TMenuItem;
		N9: TMenuItem;
		SelectAll1: TMenuItem;
		OpenProjectDlg: TOpenDialog;
		SaveProjectDlg: TSaveDialog;
		N10: TMenuItem;
		About1: TMenuItem;
		StatusBar: TStatusBar;
		ToolBarImages: TImageList;
		FileNewPopup: TPopupMenu;
		HeaderFile2: TMenuItem;
		Actions: TActionList;
		ActionFileNewProject: TAction;
		ActionFileNewCHeaderFile: TAction;
		ActionFileNewGNUAsmHeaderFile: TAction;
		ActionFileNewA68kAsmHeaderFile: TAction;
		ActionFileNewCFile: TAction;
		ActionFileNewGNUAsmFile: TAction;
		ActionFileNewA68kAsmFile: TAction;
		ActionFileOpen: TAction;
		ActionFileSave: TAction;
		ActionFileSaveAs: TAction;
		ActionFilePreferences: TAction;
		ActionFileExit: TAction;
		ActionEditUndo: TAction;
		ActionEditDelete: TAction;
		ActionEditCut: TAction;
		ActionEditCopy: TAction;
		ActionEditPaste: TAction;
		ActionEditSelectAll: TAction;
		ActionProjectAddFiles: TAction;
		ActionProjectCompile: TAction;
		ActionProjectMake: TAction;
		ActionProjectBuild: TAction;
		ActionProjectOptions: TAction;
		ActionHelpDocumentation: TAction;
		ActionHelpAbout: TAction;
		ActionTreeItemSave: TAction;
		ActionTreeItemSaveAs: TAction;
		ActionTreeItemCompile: TAction;
		ActionTreeItemRemove: TAction;
		ActionTreeItemDelete: TAction;
		CSourceFile2: TMenuItem;
		AssemblerSourceFile1: TMenuItem;
		Project3: TMenuItem;
		N11: TMenuItem;
		ActionProjectShowErrors: TAction;
		ErrorsandWarnings1: TMenuItem;
		N12: TMenuItem;
		MainMenuFind: TMenuItem;
		ActionFindFind: TAction;
		ActionFindReplace: TAction;
		ActionFindOpenFile: TAction;
		Find2: TMenuItem;
		Replace1: TMenuItem;
		OpenFileAtCursor1: TMenuItem;
		N13: TMenuItem;
		FindDlg: TFindDialog;
		ReplaceDlg: TReplaceDialog;
		ToolBarImagesDisabled: TImageList;
		PrintDlg: TPrintDialog;
		ActionFilePrint: TAction;
		Print1: TMenuItem;
		N14: TMenuItem;
		ActionFilePrintQuickly: TAction;
		ErrorListIcons: TImageList;
		DocFile: THTMLHelp;
		ActionHelpContents: TAction;
		ActionHelpIndex: TAction;
		ActionHelpSearch: TAction;
		ActionTreeItemRename: TAction;
		N19: TMenuItem;
		Rename1: TMenuItem;
		Contents1: TMenuItem;
		Index1: TMenuItem;
		Search1: TMenuItem;
		EditorPopup: TPopupMenu;
		Undo2: TMenuItem;
		N16: TMenuItem;
		Clear1: TMenuItem;
		Cut2: TMenuItem;
		Copy2: TMenuItem;
		Paste2: TMenuItem;
		N17: TMenuItem;
		SelectAll2: TMenuItem;
		MainToolBar: TToolBar;
		ToolBarNewButton: TToolButton;
		ToolButton2: TToolButton;
		ToolButton3: TToolButton;
		ToolButton4: TToolButton;
		ToolButton23: TToolButton;
		ToolButton24: TToolButton;
		ToolButton6: TToolButton;
		ToolButton7: TToolButton;
		ToolButton8: TToolButton;
		ToolButton9: TToolButton;
		ToolButton10: TToolButton;
		ToolButton5: TToolButton;
		ToolButton11: TToolButton;
		ToolButton20: TToolButton;
		ToolButton21: TToolButton;
		ToolBarCompileLine1: TToolButton;
		ToolBarCompileButton: TToolButton;
		ToolButton15: TToolButton;
		ToolBarCompileLine2: TToolButton;
		ToolButton18: TToolButton;
		ToolButton19: TToolButton;
		ActionEditRedo: TAction;
		Redo1: TMenuItem;
		Redo2: TMenuItem;
		EditorPanel: TPanel;
		NoEditor: TPanel;
		FunctionPopup: TPopupMenu;
		ActionFindFunctions: TAction;
		N15: TMenuItem;
		Functions1: TMenuItem;
		ToolButton25: TToolButton;
		NoFunctionsItem: TMenuItem;
		ActionFileNewTextFile: TAction;
		TextFile1: TMenuItem;
		TextFile2: TMenuItem;
		ActionEditIncreaseIndent: TAction;
		ActionEditDecreaseIndent: TAction;
		N18: TMenuItem;
		IncreaseIndent1: TMenuItem;
		DecreaseIndent1: TMenuItem;
		N20: TMenuItem;
		IncreaseIndent2: TMenuItem;
		DecreaseIndent2: TMenuItem;
		N21: TMenuItem;
		OpenFileatCursor2: TMenuItem;
		GNUAssemblySourceFile1: TMenuItem;
		GNUAssemblySourceFile2: TMenuItem;
		ActionFileNewQuillFile: TAction;
		QuillSourceFile1: TMenuItem;
		QuillSourceFile2: TMenuItem;
		ActionProjectStopCompilation: TAction;
		ActionProjectForceQuitCompiler: TAction;
		ToolButton16: TToolButton;
		ToolButton17: TToolButton;
		StopCompilation1: TMenuItem;
		ForceQuitCompiler1: TMenuItem;
		ActionHelpNews: TAction;
		N3: TMenuItem;
		News1: TMenuItem;
		ActionDebugRun: TAction;
		ActionDebugPause: TAction;
		ActionDebugReset: TAction;
		MainMenuDebug: TMenuItem;
		Run1: TMenuItem;
		N22: TMenuItem;
		Pause1: TMenuItem;
		Reset1: TMenuItem;
		ToolBarRunButton: TToolButton;
		ToolBarDebugLine: TToolButton;
		ToolBarPauseButton: TToolButton;
		ToolButton12: TToolButton;
		ToolButton13: TToolButton;
		ChangeNotificationTimer: TTimer;
		ToolBarBevel: TBevel;
		ToolBarEndLine: TToolButton;
		RecentFilesLine: TMenuItem;
		RecentFilesPopup: TPopupMenu;
		NoFilesItem: TMenuItem;
		ActionToolsConfigure: TAction;
		MainMenuTools: TMenuItem;
		Configure1: TMenuItem;
		ToolsLine: TMenuItem;
		ActionProjectShowProgramOutput: TAction;
		ProgramOutput1: TMenuItem;
		ActionFileNewFolder: TAction;
		N23: TMenuItem;
		Folder1: TMenuItem;
		N24: TMenuItem;
		Folder2: TMenuItem;
		FolderPopup: TPopupMenu;
    MenuItem6: TMenuItem;
    MenuItem9: TMenuItem;
    CategoryPopup: TPopupMenu;
		ActionFileNewFile: TAction;
    File1: TMenuItem;
    Folder3: TMenuItem;
    N25: TMenuItem;
    Folder4: TMenuItem;
    File2: TMenuItem;
    ActionTreeItemNewFolder: TAction;
    ActionTreeItemNewFile: TAction;
		GNUAssemblyHeaderFile1: TMenuItem;
		GNUAssemblyHeaderFile2: TMenuItem;
		A68kAssemblyHeaderFile1: TMenuItem;
    A68kAssemblyHeaderFile2: TMenuItem;
    ErrorPanel: TPanel;
    CloseErrorsButton: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    ErrorsLabel: TLabel;
    WarningsLabel: TLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure AppException(Sender: TObject; E: Exception);
		procedure ProjectTreeEditing(Sender: TObject; Node: TTreeNode;
			var AllowEdit: Boolean);
		procedure ProjectAddFiles(Sender: TObject);
		procedure ProjectTreeChange(Sender: TObject; Node: TTreeNode);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure ProjectTreeDeletion(Sender: TObject; Node: TTreeNode);
		procedure FileExit(Sender: TObject);
		procedure FileNewProject(Sender: TObject);
		procedure TreeItemRemove(Sender: TObject);
		procedure TreeItemDelete(Sender: TObject);
		procedure TreeItemCompile(Sender: TObject);
		procedure CloseErrorsButtonClick(Sender: TObject);
		procedure ErrorListDeletion(Sender: TObject; Item: TListItem);
		procedure TreeItemSave(Sender: TObject);
		procedure HelpDocumentation(Sender: TObject);
		procedure EditorEnter(Sender: TObject);
		procedure EditorExit(Sender: TObject);
		procedure EditUndo(Sender: TObject);
		procedure EditRedo(Sender: TObject);
		procedure EditClear(Sender: TObject);
		procedure EditCut(Sender: TObject);
		procedure EditCopy(Sender: TObject);
		procedure EditPaste(Sender: TObject);
		procedure EditSelectAll(Sender: TObject);
		procedure ProjectCompile(Sender: TObject);
		procedure ProjectMake(Sender: TObject);
		procedure FileOpenProject(Sender: TObject);
		procedure FileSaveAll(Sender: TObject);
		procedure FileSaveProjectAs(Sender: TObject);
		procedure EditorKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure ProjectBuild(Sender: TObject);
		procedure ProjectTreeKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure ProjectTreeMouseUp(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ProjectTreeEdited(Sender: TObject; Node: TTreeNode;
			var S: String);
		procedure ProjectOptions(Sender: TObject);
		procedure FilePreferences(Sender: TObject);
		procedure FileNewHeaderFile(Sender: TObject);
		procedure FileNewCSourceFile(Sender: TObject);
		procedure FileNewGNUAssemblerSourceFile(Sender: TObject);
		procedure FileNewAssemblerSourceFile(Sender: TObject);
		procedure FileNewQuillSourceFile(Sender: TObject);
		procedure FileNewTextFile(Sender: TObject);
		procedure TreeItemSaveAs(Sender: TObject);
		procedure HelpAbout(Sender: TObject);
		procedure DisplayHint(Sender: TObject);
		procedure ShowHideErrors(Sender: TObject);
		procedure FindString(Sender: TObject; AllFiles: Boolean);
		procedure ReplaceDlgReplace(Sender: TObject);
		procedure FindText(Sender: TObject);
		procedure ReplaceText(Sender: TObject);
		procedure FindOpenFile(Sender: TObject);
		procedure FindDlgFind(Sender: TObject);
		procedure ActionsExecute(Action: TBasicAction; var Handled: Boolean);
		procedure ProjectTreeEnter(Sender: TObject);
		procedure ProjectTreeExit(Sender: TObject);
		procedure ProjectTreeChanging(Sender: TObject; Node: TTreeNode;
			var AllowChange: Boolean);
		procedure EditorChange(Sender: TObject);
		procedure FilePrint(Sender: TObject);
		procedure FilePrintQuickly(Sender: TObject);
		procedure TreeItemRename(Sender: TObject);
		procedure HelpContents(Sender: TObject);
		procedure HelpIndex(Sender: TObject);
		procedure HelpSearch(Sender: TObject);
		procedure SplitterMoved(Sender: TObject);
		procedure ProjectTreeMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure ToolBarManagerBandMove(Sender: TObject; Control: TControl;
			var ARect: TRect);
		procedure ProjectTreeDragOver(Sender, Source: TObject; X, Y: Integer;
			State: TDragState; var Accept: Boolean);
		procedure ProjectTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
		procedure ErrorListClick(Sender: TObject);
		procedure DeleteError(Sender: TObject);
		procedure FunctionPopupPopup(Sender: TObject);
		procedure FindFunctions(Sender: TObject);
		procedure FindFunctionFromPopup(Sender: TObject);
		procedure IncreaseIndent(Sender: TObject);
		procedure DecreaseIndent(Sender: TObject);
		procedure ProjectStopCompilation(Sender: TObject);
		procedure ProjectForceQuitCompiler(Sender: TObject);
		procedure HelpNews(Sender: TObject);
		procedure DebugRun(Sender: TObject);
		procedure DebugPause(Sender: TObject);
		procedure DebugReset(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure ChangeNotificationTick(Sender: TObject);
		procedure RecentFileClick(Sender: TObject);
		procedure ToolsConfigure(Sender: TObject);
		procedure ToolClick(Sender: TObject);
		procedure ProjectTreeStartDrag(Sender: TObject;
			var DragObject: TDragObject);
		procedure ShowProgramOutput(Sender: TObject);
		procedure FileNewFolder(Sender: TObject);
    procedure FileNewFile(Sender: TObject);
    procedure ErrorListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
	private
		EditorToHide: TWinControl;
		TempLockHandle: THandle;
		FModified: Boolean;
		FInvalidated: Boolean;
		FProjectFile: string;
		InChangeNotification: Boolean;
		procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
		procedure SetProjectFile(const Value: string);
		procedure SetModified(const Value: Boolean);
		function GetCurrentEditor: TMemoComponent;
		function GetInvalidated: Boolean;
	public
		TopNode: TTreeNode;
		OriginalCaption: string;
		LinkLibHandle: HModule;
		LinkLibGetInterfaceVersion: TLinkLibGetInterfaceVersion;
		LinkLibLinkFiles: TLinkLibLinkFiles;
		LinkLibCreateArchive: TLinkLibCreateArchive;
		SourceFiles: TSourceFiles;
		JumpToError,
		OpenFolderMessage,
		AutoSave,
		DeleteErrors,
		StartingAppNow,
		OpeningProjectNow,
		NoHideEditor: Boolean;
		PreviousNode: TTreeNode;
		SyntaxCBackup: TSyntaxColoring;
		SyntaxAsmGNUBackup: TSyntaxColoring;
		SyntaxAsmBackup: TSyntaxColoring;
		SyntaxQuillBackup: TSyntaxColoring;
		Closing: Boolean;
		ProgSize: Integer;
		OptimizeInfo: TLinkLibOptimizeInfo;
		Funcs: TSourceFileFunctions;
		CurrentStrings: TMemoryStream;
		CurVTIType: TCurVTIType;
		RecentFiles: TStringList;
		ToolsList: TToolsList;
		property ProjectFile: string read FProjectFile write SetProjectFile;
		property Modified: Boolean read FModified write SetModified;
		property Invalidated: Boolean read GetInvalidated write FInvalidated;
		property CurrentEditor: TMemoComponent read GetCurrentEditor;
		procedure Modify;
		procedure WarnIfModified;
		procedure FileNew;
		procedure FileClear;
		procedure FileLoad;
		procedure FileOpen(const FN: string);
		procedure FileSave;
		procedure FileSaveAs(const FN: string);
		procedure ResetProjectSettings;
		procedure AppCompStartFile;
		procedure AppCompStop;
		procedure AppCompSetMessage(const Msg: string);
		procedure AppCompUpdate;
		procedure UpdateProgramOutput;
		procedure CopyHeaders;
		procedure DeleteHeaders;
		procedure ClearErrors;
		procedure HideErrors;
		procedure ShowErrors;
		procedure UpdateErrorWindow;
		procedure AddError(const Line: string; BugTp: TBugType; SourceF: string; ErrFunc: string; ErrMessage: string; SourceLn: Integer; Offset: Integer);
		procedure CompileProject;
		procedure MakeFileWrapped;
		procedure MakeFile;
		function AddSourceFile(const FN: string; OpenOnly: Boolean = False; SourceFileClass: TSourceFileClass = nil; Folder: TFolder = nil): TSourceFile;
		function GetNewFileName(const FolderPath, Ext: string): string;
		procedure LoadPreferences;
		procedure SavePreferences;
		procedure BeginCompilation;
		procedure EndCompilation;
		procedure RecompileFiles;
		procedure UpdateEditButtons;
		procedure UpdateStatusBar;
		procedure UpdateFuncs;
		procedure UpdateDebugSettings;
		procedure SortFiles;
		procedure DisplayFolderMessage;
		procedure ClearDebugInfo;
		function GetTiEmuInterface: ITiEmuOLE;
		procedure SendFiles(FNList: array of string);
		procedure ExecuteCommandLine(const Line: string);
		procedure SelectNode(Node: TTreeNode);
		procedure AddToRecent(const FileName: string);
		procedure UpdateRecent;
		procedure AddTool(const Title, CommandLine, WorkingDir: string; WindowState: TWindowState);
		function GetSourceTypeFolder(Node: TTreeNode): TTreeNode;
		function FindFileInsertionPoint(ParentNode: TTreeNode): TTreeNode;
		function CreateFileNode(ParentNode: TTreeNode; SourceFile: TSourceFile): TTreeNode;
		function GetSelectedFolder(SourceClass: TSourceFileClass): TFolder;
{$IFDEF CODINGEXT}
		procedure InitCodingExt;
		procedure ActionFindSymbolExecute(Sender: TObject);
{$ENDIF}
	end;

var
	MainForm: TMainForm;

type
	TRecentFileMenuItem = class(TObject)
		FileMenuItem: TMenuItem;
		PopupMenuItem: TMenuItem;
	public
		destructor Destroy; override;
	end;

const
	MaxRecentFiles = 4;

type
	TNoImageDragObject = class(TDragControlObject)
	public
		function GetDragImages: TDragImageList; override;
		procedure HideDragImage; override;
		procedure ShowDragImage; override;
	end;

procedure LinkLibError(FileName, Text: PChar; MessageType: LongInt); cdecl;
function LinkLibGetOutputFile(var DestFile: TLinkLibDestFile; FileSize, DestCalc, FileRole, FileFormat, FileType: LongInt; Extension: PChar; Executable: WordBool; var EffectiveSize: LongInt): WordBool; cdecl;

implementation

{$R *.DFM}   
{$R WindowsXP.res}

uses
	CalcUnit, ParsingUnit, ProcessUnit,
	StartupScreenUnit, PreferencesUnit, ProjectOptionsUnit,
	AboutUnit, SourceFileWinUnit, FunctionsWinUnit,
	OpenFileStatusUnit, NewsUnit, SendProgressUnit, ToolsUnit,
	ProgramOutputUnit, VTIStartUnit,
	LinkUnit,
	UtilsDos, UtilsWin, HandleWaitThreadUnit, FileReadToBufferThreadUnit,
	ShellAPI, ShlObj, IniFiles, Registry, WinSpool, ClipBrd{$IFDEF CODINGEXT}, CompletionForm{$ENDIF},
  ProgramOptionsUnit;

const
	RegKey = '\Software\SeReSoft\TI-GCC IDE';

type
	TFileRole = (frMain, frData);
	TLinkOutputFile = record
		Data: TMemoryStream;
		Tag: Byte;
		VarExt: string;
		ExeFile,
		OSUpgrade: Boolean;
	end;

var
	ProcID: Cardinal;
	SendWin: HWnd;
	LinkOutputFiles: array [TCalcDest, TFileRole] of TLinkOutputFile;
	LinkDebugFile: TLinkOutputFile;

function EnumWindowsFunc(Win: HWnd; Param: Integer): Bool; stdcall;
var
	Name: array [0..32] of Char;
	NewProcID: DWord;
begin
	Result := True;
	GetWindowThreadProcessID (Win, @NewProcID);
	if NewProcID = ProcID then begin
		GetClassName (Win, Name, 32);
		if Name = '#32770' then begin
			Result := False;
			SendWin := Win;
		end;
	end;
end;

procedure ErrorMessage(const Msg: string);
begin
	if UpperCase (Copy (Msg, 1, Length ('ERROR:'))) = 'ERROR:' then
		MainForm.AddError (Msg, btError, '', '', Trim (Copy (Msg, Length ('ERROR:') + 1, Length (Msg))), 0, 0)
	else if UpperCase (Copy (Msg, 1, Length ('WARNING:'))) = 'WARNING:' then
		MainForm.AddError (Msg, btWarning, '', '', Trim (Copy (Msg, Length ('WARNING:') + 1, Length (Msg))), 0, 0);
end;

procedure LinkLibError(FileName, Text: PChar; MessageType: LongInt); cdecl;
var
	BugTp: TBugType;
	ErrorLine: string;
begin
	if MessageType = llmtError then
		BugTp := btError
	else
		BugTp := btWarning;
	ErrorLine := StringReplace (StrPas (Text), '`', '''', [rfReplaceAll]);
	MainForm.AddError (ErrorLine, BugTp, FileName, '', ErrorLine, 0, 0);
end;

function LinkLibGetOutputFile(var DestFile: TLinkLibDestFile; FileSize, DestCalc, FileRole, FileFormat, FileType: LongInt; Extension: PChar; Executable: WordBool; var EffectiveSize: LongInt): WordBool; cdecl;
var
	CalcDest: TCalcDest;
	CalcFileRole: TFileRole;
begin
	Result := False;
	case FileFormat of
		llffTIOS:
			EffectiveSize := 2 + FileSize + 1;
		llffTIOSUpgrade:
			EffectiveSize := FileSize + SizeOf (TCalcOSFooter);
		llffGDBCOFF:
			begin
				EffectiveSize := FileSize;
			  	with LinkDebugFile do begin
		  			if not Assigned (Data) then
	  					Data := TMemoryStream.Create;
					  Data.Size := FileSize;
			  		DestFile.Data := Data.Memory;
				  end;
		  		Result := True;
	  			Exit;
			end;
		else
			Exit;
	end;
	case DestCalc of
		llcdTI89:                     CalcDest := cdTI89;
		llcdTI89 or llcdFlagTitanium: CalcDest := cdTI89Titanium;
		llcdTI92Plus:                 CalcDest := cdTI92Plus;
		llcdV200:                     CalcDest := cdV200;
		llcdTI92:                     CalcDest := cdTI92;
		else
			Exit;
	end;
	case FileRole of
		llfrMain: CalcFileRole := frMain;
		llfrData: CalcFileRole := frData;
		else
			Exit;
	end;
	with LinkOutputFiles [CalcDest, CalcFileRole] do begin
		if not Assigned (Data) then
			Data := TMemoryStream.Create;
		Data.Size := FileSize;
		DestFile.Data := Data.Memory;
		Tag := FileType;
		if Assigned (Extension) then begin
			VarExt := Extension;
			Inc (EffectiveSize, Length (VarExt) + 2);
		end else
			VarExt := '';
		ExeFile := Executable;
		OSUpgrade := (FileFormat = llffTIOSUpgrade);
	end;
	Result := True;
end;

{ Fix for TPanel & XP Theme }

procedure TPanel.Paint;
begin
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(GetClientRect);
  inherited;
end;

{ TRecentFileMenuItem }

destructor TRecentFileMenuItem.Destroy;
begin
	FileMenuItem.Free;
	PopupMenuItem.Free;
	inherited;
end;

{ TNoImageDragObject }

function TNoImageDragObject.GetDragImages: TDragImageList;
begin
	Result := nil;
end;

procedure TNoImageDragObject.HideDragImage;
begin
end;

procedure TNoImageDragObject.ShowDragImage;
begin
end;

{ TMainForm }

procedure TMainForm.Modify;
begin
	Modified := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
	TempDir: array [0..255] of Char;
	S: string;
	TempLockData: TWin32FindData;
begin
	StartingAppNow := True;
	try
		if Assigned (StartupScreenForm) then
			StartupScreenForm.DisplayText := 'Setting Up Environment...';
		Application.OnException := AppException;
		WindowState := wsMaximized;
		RecentFiles := TStringList.Create;
		ToolsList := TToolsList.Create (Self, TToolsListItem);
		TIGCCFolder := ExtractFilePath (ParamStr (0));
		Delete (TIGCCFolder, Pos (UpperCase (IDELocation), UpperCase (TIGCCFolder)), Length (TIGCCFolder));
		with TRegistry.Create do try
			RootKey := HKey_Local_Machine;
			if OpenKeyReadOnly ('\Software\TIGCC Team\TIGCC') then try
				if ValueExists ('Program Folder') then
					TIGCCFolder := ReadString ('Program Folder');
			except end;
		finally
			Free;
		end;
		try
			if FileExists (WithBackslash (TIGCCFolder) + A68kLocation + 'A68k.exe') then
				Include (SpecialSupport, ssA68k);
		except end;
		try
			if FileExists (WithBackslash (TIGCCFolder) + QuillIncludeLocation + 'Quill.drv')
				or FileExists (WithBackslash (TIGCCFolder) + CIncludeLocation + 'Quill.drv')
				or FileExists (WithBackslash (TIGCCFolder) + GCCLocation + 'Quill.drv') then
				Include (SpecialSupport, ssQuill);
		except end;
		try
			if FileExists (WithBackslash (TIGCCFolder) + PackLocation + 'Pack.exe') then
				Include (SpecialSupport, ssPack);
		except end;
		try
			if FileExists (WithBackslash (TIGCCFolder) + StdLibLocation + 'flashos.a') then
				Include (SpecialSupport, ssFlashOS);
		except end;
		try
			if FileExists (WithBackslash (TIGCCFolder) + StdLibLocation + 'fargo.a') then
				Include (SpecialSupport, ssFargo);
		except end;
		CompStartFile := AppCompStartFile;
		CompStop := AppCompStop;
		CompSetMessage := AppCompSetMessage;
		CompUpdate := AppCompUpdate;
		CompUpdateProgramOutput := UpdateProgramOutput;
		LinkLibHandle := LoadLibrary (PChar (WithBackslash (TIGCCFolder) + LinkLibLocation + 'Link.dll'));
		if LinkLibHandle <> 0 then begin
			LinkLibGetInterfaceVersion := GetProcAddress (LinkLibHandle, 'GetInterfaceVersion');
			if Assigned (LinkLibGetInterfaceVersion) and (LinkLibGetInterfaceVersion = LinkLibCurInterfaceVersion) then begin
				LinkLibLinkFiles := GetProcAddress (LinkLibHandle, 'LinkFiles');
				LinkLibCreateArchive := GetProcAddress (LinkLibHandle, 'CreateArchive');
			end;
		end;
		SourceFiles := TSourceFiles.Create;
		SyntaxCBackup := TSyntaxColoring.Create (nil);
		SyntaxCBackup.Assign (SyntaxC);
		SyntaxAsmGNUBackup := TSyntaxColoring.Create (nil);
		SyntaxAsmGNUBackup.Assign (SyntaxAsmGNU);
		SyntaxAsmBackup := TSyntaxColoring.Create (nil);
		if ssA68k in SpecialSupport then
			SyntaxAsmBackup.Assign (SyntaxAsm);
		SyntaxQuillBackup := TSyntaxColoring.Create (nil);
		if ssQuill in SpecialSupport then
			SyntaxQuillBackup.Assign (SyntaxQuill);
		TopNode := ProjectTree.Items.Item [0];
		SourceFileUnit.AppNode := TopNode;
		SourceFileUnit.NoEditor := NoEditor;
		OriginalCaption := Caption;
		Application.OnHint := DisplayHint;
		if ssA68k in SpecialSupport then begin
			ActionFileNewA68kAsmHeaderFile.Visible := True;
			ActionFileNewA68kAsmFile.Visible := True;
			with ProjectTree.Items.Insert (TopNode.Item [TAsmSourceFile.GetClassTreeIndex], 'A68k Assembly Files') do begin
				ImageIndex    := TopNode.Item[0].ImageIndex;
				SelectedIndex := TopNode.Item[0].SelectedIndex;
			end;
		end;
		if ssQuill in SpecialSupport then begin
			ActionFileNewQuillFile.Visible := True;
			S := OpenProjectDlg.Filter;
			Insert (TQuillSourceFile.GetClassFilter + '|', S, Pos (TNormalTextSourceFile.GetClassFilter, S));
			while Pos ('*.asm;*.txt', S) > 0 do
				Insert (';*.qll', S, Pos ('*.asm;*.txt', S) + Length ('*.asm'));
			OpenProjectDlg.Filter := S;
			OpenProjectDlg.FilterIndex := OpenProjectDlg.FilterIndex + 1;
			S := AddFileDlg.Filter;
			Insert (TQuillSourceFile.GetClassFilter + '|', S, Pos (TObjectSourceFile.GetClassFilter, S));
			while Pos ('*.asm;*.o', S) > 0 do
				Insert (';*.qll', S, Pos ('*.asm;*.o', S) + Length ('*.asm'));
			AddFileDlg.Filter := S;
			AddFileDlg.FilterIndex := AddFileDlg.FilterIndex + 1;
			with ProjectTree.Items.Insert (TopNode.Item [TQuillSourceFile.GetClassTreeIndex], 'Quill Files') do begin
				ImageIndex    := TopNode.Item[0].ImageIndex;
				SelectedIndex := TopNode.Item[0].SelectedIndex;
			end;
		end;
		ProjectTree.FullExpand;
		ResetProjectSettings;
		AssumeUndefined := False;
		AutoSave := True;
		AutoNews := False;
		LastNewsDate := 0;
		GetTempPath (SizeOf (TempDir), TempDir);
		Temp := AnsiString (TempDir);
		if (Length (Temp) <= 0) or (not DirExists (Temp)) then begin
			GetEnvironmentVariable ('TMP', TempDir, SizeOf (TempDir));
			Temp := AnsiString (TempDir);
		end;
		if (Length (Temp) <= 0) or (not DirExists (Temp)) then begin
			GetEnvironmentVariable ('TEMP', TempDir, SizeOf (TempDir));
			Temp := AnsiString (TempDir);
		end;
		if (Length (Temp) <= 0) or (Length (Temp) > 30) or (Pos (' ', Temp) > 0) or (Pos ('TEMP', UpperCase (Temp)) <= 0) then begin
			GetWindowsDirectory (TempDir, SizeOf (TempDir));
			Temp := WithBackslash (AnsiString (TempDir)) + 'TEMP\';
			if not DirExists (Temp) then try
				MkDir (Temp);
			except end;
		end;
		if (Length (Temp) <= 0) or (Length (Temp) > 30) or (Pos (' ', Temp) > 0) then begin
			Temp := 'C:\TEMP\';
			if not DirExists (Temp) then try
				MkDir (Temp);
			except end;
		end;
		Temp := WithBackslash (Temp) + IntToHex (GetCurrentProcessID, 8) + '\';
		if not DirExists (Temp) then try
			MkDir (Temp);
		except end;
		TempLockHandle := Windows.FindFirstFile (PChar (Temp + '*.*'), TempLockData);
		StopOnErrors := False;
		JumpToError := True;
		OpenFolderMessage := True;
		DeleteAssemblyFiles := True;
		DeleteObjectFiles := False;
{$IFDEF CanSplit}
		SplitFiles := True;
{$ENDIF}
		DeleteErrors := True;
		TransferTarget := ttVTI;
		LinkPort.PortType := lpCOM;
		LinkPort.PortNumber := 1;
		LinkCable := lcBlack;
		TabSizeAsm := 8;
		TabSizeC := 2;
		AutoBlocks := True;
		EditorOnFly := True;
		EditorDragDrop := True;
		if Assigned (StartupScreenForm) then
			StartupScreenForm.DisplayText := 'Loading Preferences...';
		try
			LoadPreferences;
		except
			ShowDefaultMessageBox ('An error occured while loading the preferences. Some parts of the program may not be configured correctly.', 'Error', mtProgramError);
		end;
		DocFile.FileName := WithBackslash (TIGCCFolder) + DocLocation + 'TIGCC.chm';
		if DirExists (WithBackslash (TIGCCFolder) + ProjectsLocation) then
			OpenProjectDlg.InitialDir := WithBackslash (TIGCCFolder) + ProjectsLocation
		else
			OpenProjectDlg.InitialDir := TIGCCFolder;
		SaveProjectDlg.InitialDir := OpenProjectDlg.InitialDir;
		AddFileDlg.InitialDir := OpenProjectDlg.InitialDir;
		DragAcceptFiles (Handle, True);
		ParsingUnit.ErrorMessageProc := ErrorMessage;
		S := '';
		if ParamCount > 0 then begin
			if LowerCase (ExtractFileExt (ParamStr (1))) = LowerCase (ProjectFileExt) then
				ProjectFile := ExpandFileName (ParamStr (1))
			else
				S := ExpandFileName (ParamStr (1));
		end;
		if Length (ProjectFile) > 0 then begin
			if Assigned (StartupScreenForm) then
				StartupScreenForm.DisplayText := 'Opening Project...';
			FileLoad;
		end;
		if Length (S) > 0 then
			AddSourceFile (S, True);
		if AutoNews then begin
			if Assigned (StartupScreenForm) then
				StartupScreenForm.DisplayText := 'Downloading Headlines...';
			with TNewsForm.Create (Self) do begin
				SilentConnect := True;
				Connect;
			end;
		end;
	except
		ShowDefaultMessageBox ('An error occured while setting up the application. Some parts of the program may not be configured correctly.', 'Error', mtProgramError);
	end;
	StartingAppNow := False;

{$IFDEF CODINGEXT}
  InitCodingExt;
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
	I: Integer;
begin
	Closing := True;
	ClearDebugInfo;
	ErrorList.Items.Clear;
	SourceFiles.Free;
	SyntaxQuillBackup.Free;
	SyntaxAsmBackup.Free;
	SyntaxAsmGNUBackup.Free;
	SyntaxCBackup.Free;
	if LinkLibHandle <> 0 then
		FreeLibrary (LinkLibHandle);
	if TempLockHandle <> INVALID_HANDLE_VALUE then
		Windows.FindClose (TempLockHandle);
	try
		RmDir (Temp);
	except end;
	ToolsList.Free;
	with RecentFiles do begin
		for I := Count - 1 downto 0 do
			Objects[I].Free;
		Free;
	end;
end;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
begin
	if not (E is ESocketError) then
		ShowDefaultMessageBox ('Internal Error: Exception ' + E.ClassName + ', Message "' + E.Message + '"'#13#10#13#10'Please fill out a bug report form at http://tigcc.ticalc.org/.', 'Internal Error', mtProgramError);
end;

procedure TMainForm.ProjectTreeEditing(Sender: TObject; Node: TTreeNode;
	var AllowEdit: Boolean);
begin
	AllowEdit := Assigned (Node.Data) or not Assigned (Node.Parent);
end;

procedure TMainForm.ProjectAddFiles(Sender: TObject);
var
	I: Integer;
	F: TSourceFile;
begin
	if AddFileDlg.Execute then begin
		for I := AddFileDlg.Files.Count - 1 downto 0 do begin
			F := AddSourceFile(ExpandFileName(AddFileDlg.Files[I]));
			if (I = 0) and Assigned (F) and Assigned (F.TreeItem) then
				SelectNode (F.TreeItem);
		end;
		Modify;
	end else
		Abort;
end;

procedure TMainForm.ProjectTreeChange(Sender: TObject; Node: TTreeNode);
var
	NewSelection: TTreeNode;
	NewEditor: TWinControl;
	IsMemo,
	DataItem,
	BinaryType: Boolean;
begin
	NewSelection := ProjectTree.Selected;
	PreviousNode := NewSelection;
	if not Closing then begin
		if Assigned (NewSelection) and Assigned (NewSelection.Data) and (TObject (NewSelection.Data) is TSourceFile) then
			NewEditor := TSourceFile(NewSelection.Data).Editor
		else
			NewEditor := NoEditor;
		if Assigned (EditorToHide) and (EditorToHide <> NewEditor) then
			EditorToHide.Align := alNone;
		if NewEditor.Parent <> EditorPanel then
			NewEditor.Parent := EditorPanel;
		if NewEditor.Align <> alClient then
			NewEditor.Align := alClient;
		IsMemo := NewEditor is TMemoComponent;
		if IsMemo then begin
			with TMemoComponent (NewEditor) do begin
				DrawingSuspended := True;
				OnEnter := EditorEnter;
				OnExit := EditorExit;
				OnKeyDown := EditorKeyDown;
				OnChange := EditorChange;
				OnSelectionChange := EditorChange;
				PopupMenu := EditorPopup;
			end;
		end;
		NewEditor.Show;
		Update;
		if IsMemo then
			with TMemoComponent (NewEditor) do begin
				DrawingSuspended := False;
				Repaint;
			end;
		if Assigned (EditorToHide) and (EditorToHide <> NewEditor) then
			EditorToHide.Hide;
		EditorToHide := nil;
		DataItem := Assigned (NewSelection) and Assigned (NewSelection.Data);
		Node := GetSourceTypeFolder (NewSelection);
		BinaryType := Assigned (Node) and ((Node.Index = TObjectSourceFile.GetClassTreeIndex) or (Node.Index = TArchiveSourceFile.GetClassTreeIndex) or (Node.Index = TOtherSourceFile.GetClassTreeIndex));
		ActionFileNewFolder.Enabled := Assigned (NewSelection) and Assigned (NewSelection.Parent);
		ActionTreeItemNewFolder.Enabled := ActionFileNewFolder.Enabled;
		ActionFileNewFile.Enabled := ActionFileNewFolder.Enabled and (not BinaryType);
		ActionTreeItemNewFile.Enabled := ActionFileNewFile.Enabled;
		if ActiveControl = ProjectTree then
			ActionEditDelete.Enabled := DataItem;
		ActionFilePrint.Enabled := DataItem and (TObject (NewSelection.Data) is TSourceFile) and TSourceFile(NewSelection.Data).Printable and (Printer.Printers.Count > 0);
		ActionFilePrintQuickly.Enabled := ActionFilePrint.Enabled;
		ActionFindFunctions.Enabled := DataItem and (TObject (NewSelection.Data) is TSourceTextSourceFile);
		UpdateStatusBar;
	end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	if Compiling then
		CanClose := False
	else
		try
			WarnIfModified;
			SavePreferences;
		except
			CanClose := False;
		end;
end;

procedure TMainForm.WarnIfModified;
var
	I: Integer;
begin
	if Modified then
		case ShowDefaultMessageBox
			('The current project has been modified.  Do you want to save the changes?',
			'Project Modified', mtQuestion, True) of
			idYes: FileSave;
			idCancel: Abort;
		end;
	with SourceFiles do
		for I := 0 to Count - 1 do
			with Items [I] as TSourceFile do
				WarnIfModified;
end;

procedure TMainForm.FileLoad;
function StringToRelocFormat(const S: string): TRelocFormat;
begin
	if S = 'None' then
		Result := rfNone
	else if S = 'Direct' then
		Result := rfDirect
	else if S = 'AMS' then
		Result := rfAMS
	else if S = 'Kernel' then
		Result := rfKernel
	else if S = 'Compressed' then
		Result := rfCompressed
	else if S = 'MLink' then
		Result := rfMLink
	else if S = 'F-Line' then
		Result := rfFLine
	else
		Result := rfUnknown;
end;
var
	F: TOpenFileStatusForm;
	I,
	J,
	Ps: Integer;
	S,
	P: string;
	SL: TStringList;
	Folder: TFolder;
	Node,
	SubNode: TTreeNode;
	CurrentFile: TSourceFile;
begin
	if not Compiling then begin
		WarnIfModified;
		Modified := False;
		OpeningProjectNow := True;
		Update;
		F := nil;
		ProjectTree.Items.BeginUpdate;
		S := ProjectFile;
		try
			FileClear;
			if (Length (S) > 0) and FileExists (S) then begin
				if not StartingAppNow then begin
					try
						F := TOpenFileStatusForm.Create (Self);
					except
						F := nil;
					end;
					if Assigned (F) then begin
						P := ExtractFileName (S);
						F.FileNameLabel.Caption := 'Opening Project ''' + Copy (P, 1, LastPos ('.', P) - 1) + '''...';
						F.Show;
						F.Update;
					end;
				end;
				SHAddToRecentDocs(SHARD_PATH, PChar (S));
				with TIniFile.Create (S) do try
					TopNode.Text          := ReadString ('Settings', 'Project Name',                   'Project1');
					ProjectFile := S;
					if                       ReadBool   ('Settings', 'Archive',                        False) then
						ProjectTarget := ptArchive
					else if                  ReadBool   ('Settings', 'Flash OS',                       False) then
						ProjectTarget := ptFlashOS
					else if                  ReadBool   ('Settings', 'Fargo',                          False) then
						ProjectTarget := ptFargo;
					UseDataVar            := ReadBool   ('Settings', 'Use Data Variable',              False);
					DataVar               := ReadString ('Settings', 'Data Variable',                  '');
					DataVarCopy           := ReadBool   ('Settings', 'Copy Data Variable',             True);
					DataVarCopyIfArchived := ReadBool   ('Settings', 'Copy Data Variable if Archived', True);
					Pack                  := ReadBool   ('Settings', 'Pack',                           False);
					PackVar               := ReadString ('Settings', 'Packed Variable',                '');
					GCCSwitches           := ReadString ('Settings', 'GCC Switches',                   '');
					AsSwitches            := ReadString ('Settings', 'GNU Assembler Switches',         '');
					AsmSwitches           := ReadString ('Settings', 'Assembler Switches',             '');
					DebugInfo             := ReadBool   ('Settings', 'Debug Info',                     False);
					StdLib                := ReadBool   ('Settings', 'Standard Library',               True);
					InitBSS               := ReadBool   ('Settings', 'Initialize BSS',                 True);
					OptimizeNOPs          := ReadBool   ('Settings', 'Optimize NOPs',                  True);
					OptimizeReturns       := ReadBool   ('Settings', 'Optimize Returns',               True);
					OptimizeBranches      := ReadBool   ('Settings', 'Optimize Branches',              True);
					OptimizeMoves         := ReadBool   ('Settings', 'Optimize Moves',                 True);
					OptimizeTests         := ReadBool   ('Settings', 'Optimize Tests',                 True);
					OptimizeCalculations  := ReadBool   ('Settings', 'Optimize Calculations',          True);
					RemoveUnusedSections  := ReadBool   ('Settings', 'Remove Unused Sections',         True);
					CutUnusedRanges       := ReadBool   ('Settings', 'Cut Unused Ranges',              True);
					ReorderSections       := ReadBool   ('Settings', 'Reorder Sections',               True);
					MergeConstants        := ReadBool   ('Settings', 'Merge Constants',                True);
					OutputBin             := ReadBool   ('Settings', 'Binary Output',                  False);
					CommandLine           := ReadString ('Settings', 'Command Line',                   '');
					PostBuildProcessFile  := ReadString ('Settings', 'Post-Build Process',             '');
					while Pos ('`', PostBuildProcessFile) > 0 do
						PostBuildProcessFile [Pos ('`', PostBuildProcessFile)] := '"';
					if Assigned (PredefinedLibOptions) then
						with PredefinedLibOptions do begin
							CalcDests := [];
							if                                     ReadBool   ('Library Options', 'Use TI-89',                    False) then
								Include (CalcDests, cdTI89);
							if                                     ReadBool   ('Library Options', 'Use TI-92 Plus',               False) then
								Include (CalcDests, cdTI92Plus);
							if                                     ReadBool   ('Library Options', 'Use V200',                     False) then
								Include (CalcDests, cdV200);
							OptimizeCalcConsts                  := ReadBool   ('Library Options', 'Optimize Calc Consts',         False);
							if                                     ReadBool   ('Library Options', 'Use PreOS',                    False) then
								KernelFormat := kfCompressedTables
							else if                                ReadBool   ('Library Options', 'Use Kernel',                   False) then
								KernelFormat := kfStandard
							else
								KernelFormat := kfNone;
							UseMinAMS                           := ReadBool   ('Library Options', 'Minimum AMS Version Defined',  False);
							MinAMS                              := ReadString ('Library Options', 'Minimum AMS Version',          '1.00');
							UnofficialOSSupport                 := ReadBool   ('Library Options', 'Unofficial OS Support',        False);
							RelocFormat    := StringToRelocFormat (ReadString ('Library Options', 'Reloc Format',                 ''));
							ROMCallFormat  := StringToRelocFormat (ReadString ('Library Options', 'ROM Call Format',              ''));
							BSSRefFormat   := StringToRelocFormat (ReadString ('Library Options', 'BSS Ref Format',               ''));
							DataRefFormat  := StringToRelocFormat (ReadString ('Library Options', 'Data Ref Format',              ''));
							UseFLineJumps                       := ReadBool   ('Library Options', 'Use F-Line Jumps',             False);
							Use4ByteFLineJumps                  := ReadBool   ('Library Options', 'Use 4-Byte F-Line Jumps',      False);
							OptimizeROMCalls                    := ReadBool   ('Library Options', 'Optimize ROM Calls',           False);
							UseInternalFLineEmulator            := ReadBool   ('Library Options', 'Use Internal F-Line Emulator', False);
							UseReturnValue                      := ReadBool   ('Library Options', 'Use Return Value',             False);
							EnableErrorReturn                   := ReadBool   ('Library Options', 'Enable Error Return',          False);
							SaveScreen                          := ReadBool   ('Library Options', 'Save Screen',                  False);
						end;
					SL := TStringList.Create;
					try
						ReadSection ('Included Files', SL);
						for I := 0 to SL.Count - 1 do begin
							P := SL.Strings [I];
							S := ReadString ('Included Files', P, '');
							if Length (S) > 0 then begin
								if Copy (P, Length (P) - Length (' Folder') + 1, Length (P)) = ' Folder' then begin
									Delete (P, Length (P) - Length (' Folder') + 1, Length (P));
									J := SL.IndexOf (P);
									if J >= 0 then begin
										Node := TopNode.Item [TSourceFile.GetAppropriateClassFromName (Copy (P, 1, LastPos (' ', P) - 1)).GetClassTreeIndex];
										repeat
											Ps := Pos ('\', S);
											if Ps > 0 then
												P := Copy (S, 1, Ps - 1)
											else
												P := S;
											SubNode := Node.GetFirstChild;
											while Assigned (SubNode) and ((not Assigned (SubNode.Data)) or (not (TObject (SubNode.Data) is TFolder)) or (SubNode.Text <> P)) do
												SubNode := SubNode.GetNextSibling;
											if Assigned (SubNode) then
												Node := SubNode
											else begin
												Folder := TFolder.Create;
												Node := ProjectTree.Items.AddChildObject (Node, P, Folder);
												Folder.TreeItem := Node;
												with Node do begin
													ImageIndex    := 0;
													SelectedIndex := 1;
												end;
											end;
											Delete (S, 1, Length (P) + 1);
										until Length (S) <= 0;
										SL.Objects [J] := Node.Data;
									end;
								end;
							end;
						end;
						for I := 0 to SL.Count - 1 do begin
							P := SL.Strings [I];
							S := ReadString ('Included Files', P, '');
							if Length (S) > 0 then begin
								if Copy (P, Length (P) - Length (' Folder') + 1, Length (P)) <> ' Folder' then begin
									if Pos (':', S) <= 0 then
										S := ExtractFilePath (ProjectFile) + S;
									if FileExists (S) then
										AddSourceFile (S, False, TSourceFile.GetAppropriateClassFromName (Copy (P, 1, LastPos (' ', P) - 1)), TFolder (SL.Objects [I]))
									else
										ShowDefaultMessageBox ('File not found:'#13#10#13#10 + S, 'Error', mtProgramError);
								end;
							end;
						end;
					finally
						SL.Free;
					end;
					S := ReadString ('File Editing', 'Open File', '');
					if Length (S) > 0 then begin
						CurrentFile := SourceFiles.FindFile (S);
						if Assigned (CurrentFile) then
							CurrentFile.Edit;
					end;
				finally
					Free;
				end;
				ProjectTree.FullExpand;
				NoEditor.Show;
				UpdateDebugSettings;
				AddToRecent (ProjectFile);
			end;
			SortFiles;
		finally
			ProjectTree.Items.EndUpdate;
			OpeningProjectNow := False;
			if Assigned (F) then
				F.Free;
		end;
		Modified := False;
	end;
end;

procedure TMainForm.FileNew;
begin
	if not Compiling then begin
		WarnIfModified;
		Modified := False;
		ProjectFile := '';
		FileClear;
	end;
end;

procedure TMainForm.FileClear;
procedure TryDeleteNode(Node: TTreeNode);
var
	I: Integer;
begin
	with Node do begin
		for I := Count - 1 downto 0 do
			TryDeleteNode (Item [I]);
		if Assigned (Data) then
			Delete;
	end;
end;
begin
	if not Compiling then begin
		WarnIfModified;
		Modified := False;
		Invalidated := True;
		ActiveControl := ProjectTree;
		ErrorList.Items.BeginUpdate;
		ErrorList.Items.Clear;
		ErrorList.Items.EndUpdate;
		ProjectTree.Items.BeginUpdate;
		ClearDebugInfo;
		TopNode.Text := 'Project1';
		SelectNode (TopNode);
		TryDeleteNode (TopNode);
		ProjectTree.FullExpand;
		ProjectTree.Items.EndUpdate;
		ResetProjectSettings;
		NoEditor.Show;
		UpdateDebugSettings;
		Update;
		Modified := False;
	end;
end;

procedure TMainForm.FileOpen(const FN: string);
begin
	if not Compiling then begin
		WarnIfModified;
		Modified := False;
		ProjectTree.Items.BeginUpdate;
		ProjectFile := FN;
		FileLoad;
		ProjectTree.Items.EndUpdate;
	end;
end;

procedure TMainForm.FileSave;
function RelocFormatToString(RelocFormat: TRelocFormat): string;
begin
	case RelocFormat of
		rfNone:        Result := 'None';
		rfDirect:      Result := 'Direct';
		rfAMS:         Result := 'AMS';
		rfKernel:      Result := 'Kernel';
		rfCompressed:  Result := 'Compressed';
    rfMLink:       Result := 'MLink';
		rfFLine:       Result := 'F-Line';
		else           Result := 'Unknown';
	end;
end;
var
	I,
	ClassCount: Integer;
	S: string;
	SL: TStringList;
begin
	if ProjectFile = '' then
		FileSaveProjectAs (Self)
	else begin
		try
			with SourceFiles do
				for I := 0 to Count - 1 do
					with Items [I] as TSourceFile do begin
						if UpperCase (ExtractFilePath (FileName)) = 'C:\' then
							FileName := ExtractFilePath (ProjectFile) + LogicalFileName;
						if Modified or not FileExists (FileName) then
							Save;
					end;
			with TIniFile.Create (ProjectFile) do try
				WriteBool   ('Settings', 'Archive',                        ProjectTarget = ptArchive);
				WriteBool   ('Settings', 'Flash OS',                       ProjectTarget = ptFlashOS);
				WriteBool   ('Settings', 'Fargo',                          ProjectTarget = ptFargo);
				WriteBool   ('Settings', 'Use Data Variable',              UseDataVar);
				WriteString ('Settings', 'Data Variable',                  DataVar);
				WriteBool   ('Settings', 'Copy Data Variable',             DataVarCopy);
				WriteBool   ('Settings', 'Copy Data Variable if Archived', DataVarCopyIfArchived);
				WriteBool   ('Settings', 'Pack',                           Pack);
				WriteString ('Settings', 'Packed Variable',                PackVar);
				WriteString ('Settings', 'Project Name',                   TopNode.Text);
				WriteString ('Settings', 'GCC Switches',                   GCCSwitches);
				WriteString ('Settings', 'GNU Assembler Switches',         AsSwitches);
				WriteString ('Settings', 'Assembler Switches',             AsmSwitches);
				WriteBool   ('Settings', 'Debug Info',                     DebugInfo);
				WriteBool   ('Settings', 'Standard Library',               StdLib);
				WriteBool   ('Settings', 'Initialize BSS',                 InitBSS);
				WriteBool   ('Settings', 'Optimize NOPs',                  OptimizeNOPs);
				WriteBool   ('Settings', 'Optimize Returns',               OptimizeReturns);
				WriteBool   ('Settings', 'Optimize Branches',              OptimizeBranches);
				WriteBool   ('Settings', 'Optimize Moves',                 OptimizeMoves);
				WriteBool   ('Settings', 'Optimize Tests',                 OptimizeTests);
				WriteBool   ('Settings', 'Optimize Calculations',          OptimizeCalculations);
				WriteBool   ('Settings', 'Remove Unused Sections',         RemoveUnusedSections);
				WriteBool   ('Settings', 'Cut Unused Ranges',              CutUnusedRanges);
				WriteBool   ('Settings', 'Reorder Sections',               ReorderSections);
				WriteBool   ('Settings', 'Merge Constants',                MergeConstants);
				WriteBool   ('Settings', 'Binary Output',                  OutputBin);
				WriteString ('Settings', 'Command Line',                   CommandLine);
				S := PostBuildProcessFile;
				while Pos ('"', S) > 0 do
					S [Pos ('"', S)] := '`';
				WriteString ('Settings', 'Post-Build Process',             S);
				if Assigned (PredefinedLibOptions) then
					with PredefinedLibOptions do begin
						WriteBool   ('Library Options', 'Use TI-89',                    cdTI89     in CalcDests);
						WriteBool   ('Library Options', 'Use TI-92 Plus',               cdTI92Plus in CalcDests);
						WriteBool   ('Library Options', 'Use V200',                     cdV200     in CalcDests);
						WriteBool   ('Library Options', 'Optimize Calc Consts',         OptimizeCalcConsts);
						WriteBool   ('Library Options', 'Use Kernel',                   KernelFormat <> kfNone);
						WriteBool   ('Library Options', 'Use PreOS',                    KernelFormat = kfCompressedTables);
						WriteBool   ('Library Options', 'Minimum AMS Version Defined',  UseMinAMS);
						WriteString ('Library Options', 'Minimum AMS Version',          MinAMS);
						WriteBool   ('Library Options', 'Unofficial OS Support',        UnofficialOSSupport);
						WriteString ('Library Options', 'Reloc Format',                 RelocFormatToString (RelocFormat));
						WriteString ('Library Options', 'ROM Call Format',              RelocFormatToString (ROMCallFormat));
						WriteString ('Library Options', 'BSS Ref Format',               RelocFormatToString (BSSRefFormat));
						WriteString ('Library Options', 'Data Ref Format',              RelocFormatToString (DataRefFormat));
						WriteBool   ('Library Options', 'Use F-Line Jumps',             UseFLineJumps);
						WriteBool   ('Library Options', 'Use 4-Byte F-Line Jumps',      Use4ByteFLineJumps);
						WriteBool   ('Library Options', 'Optimize ROM Calls',           OptimizeROMCalls);
						WriteBool   ('Library Options', 'Use Internal F-Line Emulator', UseInternalFLineEmulator);
						WriteBool   ('Library Options', 'Use Return Value',             UseReturnValue);
						WriteBool   ('Library Options', 'Enable Error Return',          EnableErrorReturn);
						WriteBool   ('Library Options', 'Save Screen',                  SaveScreen);
					end;
				EraseSection ('Included Files');
				SL := TStringList.Create;
				try
					with SourceFiles do
						for I := 0 to Count - 1 do
							with Items [I] as TSourceFile do
								if InProject then begin
									S := SL.Values [ClassItemName];
									if Length (S) > 0 then
										ClassCount := StrToInt (S) + 1
									else
										ClassCount := 1;
									SL.Values [ClassItemName] := IntToStr (ClassCount);
									WriteString ('Included Files', ClassItemName + ' ' + IntToStr (ClassCount), DynamicName);
									S := FolderPath;
									if Length (S) > 0 then
										WriteString ('Included Files', ClassItemName + ' ' + IntToStr (ClassCount) + ' Folder', S);
								end;
				finally
					SL.Free;
				end;
				S := '';
				if Assigned (ProjectTree.Selected) and Assigned (ProjectTree.Selected.Data) and (TObject (ProjectTree.Selected.Data) is TSourceFile) then
					S := TSourceFile(ProjectTree.Selected.Data).FileName;
				WriteString ('File Editing', 'Open File', S);
				UpdateFile;
			finally
				Free;
			end;
			Modified := False;
		except
			ShowDefaultMessageBox ('Error saving project file.', 'Error', mtProgramError);
		end;
		AddToRecent (ProjectFile);
	end;
end;

procedure TMainForm.FileSaveAs(const FN: string);
var
	I: Integer;
begin
	with SourceFiles do
		for I := 0 to Count - 1 do
			with Items [I] as TSourceFile do
				if Pos ('\', DynamicName) <= 0 then
					WriteToFile (ExtractFilePath (FN) + DynamicName, True);
	ProjectFile := FN;
	FileSave;
end;

procedure TMainForm.ProjectTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
	PreviousNode := nil;
	EditorToHide := nil;
	if Assigned (Node.Data) then
		if TObject (Node.Data) is TSourceFile then begin
			with TSourceFile (Node.Data) do begin
				TreeItem := nil;
				Free;
			end;
		end else
			with TObject (Node.Data) do
				Free;
	ProjectTree.Invalidate;
end;

procedure TMainForm.FileExit(Sender: TObject);
begin
	Close;
end;

procedure TMainForm.FileNewProject(Sender: TObject);
begin
	WarnIfModified;
	Modified := False;
	FileNew;
end;

procedure TMainForm.TreeItemRemove(Sender: TObject);
var
	Node,
	CurNode: TTreeNode;
begin
	Node := ProjectTree.Selected;
	if (not Compiling) and Assigned (Node) and Assigned (Node.Data) then begin
		CurNode := Node;
		while Assigned (CurNode) and (CurNode <> Node.GetNextSibling) do begin
			if Assigned (CurNode.Data) and (TObject (CurNode.Data) is TSourceFile) then
				TSourceFile(CurNode.Data).WarnIfModified;
			CurNode := CurNode.GetNext;
			if Node.GetNextSibling = nil then
				break;
		end;
		Node.Delete;
		Modify;
	end;
end;

procedure TMainForm.TreeItemDelete(Sender: TObject);
var
	Node: TTreeNode;
begin
	Node := ProjectTree.Selected;
	if (not Compiling) and Assigned (Node) and Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) then begin
		if ShowDefaultMessageBox
			('Are you sure you want to delete this source file?  You cannot undo this operation.',
			'Confirm Deletion', mtQuestion) = idYes then begin
			with TSourceFile (Node.Data) do
				if (FileName <> '') and FileExists (FileName) then
					DeleteFile (FileName);
			Node.Delete;
			Modify;
		end;
	end;
end;

procedure TMainForm.CopyHeaders;
var
	I,
	J: Integer;
	HasA68kFiles: Boolean;
begin
	HasA68kFiles := False;
	with SourceFiles do begin
		for I := 0 to Count - 1 do
			if Items [I] is TAsmSourceFile then begin
				HasA68kFiles := True;
				Break;
			end;
		if HasA68kFiles then
			with TFileReferences.Create do try
				SearchForFiles (WithBackslash (TIGCCFolder) + ASMIncludeLocation + '*.*', atAll);
				for J := 0 to Count - 1 do
					CopyFile (PChar (Items[J].FullName), PChar (Temp + ExtractFileName (Items[J].FullName)), False);
			finally
				Free;
			end;
		for I := 0 to Count - 1 do begin
			if Items [I] is THeaderSourceFile then begin
				with Items [I] as THeaderSourceFile do begin
					if Invalidated then
						RecompileFiles;
					if InProject then
						SplitAndWriteToFile (Temp + LogicalFileName);
				end;
			end else if Items [I] is TOtherSourceFile then
				with Items [I] as TSourceFile do
					if InProject then
						WriteToFile (Temp + LogicalFileName);
		end;
	end;
end;

procedure TMainForm.DeleteHeaders;
var
	I: Integer;
	FN: string;
begin
	with TFileReferences.Create do try
		SearchForFiles (Temp + '*.h', atAll);
		for I := 0 to Count - 1 do try
			DeleteFile (Items[I].FullName);
		except end;
	finally
		Free;
	end;
	with SourceFiles do
		for I := 0 to Count - 1 do
			if (Items [I] is THeaderSourceFile) or (Items [I] is TOtherSourceFile) then
				with Items [I] as TSourceFile do begin
					FN := Temp + LogicalFileName;
					if FileExists (FN) then try
						DeleteFile (FN);
					except end;
					RemovePathFor (FN, Temp);
				end;
end;

procedure TMainForm.TreeItemCompile(Sender: TObject);
var
	Node: TTreeNode;
begin
	Node := ProjectTree.Selected;
	if (not Compiling) and Assigned (Node) and Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) and (TSourceFile(Node.Data).Compilable) then begin
		BeginCompilation;
		CopyHeaders;
		TSourceFile(Node.Data).Compile;
		DeleteHeaders;
		EndCompilation;
	end;
end;

procedure TMainForm.CloseErrorsButtonClick(Sender: TObject);
begin
	HideErrors;
end;

procedure TMainForm.AddError(const Line: string; BugTp: TBugType; SourceF: string; ErrFunc: string; ErrMessage: string; SourceLn: Integer; Offset: Integer);
var
	I,
	J: Integer;
	O: TFoundError;
	Ignore: Boolean;
	Token: string;
begin
	ErrMessage := Trim (ErrMessage);
	if ErrMessage <> '' then begin
		Ignore := False;
		if BugTp in [btWarning, btInfo] then begin
			if ((ErrFunc = '') and (UpperCase (ErrMessage) = '''STATIC'' IS NOT AT BEGINNING OF DECLARATION'))
				or (Pos ('__DUMMY__', UpperCase (ErrMessage)) > 0)
				or (Pos ('''__R''', UpperCase (ErrMessage)) > 0)
				or (Pos ('MULTI-LINE STRING LITERALS ARE DEPRECATED', UpperCase (ErrMessage)) > 0) then
				Ignore := True;
		end else begin
			if (Pos ('(EACH UNDECLARED IDENTIFIER IS REPORTED ONLY ONCE', UpperCase (ErrMessage)) > 0)
				or (Pos ('FOR EACH FUNCTION IT APPEARS IN.)', UpperCase (ErrMessage)) > 0) then
				Ignore := True;
		end;
		if not Ignore then begin
			if ErrFunc = '__main' then
				ErrFunc := '_main';
			if ErrFunc = '__exit' then
				ErrFunc := '_exit';
			ErrorList.Items.BeginUpdate;
			try
				ErrMessage [1] := UpCase (ErrMessage [1]);
				O := TFoundError.Create;
				with O do begin
					SourceFile := nil;
					if (Length (SourceF) > 0) and (LowerCase (ExtractFileExt (SourceF)) <> '.exe') and (LowerCase (ExtractFileExt (SourceF)) <> '.a') then try
						if Pos ('\', SourceF) > 0 then begin
							SourceFile := SourceFiles.FindFile (SourceF);
							if (not Assigned (SourceFile)) and (LowerCase (ExtractFileExt (SourceF)) = '.o') then begin
								SourceFile := SourceFiles.FindFile (ChangeFileExt (SourceF, '.c'));
								if not Assigned (SourceFile) then
									SourceFile := SourceFiles.FindFile (ChangeFileExt (SourceF, '.s'));
								if not Assigned (SourceFile) then
									SourceFile := SourceFiles.FindFile (ChangeFileExt (SourceF, '.asm'));
							end;
						end else begin
							SourceFile := SourceFiles.FindFileNameOnly (SourceF);
							if (not Assigned (SourceFile)) and (LowerCase (ExtractFileExt (SourceF)) = '.o') then begin
								SourceFile := SourceFiles.FindFileNameOnly (ChangeFileExt (SourceF, '.c'));
								if not Assigned (SourceFile) then
									SourceFile := SourceFiles.FindFileNameOnly (ChangeFileExt (SourceF, '.s'));
								if not Assigned (SourceFile) then
									SourceFile := SourceFiles.FindFileNameOnly (ChangeFileExt (SourceF, '.asm'));
							end;
						end;
						if not Assigned (SourceFile) then begin
							if (Pos ('\', SourceF) > 0) and FileExists (SourceF) then
								SourceFile := AddSourceFile (SourceF, True)
							else if FileExists (WithBackslash (TIGCCFolder) + CIncludeLocation + ExtractFileName (SourceF)) then
								SourceFile := AddSourceFile (WithBackslash (TIGCCFolder) + CIncludeLocation + ExtractFileName (SourceF), True)
							else if FileExists (WithBackslash (TIGCCFolder) + ASMIncludeLocation + ExtractFileName (SourceF)) then
								SourceFile := AddSourceFile (WithBackslash (TIGCCFolder) + ASMIncludeLocation + ExtractFileName (SourceF), True)
							else if FileExists (WithBackslash (TIGCCFolder) + GASIncludeLocation + ExtractFileName (SourceF)) then
								SourceFile := AddSourceFile (WithBackslash (TIGCCFolder) + GASIncludeLocation + ExtractFileName (SourceF), True);
						end;
					except end;
					BugType := BugTp;
					WholeLine := Line;
					ErrorMessage := ErrMessage;
					ErrFunction := ErrFunc;
					if Assigned (SourceFile) and (SourceFile is TTextSourceFile) then
						with SourceFile as TTextSourceFile do
							if Assigned (TextEditor) then begin
								I := GetCompiledLineStart (SourceLn) + Offset;
								if I > 0 then begin
									if Pos ('''', ErrorMessage) > 0 then begin
										Token := Copy (ErrorMessage, Pos ('''', ErrorMessage) + 1, Length (ErrorMessage));
										Delete (Token, Pos ('''', Token), Length (Token));
										if Length (Token) > 0 then begin
											J := I;
											while (J <= TextEditor.TextLength) and (TextEditor.Text [J] in [#9, #10, #13, #32]) and (Copy (TextEditor.Text, J, Length (Token)) <> Token) do
												Inc (J);
											if Copy (TextEditor.Text, J, Length (Token)) = Token then
												I := J;
										end;
									end;
									O.Range := TMCRange.Create (TextEditor.TrackedRanges);
									O.Range.RStart := I;
									O.Range.OnOverwrite := DeleteError;
								end else
									O.Range := nil;
							end;
				end;
				with ErrorList.Items.Add do begin
					Data := Pointer (O);
					ImageIndex := Integer (BugTp);
					Caption := ErrMessage;
					if Assigned (O.SourceFile) then
						SubItems.Add (O.SourceFile.SourceName)
					else
						SubItems.Add ('');
					SubItems.Add (ErrFunc);
					ShowErrors;
					if JumpToError and (BugTp = btError) and (not Assigned (ErrorList.Selected)) then begin
						Selected := True;
						ErrorListClick (Self);
					end;
				end;
			except end;
			ErrorList.Items.EndUpdate;
		end;
	end;
	UpdateErrorWindow;
end;

procedure TMainForm.ClearErrors;
begin
	HideErrors;
	with ErrorList.Items do begin
		BeginUpdate;
		Clear;
		EndUpdate;
	end;
	ActionProjectShowErrors.Enabled := False;
end;

procedure TMainForm.HideErrors;
begin
	Splitter2.Hide;
	ErrWinPanel.Hide;
	ActionProjectShowErrors.Checked := False;
end;

procedure TMainForm.ShowErrors;
begin
	if ErrorList.Items.Count > 0 then begin
		ErrWinPanel.Show;
		Splitter2.Show;
		Splitter2.Top := ErrWinPanel.Top - Splitter2.Height;
		ActionProjectShowErrors.Checked := True;
		ErrorList.Refresh;
	end;
end;

procedure TMainForm.ErrorListDeletion(Sender: TObject; Item: TListItem);
var
	O: TObject;
begin
	if Assigned (Item.Data) then begin
		O := TObject(Item.Data);
		Item.Data := nil;
		O.Free;
	end;
end;

procedure TMainForm.UpdateErrorWindow;
var
	I: Integer;
	EC,
	WC: Integer;
begin
	with ErrorList, Items do begin
		EC := 0;
		WC := 0;
		for I := 0 to Count - 1 do
			with TFoundError (Item[I].Data) do
				case BugType of
					btWarning:
						Inc (WC);
					btError:
						Inc (EC);
				end;
		ErrorsLabel.Caption := IntToStr (EC);
		WarningsLabel.Caption := IntToStr (WC);
		if Count <= 0 then
			HideErrors;
	end;
	ActionProjectShowErrors.Enabled := ErrorList.Items.Count > 0;
	if not ActionProjectShowErrors.Enabled then
		ActionProjectShowErrors.Checked := False;
end;

procedure TMainForm.TreeItemSave(Sender: TObject);
var
	Node: TTreeNode;
begin
	Node := ProjectTree.Selected;
	if Assigned (Node) and Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) then
		TSourceFile(Node.Data).Save;
end;

procedure TMainForm.HelpDocumentation(Sender: TObject);
begin
	try
		DocFile.Display;
	except
		ShowDefaultMessageBox ('Error opening documentation.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.EditorEnter(Sender: TObject);
begin
	ActionEditPaste.Enabled := True;
	ActionEditSelectAll.Enabled := True;
	ActionEditIncreaseIndent.Enabled := True;
	ActionEditDecreaseIndent.Enabled := True;
	ActionFindOpenFile.Enabled := True;
	UpdateEditButtons;
end;

procedure TMainForm.EditorExit(Sender: TObject);
begin
	ActionEditDelete.Enabled := False;
	ActionEditCut.Enabled := False;
	ActionEditCopy.Enabled := False;
	ActionEditPaste.Enabled := False;
	ActionEditSelectAll.Enabled := False;
	ActionEditIncreaseIndent.Enabled := False;
	ActionEditDecreaseIndent.Enabled := False;
	ActionFindOpenFile.Enabled := False;
	ActionEditUndo.Enabled := False;
	ActionEditRedo.Enabled := False;
end;

procedure TMainForm.EditUndo(Sender: TObject);
begin
	if Assigned (CurrentEditor) then
		CurrentEditor.Undo;
end;

procedure TMainForm.EditRedo(Sender: TObject);
begin
	if Assigned (CurrentEditor) then
		CurrentEditor.Redo;
end;

procedure TMainForm.EditClear(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) and (ActiveControl = Editor) then
		Editor.ClearSelection
	else if ActiveControl = ProjectTree then
		TreeItemRemove (Sender);
end;

procedure TMainForm.EditCut(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) and (ActiveControl = Editor) then
		Editor.CutToClipboard;
end;

procedure TMainForm.EditCopy(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) and (ActiveControl = Editor) then
		Editor.CopyToClipboard;
end;

procedure TMainForm.EditPaste(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) and (ActiveControl = Editor) then
		Editor.PasteFromClipboard;
end;

procedure TMainForm.EditSelectAll(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) and (ActiveControl = Editor) then
		Editor.SelectAll;
end;

procedure TMainForm.ProjectCompile(Sender: TObject);
var
	I: Integer;
begin
	if not Compiling then begin
		if AutoSave then
			FileSave
		else
			with SourceFiles do
				for I := 0 to Count - 1 do
					with Items [I] as TSourceFile do
						if not InProject then
							Save;
		BeginCompilation;
		CompileProject;
		EndCompilation;
	end;
end;

procedure TMainForm.ProjectMake(Sender: TObject);
begin
	MakeFileWrapped;
	DisplayFolderMessage;
end;

procedure TMainForm.MakeFileWrapped;
var
	I: Integer;
begin
	if not Compiling then begin
		if AutoSave then
			FileSave
		else
			with SourceFiles do
				for I := 0 to Count - 1 do
					with Items [I] as TSourceFile do
						if not InProject then
							Save;
		OperationSuccessful := False;
		if ProjectFile = '' then
			ShowDefaultMessageBox ('You need to save your project before you can create executable files.', 'Project not Saved', mtProgramError)
		else begin
			BeginCompilation;
			CompileProject;
			Application.ProcessMessages;
			if OperationSuccessful and not OperationCancelled then
				MakeFile;
			EndCompilation;
		end;
	end;
end;

procedure TMainForm.MakeFile;
	procedure FileNotCompiled(const FileName: string);
	begin
		ShowDefaultMessageBox
			('The file ''' + FileName + ''' has not been compiled.  You have to compile it first before you can link it to your project.',
			'No Object File', mtProgramError);
	end;
var MainFiles: array [TCalcDest] of string;
	procedure HandleContents(const ProjectFile, FolderName, VarName, DataFolderName, DataVarName: string; Pack: Boolean; const PackVar: string; CalcDest: TCalcDest);
	var
		InputStream,
		OutputStream: TMemoryStream;
		OutputSize: LongWord;
		FileSize: Integer;
		F: file;
		B: Byte;
	begin
		CompUpdate;
		OperationSuccessful := False;
		with LinkOutputFiles [CalcDest, frMain] do
			if Assigned (Data) then begin
				OutputStream := TMemoryStream.Create;
				try
					if OSUpgrade then begin
						OutputSize := GetOSUpgradeFileSize (Data.Size, OutputBin);
						OutputStream.Size := OutputSize;
						ProduceOSUpgradeFile (OutputStream.Memory, Data.Memory, Data.Size, OutputBin);
						MainFiles [CalcDest] := ChangeFileExt (ProjectFile, GetOSUpgradeFileExt (CalcDest, OutputBin));
						OutputStream.SaveToFile (MainFiles [CalcDest]);
						if LongWord (ProgSize) < OutputSize then
							ProgSize := OutputSize;
						OperationSuccessful := True;
					end else begin
						FileSize := 2 + Data.Size + 1;
						if Pack then begin
							AssignFile (F, Temp + 'tempprog.bin');
							Rewrite (F, 1);
							B := (FileSize - 2) shr 8;
							BlockWrite (F, B, 1);
							B := (FileSize - 2);
							BlockWrite (F, B, 1);
							BlockWrite (F, Data.Memory^, Data.Size);
							B := Tag;
							BlockWrite (F, B, 1);
							CloseFile (F);
							MainConsole.Title := 'Compressor';
							CompUpdate;
							try
								MainConsole.StartProcess (WithBackslash (TIGCCFolder) + PackLocation + 'Pack.exe', 'tempprog.bin tempprog.pck', WithoutBackslash (Temp));
								WaitForMainConsole ('Compression');
							except
								ShowDefaultMessageBox ('Could not start compressor.', 'Error', mtProgramError);
							end;
							CompUpdate;
							if not OperationCancelled then begin
								if FileExists (Temp + 'tempprog.pck') then begin
									InputStream := TMemoryStream.Create;
									with InputStream do try
										LoadFromFile (Temp + 'tempprog.pck');
										OutputSize := GetTransferFileSize (Size, 'ppg', OutputBin);
										if OutputSize > 0 then begin
											OutputStream.Size := OutputSize;
											ProduceTransferFile (OutputStream.Memory, Memory, Size, CalcDest, FolderName, PackVar, $F8, 'ppg', OutputBin);
											MainFiles [CalcDest] := ChangeFileExt (ProjectFile, GetTransferFileExt (CalcDest, $F8, OutputBin));
											OutputStream.SaveToFile (MainFiles [CalcDest]);
											if LongWord (ProgSize) < OutputSize then
												ProgSize := OutputSize;
											OperationSuccessful := True;
										end else
											ShowDefaultMessageBox ('Compressed size of ' + IntToStr (Size) + ' bytes is too large, unexpectedly.', 'Error', mtProgramError);
									finally
										Free;
									end;
								end else
									ShowDefaultMessageBox ('Compression failed. Please check the program size.', 'Error', mtProgramError);
							end;
							if FileExists (Temp + 'tempprog.bin') then
								DeleteFile (Temp + 'tempprog.bin');
							if FileExists (Temp + 'tempprog.pck') then
								DeleteFile (Temp + 'tempprog.pck');
						end else begin
							OutputSize := GetTransferFileSize (Data.Size, VarExt, OutputBin);
							if OutputSize > 0 then begin
								OutputStream.Size := OutputSize;
								ProduceTransferFile (OutputStream.Memory, Data.Memory, Data.Size, CalcDest, FolderName, VarName, Tag, VarExt, OutputBin);
								MainFiles [CalcDest] := ChangeFileExt (ProjectFile, GetTransferFileExt (CalcDest, Tag, OutputBin));
								OutputStream.SaveToFile (MainFiles [CalcDest]);
								if LongWord (ProgSize) < OutputSize then
									ProgSize := OutputSize;
								OperationSuccessful := True;
							end else
								ShowDefaultMessageBox ('Variable size of ' + IntToStr (FileSize) + ' bytes is too large, unexpectedly.', 'Error', mtProgramError);
						end;
					end;
				finally
					OutputStream.Free;
				end;
				CompUpdate;
			end else
				ShowDefaultMessageBox ('Unexpected destination calculator mismatch.', 'Error', mtProgramError);
		with LinkOutputFiles [CalcDest, frData] do
			if Assigned (Data) and (not OSUpgrade) then begin
				FileSize := 2 + Data.Size + 1;
				OutputStream := TMemoryStream.Create;
				try
					OutputSize := GetTransferFileSize (Data.Size, VarExt, OutputBin);
					if OutputSize > 0 then begin
						OutputStream.Size := OutputSize;
						ProduceTransferFile (OutputStream.Memory, Data.Memory, Data.Size, CalcDest, DataFolderName, DataVarName, Tag, VarExt, OutputBin);
						OutputStream.SaveToFile (ChangeFileExt (ChangeFileExt (ProjectFile, '') + '-data', GetTransferFileExt (CalcDest, Tag, OutputBin)));
						OperationSuccessful := True;
					end else
						ShowDefaultMessageBox ('Data variable size of ' + IntToStr (FileSize) + ' bytes is too large, unexpectedly.', 'Error', mtProgramError);
				finally
					OutputStream.Free;
				end;
				CompUpdate;
			end;
	end;
	procedure HandleDebugContents(const ProjectFile: string);
	begin
		with LinkDebugFile do
			if Assigned (Data) then
				Data.SaveToFile (ChangeFileExt (ProjectFile, '.dbg'));
	end;
	procedure CreatePackStarter(const ProjectFile, StarterFileName, FolderName, VarName, PackVar: string; CalcDests: TCalcDests);
	var
		CurCalcDest: TCalcDest;
		CustomStarterObject: string;
		ObjectFileNames: array [0..1] of PChar;
		DataVarInfo: TLinkLibDataVarInfo;
		OptimizeInfo: TLinkLibOptimizeInfo;
	begin
		CompUpdate;
		OperationSuccessful := False;
		for CurCalcDest := FirstCalcDest to LastCalcDest do begin
			LinkOutputFiles[CurCalcDest,frMain].Data := nil;
			LinkOutputFiles[CurCalcDest,frData].Data := nil;
		end;
		try
			CustomStarterObject := Temp + StarterFileName;
			if FileExists (WithBackslash (TIGCCFolder) + PStarterLocation + StarterFileName) then try
				ParsePStarter (WithBackslash (TIGCCFolder) + PStarterLocation + StarterFileName, CustomStarterObject, PackVar);
			except
				ShowDefaultMessageBox ('Error processing starter object file.', 'Error', mtProgramError);
				Exit;
			end else begin
				ShowDefaultMessageBox ('Cannot find starter object file.', 'Error', mtProgramError);
				Exit;
			end;
			if not OperationCancelled then begin
				ObjectFileNames [0] := PChar (CustomStarterObject);
				ObjectFileNames [1] := nil;
				FillChar (DataVarInfo, SizeOf (DataVarInfo), 0);
				FillChar (OptimizeInfo, SizeOf (OptimizeInfo), 0);
				if Assigned (LinkLibLinkFiles) then
					OperationSuccessful := LinkLibLinkFiles (@ObjectFileNames, nil, LinkLibError, LinkLibGetOutputFile, nil, False, False, False, DataVarInfo, OptimizeInfo, False) = 0
				else
					ShowDefaultMessageBox ('Linker not loaded.', 'Error', mtProgramError);
			end;
			for CurCalcDest := cdTI89 to cdV200 do
				if OperationSuccessful and (not OperationCancelled) and (CurCalcDest in CalcDests) then
					HandleContents (ProjectFile, FolderName, VarName, '', '', False, '', CurCalcDest);
		finally
			for CurCalcDest := FirstCalcDest to LastCalcDest do begin
				if Assigned (LinkOutputFiles[CurCalcDest,frMain].Data) then
					LinkOutputFiles[CurCalcDest,frMain].Data.Free;
				if Assigned (LinkOutputFiles[CurCalcDest,frData].Data) then
					LinkOutputFiles[CurCalcDest,frData].Data.Free;
				LinkOutputFiles[CurCalcDest,frMain].Data := nil;
				LinkOutputFiles[CurCalcDest,frData].Data := nil;
			end;
			if FileExists (CustomStarterObject) then
				DeleteFile (CustomStarterObject);
		end;
	end;
var
	FolderName,
	VarName,
	DataFolderName,
	DataVarName: array [0..MaxNameLength] of Char;
	I,
	P: Integer;
	S1,
	DestFile: string;
	Node: TTreeNode;
	SourceFile: TSourceFile;
	FileNameList: TStringList;
	ObjectFileNames,
	ArchiveFileNames: PPChar;
	ObjectFileCount,
	ArchiveFileCount: Integer;
	DataVarInfo: TLinkLibDataVarInfo;
	CalcDests: TCalcDests;
	CurCalcDest: TCalcDest;
	SR: TSearchRec;
begin
	UpdateErrorWindow;
	OperationCancelled := False;
	OperationSuccessful := False;
	ProgSize := 0;
	CalcDests := [];
	OperationSuccessful := True;
	with TopNode do begin
		I := Pos ('\', Text);
		if I > 0 then begin
			StrPLCopy (VarName, LowerCase (Copy (Text, I + 1, MaxNameLength)), MaxNameLength);
			StrPLCopy (FolderName, LowerCase (Copy (Text, 1, I - 1)), MaxNameLength);
			CharLower (FolderName);
		end else begin
			StrPLCopy (VarName, LowerCase (Text), MaxNameLength);
			FolderName := 'main';
		end;
		CharLower (VarName);
	end;
	with SourceFiles do
		for I := 0 to Count - 1 do
			with TSourceFile (Items [I]) do
				if InProject and Compilable and (not FileExists (ChangeFileExt (FileName, '.o'))) then begin
					OperationSuccessful := False;
					FileNotCompiled (SourceName);
				end;
	I := Pos ('\', DataVar);
	if I > 0 then begin
		StrPLCopy (DataVarName, LowerCase (Copy (DataVar, I + 1, MaxNameLength)), MaxNameLength);
		StrPLCopy (DataFolderName, LowerCase (Copy (DataVar, 1, I - 1)), MaxNameLength);
		CharLower (DataFolderName);
	end else begin
		StrPLCopy (DataVarName, LowerCase (DataVar), MaxNameLength);
		DataFolderName := FolderName;
	end;
	CharLower (DataVarName);
	if OperationSuccessful then begin
		OperationSuccessful := False;
		if Assigned (LinkLibLinkFiles) and Assigned (LinkLibCreateArchive) then begin
			CompStartFile;
			CompSetMessage ('Linking Project ''' + WithoutExt (ExtractFileName (ProjectFile)) + '''');
			if FileExists (ChangeFileExt (ProjectFile, '-titanium.89z')) then
				DeleteFile (ChangeFileExt (ProjectFile, '-titanium.89z'));
			ObjectFileCount := 0;
			ObjectFileNames := nil;
			ArchiveFileCount := 0;
			ArchiveFileNames := nil;
			FileNameList := TStringList.Create;
			try
				Node := TopNode;
				while Assigned (Node) do begin
					if Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) then begin
						SourceFile := Node.Data;
						if Assigned (SourceFile) then
							with SourceFile do
								if InProject then begin
									if SourceFile is TArchiveSourceFile then begin
										Inc (ArchiveFileCount);
										ReallocMem (ArchiveFileNames, SizeOf (PChar) * (ArchiveFileCount + 1));
										ArchiveFileNames [ArchiveFileCount - 1] := PChar (FileNameList.Strings [FileNameList.Add (FileName)]);
										ArchiveFileNames [ArchiveFileCount - 0] := nil;
									end else if Compilable or (SourceFile is TObjectSourceFile) then begin
										Inc (ObjectFileCount);
										ReallocMem (ObjectFileNames, SizeOf (PChar) * (ObjectFileCount + 1));
										ObjectFileNames [ObjectFileCount - 1] := PChar (FileNameList.Strings [FileNameList.Add (ChangeFileExt (FileName, '.o'))]);
										ObjectFileNames [ObjectFileCount] := nil;
									end;
								end;
					end;
					Node := Node.GetNext;
				end;
				if StdLib then begin
					if ProjectTarget = ptFlashOS then
						S1 := WithBackslash (TIGCCFolder) + StdLibLocation + 'flashos.a'
					else if ProjectTarget = ptFargo then
						S1 := WithBackslash (TIGCCFolder) + StdLibLocation + 'fargo.a'
					else
						S1 := WithBackslash (TIGCCFolder) + StdLibLocation + 'tigcc.a';
					if FileExists (S1) then begin
						Inc (ArchiveFileCount);
						ReallocMem (ArchiveFileNames, SizeOf (PChar) * (ArchiveFileCount + 1));
						ArchiveFileNames [ArchiveFileCount - 1] := PChar (FileNameList.Strings [FileNameList.Add (S1)]);
						ArchiveFileNames [ArchiveFileCount] := nil;
					end;
				end;
				if ProjectTarget = ptArchive then begin
					DestFile := ChangeFileExt (ProjectFile, '.a');
					OperationSuccessful := LinkLibCreateArchive (PChar (DestFile), ObjectFileNames, LinkLibError, True) = 0;
					if OperationSuccessful then begin
						if FindFirst (DestFile, faAnyFile, SR) = 0 then
							ProgSize := SR.Size;
						FindClose (SR);
					end;
				end else begin
					for CurCalcDest := FirstCalcDest to LastCalcDest do begin
						LinkOutputFiles[CurCalcDest,frMain].Data := nil;
						LinkOutputFiles[CurCalcDest,frData].Data := nil;
					end;
					LinkDebugFile.Data := nil;
					try
						FillChar (DataVarInfo, SizeOf (DataVarInfo), 0);
						FillChar (OptimizeInfo, SizeOf (OptimizeInfo), 0);
						if UseDataVar then
							with DataVarInfo do begin
								VarName := PChar (LowerCase (MasterUnit.DataVar));
								CreateCopy := MasterUnit.DataVarCopy;
								CopyOnlyIfArchived := MasterUnit.DataVarCopyIfArchived;
							end;
						with OptimizeInfo do begin
							RemoveUnused     := MasterUnit.RemoveUnusedSections;
							OptimizeNOPs     := MasterUnit.OptimizeNOPs;
							OptimizeReturns  := MasterUnit.OptimizeReturns;
							OptimizeBranches := MasterUnit.OptimizeBranches;
							OptimizeMoves    := MasterUnit.OptimizeMoves;
							OptimizeTests    := MasterUnit.OptimizeTests;
							OptimizeCalcs    := MasterUnit.OptimizeCalculations;
							CutRanges        := MasterUnit.CutUnusedRanges;
							ReorderSections  := MasterUnit.ReorderSections;
							MergeConstants   := MasterUnit.MergeConstants;
						end;
						OperationSuccessful := LinkLibLinkFiles (ObjectFileNames, ArchiveFileNames, LinkLibError, LinkLibGetOutputFile, nil, False, ProjectTarget = ptFlashOS, ProjectTarget = ptFargo, DataVarInfo, OptimizeInfo, not InitBSS) = 0;
						if OperationSuccessful and (not OperationCancelled) then begin
							if Pack and (ssPack in SpecialSupport) then begin
								CompStartFile;
								CompSetMessage ('Compressing');
							end;
							for CurCalcDest := FirstCalcDest to LastCalcDest do
								if Assigned (LinkOutputFiles[CurCalcDest,frMain].Data) then
									if OperationSuccessful and (not OperationCancelled) then begin
										Include (CalcDests, CurCalcDest);
										HandleContents (ProjectFile, FolderName, VarName, DataFolderName, DataVarName, Pack and (ssPack in SpecialSupport) and (CurCalcDest <> cdTI92), PackVar, CurCalcDest);
									end;
							if Assigned (LinkDebugFile.Data) then begin
								if OperationSuccessful and (not OperationCancelled) then begin
									HandleDebugContents (ProjectFile);
								end;
							end;
						end;
					finally
						for CurCalcDest := FirstCalcDest to LastCalcDest do begin
							if Assigned (LinkOutputFiles[CurCalcDest,frMain].Data) then
								LinkOutputFiles[CurCalcDest,frMain].Data.Free;
							if Assigned (LinkOutputFiles[CurCalcDest,frData].Data) then
								LinkOutputFiles[CurCalcDest,frData].Data.Free;
							LinkOutputFiles[CurCalcDest,frMain].Data := nil;
							LinkOutputFiles[CurCalcDest,frData].Data := nil;
							if Assigned (LinkDebugFile.Data) then
								LinkDebugFile.Data.Free;
							LinkDebugFile.Data := nil;
						end;
					end;
				end;
			except
				if Assigned (ArchiveFileNames) then
					FreeMem (ArchiveFileNames);
				if Assigned (ObjectFileNames) then
					FreeMem (ObjectFileNames);
				FileNameList.Free;
			end;
			if OperationSuccessful and (not OperationCancelled) and Pack and (ssPack in SpecialSupport) then
				CreatePackStarter (ProjectFile, 'PStarter.o', FolderName, VarName, PackVar, CalcDests - [cdTI92]);
			if OperationSuccessful and (not OperationCancelled) and (Length (PostBuildProcessFile) > 0) then begin
				CompStartFile;
				CompSetMessage ('Calling User-Defined Program');
				MainConsole.Title := 'User-Defined Program';
				S1 := PostBuildProcessFile;
				P := Pos ('($TI89FILE)', UpperCase (S1));
				if P > 0 then begin
					Delete (S1, P, Length ('($TI89FILE)'));
					if cdTI89 in CalcDests then
						Insert (MainFiles [cdTI89], S1, P);
				end;
				P := Pos ('($TI92PLUSFILE)', UpperCase (S1));
				if P > 0 then begin
					Delete (S1, P, Length ('($TI92PLUSFILE)'));
					if cdTI92Plus in CalcDests then
						Insert (MainFiles [cdTI92Plus], S1, P);
				end;
				P := Pos ('($V200FILE)', UpperCase (S1));
				if P > 0 then begin
					Delete (S1, P, Length ('($V200FILE)'));
					if cdV200 in CalcDests then
						Insert (MainFiles [cdV200], S1, P);
				end;
				P := Pos ('($TI92FILE)', UpperCase (S1));
				if P > 0 then begin
					Delete (S1, P, Length ('($TI92FILE)'));
					if cdTI92 in CalcDests then
						Insert (MainFiles [cdTI92], S1, P);
				end;
				try
					MainConsole.StartProcess ('', S1, '');
					WaitForMainConsole ('User-Defined');
				except
					ShowDefaultMessageBox ('Could not start the user-defined program.', 'Error', mtProgramError);
					OperationSuccessful := False;
				end;
				UpdateProgramOutput;
			end;
			if not OperationSuccessful then
				ProgSize := 0;
			CompUpdate;
			try
				if FileExists (Temp + 'TEMPPROG.PCK') then
					DeleteFile (Temp + 'TEMPPROG.PCK');
				if OperationSuccessful and (not OperationCancelled) and DeleteAssemblyFiles then
					with SourceFiles do
						for I := 0 to Count - 1 do
							with TSourceFile (Items [I]) do
								if InProject and (Items [I] is TCSourceFile) then
									if FileExists (ChangeFileExt (FileName, '.s')) then
										DeleteFile (ChangeFileExt (FileName, '.s'));
				if OperationSuccessful and (not OperationCancelled) and DeleteObjectFiles then
					with SourceFiles do
						for I := 0 to Count - 1 do
							with TSourceFile (Items [I]) do
								if InProject and Compilable then begin
									Invalidate;
									if FileExists (ChangeFileExt (FileName, '.o')) then
										DeleteFile (ChangeFileExt (FileName, '.o'));
								end;
			except end;
			UpdateErrorWindow;
			ShowErrors;
			CompUpdate;
			if OperationSuccessful and (not OperationCancelled) then begin
				Invalidated := False;
				CompUpdate;
			end;
		end else
			ShowDefaultMessageBox ('Linker not loaded.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.FileOpenProject(Sender: TObject);
begin
	if OpenProjectDlg.Execute then begin
		if LowerCase (ExtractFileExt (OpenProjectDlg.FileName)) = LowerCase (ProjectFileExt) then
			FileOpen (ExpandFileName (OpenProjectDlg.FileName))
		else
			AddSourceFile (OpenProjectDlg.FileName, True);
	end else
		Abort;
end;

procedure TMainForm.FileSaveAll(Sender: TObject);
begin
	FileSave;
end;

procedure TMainForm.FileSaveProjectAs(Sender: TObject);
begin
	SaveProjectDlg.FileName := ProjectFile;
	if SaveProjectDlg.Execute then
		FileSaveAs (ExpandFileName (SaveProjectDlg.FileName))
	else
		Abort;
end;

procedure TMainForm.EditorKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
var
	I: Integer;
	S: string;
begin
	if Key = vk_F1 then
		if Sender is TMemoComponent then
			with Sender as TMemoComponent do begin
				S := '';
				for I := Selection.RStart - 1 downto 1 do begin
					if Text [I] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$', '#'] then
						S := Text [I] + S
					else
						Break;
				end;
				for I := Selection.RStart to TextLength do begin
					if Text [I] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$', '#'] then
						S := S + Text [I]
					else
						Break;
				end;
				if S <> '' then begin
					with SourceFiles do
						for I := 0 to Count - 1 do
							with Items [I] as TSourceFile do
								if Assigned (ParentForm) then
									ParentForm.WindowState := wsMinimized;
					Application.ProcessMessages;
					DocFile.KeywordLookup (S);
				end;
			end;
end;

procedure TMainForm.ProjectBuild(Sender: TObject);
var
	I: Integer;
begin
	if not Compiling then begin
		if AutoSave then
			FileSave
		else
			with SourceFiles do
				for I := 0 to Count - 1 do
					with Items [I] as TSourceFile do
						if not InProject then
							Save;
		OperationSuccessful := False;
		if ProjectFile = '' then
			ShowDefaultMessageBox ('You need to save your project before you can create executable files.', 'Project not Saved', mtProgramError)
		else begin
			BeginCompilation;
			RecompileFiles;
			CompileProject;
			Application.ProcessMessages;
			if OperationSuccessful and not OperationCancelled then
				MakeFile;
			EndCompilation;
			DisplayFolderMessage;
		end;
	end;
end;

procedure TMainForm.ProjectTreeKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (Key = vk_Delete) and not ProjectTree.IsEditing then
		TreeItemRemove (Sender);
end;

procedure TMainForm.ProjectTreeMouseUp(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight then
		with ProjectTree do
			if Assigned (Selected) then begin
				if Assigned (Selected.Data) then begin
					if TObject (Selected.Data) is TSourceFile then begin
						with TSourceFile (Selected.Data) do begin
							ActionTreeItemSave.Enabled := TSourceFile (Selected.Data) is TTextSourceFile;
							ActionTreeItemSaveAs.Enabled := True;
							ActionTreeItemCompile.Enabled := Compilable;
						end;
						with Mouse.CursorPos do
							SourceFilePopup.Popup (X, Y);
					end else if TObject (Selected.Data) is TFolder then begin
						with Mouse.CursorPos do
							FolderPopup.Popup (X, Y);
					end;
				end else if Assigned (Selected.Parent) then begin
					with Mouse.CursorPos do
						CategoryPopup.Popup (X, Y);
				end;
			end;
end;

procedure TMainForm.ProjectTreeEdited(Sender: TObject; Node: TTreeNode;
	var S: String);
var
	I: Integer;
	HasFolder,
	NameConflict: Boolean;
begin
	if Node = TopNode then begin
		if Length (S) <= 0 then
			S := 'Project1'
		else begin
			HasFolder := False;
			for I := Length (S) downto 1 do begin
				if S [I] = '\' then begin
					if HasFolder then
						Delete (S, I, 1)
					else
						HasFolder := True;
				end else if (not (IsCharAlphaNumeric (S [I]) or (S [I] in ['A'..'Z', 'a'..'z', '0'..'9']))) then
					Delete (S, I, 1);
			end;
			if Length (S) <= 0 then
				S := 'Project1'
			else if not (IsCharAlpha (S [1]) or (S [1] in ['A'..'Z', 'a'..'z', '\'])) then
				S := 'X' + S;
			I := Pos ('\', S);
			if I > 0 then begin
				while I - 1 > MaxNameLength do begin
					Delete (S, I - 1, 1);
					Dec (I);
				end;
				while Length (S) - I > MaxNameLength do
					Delete (S, Length (S), 1);
				if I + 1 > Length (S) then
					S := S + 'Project1'
				else if I = 1 then
					Delete (S, 1, 1)
				else if not (IsCharAlpha (S [I + 1]) or (S [I + 1] in ['A'..'Z', 'a'..'z'])) then
					Insert ('X', S, I + 1);
			end else
				S := Copy (S, 1, MaxNameLength);
		end;
		Modify;
	end else begin
		if Assigned (Node.Data) then begin
			if (TObject (Node.Data) is TSourceFile) then begin
				if S = '' then
					S := 'File1';
				NameConflict := False;
				if TSourceFile(Node.Data).Compilable then
					with SourceFiles do
						for I := 0 to Count - 1 do
							if Items [I] <> Node.Data then
								with Items [I] as TSourceFile do
									if Compilable and (LowerCase (SourceName) = LowerCase (S)) and (WithoutBackslash (FolderPath) = WithoutBackslash (TSourceFile(Node.Data).FolderPath)) then begin
										NameConflict := True;
										Break;
									end;
				if not NameConflict then try
					if Assigned (Node.Data) then
						TSourceFile(Node.Data).SourceName := S;
					Modify;
					Exit;
				except end;
				S := Node.Text;
				ShowDefaultMessageBox ('The name you chose conflicts with that of another file.', 'Error', mtProgramError)
			end else begin
				S := StringReplace (S, ' ', '', [rfReplaceAll]);
				if Length (S) <= 0 then
					S := Node.Text;
				RecompileFiles;
			end;
		end;
	end;
end;

procedure TMainForm.SetProjectFile(const Value: string);
var
	S: string;
	B: Boolean;
begin
	FProjectFile := Value;
	MasterUnit.ProjectFileName := Value;
	if Value = '' then
		Caption := OriginalCaption
	else
		Caption := OriginalCaption + ' - ' + WithoutExt (ExtractFileName (Value));
	Application.Title := Caption;
	if TopNode.Text = 'Project1' then begin
		B := Modified;
		S := WithoutExt (ExtractFileName (Value));
		ProjectTreeEdited (ProjectTree, TopNode, S);
		TopNode.Text := S;
		Modified := B;
	end;
	S := ExtractFilePath (Value);
	OpenProjectDlg.InitialDir := WithoutBackslash (S);
	OpenProjectDlg.FileName := Value;
	SaveProjectDlg.InitialDir := WithoutBackslash (S);
	SaveProjectDlg.FileName := Value;
	AddFileDlg.InitialDir := S;
end;

function TMainForm.AddSourceFile(const FN: string; OpenOnly: Boolean = False; SourceFileClass: TSourceFileClass = nil; Folder: TFolder = nil): TSourceFile;
var
	F: TForm;
	S: string;
	O: TOpenFileStatusForm;
begin
	if Compiling and (not OpenOnly) then
		Result := nil
	else begin
		Result := SourceFiles.FindFile (FN);
		if Assigned (Result) and Result.InProject then begin
			Result := nil;
			ShowDefaultMessageBox ('The file ''' + ExtractFileName (FN) + ''' is already included in the project.', 'File Already Included', mtProgramError);
		end else if OpenOnly and Assigned (Result) then begin
			Result := nil;
			ShowDefaultMessageBox ('The file ''' + ExtractFileName (FN) + ''' has already been opened.', 'File Already Opened', mtProgramError);
		end else if (not OpenOnly) and (LowerCase (ExtractFileExt (FN)) = '.qll') and (Assigned (SourceFiles.FindFileOfTypeInProject (TQuillSourceFile))) then begin
			Result := nil;
			ShowDefaultMessageBox ('There may be only one Quill source file in each project.', 'Quill Error', mtProgramError);
		end else begin
			if not Assigned (Result) then begin
				O := nil;
				try
					if not OpeningProjectNow then begin
						O := TOpenFileStatusForm.Create (Self);
						S := ExtractFileName (FN);
						O.FileNameLabel.Caption := 'Opening File ''' + Copy (S, 1, LastPos ('.', S) - 1) + '''...';
						O.Show;
						O.Update;
					end;
					if not Assigned (SourceFileClass) then
						SourceFileClass := TSourceFile.GetAppropriateClassFromExt (ExtractFileExt (FN));
					Result := SourceFileClass.Create (SourceFiles);
					Result.Folder := Folder;
					Result.LoadFromFile (FN, True);
					Result.OnError := AddError;
				finally
					if Assigned (O) then
						O.Free;
				end;
				if OpenOnly and (not Assigned (Result.Editor)) then begin
					Result.Free;
					Result := nil;
					ShowDefaultMessageBox ('The file ''' + ExtractFileName (FN) + ''' is not a valid file for opening.', 'Invalid File', mtProgramError);
				end;
			end;
			if Assigned (Result) then
				with Result do begin
					ErrorList := Self.ErrorList;
					if OpenOnly then begin
						if not Assigned (ParentForm) then begin
							ParentForm := TSourceFileForm.Create (nil);
							with ParentForm as TSourceFileForm do begin
								Enabled := False;
								SourceFile := Result;
								Show;
							end;
							if Assigned (Editor) then
								with Editor do begin
									Parent := ParentForm;
									Align := alClient;
									Show;
									if Enabled then
										SetFocus;
								end;
						end;
					end else begin
						if Assigned (ParentForm) then begin
							F := ParentForm;
							ParentForm := nil;
							F.Free;
						end;
						TreeItem := CreateFileNode (TopNode.Item [ClassTreeIndex], Result);
						with TreeItem do begin
							ImageIndex    := ClassImageIndex;
							SelectedIndex := ImageIndex;
						end;
					end;
				end;
		end;
		if not OpeningProjectNow then
			SortFiles;
	end;
end;

procedure TMainForm.ProjectOptions(Sender: TObject);
begin
	with TProjectOptionsForm.Create (Self) do try
		FlashOSRadioButton.Visible := ssFlashOS in SpecialSupport;
		FargoRadioButton.Visible := ssFargo in SpecialSupport;
		if ProjectTarget = ptArchive then
			ArchiveRadioButton.Checked := True
		else if (ProjectTarget = ptFlashOS) and (FlashOSRadioButton.Visible) then
			FlashOSRadioButton.Checked := True
		else if (ProjectTarget = ptFargo) and (FargoRadioButton.Visible) then
			FargoRadioButton.Checked := True
		else
			ExecutableRadioButton.Checked := True;
		DataVarCheckBox.Checked := UseDataVar;
		DataVarEdit.Text := DataVar;
		if DataVarCopy then begin
			if DataVarCopyIfArchived then
				DataVarCopyIfArchivedRadioButton.Checked := True
			else
				DataVarCopyAlwaysRadioButton.Checked := True;
		end else
			DataVarCopyNeverRadioButton.Checked := True;
		PackCheckBox.Checked := Pack and (ssPack in SpecialSupport);
		PackVarEdit.Text := PackVar;
		GCCSwitchesEdit.Text := GCCSwitches;
		AsSwitchesEdit.Text := AsSwitches;
		AsmSwitchesEdit.Text := AsmSwitches;
		DebugInfoCheckBox.Checked := DebugInfo;
		OptimizeNOPsCheckBox.Checked := OptimizeNOPs;
		OptimizeReturnsCheckBox.Checked := OptimizeReturns;
		OptimizeBranchesCheckBox.Checked := OptimizeBranches;
		OptimizeMovesCheckBox.Checked := OptimizeMoves;
		OptimizeTestsCheckBox.Checked := OptimizeTests;
		OptimizeCalculationsCheckBox.Checked := OptimizeCalculations;
		RemoveUnusedSectionsCheckBox.Checked := RemoveUnusedSections;
		CutUnusedRangesCheckBox.Checked := CutUnusedRanges;
		ReorderSectionsCheckBox.Checked := ReorderSections;
		MergeConstantsCheckBox.Checked := MergeConstants;
		StdLibCheckBox.Checked := StdLib;
		InitBSSCheckBox.Checked := InitBSS;
		OutputBinCheckBox.Checked := OutputBin;
		CommandLineEdit.Text := CommandLine;
		ProcessFileEdit.Text := PostBuildProcessFile;
		InitialLibOptions := PredefinedLibOptions;
		if ShowModal = mrOK then begin
			if ArchiveRadioButton.Checked then
				ProjectTarget := ptArchive
			else if FlashOSRadioButton.Checked then
				ProjectTarget := ptFlashOS
			else if FargoRadioButton.Checked then
				ProjectTarget := ptFargo
			else
				ProjectTarget := ptRegular;
			Pack := PackCheckBox.Checked;
			PackVar := PackVarEdit.Text;
			UseDataVar := DataVarCheckBox.Checked;
			DataVar := DataVarEdit.Text;
			DataVarCopy := False;
			DataVarCopyIfArchived := False;
			if DataVarCopyAlwaysRadioButton.Checked then
				DataVarCopy := True
			else if DataVarCopyIfArchivedRadioButton.Checked then begin
				DataVarCopy := True;
				DataVarCopyIfArchived := True;
			end;
			GCCSwitches := GCCSwitchesEdit.Text;
			AsSwitches := AsSwitchesEdit.Text;
			AsmSwitches := AsmSwitchesEdit.Text;
			DebugInfo := DebugInfoCheckBox.Checked;
			OptimizeNOPs := OptimizeNOPsCheckBox.Checked;
			OptimizeReturns := OptimizeReturnsCheckBox.Checked;
			OptimizeBranches := OptimizeBranchesCheckBox.Checked;
			OptimizeMoves := OptimizeMovesCheckBox.Checked;
			OptimizeTests := OptimizeTestsCheckBox.Checked;
			OptimizeCalculations := OptimizeCalculationsCheckBox.Checked;
			RemoveUnusedSections := RemoveUnusedSectionsCheckBox.Checked;
			CutUnusedRanges := CutUnusedRangesCheckBox.Checked;
			ReorderSections := ReorderSectionsCheckBox.Checked;
			MergeConstants := MergeConstantsCheckBox.Checked;
			StdLib := StdLibCheckBox.Checked;
			InitBSS := InitBSSCheckBox.Checked;
			OutputBin := OutputBinCheckBox.Checked;
			CommandLine := CommandLineEdit.Text;
			PostBuildProcessFile := ProcessFileEdit.Text;
			if Assigned (ProgramOptionsForm) and Assigned (PredefinedLibOptions) then
				with ProgramOptionsForm, PredefinedLibOptions do begin
					CalcDests := [];
					if TI89CheckBox.Checked then
						Include (CalcDests, cdTI89);
					if TI92PlusCheckBox.Checked then
						Include (CalcDests, cdTI92Plus);
					if V200CheckBox.Checked then
						Include (CalcDests, cdV200);
					OptimizeCalcConsts := OptimizeCalcConstsCheckBox.Checked;
					if PreOsRadioButton.Checked then
						KernelFormat := kfCompressedTables
					else if DoorsRadioButton.Checked then
						KernelFormat := kfStandard
					else
						KernelFormat := kfNone;
					UseMinAMS := MinAMSCheckBox.Checked;
					if Length (MinAMSEdit.Text) > 0 then
						MinAMS := MinAMSEdit.Text;
					UnofficialOSSupport := UnofficialOSSupportCheckBox.Checked;
					if RelocKernelRadioButton.Checked then
						RelocFormat := rfKernel
					else if RelocCompressedRadioButton.Checked then
						RelocFormat := rfCompressed
					else if RelocMlinkRadioButton.Checked then
						RelocFormat := rfMlink
					else
						RelocFormat := rfAMS;
					if ROMCallKernelRadioButton.Checked then
						ROMCallFormat := rfKernel
					else if ROMCallCompressedRadioButton.Checked then
						ROMCallFormat := rfCompressed
					else if ROMCallMlinkRadioButton.Checked then
						ROMCallFormat := rfMlink
					else if ROMCallFLineRadioButton.Checked then
						ROMCallFormat := rfFLine
					else
						ROMCallFormat := rfDirect;
					if BSSKernelRadioButton.Checked then
						BSSRefFormat := rfKernel
					else if BSSCompressedRadioButton.Checked then
						BSSRefFormat := rfCompressed   
					else if BSSMlinkRadioButton.Checked then
						BSSRefFormat := rfMlink
					else
						BSSRefFormat := rfNone;
					if DataVarKernelRadioButton.Checked then
						DataRefFormat := rfKernel
					else if DataVarCompressedRadioButton.Checked then
						DataRefFormat := rfCompressed  
					else if DataVarMlinkRadioButton.Checked then
						DataRefFormat := rfMlink
					else
						DataRefFormat := rfNone;
					UseFLineJumps      := RelocFLineJumpsCheckBox.Checked;
					Use4ByteFLineJumps := RelocFLineJumps4ByteCheckBox.Checked;
					OptimizeROMCalls := ROMCallOptimizedCheckBox.Checked;
					UseInternalFLineEmulator := InternalFLineEmulatorCheckBox.Checked;
					UseReturnValue           := ReturnValueRadioButton.Checked;
					EnableErrorReturn        := EnableErrorReturnCheckBox.Checked;
					SaveScreen               := LCDSaveCheckBox.Checked;
				end;
			RecompileFiles;
			if not DebugInfo then
				ClearDebugInfo;
			UpdateDebugSettings;
			Modify;
		end;
	finally
		Free;
	end;
end;

procedure TMainForm.FilePreferences(Sender: TObject);
var
	I: Integer;
begin
	with TPreferencesForm.Create (Self) do try
		StopCompilationBox.Checked := StopOnErrors;
		JumpToErrorBox.Checked := JumpToError;
		OpenFolderBox.Checked := OpenFolderMessage;
		DeleteAssemblyFilesBox.Checked := DeleteAssemblyFiles;
		DeleteObjectFilesBox.Checked := DeleteObjectFiles;
{$IFDEF CanSplit}
		SplitFilesCheckBox.Checked := SplitFiles;
{$ENDIF}
		AutoSaveCheckBox.Checked := AutoSave;
		AutoNewsCheckBox.Checked := AutoNews;
		DeleteErrorsCheckBox.Checked := DeleteErrors;
		AllowImplicitCheckBox.Checked := not AssumeUndefined;
		FlatButtonsCheckBox.Checked := MainToolbar.Flat;
		MenuBitmapsCheckBox.Checked := Assigned (MainMenu.Images);
		case TransferTarget of
			ttVTI:
				VTIBox.Checked := True;
			ttCalc:
				RealCalcBox.Checked := True;
			else
				NoneBox.Checked := True;
		end;
		VTIPathEdit.Text := VTIPath;
		case LinkPort.PortType of
			lpCOM: begin
				case LinkPort.PortNumber of
					1: PortCOM1Box.Checked := True;
					2: PortCOM2Box.Checked := True;
					3: PortCOM3Box.Checked := True;
					4: PortCOM4Box.Checked := True;
				end;
			end;
		end;
		case LinkCable of
			lcBlack: CableBlackBox.Checked := True;
			lcGray:  CableGrayBox.Checked := True;
		end;
		AsmTabSizeEdit.Text := IntToStr (TabSizeAsm);
		CTabSizeEdit.Text := IntToStr (TabSizeC);
		BackColor := EditorColor;
		ColorCheckBox.Checked := BackColor <> clWindow;
		EditorFont.Assign (MasterUnit.EditorFont);
		OnFlyCheckBox.Checked := EditorOnFly;
		DragDropEditCheckBox.Checked := EditorDragDrop;
		RemoveTrailingSpcCheckBox.Checked := EditorRemoveTrSp;
		AutoBlockCheckBox.Checked := AutoBlocks;
		SyntaxC.Assign (MasterUnit.SyntaxC);
		SyntaxAsmGNU.Assign (MasterUnit.SyntaxAsmGNU);
		if ssA68k in SpecialSupport then
			SyntaxAsm.Assign (MasterUnit.SyntaxAsm);
		if ssQuill in SpecialSupport then
			SyntaxQuill.Assign (MasterUnit.SyntaxQuill);
		if ShowModal = mrOK then begin
			Screen.Cursor := crHourGlass;
			StopOnErrors := StopCompilationBox.Checked;
			JumpToError := JumpToErrorBox.Checked;
			OpenFolderMessage := OpenFolderBox.Checked;
			DeleteAssemblyFiles := DeleteAssemblyFilesBox.Checked;
			DeleteObjectFiles := DeleteObjectFilesBox.Checked;
{$IFDEF CanSplit}
			SplitFiles := SplitFilesCheckBox.Checked;
{$ENDIF}
			AutoSave := AutoSaveCheckBox.Checked;
			AutoNews := AutoNewsCheckBox.Checked;
			DeleteErrors := DeleteErrorsCheckBox.Checked;
			AssumeUndefined := not AllowImplicitCheckBox.Checked;
			MainToolbar.Flat := FlatButtonsCheckBox.Checked;
			if MainToolbar.Flat then
				MainToolbar.Height := MainToolbar.ButtonHeight + 1
			else
				MainToolbar.Height := MainToolbar.ButtonHeight + 3;
			if MenuBitmapsCheckBox.Checked then
				MainMenu.Images := ToolbarImages
			else
				MainMenu.Images := nil;
			if VTIBox.Checked then
				TransferTarget := ttVTI
			else if RealCalcBox.Checked then
				TransferTarget := ttCalc
			else
				TransferTarget := ttNone;
			VTIPath := VTIPathEdit.Text;
			LinkPort.PortType := lpCOM;
			if PortCOM1Box.Checked then
				LinkPort.PortNumber := 1
			else if PortCOM2Box.Checked then
				LinkPort.PortNumber := 2
			else if PortCOM3Box.Checked then
				LinkPort.PortNumber := 3
			else if PortCOM4Box.Checked then
				LinkPort.PortNumber := 4;
			if CableBlackBox.Checked then
				LinkCable := lcBlack
			else if CableGrayBox.Checked then
				LinkCable := lcGray;
			try
				TabSizeC := StrToInt (CTabSizeEdit.Text);
			except
				TabSizeC := 2;
			end;
			try
				TabSizeAsm := StrToInt (AsmTabSizeEdit.Text);
			except
				TabSizeAsm := 2;
			end;
			if ColorCheckBox.Checked then
				EditorColor := BackColor
			else
				EditorColor := clWindow;
			MasterUnit.EditorFont.Assign (EditorFont);
			EditorOnFly := OnFlyCheckBox.Checked;
			EditorDragDrop := DragDropEditCheckBox.Checked;
			EditorRemoveTrSp := RemoveTrailingSpcCheckBox.Checked;
			AutoBlocks := AutoBlockCheckBox.Checked;
			with SourceFiles do
				for I := 0 to Count - 1 do
					if Items [I] is TTextSourceFile then
						(Items [I] as TTextSourceFile).UpdateEditor;
			if SyntaxTabClicked then begin
				MasterUnit.SyntaxC.Assign (SyntaxC);
				MasterUnit.SyntaxAsmGNU.Assign (SyntaxAsmGNU);
				MasterUnit.SyntaxAsm.Assign (SyntaxAsm);
				MasterUnit.SyntaxQuill.Assign (SyntaxQuill);
				with SourceFiles do
					for I := 0 to Count - 1 do
						if Items [I] is TSourceTextSourceFile then
							(Items [I] as TSourceTextSourceFile).UpdateSyntax;
			end;
			SavePreferences;
			UpdateDebugSettings;
			Screen.Cursor := crDefault;
		end;
	finally
		Free;
	end;
end;

procedure TMainForm.LoadPreferences;
function LookBackForComponent(Stream: TMemoryStream): Integer;
var
	I: Integer;
	P: PChar;
begin
	Result := 0;
	with Stream do begin
		P := Memory;
		for I := Size - 3 downto 1 do
			if (P [I] = 'T') and (P [I + 1] = 'P') and (P [I + 2] = 'F') then begin
				Result := I;
				Break;
			end;
	end;
end;
var
	I,
	Tmp: Integer;
	L: TStringList;
	Strm: TMemoryStream;
	SyntaxCopy: TSyntaxColoringCopy;
begin
	with TRegistry.Create do try
		if OpenKeyReadOnly (RegKey) then try
			if ValueExists ('Open File') then
				ProjectFile := ReadString ('Open File');
			if ValueExists ('Recent Files') then begin
				L := TStringList.Create;
				try
					L.CommaText := ReadString ('Recent Files');
					for I := L.Count - 1 downto 0 do
						AddToRecent (L.Strings [I]);
				finally
					L.Free;
				end;
			end;
			if ValueExists ('Stop on Errors') then
				StopOnErrors := ReadBool ('Stop on Errors');
			if ValueExists ('Jump To First Error') then
				JumpToError := ReadBool ('Jump To First Error');
			if ValueExists ('Open Folder Message') then
				OpenFolderMessage := ReadBool ('Open Folder Message');
			if ValueExists ('Delete Assembly Files') then
				DeleteAssemblyFiles := ReadBool ('Delete Assembly Files');
			if ValueExists ('Delete Object Files') then
				DeleteObjectFiles := ReadBool ('Delete Object Files');
{$IFDEF CanSplit}
			if ValueExists ('Split Files') then
				SplitFiles := ReadBool ('Split Files');
{$ENDIF}
			if ValueExists ('AutoSave') then
				AutoSave := ReadBool ('AutoSave');
			if ValueExists ('AutoNews') then
				AutoNews := ReadBool ('AutoNews');
			if ValueExists ('Delete Errors') then
				DeleteErrors := ReadBool ('Delete Errors');
			if ValueExists ('Assume Undefined References') then
				AssumeUndefined := ReadBool ('Assume Undefined References');
			if ValueExists ('Flat Buttons') then
				MainToolbar.Flat := ReadBool ('Flat Buttons');
			if MainToolbar.Flat then
				MainToolbar.Height := MainToolbar.ButtonHeight + 1
			else
				MainToolbar.Height := MainToolbar.ButtonHeight + 3;
			if ValueExists ('Menu Bitmaps') then begin
				if ReadBool ('Menu Bitmaps') then
					MainMenu.Images := ToolbarImages
				else
					MainMenu.Images := nil;
			end;
			if ValueExists ('Transfer Target') then
				TransferTarget := TTransferTarget (ReadInteger ('Transfer Target') + 1);
			if ValueExists ('TiEmu Path') then
				VTIPath := ReadString ('TiEmu Path');
			if ValueExists ('Link Port') then
				LinkPort.PortNumber := ReadInteger ('Link Port') and $FF;
			if ValueExists ('Link Cable') then begin
				if ReadInteger ('Link Cable') = 2 then
					LinkCable := lcGray
				else
					LinkCable := lcBlack;
			end;
			if ValueExists ('ASM Tab Size') then
				TabSizeAsm := ReadInteger ('ASM Tab Size');
			if ValueExists ('C Tab Size') then
				TabSizeC := ReadInteger ('C Tab Size');
			if ValueExists ('Editor Background Color') then
				EditorColor := ReadInteger ('Editor Background Color');
			if ValueExists ('Editor Font') then
				EditorFont.Name := ReadString ('Editor Font');
			if ValueExists ('Editor Font Style Bold') then begin
				EditorFont.Style := [];
				if ReadBool ('Editor Font Style Bold') then
					EditorFont.Style := EditorFont.Style + [fsBold];
				if ReadBool ('Editor Font Style Italic') then
					EditorFont.Style := EditorFont.Style + [fsItalic];
				if ReadBool ('Editor Font Style Underline') then
					EditorFont.Style := EditorFont.Style + [fsUnderline];
				if ReadBool ('Editor Font Style StrikeOut') then
					EditorFont.Style := EditorFont.Style + [fsStrikeOut];
			end;
			if ValueExists ('Editor Font Size') then
				EditorFont.Size := ReadInteger ('Editor Font Size');
			if ValueExists ('Editor Font Color') then
				EditorFont.Color := ReadInteger ('Editor Font Color');
			if ValueExists ('Editor Font Pitch') then
				EditorFont.Pitch := TFontPitch (ReadInteger ('Editor Font Pitch'));
			if ValueExists ('Editor Split On Fly') then
				EditorOnFly := ReadBool ('Editor Split On Fly');
			if ValueExists ('Editor Drag and Drop') then
				EditorDragDrop := ReadBool ('Editor Drag and Drop');
			if ValueExists ('Editor Remove Trailing Spaces') then
				EditorRemoveTrSp := ReadBool ('Editor Remove Trailing Spaces');
			if ValueExists ('AutoBlocks') then
				AutoBlocks := ReadBool ('AutoBlocks');
			if ValueExists ('Tree Width') then
				ProjectTree.Width := ReadInteger ('Tree Width');
			if ValueExists ('Error Window Height') then
				ErrWinPanel.Height := ReadInteger ('Error Window Height');
			if ValueExists ('Error List Message') then
				ErrorList.Columns[0].Width := ReadInteger ('Error List Message');
			if ValueExists ('Error List File') then
				ErrorList.Columns[1].Width := ReadInteger ('Error List File');
			if ValueExists ('Error List Function') then
				ErrorList.Columns[2].Width := ReadInteger ('Error List Function');
			if ValueExists ('Last News Update') then
				LastNewsDate := ReadInteger ('Last News Update');
			if ValueExists ('Proxy Name') then
				ProxyName := ReadString ('Proxy Name');
			if ValueExists ('Proxy Port') then
				ProxyPort := ReadInteger ('Proxy Port');
			Strm := TMemoryStream.Create;
			if ValueExists ('Editor C Syntax Coloring') then try
				SyntaxCopy := TSyntaxColoringCopy.Create (nil);
				try
					SyntaxCopy.Assign (SyntaxC);
					Strm.SetSize (GetDataSize ('Editor C Syntax Coloring'));
					if Strm.Size > 0 then begin
						ReadBinaryData ('Editor C Syntax Coloring', Strm.Memory^, Strm.Size);
						Strm.Position := LookBackForComponent (Strm);
						Strm.ReadComponent (SyntaxCopy);
						SyntaxC.Assign (SyntaxCopy);
					end;
				finally
					SyntaxCopy.Free;
				end;
			except end;
			if ValueExists ('Editor GNU ASM Syntax Coloring') then try
				SyntaxCopy := TSyntaxColoringCopy.Create (nil);
				try
					SyntaxCopy.Assign (SyntaxAsmGNU);
					Strm.SetSize (GetDataSize ('Editor GNU ASM Syntax Coloring'));
					if Strm.Size > 0 then begin
						ReadBinaryData ('Editor GNU ASM Syntax Coloring', Strm.Memory^, Strm.Size);
						Strm.Position := LookBackForComponent (Strm);
						Strm.ReadComponent (SyntaxCopy);
						SyntaxAsmGNU.Assign (SyntaxCopy);
					end;
				finally
					SyntaxCopy.Free;
				end;
			except end;
			if (ssA68k in SpecialSupport) and ValueExists ('Editor ASM Syntax Coloring') then try
				SyntaxCopy := TSyntaxColoringCopy.Create (nil);
				try
					SyntaxCopy.Assign (SyntaxAsm);
					Strm.SetSize (GetDataSize ('Editor ASM Syntax Coloring'));
					if Strm.Size > 0 then begin
						ReadBinaryData ('Editor ASM Syntax Coloring', Strm.Memory^, Strm.Size);
						Strm.Position := LookBackForComponent (Strm);
						Strm.ReadComponent (SyntaxCopy);
						SyntaxAsm.Assign (SyntaxCopy);
					end;
				finally
					SyntaxCopy.Free;
				end;
			except end;
			if (ssQuill in SpecialSupport) and ValueExists ('Editor Quill Syntax Coloring') then try
				SyntaxCopy := TSyntaxColoringCopy.Create (nil);
				try
					SyntaxCopy.Assign (SyntaxQuill);
					Strm.SetSize (GetDataSize ('Editor Quill Syntax Coloring'));
					if Strm.Size > 0 then begin
						ReadBinaryData ('Editor Quill Syntax Coloring', Strm.Memory^, Strm.Size);
						Strm.Position := LookBackForComponent (Strm);
						Strm.ReadComponent (SyntaxCopy);
						SyntaxQuill.Assign (SyntaxCopy);
					end;
				finally
					SyntaxCopy.Free;
				end;
			except end;
			Strm.Free;
			if ValueExists ('Tools Count') then begin
				Tmp := ReadInteger ('Tools Count');
				ToolsList.Clear;
				for I := 1 to Tmp do
					if OpenKeyReadOnly (RegKey + '\Tool ' + IntToStr (I)) then try
						ToolsLine.Visible := True;
						AddTool (ReadString ('Title'), ReadString ('Command Line'), ReadString ('Working Directory'), TWindowState (ReadInteger ('Window State')));
					except end;
			end;
		except end;
	finally
		Free;
		SplitterMoved (Self);
	end;
	with SourceFiles do
		for I := 0 to Count - 1 do begin
			if Items [I] is TTextSourceFile then
				(Items [I] as TTextSourceFile).UpdateEditor;
			if Items [I] is TSourceTextSourceFile then
				(Items [I] as TSourceTextSourceFile).UpdateSyntax;
		end;
	UpdateDebugSettings;
end;

procedure TMainForm.SavePreferences;
var
	I: Integer;
	Strm: TMemoryStream;
	SyntaxCopy: TSyntaxColoringCopy;
begin
	with TRegistry.Create do try
		if OpenKey (RegKey, True) then try
			WriteString ('Open File', ProjectFile);
			WriteString ('Recent Files', RecentFiles.CommaText);
			WriteBool ('Stop on Errors', StopOnErrors);
			WriteBool ('Jump To First Error', JumpToError);
			WriteBool ('Open Folder Message', OpenFolderMessage);
			WriteBool ('Delete Assembly Files', DeleteAssemblyFiles);
			WriteBool ('Delete Object Files', DeleteObjectFiles);
{$IFDEF CanSplit}
			WriteBool ('Split Files', SplitFiles);
{$ENDIF}
			WriteBool ('AutoSave', AutoSave);
			WriteBool ('AutoNews', AutoNews);
			WriteBool ('Delete Errors', DeleteErrors);
			WriteBool ('Assume Undefined References', AssumeUndefined);
			WriteBool ('Flat Buttons', MainToolbar.Flat);
			WriteBool ('Menu Bitmaps', Assigned (MainMenu.Images));
			WriteInteger ('Transfer Target', Integer (TransferTarget) - 1);
			WriteString ('TiEmu Path', VTIPath);
			WriteInteger ('Link Port', LinkPort.PortNumber);
			case LinkCable of
				lcBlack: WriteInteger ('Link Cable', 1);
				lcGray:  WriteInteger ('Link Cable', 2);
			end;
			WriteInteger ('ASM Tab Size', TabSizeAsm);
			WriteInteger ('C Tab Size', TabSizeC);
			WriteInteger ('Editor Background Color', EditorColor);
			WriteString ('Editor Font', EditorFont.Name);
			WriteBool ('Editor Font Style Bold', fsBold in EditorFont.Style);
			WriteBool ('Editor Font Style Italic', fsItalic in EditorFont.Style);
			WriteBool ('Editor Font Style Underline', fsUnderline in EditorFont.Style);
			WriteBool ('Editor Font Style StrikeOut', fsStrikeOut in EditorFont.Style);
			WriteInteger ('Editor Font Size', EditorFont.Size);
			WriteInteger ('Editor Font Color', EditorFont.Color);
			WriteInteger ('Editor Font Pitch', Integer (EditorFont.Pitch));
			WriteBool ('Editor Split On Fly', EditorOnFly);
			WriteBool ('Editor Drag and Drop', EditorDragDrop);
			WriteBool ('Editor Remove Trailing Spaces', EditorRemoveTrSp);
			WriteBool ('AutoBlocks', AutoBlocks);
			WriteInteger ('Tree Width', ProjectTree.Width);
			WriteInteger ('Error Window Height', ErrWinPanel.Height);
			WriteInteger ('Error List Message', ErrorList.Columns[0].Width);
			WriteInteger ('Error List File', ErrorList.Columns[1].Width);
			WriteInteger ('Error List Function', ErrorList.Columns[2].Width);
			WriteInteger ('Last News Update', LastNewsDate);
			WriteString ('Proxy Name', ProxyName);
			WriteInteger ('Proxy Port', ProxyPort);
			Strm := TMemoryStream.Create;
			try
				SyntaxCopy := TSyntaxColoringCopy.Create (nil);
				try
					SyntaxCopy.Assign (SyntaxC);
					Strm.WriteComponent (SyntaxCopy);
					WriteBinaryData ('Editor C Syntax Coloring', Strm.Memory^, Strm.Size);
				finally
					SyntaxCopy.Free;
				end;
				Strm.Clear;
				SyntaxCopy := TSyntaxColoringCopy.Create (nil);
				try
					SyntaxCopy.Assign (SyntaxAsmGNU);
					Strm.WriteComponent (SyntaxCopy);
					WriteBinaryData ('Editor GNU ASM Syntax Coloring', Strm.Memory^, Strm.Size);
				finally
					SyntaxCopy.Free;
				end;
				if ssA68k in SpecialSupport then begin
					Strm.Clear;
					SyntaxCopy := TSyntaxColoringCopy.Create (nil);
					try
						SyntaxCopy.Assign (SyntaxAsm);
						Strm.WriteComponent (SyntaxCopy);
						WriteBinaryData ('Editor ASM Syntax Coloring', Strm.Memory^, Strm.Size);
					finally
						SyntaxCopy.Free;
					end;
				end;
				if ssQuill in SpecialSupport then begin
					Strm.Clear;
					SyntaxCopy := TSyntaxColoringCopy.Create (nil);
					try
						SyntaxCopy.Assign (SyntaxQuill);
						Strm.WriteComponent (SyntaxCopy);
						WriteBinaryData ('Editor Quill Syntax Coloring', Strm.Memory^, Strm.Size);
					finally
						SyntaxCopy.Free;
					end;
				end;
			finally
				Strm.Free;
			end;
			WriteInteger ('Tools Count', ToolsList.Count);
			with ToolsList do
				for I := 0 to Count - 1 do
					with TToolsListItem (Items [I]) do
						if OpenKey (RegKey + '\Tool ' + IntToStr (I + 1), True) then try
							WriteString ('Title', Title);
							WriteString ('Command Line', CommandLine);
							WriteString ('Working Directory', WorkingDir);
							WriteInteger ('Window State', Integer (WindowState));
						except end;
		except end;
	finally
		Free;
	end;
end;

procedure TMainForm.CompileProject;
var
	I: Integer;
	StopIt: Boolean;
begin
	OperationSuccessful := False;
	OperationCancelled := False;
	StopIt := True;
	with SourceFiles do
		for I := 0 to Count - 1 do
			with Items [I] as TSourceFile do
				if Compilable or (Items [I] is TObjectSourceFile) then begin
					StopIt := False;
					Break;
				end;
	if not StopIt then begin
		OperationSuccessful := True;
		CopyHeaders;
		with SourceFiles do
			for I := 0 to Count - 1 do
				if (OperationSuccessful or not StopOnErrors) and not OperationCancelled then
					with Items [I] as TSourceFile do
						if Compilable and Invalidated then begin
							Compile;
							if not OperationSuccessful then
								StopIt := True;
							Application.ProcessMessages;
						end;
		DeleteHeaders;
		if StopIt then
			OperationSuccessful := False;
	end else
		ShowDefaultMessageBox ('There are no files to compile in the current project.', 'Error', mtProgramError);
end;

function TMainForm.GetNewFileName(const FolderPath, Ext: string): string;
var
	I: Integer;
begin
	if ProjectFile = '' then
		Result := 'C:\New File'
	else
		Result := WithBackslash (ExtractFilePath (ProjectFile) + FolderPath) + 'New File';
	if FileExists (Result + Ext) or Assigned (SourceFiles.FindFileWithoutExt (Result)) then begin
		I := 2;
		while FileExists (Result + ' ' + IntToStr (I) + Ext) or Assigned (SourceFiles.FindFileWithoutExt (Result + ' ' + IntToStr (I))) do
			Inc (I);
		Result := Result + ' ' + IntToStr (I);
	end;
	Result := Result + Ext;
end;

procedure TMainForm.FileNewHeaderFile(Sender: TObject);
var
	O: THeaderSourceFile;
begin
	if not Compiling then begin
		O := THeaderSourceFile.Create (SourceFiles);
		with O do begin
			Folder := GetSelectedFolder (THeaderSourceFile);
			FileName := GetNewFileName (FolderPath, '.h');
			OnError := AddError;
			TreeItem := CreateFileNode (TopNode.Item [ClassTreeIndex], O);
			with TreeItem do begin
				ImageIndex    := ClassImageIndex;
				SelectedIndex := ImageIndex;
			end;
			ErrorList := Self.ErrorList;
			if Assigned (TextEditor) then
				with TextEditor do begin
					Hide;
					Parent := EditorPanel;
					Align := alClient;
					OnEnter := EditorEnter;
					OnExit := EditorExit;
					OnKeyDown := EditorKeyDown;
					OnChange := EditorChange;
					OnSelectionChange := EditorChange;
					PopupMenu := EditorPopup;
				end;
			if (Sender = ActionFileNewGNUAsmHeaderFile) or ((Sender is TMenuItem) and (TMenuItem(Sender).Action = ActionFileNewGNUAsmHeaderFile)) then
				Content := '| Header File'#13#10'| Created ' + DateToStr (Now) + '; ' + TimeToStr (Now) + #13#10
			else if (Sender = ActionFileNewA68kAsmHeaderFile) or ((Sender is TMenuItem) and (TMenuItem(Sender).Action = ActionFileNewA68kAsmHeaderFile)) then
				Content := '; Header File'#13#10'; Created ' + DateToStr (Now) + '; ' + TimeToStr (Now) + #13#10
			else
				Content := '// Header File'#13#10'// Created ' + DateToStr (Now) + '; ' + TimeToStr (Now) + #13#10;
			UpdateSyntax;
			if Assigned (TextEditor) then
				TextEditor.ClearUndo;
			Modified := False;
			Invalidate;
			SelectNode (TreeItem);
			TreeItem.EditText;
		end;
	end;
end;

procedure TMainForm.FileNewCSourceFile(Sender: TObject);
const
	CodeStr = '// Place your code here.';
var
	O: TCSourceFile;
	I,
	P: Integer;
	NewContent: string;
	FirstFile: Boolean;
begin
	if not Compiling then begin
		FirstFile := not Assigned (SourceFiles.FindFileOfTypeInProject (TCSourceFile));
		O := TCSourceFile.Create (SourceFiles);
		with O do begin
			Folder := GetSelectedFolder (TCSourceFile);
			FileName := GetNewFileName (FolderPath, '.c');
			OnError := AddError;
			TreeItem := CreateFileNode (TopNode.Item [ClassTreeIndex], O);
			with TreeItem do begin
				ImageIndex    := ClassImageIndex;
				SelectedIndex := ImageIndex;
			end;
			ErrorList := Self.ErrorList;
			if Assigned (TextEditor) then
				with TextEditor do begin
					Hide;
					Parent := EditorPanel;
					Align := alClient;
					OnEnter := EditorEnter;
					OnExit := EditorExit;
					OnKeyDown := EditorKeyDown;
					OnChange := EditorChange;
					OnSelectionChange := EditorChange;
					PopupMenu := EditorPopup;
				end;
			NewContent := '// C Source File'#13#10'// Created ' + DateToStr (Now) + '; ' + TimeToStr (Now) + #13#10;
			case ProjectTarget of
				ptRegular: begin
					if FirstFile then begin
						NewContent := NewContent + #13#10
							+ '// Delete or comment out the items you do not need.'#13#10
							+ '#define COMMENT_STRING         "Place your comment here."'#13#10
							+ '#define COMMENT_PROGRAM_NAME   "Place your program name here."'#13#10
							+ '#define COMMENT_VERSION_STRING "Place your version string here."'#13#10
							+ '#define COMMENT_VERSION_NUMBER 0,0,0,0 /* major, minor, revision, subrevision */'#13#10
							+ '#define COMMENT_AUTHORS        "Place your author name(s) here."'#13#10
							+ '#define COMMENT_BW_ICON \'#13#10
							+ #9'{0b0000000000000000, \'#13#10;
						for I := 1 to 14 do
							NewContent := NewContent
								+ #9' 0b0000000000000000, \'#13#10;
						NewContent := NewContent
							+ #9' 0b0000000000000000}'#13#10
							+ '#define COMMENT_GRAY_ICON \'#13#10
							+ #9'{0b0000000000000000, \'#13#10;
						for I := 1 to 14 do
							NewContent := NewContent
								+ #9' 0b0000000000000000, \'#13#10;
						NewContent := NewContent
							+ #9' 0b0000000000000000}, \'#13#10
							+ #9'{0b0000000000000000, \'#13#10;
						for I := 1 to 14 do
							NewContent := NewContent
								+ #9' 0b0000000000000000, \'#13#10;
						NewContent := NewContent
							+ #9' 0b0000000000000000}'#13#10;
					end;
					NewContent := NewContent + #13#10'#include <tigcclib.h>'#13#10;
				end;
				ptArchive:
					NewContent := NewContent + #13#10'#define _GENERIC_ARCHIVE'#13#10'#include <tigcclib.h>'#13#10;
			end;
			if FirstFile and (not (ProjectTarget in [ptFlashOS, ptArchive])) then
				NewContent := NewContent + #13#10'// Main Function'#13#10'void _main(void)'#13#10
					+ '{'#13#10
					+ #9 + CodeStr + #13#10
					+ '}'#13#10;
			if Assigned (TextEditor) then
				TextEditor.AllowUndo := False;
			Content := NewContent;
			if Assigned (TextEditor) then begin
				TextEditor.AllowUndo := True;
				P := Pos (CodeStr, TextEditor.Text);
				if P > 0 then
					with TextEditor.Selection do begin
						RStart := P;
						RLength := Length (CodeStr);
					end;
			end;
			Modified := False;
			Invalidate;
			SelectNode (TreeItem);
			TreeItem.EditText;
		end;
	end;
end;

procedure TMainForm.FileNewGNUAssemblerSourceFile(Sender: TObject);
var
	O: TGNUAsmSourceFile;
begin
	if not Compiling then begin
		O := TGNUAsmSourceFile.Create (SourceFiles);
		with O do begin
			Folder := GetSelectedFolder (TGNUAsmSourceFile);
			FileName := GetNewFileName (FolderPath, '.s');
			OnError := AddError;
			TreeItem := CreateFileNode (TopNode.Item [ClassTreeIndex], O);
			with TreeItem do begin
				ImageIndex    := ClassImageIndex;
				SelectedIndex := ImageIndex;
			end;
			ErrorList := Self.ErrorList;
			if Assigned (TextEditor) then
				with TextEditor do begin
					Hide;
					Parent := EditorPanel;
					Align := alClient;
					OnEnter := EditorEnter;
					OnExit := EditorExit;
					OnKeyDown := EditorKeyDown;
					OnChange := EditorChange;
					OnSelectionChange := EditorChange;
					PopupMenu := EditorPopup;
				end;
			Content := '| Assembly Source File'#13#10'| Created ' + DateToStr (Now) + '; ' + TimeToStr (Now) + #13#10;
			if Assigned (TextEditor) then
				TextEditor.ClearUndo;
			Modified := False;
			Invalidate;
			SelectNode (TreeItem);
			TreeItem.EditText;
		end;
	end;
end;

procedure TMainForm.FileNewAssemblerSourceFile(Sender: TObject);
var
	O: TAsmSourceFile;
begin
	if not Compiling then begin
		O := TAsmSourceFile.Create (SourceFiles);
		with O do begin
			Folder := GetSelectedFolder (TAsmSourceFile);
			FileName := GetNewFileName (FolderPath, '.asm');
			OnError := AddError;
			TreeItem := CreateFileNode (TopNode.Item [ClassTreeIndex], O);
			with TreeItem do begin
				ImageIndex    := ClassImageIndex;
				SelectedIndex := ImageIndex;
			end;
			ErrorList := Self.ErrorList;
			if Assigned (TextEditor) then
				with TextEditor do begin
					Hide;
					Parent := EditorPanel;
					Align := alClient;
					OnEnter := EditorEnter;
					OnExit := EditorExit;
					OnKeyDown := EditorKeyDown;
					OnChange := EditorChange;
					OnSelectionChange := EditorChange;
					PopupMenu := EditorPopup;
				end;
			Content := '; Assembly Source File'#13#10'; Created ' + DateToStr (Now) + ', ' + TimeToStr (Now) + #13#10;
			if Assigned (TextEditor) then
				TextEditor.ClearUndo;
			Modified := False;
			Invalidate;
			SelectNode (TreeItem);
			TreeItem.EditText;
		end;
	end;
end;

procedure TMainForm.FileNewQuillSourceFile(Sender: TObject);
var
	O: TQuillSourceFile;
begin
	if not Compiling then begin
		if Assigned (SourceFiles.FindFileOfTypeInProject (TQuillSourceFile)) then
			ShowDefaultMessageBox ('There may be only one Quill source file in each project.', 'Quill Error', mtProgramError)
		else begin
			O := TQuillSourceFile.Create (SourceFiles);
			with O do begin
				Folder := GetSelectedFolder (TQuillSourceFile);
				FileName := GetNewFileName (FolderPath, '.qll');
				OnError := AddError;
				TreeItem := CreateFileNode (TopNode.Item [ClassTreeIndex], O);
				with TreeItem do begin
					ImageIndex    := ClassImageIndex;
					SelectedIndex := ImageIndex;
				end;
				ErrorList := Self.ErrorList;
				if Assigned (TextEditor) then
					with TextEditor do begin
						Hide;
						Parent := EditorPanel;
						Align := alClient;
						OnEnter := EditorEnter;
						OnExit := EditorExit;
						OnKeyDown := EditorKeyDown;
						OnChange := EditorChange;
						OnSelectionChange := EditorChange;
						PopupMenu := EditorPopup;
					end;
				if FileExists (WithBackslash (TIGCCFolder) + QuillIncludeLocation + 'Template.qll') then
					LoadFromFile (WithBackslash (TIGCCFolder) + QuillIncludeLocation + 'Template.qll')
				else if FileExists (WithBackslash (TIGCCFolder) + CIncludeLocation + 'Template.qll') then
					LoadFromFile (WithBackslash (TIGCCFolder) + CIncludeLocation + 'Template.qll')
				else if FileExists (WithBackslash (TIGCCFolder) + GCCLocation + 'Template.qll') then
					LoadFromFile (WithBackslash (TIGCCFolder) + GCCLocation + 'Template.qll');
				Content := '// Quill Source File'#13#10'// Created ' + DateToStr (Now) + '; ' + TimeToStr (Now) + #13#10#13#10 + Content;
				if Assigned (TextEditor) then
					TextEditor.ClearUndo;
				Modified := False;
				Invalidate;
				SelectNode (TreeItem);
				TreeItem.EditText;
			end;
		end;
	end;
end;

procedure TMainForm.FileNewTextFile(Sender: TObject);
var
	O: TNormalTextSourceFile;
begin
	O := TNormalTextSourceFile.Create (SourceFiles);
	with O do begin
		Folder := GetSelectedFolder (TTextSourceFile);
		FileName := GetNewFileName (FolderPath, '.txt');
		OnError := AddError;
		TreeItem := CreateFileNode (TopNode.Item [ClassTreeIndex], O);
		with TreeItem do begin
			ImageIndex    := ClassImageIndex;
			SelectedIndex := ImageIndex;
		end;
		ErrorList := Self.ErrorList;
		if Assigned (TextEditor) then
			with TextEditor do begin
				Hide;
				Parent := EditorPanel;
				Align := alClient;
				OnEnter := EditorEnter;
				OnExit := EditorExit;
				OnKeyDown := EditorKeyDown;
				OnChange := EditorChange;
				OnSelectionChange := EditorChange;
				PopupMenu := EditorPopup;
			end;
		if Assigned (TextEditor) then
			TextEditor.ClearUndo;
		Modified := False;
		SelectNode (TreeItem);
		TreeItem.EditText;
	end;
end;

procedure TMainForm.TreeItemSaveAs(Sender: TObject);
var
	Node: TTreeNode;
begin
	Node := ProjectTree.Selected;
	if Assigned (Node) and Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) then begin
		TSourceFile(Node.Data).SaveAs;
		Modify;
		UpdateStatusBar;
	end;
end;

procedure TMainForm.BeginCompilation;
var
	I: Integer;
begin
	ClearErrors;
	with SourceFiles do
		for I := 0 to Count - 1 do
			with Items [I] as TSourceFile do
				if not InProject then begin
					RecompileFiles;
					Break;
				end;
	OperationCancelled := False;
	OperationSuccessful := False;
end;

procedure TMainForm.EndCompilation;
begin
	CompStop;
	Application.Restore;
	UpdateErrorWindow;
	ShowErrors;
end;

procedure TMainForm.HelpAbout(Sender: TObject);
begin
	with TAboutForm.Create (Self) do try
		ShowModal;
	finally
		Free;
	end;
end;

procedure TMainForm.DisplayHint(Sender: TObject);
begin
	with StatusBar.Panels do begin
		BeginUpdate;
		with Items [0] do begin
			if Application.Hint = '' then
				Width := 0
			else
				Width := 1000000;
			Text := Application.Hint;
		end;
		EndUpdate;
	end;
end;

procedure TMainForm.ShowHideErrors(Sender: TObject);
begin
	with ActionProjectShowErrors do begin
		if Checked then
			HideErrors
		else
			ShowErrors;
	end;
end;

procedure TMainForm.UpdateEditButtons;
var
	TextSel: Boolean;
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) then begin
		TextSel := (Editor.SelLength > 0) and (ActiveControl = Editor);
		ActionEditDelete.Enabled := TextSel;
		ActionEditCut.Enabled := TextSel;
		ActionEditCopy.Enabled := TextSel;
		ActionEditUndo.Enabled := (ActiveControl = Editor) and Editor.CanUndo;
		ActionEditRedo.Enabled := (ActiveControl = Editor) and Editor.CanRedo;
	end;
end;

procedure TMainForm.RecompileFiles;
var
	I: Integer;
begin
	with SourceFiles do
		for I := 0 to Count - 1 do
			with Items [I] as TSourceFile do
				if Items [I] is THeaderSourceFile then
					Invalidated := False
				else if Compilable then
					Invalidate;
end;

procedure TMainForm.FindString(Sender: TObject; AllFiles: Boolean);
var
	P: Integer;
	S,
	T: string;
	Valid: Boolean;
	FPos: Integer;
	Editor: TMemoComponent;
	NewSelection: TTreeNode;
	FirstFile: Boolean;
begin
	NewSelection := ProjectTree.Selected;
	if not Assigned (NewSelection) then
		NewSelection := TopNode;
	Valid := False;
	FirstFile := True;
	repeat
		if Assigned (NewSelection.Data) and (TObject (NewSelection.Data) is TSourceFile) then
			if TSourceFile (NewSelection.Data) is TTextSourceFile then begin
				Editor := TTextSourceFile(NewSelection.Data).TextEditor;
				if Assigned (Editor) then begin
					with Sender as TFindDialog do begin
						if (frFindNext in Options) and FirstFile then
							P := Editor.Selection.REnd
						else
							if frDown in Options then
								P := 0
							else
								P := Length (Editor.Text);
						repeat
							if frDown in Options then
								T := Copy (Editor.Text, P + 1, Length (Editor.Text))
							else
								T := Copy (Editor.Text, 1, P - 1);
							if frMatchCase in Options then
								S := FindText
							else begin
								S := UpperCase (FindText);
								T := UpperCase (T);
							end;
							if frDown in Options then
								FPos := Pos (S, T)
							else
								FPos := LastPos (S, T);
							if FPos > 0 then begin
								Valid := True;
								if frWholeWord in Options then begin
									if ((FPos > 1) and (T <> '') and (T [FPos - 1] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'])) then
										Valid := False;
									if ((FPos + Length (S) < Length (T)) and (T <> '') and (T [FPos + Length (S)] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'])) then
										Valid := False;
								end;
								if not (frDown in Options) then
									P := 0;
								if not Valid then
									Inc (P, FPos);
							end;
						until Valid or (FPos <= 0);
						if Valid then begin
							SelectNode (NewSelection);
							with Editor do begin
								with Selection do begin
									DoChanging;
									RStart := P + FPos;
									RLength := Length (S);
									DoChange;
									ScrollInView (2);
								end;
							end;
						end;
					end;
				end;
			end;
		if AllFiles and (not Valid) then begin
			if frDown in (Sender as TFindDialog).Options then begin
				NewSelection := NewSelection.GetNext;
				if not Assigned (NewSelection) then
					NewSelection := TopNode;
			end else begin
				NewSelection := NewSelection.GetPrev;
				if not Assigned (NewSelection) then begin
					NewSelection := TopNode;
					while NewSelection.Count > 0 do
						NewSelection := NewSelection.Item [NewSelection.Count - 1];
				end;
			end;
			FirstFile := False;
		end;
	until (not AllFiles) or Valid or (NewSelection = ProjectTree.Selected);
	if not Valid then
		Abort;
end;

procedure TMainForm.ReplaceDlgReplace(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) then
		with Sender as TReplaceDialog do
			if (frReplace in Options) or (frReplaceAll in Options) then
				repeat
					if UpperCase (Editor.Selection.Text) = UpperCase (FindText) then
						Editor.Selection.Text := ReplaceText;
					Options := Options + [frFindNext];
					FindString (Sender, False);
				until not (frReplaceAll in Options);
end;

procedure TMainForm.FindText(Sender: TObject);
begin
	try
		if Assigned (CurrentEditor) then
			if CurrentEditor.Selection.RLength > 0 then
				FindDlg.FindText := CurrentEditor.Selection.Text;
		FindDlg.Execute;
	except end;
end;

procedure TMainForm.ReplaceText(Sender: TObject);
begin
	try
		if Assigned (CurrentEditor) then
			if CurrentEditor.Selection.RLength > 0 then begin
				ReplaceDlg.FindText := CurrentEditor.Selection.Text;
				ReplaceDlg.ReplaceText := ReplaceDlg.FindText;
			end;
		ReplaceDlg.Execute;
	except end;
end;

procedure TMainForm.FindOpenFile(Sender: TObject);
var
	I: Integer;
	S: string;
	QuotesInLine: Boolean;
	SelectedNode: TTreeNode;
	SourceFile: TSourceFile;
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) then begin
		QuotesInLine := Pos ('"', Editor.Lines [Editor.Selection.StartRowCol.Row - 1]) > 0;
		S := '';
		for I := Editor.Selection.RStart - 1 downto 1 do begin
			if (not QuotesInLine) and (Editor.Text [I] = ' ') then
				Break;
			if Editor.Text [I] in [' ', 'A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.', '\', ':'] then
				S := Editor.Text [I] + S
			else
				Break;
		end;
		for I := Editor.Selection.RStart to Length (Editor.Text) do begin
			if (not QuotesInLine) and (Editor.Text [I] = ' ') then
				Break;
			if Editor.Text [I] in [' ', 'A'..'Z', 'a'..'z', '0'..'9', '_', '-', '.', '\', ':'] then
				S := S + Editor.Text [I]
			else
				Break;
		end;
		S := Trim (S);
		if Length (S) > 0 then begin
			SourceFile := SourceFiles.FindFileNameOnly (S);
			if Assigned (SourceFile) then
				SourceFile.Edit
			else begin
				SelectedNode := ProjectTree.Selected;
				if Assigned (SelectedNode) and (TObject (SelectedNode.Data) is TSourceFile) then
					SourceFile := TSourceFile (SelectedNode.Data);
				if FileExists (ExpandFileName (S)) then
					AddSourceFile (ExpandFileName (S), True)
				else if Assigned (SourceFile) and (SourceFile is TGNUAsmSourceFile) and FileExists (WithBackslash (TIGCCFolder) + GASIncludeLocation + S) then
					AddSourceFile (WithBackslash (TIGCCFolder) + GASIncludeLocation + S, True)
				else if Assigned (SourceFile) and (SourceFile is TAsmSourceFile) and FileExists (WithBackslash (TIGCCFolder) + ASMIncludeLocation + S) then
					AddSourceFile (WithBackslash (TIGCCFolder) + ASMIncludeLocation + S, True)
				else if FileExists (WithBackslash (TIGCCFolder) + CIncludeLocation + S) then
					AddSourceFile (WithBackslash (TIGCCFolder) + CIncludeLocation + S, True)
				else
					ShowDefaultMessageBox ('File ''' + S + ''' not found.', 'Search Failed', mtProgramError);
			end;
		end;
	end;
end;

procedure TMainForm.FindDlgFind(Sender: TObject);
begin
	try
		FindString (Sender, True);
	except
		ShowDefaultMessageBox ('Text ''' + (Sender as TFindDialog).FindText + ''' not found.', 'Search Failed', mtProgramError);
	end;
end;

procedure TMainForm.ActionsExecute(Action: TBasicAction;
	var Handled: Boolean);
begin
	Application.ProcessMessages;
	Handled := False;
end;

procedure TMainForm.ProjectTreeEnter(Sender: TObject);
begin
	ActionEditDelete.Enabled := Assigned (ProjectTree.Selected) and Assigned (ProjectTree.Selected.Data);
end;

procedure TMainForm.ProjectTreeExit(Sender: TObject);
begin
	ActionEditDelete.Enabled := False;
end;

procedure TMainForm.ProjectTreeChanging(Sender: TObject; Node: TTreeNode;
	var AllowChange: Boolean);
begin
	if not NoHideEditor then begin
		if Assigned (EditorToHide) then
			EditorToHide.Hide;
		if (not Closing) and Assigned (PreviousNode) and Assigned (PreviousNode.Data) and (TObject (PreviousNode.Data) is TSourceFile) then
			EditorToHide := TSourceFile(PreviousNode.Data).Editor
		else
			EditorToHide := NoEditor;
	end;
end;

procedure TMainForm.EditorChange(Sender: TObject);
begin
	if ActiveControl = Sender then
		UpdateEditButtons;
	UpdateStatusBar;
end;

procedure TMainForm.FilePrint(Sender: TObject);
var
	Node: TTreeNode;
	Cp: Integer;
begin
	Node := ProjectTree.Selected;
	if Assigned (Node) and Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) and (Printer.Printers.Count > 0) then try
		PrintDlg.PrintRange := prAllPages;
		PrintDlg.MaxPage := TSourceFile(Node.Data).CountPages;
		PrintDlg.ToPage := PrintDlg.MaxPage;
		if Assigned (CurrentEditor) then begin
			if CurrentEditor.Selection.RLength > 0 then
				PrintDlg.Options := PrintDlg.Options + [poSelection]
			else
				PrintDlg.Options := PrintDlg.Options - [poSelection];
		end;
		if Assigned (Sender) then begin
			if not PrintDlg.Execute then
				Abort;
			Cp := PrintDlg.Copies;
			if Cp < 1 then
				Cp := 1;
		end else
			Cp := 1;
		TSourceFile(Node.Data).Print (Cp, PrintDlg.PrintRange, PrintDlg.FromPage, PrintDlg.ToPage);
	except
		Abort;
	end;
end;

procedure TMainForm.FilePrintQuickly(Sender: TObject);
begin
	FilePrint (nil);
end;

procedure TMainForm.TreeItemRename(Sender: TObject);
begin
	if Assigned (ProjectTree.Selected) then
		ProjectTree.Selected.EditText;
end;

procedure TMainForm.HelpContents(Sender: TObject);
begin
	try
		DocFile.DisplayContentsTab;
	except
		ShowDefaultMessageBox ('Error opening documentation.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.HelpIndex(Sender: TObject);
begin
	try
		DocFile.DisplayIndexTab;
	except
		ShowDefaultMessageBox ('Error opening documentation.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.HelpSearch(Sender: TObject);
begin
	try
		DocFile.DisplaySearchTab;
	except
		ShowDefaultMessageBox ('Error opening documentation.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.SplitterMoved(Sender: TObject);
begin
	StatusBar.Panels[1].Width := ProjectTree.Width;
	Update;
end;

procedure TMainForm.UpdateStatusBar;
var
	S: string;
	I: Integer;
	Total: Integer;
	Cell: TTextCell;
	Node,
	ClassNode: TTreeNode;
begin
	with StatusBar.Panels do begin
		Total := 0;
		Node := TopNode;
		while Assigned (Node) do begin
			if Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) then
				Inc (Total);
			Node := Node.GetNext;
		end;
		S := IntToStr (Total) + ' File';
		if Total <> 1 then
			S := S + 's';
		S := S + ' Total';
		if Assigned (ProjectTree.Selected) then begin
			ClassNode := GetSourceTypeFolder (ProjectTree.Selected);
			if Assigned (ClassNode) then begin
				Total := 0;
				Node := ClassNode;
				while Assigned (Node) and (Node <> ClassNode.GetNextSibling) do begin
					if Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) then
						Inc (Total);
					Node := Node.GetNext;
				end;
				S := S + ', ' + IntToStr (Total) + ' in Category';
			end;
		end;
		if Items[1].Text <> S then
			Items[1].Text := S;
		if Assigned (CurrentEditor) then begin
			with CurrentEditor do begin
				if Enabled then begin
					if Selection.RLength > 0 then begin
						I := Selection.RLength + Selection.StartRowCol.Row - Selection.EndRowCol.Row;
						if Selection.EndRowCol.Col > LineLength [Selection.EndRowCol.Row] then
							Dec (I);
						S := IntToStr (I);
						if Items[3].Text <> S then
							Items[3].Text := S;
						if Items[4].Text <> '' then
							Items[4].Text := '';
						if Items[3].Width <> 60 then
							Items[3].Width := 60;
						if Items[4].Width <> 0 then
							Items[4].Width := 0;
					end else begin
						Cell := Selection.StartRowCol;
						S := IntToStr (Cell.Row);
						if Items[3].Text <> S then
							Items[3].Text := S;
						S := IntToStr (Cell.Col);
						if Items[4].Text <> S then
							Items[4].Text := S;
						if Items[3].Width <> 30 then
							Items[3].Width := 30;
						if Items[4].Width <> 30 then
							Items[4].Width := 30;
					end;
					I := TextLength - LineCount + 1;
					S := IntToStr (I) + ' Character';
					if I <> 1 then
						S := S + 's';
					if Items[11].Text <> S then
						Items[11].Text := S;
					if Items[10].Width <> 1 then
						Items[10].Width := 1;
					if Items[11].Width <> 93 then
						Items[11].Width := 93;
					if Items[12].Width <> 1 then
						Items[12].Width := 1;
				end;
			end;
		end else begin
			if Items[3].Text <> '' then
				Items[3].Text := '';
			if Items[4].Text <> '' then
				Items[4].Text := '';
			if Items[11].Text <> '' then
				Items[11].Text := '';
			if Items[3].Width <> 0 then
				Items[3].Width := 0;
			if Items[4].Width <> 0 then
				Items[4].Width := 0;
			if Items[10].Width <> 0 then
				Items[10].Width := 0;
			if Items[11].Width <> 0 then
				Items[11].Width := 0;
			if Items[12].Width <> 0 then
				Items[12].Width := 0;
		end;
		Node := ProjectTree.Selected;
		S := '';
		if Assigned (Node) then begin
			if Assigned (Node.Data) and (TObject (Node.Data) is TSourceFile) then begin
				with TSourceFile (Node.Data) do
					if (ProjectFile = '') and (ExtractFilePath (FileName) = 'C:\') then
						S := LogicalFileName
					else
						S := FileName;
			end else
				if Node = TopNode then
					S := ProjectFile;
		end;
		if Items[13].Text <> S then
			Items[13].Text := S;
	end;
	Update;
end;

procedure TMainForm.ProjectTreeMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight then
		SelectNode (ProjectTree.GetNodeAt (X, Y));
end;

procedure TMainForm.SetModified(const Value: Boolean);
begin
	FModified := Value;
	UpdateStatusBar;
	if Value then
		Invalidated := True;
end;

procedure TMainForm.ToolBarManagerBandMove(Sender: TObject;
	Control: TControl; var ARect: TRect);
begin
	Update;
end;

procedure TMainForm.ProjectTreeDragOver(Sender, Source: TObject; X,
	Y: Integer; State: TDragState; var Accept: Boolean);
var
	S,
	D: TTreeNode;
	SameType,
	FolderDest: Boolean;
begin
	Accept := False;
	with ProjectTree do begin
		S := Selected;
		if Assigned (S) and Assigned (S.Data) then begin
			D := GetNodeAt (X, Y);
			if Assigned (D) and (D <> TopNode) and (D <> S) and (D <> S.Parent) then begin
				SameType := (GetSourceTypeFolder (S) = GetSourceTypeFolder (D));
				FolderDest := (not Assigned (D.Data)) or (TObject (D.Data) is TFolder);
				if TObject (S.Data) is TSourceFile then
					Accept := SameType or FolderDest
				else
					Accept := SameType and FolderDest;
			end;
		end;
	end;
end;

procedure TMainForm.ProjectTreeDragDrop(Sender, Source: TObject; X,
	Y: Integer);
var
	S,
	D: TTreeNode;
	FN: string;
	F: TSourceFile;
	Folder: TFolder;
begin
	with ProjectTree do begin
		S := Selected;
		if Assigned (S) and Assigned (S.Data) then begin
			D := GetNodeAt (X, Y);
			if Assigned (D) and (D <> TopNode) then begin
				// Handle moving within one type.
				if GetSourceTypeFolder (S) = GetSourceTypeFolder (D) then begin
					// Handle moving to a different folder.
					if (not Assigned (D.Data)) or (TObject (D.Data) is TFolder) then begin
						S.MoveTo (D, naAddChild);
						if TObject (S.Data) is TSourceFile then
							with TSourceFile (S.Data) do begin
								Folder := D.Data;
								Invalidate;
							end;
					// Handle moving within one folder.
					end else begin
						if (S <> D) and (S.Parent = D.Parent) then begin
							if D.Index > S.Index then begin
								if D.GetNextSibling <> nil then
									S.MoveTo (D.GetNextSibling, naInsert)
								else
									S.MoveTo (D, naAdd);
							end else
								S.MoveTo (D, naInsert);
						end;
					end;
					Modify;
				// Handle moving to a different type.
				end else begin
					if S.Parent <> D then begin
						if TObject (S.Data) is TSourceFile then begin
							with TSourceFile (S.Data) do begin
								if FileExists (FileName) then
									WarnIfModified
								else
                  Save;
								FN := FileName;
							end;
							S.Delete;
							Folder := nil;
							if Assigned (D.Data) and (TObject (D.Data) is TFolder) then
								Folder := D.Data;
							D := GetSourceTypeFolder (D);
							if Assigned (D) then begin
								F := AddSourceFile (FN, False, TSourceFile.GetAppropriateClassFromTreeIndex (D.Index), Folder);
								if Assigned (F) and Assigned (F.TreeItem) then
									SelectNode (F.TreeItem);
							end;
							RecompileFiles;
							Modify;
						end;
					end;
				end;
			end;
			if Assigned (CurrentEditor) then
				CurrentEditor.Refresh;
			SortFiles;
		end;
	end;
end;

procedure TMainForm.ErrorListClick(Sender: TObject);
begin
	if Enabled and Assigned (ErrorList.Selected) and (ErrorList.SelCount = 1) then
		TFoundError(ErrorList.Selected.Data).GoToPosition;
end;

function TMainForm.GetCurrentEditor: TMemoComponent;
var
	Node: TTreeNode;
begin
	Node := ProjectTree.Selected;
	if Assigned (Node) and Assigned (Node.Data) and (TObject (Node.Data) is TTextSourceFile) then
		Result := TTextSourceFile(Node.Data).TextEditor
	else
		Result := nil;
end;

procedure TMainForm.DeleteError(Sender: TObject);
var
	I: Integer;
begin
	if DeleteErrors then begin
		with ErrorList.Items do
			for I := Count - 1 downto 0 do
				if Assigned (Item[I].Data) then
					with TFoundError (Item[I].Data) do
						if Range = Sender then
							Item[I].Delete;
		UpdateErrorWindow;
	end;
end;

procedure TMainForm.UpdateFuncs;
var
	Node: TTreeNode;
begin
	Node := ProjectTree.Selected;
	if Assigned (Node) and Assigned (Node.Data) and (TObject (Node.Data) is TSourceTextSourceFile) then
		with TSourceTextSourceFile (Node.Data) do
			Funcs := GetFunctions
	else
		SetLength (Funcs, 0);
end;

procedure TMainForm.FunctionPopupPopup(Sender: TObject);
var
	I,
	P: Integer;
	M: TMenuItem;
begin
	with (Sender as TPopupMenu).Items do begin
		for I := Count - 1 downto 0 do
			Remove (Items [I]);
		UpdateFuncs;
		for I := Low (Funcs) to High (Funcs) do begin
			M := TMenuItem.Create (Self);
			with M do begin
				Tag := I;
				Caption := Funcs[I].Name;
				OnClick := FindFunctionFromPopup;
			end;
			P := Pos ('main', Funcs[I].Name);
			if (P <> 0) and (P = Length (Funcs[I].Name) - Length ('main') + 1) then
				Insert (0, M)
			else
				Add (M);
		end;
		if Count <= 0 then
			Add (NoFunctionsItem);
	end;
end;

procedure TMainForm.FindFunctions(Sender: TObject);
var
	I: Integer;
	Editor: TMemoComponent;
begin
	UpdateFuncs;
	with TFunctionsForm.Create (Self) do try
		Funcs := @Self.Funcs;
		with FuncList.Items do begin
			BeginUpdate;
			Clear;
			for I := Low (Self.Funcs) to High (Self.Funcs) do
				AddObject (Self.Funcs[I].Name, TObject (I));
			EndUpdate;
		end;
		FuncListClick (FuncList);
		case ShowModal of
			mrYes: begin
				Editor := CurrentEditor;
				if Assigned (Editor) then begin
					Editor.Selection.NoSelAtPos (Editor.CellToCharIdx (TextCell (Self.Funcs[Integer(FuncList.Items.Objects[FuncList.ItemIndex])].PrototypeLine, 1)));
					Editor.Selection.ScrollInView (5);
				end;
			end;
			mrNo: begin
				Editor := CurrentEditor;
				if Assigned (Editor) then begin
					Editor.Selection.NoSelAtPos (Editor.CellToCharIdx (TextCell (Self.Funcs[Integer(FuncList.Items.Objects[FuncList.ItemIndex])].ImplementationLine, 1)));
					Editor.Selection.ScrollInView (5);
				end;
			end;
		end;
		with FuncList.Items do begin
			BeginUpdate;
			for I := Count - 1 downto 0 do begin
				Objects [I] := nil;
				Delete (I);
			end;
			EndUpdate;
		end;
	finally
		Free;
	end;
end;

procedure TMainForm.FindFunctionFromPopup(Sender: TObject);
var
	L: Integer;
	Editor: TMemoComponent;
begin
	if Sender is TMenuItem then
		with Sender as TMenuItem do begin
			if Funcs[Tag].ImplementationLine > 0 then
				L := Funcs[Tag].ImplementationLine
			else
				L := Funcs[Tag].PrototypeLine;
			if L > 0 then begin
				Editor := CurrentEditor;
				if Assigned (Editor) then begin
					Editor.Selection.NoSelAtPos (Editor.CellToCharIdx (TextCell (L, 1)));
					Editor.Selection.ScrollInView (5);
					if Editor.Visible and Editor.Enabled then
						ActiveControl := Editor;
				end;
			end;
		end;
end;

procedure TMainForm.IncreaseIndent(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) then
		Editor.ChangeIndent (1);
end;

procedure TMainForm.DecreaseIndent(Sender: TObject);
var
	Editor: TMemoComponent;
begin
	Editor := CurrentEditor;
	if Assigned (Editor) then
		Editor.ChangeIndent (-1);
end;

procedure TMainForm.SortFiles;
var
	CurFileIndex: Integer;
procedure DoSortFiles(Node: TTreeNode);
var
	I: Integer;
begin
	with Node do begin
		if Assigned (Data) and (TObject (Data) is TSourceFile) then begin
			with TSourceFile (Data) do
				if Index > CurFileIndex then
					Index := CurFileIndex;
			Inc (CurFileIndex);
		end;
		for I := 0 to Count - 1 do
			DoSortFiles (Item [I]);
	end;
end;
var
	I: Integer;
begin
	CurFileIndex := 0;
	{	This is supposed to be: DoSortFiles (TopNode);
		However, header files are dependent on the other
		files and therefore have to be put after all the
		others. }
	with TopNode do begin
		for I := 0 to Count - 1 do
			if I <> THeaderSourceFile.GetClassTreeIndex then
				DoSortFiles (Item [I]);
		if Count > THeaderSourceFile.GetClassTreeIndex then
			DoSortFiles (Item [THeaderSourceFile.GetClassTreeIndex]);
	end;
end;

procedure TMainForm.AppCompSetMessage(const Msg: string);
begin
	with StatusBar do
		Panels.Items[6].Text := Msg + '...';
	if not Compiling then begin
		Compiling := True;
		CompUpdate;
		UpdateDebugSettings;
		with StatusBar.Panels do begin
			Items[5].Width := 5;
			Items[6].Width := 180;
			Items[7].Width := 100;
			Items[8].Width := 100;
			Items[9].Width := 1000000;
		end;
	end;
end;

procedure TMainForm.AppCompStartFile;
var
	I: Integer;
begin
	Application.Hint := '';
	CompFinishAndStop := False;
	CompStopNow := False;
	CompFileStartTime := Now;
	CompLastTime := 0;
	if not Compiling then begin
		CompStartTime := CompFileStartTime;
		ToolBarNewButton.Enabled := False;
		MenuFileNew.Enabled := False;
		ActionFileOpen.Enabled := False;
		ActionFileExit.Enabled := False;
		ActionProjectAddFiles.Enabled := False;
		ActionProjectCompile.Visible := False;
		ActionProjectMake.Visible := False;
		ActionProjectBuild.Visible := False;
		ActionProjectStopCompilation.Enabled := True;
		ActionProjectForceQuitCompiler.Enabled := True;
		ActionProjectStopCompilation.Visible := True;
		ActionProjectForceQuitCompiler.Visible := True;
		with RecentFiles do
			for I := 0 to Count - 1 do
				with TRecentFileMenuItem (Objects [I]) do begin
					FileMenuItem.Enabled := False;
					PopupMenuItem.Enabled := False;
				end;
	end;
end;

procedure TMainForm.AppCompStop;
var
	I: Integer;
begin
	if Compiling then begin
		Compiling := False;
		ActionProjectStopCompilation.Visible := False;
		ActionProjectForceQuitCompiler.Visible := False;
		ToolBarNewButton.Enabled := True;
		MenuFileNew.Enabled := True;
		ActionFileOpen.Enabled := True;
		ActionFileExit.Enabled := True;
		ActionProjectAddFiles.Enabled := True;
		ActionProjectCompile.Visible := True;
		ActionProjectMake.Visible := True;
		ActionProjectBuild.Visible := True;
		with RecentFiles do
			for I := 0 to Count - 1 do
				with TRecentFileMenuItem (Objects [I]) do begin
					FileMenuItem.Enabled := True;
					PopupMenuItem.Enabled := True;
				end;
		UpdateDebugSettings;
		with StatusBar.Panels do begin
			Items[5].Width := 0;
			Items[6].Width := 0;
			Items[7].Width := 0;
			Items[8].Width := 0;
			Items[9].Width := 0;
		end;
	end;
end;

procedure TMainForm.AppCompUpdate;
const
	SecondsPerDay = 24 * 60 * 60;
function CustomTimeToStr(ConvTime: TDateTime): string;
var
	Seconds: Integer;
	MinStr,
	SecStr: string;
begin
	Seconds := Trunc (ConvTime * SecondsPerDay);
	MinStr := IntToStr (Seconds div 60);
	SecStr := IntToStr (Seconds mod 60);
	while Length (SecStr) < 2 do
		SecStr := '0' + SecStr;
	Result := MinStr + ':' + SecStr;
end;
var
	CurTime: TDateTime;
begin
	CurTime := Now;
	if Trunc (CompLastTime * SecondsPerDay) <> Trunc (CurTime * SecondsPerDay) then begin
		CompLastTime := CurTime;
		with StatusBar, Panels do begin
			Items[7].Text := 'File Time: ' + CustomTimeToStr (CurTime - CompFileStartTime);
			Items[8].Text := 'Total Time: ' + CustomTimeToStr (CurTime - CompStartTime);
			Update;
		end;
	end;
end;

procedure TMainForm.DisplayFolderMessage;
var
	S: string;
begin
	if OperationSuccessful and (not OperationCancelled) and OpenFolderMessage then begin
		if ProjectTarget = ptArchive then
			S := 'The project has been compiled successfully.'#13#10#13#10'Archive Size: ' + IntToStr (ProgSize) + ' Bytes'#13#10#13#10'Do you want to open the project folder?'
		else
			with OptimizeInfo do begin
				S := 'The project has been compiled successfully.'#13#10#13#10'Program Variable Size: ' + IntToStr (ProgramSize) + ' Bytes'#13#10;
				if DataSize > 0 then
					S := S + 'Data Variable Size: ' + IntToStr (DataSize) + ' Bytes'#13#10;
				if BSSSize > 0 then
					S := S + 'BSS Size: ' + IntToStr (BSSSize) + ' Bytes'#13#10;
				S := S + 'Absolute Relocs: ' + IntToStr (RelocCount) + #13#10'Natively Emitted Relocs: ' + IntToStr (NativeRelocCount) + #13#10;
				if OptimizeBranchesResult > 0 then begin
					if OptimizeBranches then
						S := S + 'Relocs Saved'
					else
						S := S + 'Relocs Savable';
					S := S + ' by Branch Optimization: ' + IntToStr (OptimizeBranchesResult) + #13#10;
				end;
				if OptimizeMovesResult > 0 then begin
					if OptimizeMoves then
						S := S + 'Relocs Saved'
					else
						S := S + 'Relocs Savable';
					S := S + ' by Move Optimization: ' + IntToStr (OptimizeMovesResult) + #13#10;
				end;
				if OptimizeTestsResult > 0 then begin
					if OptimizeTests then
						S := S + 'Relocs Saved'
					else
						S := S + 'Relocs Savable';
					S := S + ' by Test Optimization: ' + IntToStr (OptimizeTestsResult) + #13#10;
				end;
				if OptimizeCalcsResult > 0 then begin
					if OptimizeCalcs then
						S := S + 'Relocs Saved'
					else
						S := S + 'Relocs Savable';
					S := S + ' by Calculation Optimization: ' + IntToStr (OptimizeCalcsResult) + #13#10;
				end;
				if UseFLineJumpsResult > 0 then begin
					if UseFLineJumps or Use4ByteFLineJumps then
						S := S + 'Relocs Saved'
					else
						S := S + 'Relocs Savable';
					S := S + ' by F-Line Jumps: ' + IntToStr (UseFLineJumpsResult) + #13#10;
				end;
				if CutRangesResult > 0 then begin
					if CutRanges then
						S := S + 'Space Saved'
					else
						S := S + 'Space Savable';
					S := S + ' by Range-Cutting: ' + IntToStr (CutRangesResult) + ' Bytes'#13#10;
				end;
				if NearAssemblyResult > 0 then
					S := S + 'Space Savable by Using GNU Assembler ''-l'' Switch: ' + IntToStr (NearAssemblyResult) + ' Bytes'#13#10;
				S := S + #13#10'Do you want to open the project folder?';
			end;
		if ShowDefaultMessageBox (S, 'Compilation Successful', mtQuestion) = idYes then
			ShellExecute (0, nil, PChar (ExtractFilePath (ProjectFile)), nil, nil, sw_ShowNormal);
	end;
end;

procedure TMainForm.ProjectStopCompilation(Sender: TObject);
begin
	CompFinishAndStop := True;
	ActionProjectStopCompilation.Enabled := False;
end;

procedure TMainForm.ProjectForceQuitCompiler(Sender: TObject);
begin
	CompStopNow := True;
end;

procedure TMainForm.WMDropFiles(var Msg: TMessage);
var
	DropHandle: THandle;
	I,
	FileCount: Integer;
	CurFile: array [0..512] of Char;
begin
	DropHandle := Msg.WParam;
	FileCount := DragQueryFile (DropHandle, High (Cardinal), nil, 0);
	for I := 0 to FileCount - 1 do begin
		DragQueryFile (DropHandle, I, CurFile, SizeOf (CurFile) - 1);
		if LowerCase (ExtractFileExt (AnsiString (CurFile))) = LowerCase (ProjectFileExt) then
			FileOpen (AnsiString (CurFile))
		else
			AddSourceFile (AnsiString (CurFile), True);
	end;
	DragFinish (DropHandle);
end;

procedure TMainForm.HelpNews(Sender: TObject);
begin
	with TNewsForm.Create (Self) do try
		Execute;
	finally
		Free;
	end;
end;

function TMainForm.GetTiEmuInterface: ITiEmuOLE;
var
	ExitCode: Cardinal;
	Unknown: IUnknown;
	OLEResult: HResult;
begin
	OLEResult := GetActiveObject(CLASS_TiEmuOLE, nil, Unknown);
	if OLEResult = S_OK then begin
		OleCheck(Unknown.QueryInterface(ITiEmuOLE, Result));
	end else begin
		{ If no TiEmu path is set, try looking it up from the registry. }
		if Length (VTIPath) = 0 then begin
			with TRegistry.Create do try
				RootKey := HKey_Classes_Root;
				if OpenKeyReadOnly ('\CLSID\{B2A17B13-9D6F-4DD4-A2A9-6FE06ADC1D33}\LocalServer32') then try
					if ValueExists (nil) then
						VTIPath := ReadString (nil);
				except end;
			finally
				Free;
			end;
		end;
		if Length (VTIPath) > 0 then begin
			with TVTIStartForm.Create (Self) do try
				if ShowModal = mrOK then begin
					Result := TiEmuInterface;
				end else
					Abort;
			finally
				Free;
			end;
		end else begin
			ShowDefaultMessageBox ('TiEmu is not running.', 'Error', mtProgramError);
			Abort;
		end;
	end;
end;

procedure TMainForm.SendFiles(FNList: array of string);
var
	TiEmuInterface: ITiEmuOLE;
procedure SendKey(Key: Byte);
begin
	PostMessage (Win, WM_KEYDOWN, Key, 0);
	PostMessage (Win, WM_KEYUP, Key, 0);
end;
var
	I: Integer;
	EditWin,
	ButtonWin: HWnd;
	StartTime: Cardinal;
	FileString: string;
	Name: array [0..32] of Char;
	Connection: TLinkConnection;
	Size: Word;
	Total: Cardinal;
	Progress: DWord;
	ProgressForm: TSendProgressForm;
begin
	OperationSuccessful := False;
	OperationCancelled := False;
	if Length (FNList) > 0 then begin
		if TransferTarget = ttVTI then begin
			TiEmuInterface := GetTiEmuInterface;
			GetWindowThreadProcessID (Win, @ProcID);
			SendKey (VK_SCROLL);
			SendKey (VK_ESCAPE);
			if CurVTIType = cvTI89 then
				SendKey (VK_HOME);
			SendKey (VK_F10);
			StartTime := GetTickCount;
			SendWin := 0;
			repeat
				EnumWindows (@EnumWindowsFunc, 0);
			until (SendWin <> 0) or (GetTickCount - StartTime >= 10000);
			if SendWin = 0 then begin
				ShowDefaultMessageBox ('Error displaying send dialog.', 'Error', mtProgramError);
				Abort;
			end else begin
				SetForegroundWindow (SendWin);
				repeat
					EditWin := GetWindow (SendWin, GW_CHILD);
					GetClassName (EditWin, Name, 32);
					while (EditWin <> 0) and (UpperCase (AnsiString (Name)) <> 'EDIT') do begin
						EditWin := GetWindow (EditWin, GW_HWNDNEXT);
						if EditWin <> 0 then
							GetClassName (EditWin, Name, 32);
					end;
					if EditWin <> 0 then begin
						StartTime := GetTickCount;
						while (SendMessage (EditWin, WM_GETTEXTLENGTH, 0, 0) <= 0) and (GetTickCount - StartTime < 5000) do;
						FileString := '';
						for I := Low (FNList) to High (FNList) do begin
							if CurVTIType = cvTI92Plus then
								FNList [I] := StringReplace (FNList [I], '.89', '.9x', []);
							if not FileExists (FNList [I]) then begin
								ShowDefaultMessageBox ('The file "' + FNList [I] + '" could not be found.', 'Error', mtProgramError);
								Abort;
							end;
							Insert ('"' + FNList [I] + '" ', FileString, Length (FileString) + 1);
						end;
						Delete (FileString, Length (FileString), 1);
						SendMessage (EditWin, WM_SETTEXT, 0, Integer (PChar (FileString)));
					end;
					ButtonWin := GetWindow (SendWin, GW_CHILD);
					GetClassName (ButtonWin, Name, 32);
					while (ButtonWin <> 0) and ((UpperCase (AnsiString (Name)) <> 'BUTTON') or ((GetWindowLong (ButtonWin, GWL_STYLE) and BS_DEFPUSHBUTTON) = 0) or ((GetWindowLong (ButtonWin, GWL_STYLE) and BS_CHECKBOX) <> 0)) do begin
						ButtonWin := GetWindow (ButtonWin, GW_HWNDNEXT);
						if ButtonWin <> 0 then
							GetClassName (ButtonWin, Name, 32);
					end;
				until SendMessage (EditWin, WM_GETTEXTLENGTH, 0, 0) >= Length (FNList [Low (FNList)]);
				if ButtonWin <> 0 then begin
					SendMessage (ButtonWin, WM_LBUTTONDOWN, 0, 0);
					SendMessage (ButtonWin, WM_LBUTTONUP, 0, 0);
				end;
			end;
			ShowWindow (Win, SW_SHOWNORMAL);
			SetForegroundWindow (Win);
		end else if TransferTarget = ttCalc then begin
			FillChar (Connection, SizeOf (Connection), 0);
			Connection.Port := LinkPort;
			Connection.CableType := LinkCable;
			if CreateConnection (Connection) then try
				if OpenConnection (Connection) and GetCalcType (Connection) then begin
					Total := 0;
					for I := Low (FNList) to High (FNList) do begin
						case Connection.CalcType of
							cdTI92Plus:
								FNList [I] := StringReplace (FNList [I], '.89', '.9x', []);
							cdV200:
								FNList [I] := StringReplace (FNList [I], '.89', '.v2', []);
							cdTI92:
								FNList [I] := StringReplace (FNList [I], '.89', '.92', []);
						end;
						if not FileExists (FNList [I]) then begin
							ShowDefaultMessageBox ('The file "' + FNList [I] + '" could not be found.', 'Error', mtProgramError);
							Abort;
						end;
						if not CheckFileFormat (Connection, PChar (FNList [I]), nil, nil, @Size) then begin
							ShowDefaultMessageBox ('Error sending file.', 'Error', mtProgramError);
							Abort;
						end;
						Inc (Total, Size);
					end;
					Enabled := False;
					try
						ProgressForm := TSendProgressForm.Create (Self);
						with ProgressForm do try
							ProgressBar.Max := Total;
							Show;
							Update;
							Progress := 0;
							for I := Low (FNList) to High (FNList) do begin
								FileNameLabel.Caption := ExtractFileName (FNList [I]);
								if not SendFile (Connection, nil, nil, PChar (FNList [I]), @Progress, ProgressProg, ProgressForm) then begin
									if Cancelled then
										OperationCancelled := True
									else
										ShowDefaultMessageBox ('Error sending file. Please check whether your calculator is on the home screen.', 'Error', mtProgramError);
									Abort;
								end;
							end;
						finally
							CloseNow := True;
							Free;
						end;
					finally
						Enabled := True;
					end;
				end else begin
					ShowDefaultMessageBox ('Error linking to calculator.', 'Error', mtProgramError);
					Abort;
				end;
			finally
				CloseConnection (Connection);
			end else begin
				ShowDefaultMessageBox ('Error opening link port.', 'Error', mtProgramError);
				Abort;
			end;
		end;
		OperationSuccessful := True;
	end;
end;

procedure TMainForm.ExecuteCommandLine(const Line: string);
var
	TiEmuInterface: ITiEmuOLE;
	I: Integer;
	Connection: TLinkConnection;
begin
	if TransferTarget = ttVTI then begin
		TiEmuInterface := GetTiEmuInterface;
		try begin
			if not TiEmuInterface.execute_command(Line) then Abort;
		end except begin
			ShowDefaultMessageBox ('OLE function call failed.', 'Error', mtProgramError);
			Abort;
		end;
	end else if TransferTarget = ttCalc then begin
		FillChar (Connection, SizeOf (Connection), 0);
		Connection.Port := LinkPort;
		Connection.CableType := LinkCable;
		if CreateConnection (Connection) then try
			if OpenConnection (Connection) then
				ExecuteHomeLine (Connection, Line)
			else begin
				ShowDefaultMessageBox ('Error linking to calculator.', 'Error', mtProgramError);
				Abort;
			end;
		finally
			CloseConnection (Connection);
		end else begin
			ShowDefaultMessageBox ('Error opening link port.', 'Error', mtProgramError);
			Abort;
		end;
	end;
end;

procedure TMainForm.DebugRun(Sender: TObject);
var
	Files: array of string;
begin
	OperationSuccessful := True;
	OperationCancelled := False;
	if Invalidated then
		MakeFileWrapped;
	if OperationSuccessful and (not OperationCancelled) and (FileExists (ChangeFileExt (ProjectFile, '.89z')) or FileExists (ChangeFileExt (ProjectFile, '.9xz')) or FileExists (ChangeFileExt (ProjectFile, '.v2z')) or FileExists (ChangeFileExt (ProjectFile, '.92p'))) then begin
		if Runnable then begin
			SetLength (Files, 0);
			case ProjectTarget of
				ptRegular: begin
					SetLength (Files, 1);
					Files [High (Files)] := ChangeFileExt (ProjectFile, '.89z');
					if Pack and (ssPack in SpecialSupport) then begin
						SetLength (Files, Length (Files) + 1);
						Files [High (Files)] := ChangeFileExt (ProjectFile, '.89y');
					end;
					if UseDataVar then begin
						SetLength (Files, Length (Files) + 1);
						Files [High (Files)] := ChangeFileExt (ProjectFile, '-data.89y');
					end;
				end;
				ptFargo: begin
					SetLength (Files, 1);
					Files [High (Files)] := ChangeFileExt (ProjectFile, '.92p');
				end;
			end;
			if Length (Files) > 0 then begin
				SendFiles (Files);
				if OperationSuccessful then
					ExecuteCommandLine (TopNode.Text + '(' + CommandLine + ')');
			end;
		end else
			DisplayFolderMessage;
	end;
end;

procedure TMainForm.DebugPause(Sender: TObject);
var
	TiEmuInterface: ITiEmuOLE;
procedure SendKey(Key: Byte);
begin
	PostMessage (Win, WM_KEYDOWN, Key, 0);
	PostMessage (Win, WM_KEYUP, Key, 0);
end;
begin
	TiEmuInterface := GetTiEmuInterface;
	try begin
		TiEmuInterface.enter_debugger;
	end except begin
		ShowDefaultMessageBox ('OLE function call failed.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.DebugReset(Sender: TObject);
var
	TiEmuInterface: ITiEmuOLE;
procedure SendKey(Key: Byte);
begin
	PostMessage (Win, WM_KEYDOWN, Key, 0);
	PostMessage (Win, WM_KEYUP, Key, 0);
end;
begin
	TiEmuInterface := GetTiEmuInterface;
	try begin
		TiEmuInterface.reset_calc(false);
	end except begin
		ShowDefaultMessageBox ('OLE function call failed.', 'Error', mtProgramError);
	end;
end;

function TMainForm.GetInvalidated: Boolean;
var
	I: Integer;
begin
	Result := FInvalidated;
	if not Result then
		with SourceFiles do
			for I := 0 to Count - 1 do
				with Items [I] as TSourceFile do
					if (Compilable or (Items [I] is THeaderSourceFile)) and Invalidated then begin
						Result := True;
						Break;
					end;
end;

procedure TMainForm.UpdateDebugSettings;
var
	CanRun: Boolean;
begin
	CanRun := Runnable;
	ActionDebugRun.Enabled := not Compiling;
	ActionDebugPause.Enabled := (not Compiling) and (TransferTarget = ttVTI);
	ActionDebugReset.Enabled := (not Compiling) and (TransferTarget = ttVTI);
	MainMenuDebug.Visible := CanRun;
	if not CanRun then
		ToolBarDebugLine.Parent := nil;
	ToolBarRunButton.Visible := CanRun;
	ToolBarPauseButton.Visible := CanRun;
	ToolBarDebugLine.Visible := CanRun;
	if CanRun then begin
		ToolBarDebugLine.Parent := MainToolBar;
		ToolBarDebugLine.Left := ToolBarPauseButton.Left + ToolBarPauseButton.Width;
	end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
var
	NewSelection: TTreeNode;
begin
	if (Key = vk_F9) and (Shift = [ssShift, ssCtrl, ssAlt]) then begin
		if TransferTarget = ttVTI then
			TransferTarget := ttCalc
		else
			TransferTarget := ttVTI;
		SavePreferences;
		UpdateDebugSettings;
	end else if ((Key = vk_Tab) or (Key = Ord('G'))) and (Shift = [ssCtrl]) then begin
		NewSelection := ProjectTree.Selected;
		if Assigned (NewSelection) then begin
			repeat
				NewSelection := NewSelection.GetNext;
				if not Assigned (NewSelection) then
					NewSelection := TopNode;
			until (NewSelection.IsVisible and Assigned (NewSelection.Data)) or (NewSelection = ProjectTree.Selected);
			if Assigned (NewSelection.Data) then
				SelectNode (NewSelection);
			if Assigned (CurrentEditor) and CurrentEditor.Enabled then try
				CurrentEditor.SetFocus;
			except end;
		end;
	end;
end;

procedure TMainForm.SelectNode(Node: TTreeNode);
var
	Allow: Boolean;
begin
	if PreviousNode <> Node then begin
		Allow := True;
		ProjectTreeChanging (ProjectTree, Node, Allow);
		if Allow then begin
			NoHideEditor := True;
			ProjectTree.Selected := Node;
			NoHideEditor := False;
		end;
	end;
end;

procedure TMainForm.ChangeNotificationTick(Sender: TObject);
var
	I: Integer;
begin
	if not InChangeNotification then begin
		InChangeNotification := True;
		with SourceFiles do
			for I := 0 to Count - 1 do
				with TSourceFile (Items [I]) do
					TestChange;
		InChangeNotification := False;
	end;
end;

procedure TMainForm.ResetProjectSettings;
begin
	ProjectTarget         := ptRegular;
	UseDataVar            := False;
	DataVar               := '';
	DataVarCopy           := True;
	DataVarCopyIfArchived := True;
	Pack                  := False;
	PackVar               := '';
	GCCSwitches           := '-Os -Wall -W -Wwrite-strings -ffunction-sections -fdata-sections';
	AsSwitches            := '';
	AsmSwitches           := '-g -t';
	DebugInfo             := False;
	StdLib                := True;
	InitBSS               := True;
	OptimizeNOPs          := True;
	OptimizeReturns       := True;
	OptimizeBranches      := True;
	OptimizeMoves         := True;
	OptimizeTests         := True;
	OptimizeCalculations  := True;
	RemoveUnusedSections  := True;
	CutUnusedRanges       := True;
	ReorderSections       := True;
	MergeConstants        := True;
	OutputBin             := False;
	CommandLine           := '';
	PostBuildProcessFile  := '';
	if Assigned (PredefinedLibOptions) then
		with PredefinedLibOptions do begin
			CalcDests                := [cdTI89, cdTI92Plus, cdV200];
			OptimizeCalcConsts       := False;
			KernelFormat             := kfNone;
			UseMinAMS                := True;
			MinAMS                   := '1.00';
			RelocFormat              := rfAMS;
			ROMCallFormat            := rfDirect;
			BSSRefFormat             := rfKernel;
			DataRefFormat            := rfKernel;
			UseFLineJumps            := False;
			UseInternalFLineEmulator := False;
			UseReturnValue           := False;
			EnableErrorReturn        := False;
			SaveScreen               := True;
		end;
end;

procedure TMainForm.AddToRecent(const FileName: string);
var
	I: Integer;
	O: TRecentFileMenuItem;
begin
	with RecentFiles do begin
		for I := 0 to Count - 1 do
			if (I >= MaxRecentFiles - 1) or (UpperCase (Strings [I]) = UpperCase (FileName)) then begin
				if I = 0 then
					Exit
				else begin
					Objects[I].Free;
					Delete (I);
					Break;
				end;
			end;
		RecentFilesLine.Visible := True;
		O := TRecentFileMenuItem.Create;
		with O do begin
			FileMenuItem := TMenuItem.Create (Self);
			with FileMenuItem do begin
				Caption := WithoutExt (ExtractFileName (FileName));
				Hint := FileName;
				OnClick := RecentFileClick;
			end;
			MainMenuFile.Insert (RecentFilesLine.MenuIndex + 1, FileMenuItem);
			PopupMenuItem := TMenuItem.Create (Self);
			with PopupMenuItem do begin
				Caption := FileName;
				Hint := FileName;
				OnClick := RecentFileClick;
			end;
			RecentFilesPopup.Items.Insert (0, PopupMenuItem);
		end;
		InsertObject (0, FileName, O);
	end;
	UpdateRecent;
end;

procedure TMainForm.UpdateRecent;
begin
	RecentFilesLine.Visible := RecentFiles.Count > 0;
	NoFilesItem.Visible := RecentFiles.Count <= 0;
end;

procedure TMainForm.RecentFileClick(Sender: TObject);
var
	I: Integer;
	FileName: string;
begin
	FileName := (Sender as TMenuItem).Hint;
	if FileExists (FileName) then
		FileOpen (FileName)
	else begin
		with RecentFiles do
			for I := 0 to Count - 1 do
				if UpperCase (Strings [I]) = UpperCase (FileName) then begin
					Objects[I].Free;
					Delete (I);
					Break;
				end;
		UpdateRecent;
		ShowDefaultMessageBox ('File ''' + WithoutExt (ExtractFileName (FileName)) + ''' not found.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.ToolsConfigure(Sender: TObject);
var
	I: Integer;
begin
	with TToolsForm.Create (Self) do try
		with Self.ToolsList do
			for I := 0 to Count - 1 do
				with ToolsList.Items.Add, TToolsListItem (Items [I]) do begin
					Caption := Title;
					SubItems.Add (CommandLine);
					SubItems.Add (WorkingDir);
					SubItems.Add (WindowStateToString (WindowState));
				end;
		if ShowModal = mrOK then begin
			Self.ToolsList.Clear;
			with ToolsList.Items do begin
				ToolsLine.Visible := Count > 0;
				for I := 0 to Count - 1 do
					with Item [I] do
						AddTool (Caption, SubItems [0], SubItems [1], StringToWindowState (SubItems [2]));
			end;
			SavePreferences;
		end;
	finally
		Free;
	end;
end;

procedure TMainForm.ToolClick(Sender: TObject);
var
	WorkDir: PChar;
	StartupInfo: TStartupInfo;
	ProcessInfo: TProcessInformation;
begin
	with TToolsListItem ((Sender as TMenuItem).Tag) do begin
		FillChar (StartupInfo, SizeOf (StartupInfo), 0);
		StartupInfo.cb := SizeOf (StartupInfo);
		if Length (WorkingDir) > 0 then
			WorkDir := PChar (WorkingDir)
		else
			WorkDir := nil;
		StartupInfo.dwFlags := StartF_UseShowWindow;
		case WindowState of
			wsMaximized:
				StartupInfo.wShowWindow := SW_SHOWMAXIMIZED;
			wsMinimized:
				StartupInfo.wShowWindow := SW_SHOWMINIMIZED;
			else
				StartupInfo.dwFlags := 0;
		end;
		if CreateProcess (nil, PChar (CommandLine), nil, nil, False, CREATE_NEW_PROCESS_GROUP or DETACHED_PROCESS, nil, WorkDir, StartupInfo, ProcessInfo) then begin
			CloseHandle (ProcessInfo.hProcess);
			CloseHandle (ProcessInfo.hThread);
		end else
			ShowDefaultMessageBox ('An error occurred while trying to start the application.', 'Error', mtProgramError);
	end;
end;

procedure TMainForm.AddTool(const Title, CommandLine, WorkingDir: string; WindowState: TWindowState);
var
	O: TToolsListItem;
begin
	O := TToolsListItem (Self.ToolsList.Add);
	O.Title := Title;
	O.CommandLine := CommandLine;
	O.WorkingDir := WorkingDir;
	O.WindowState := WindowState;
	O.MenuItem := TMenuItem.Create (Self);
	with O.MenuItem do begin
		Tag := Integer (O);
		Caption := Title;
		Hint := CommandLine;
		OnClick := ToolClick;
	end;
	MainMenuTools.Insert (MainMenuTools.Count, O.MenuItem);
end;

procedure TMainForm.ProjectTreeStartDrag(Sender: TObject;
	var DragObject: TDragObject);
begin
	DragObject := TNoImageDragObject.Create (Sender as TControl);
end;

procedure TMainForm.UpdateProgramOutput;
begin
	ActionProjectShowProgramOutput.Enabled := (MainConsole.LastOutSize > 2) or (MainConsole.LastErrSize > 2);
end;

procedure TMainForm.ShowProgramOutput(Sender: TObject);
begin
	with TProgramOutputForm.Create (Self) do try
		OutputMemo.Text := MainConsole.LastOutText;
		ErrorMemo.Text := MainConsole.LastErrText;
		ShowModal;
	finally
		Free;
	end;
end;

procedure TMainForm.ClearDebugInfo;
begin
end;

procedure TMainForm.FileNewFolder(Sender: TObject);
var
	ParentNode,
	Node: TTreeNode;
	Folder: TFolder;
begin
	ParentNode := ProjectTree.Selected;
	while Assigned (ParentNode) and Assigned (ParentNode.Data) and (not (TObject (ParentNode.Data) is TFolder)) do
		ParentNode := ParentNode.Parent;
	if Assigned (ParentNode) and Assigned (ParentNode.Parent) then begin
		Folder := TFolder.Create;
		Node := ProjectTree.Items.AddChildObject (ParentNode, 'New Folder', Folder);
		Folder.TreeItem := Node;
		with Node do begin
			ImageIndex    := 0;
			SelectedIndex := 1;
      Selected := True;
			EditText;
		end;
	end;
end;

function TMainForm.GetSourceTypeFolder(Node: TTreeNode): TTreeNode;
begin
	if Assigned (Node) and Assigned (Node.Parent) then begin
		Result := Node;
		while Assigned (Result) and Assigned (Result.Data) do
			Result := Result.Parent;
	end else
		Result := nil;
end;

function TMainForm.FindFileInsertionPoint(ParentNode: TTreeNode): TTreeNode;
begin
	if Assigned (ParentNode) then begin
		Result := ParentNode.GetFirstChild;
		while Assigned (Result) and Assigned (Result.Data) and (TObject (Result.Data) is TSourceFile) do
			Result := Result.GetNextSibling;
	end else
		Result := nil;
end;

function TMainForm.CreateFileNode(ParentNode: TTreeNode; SourceFile: TSourceFile): TTreeNode;
begin
	if Assigned (SourceFile.Folder) then
		ParentNode := SourceFile.Folder.TreeItem;
	if Assigned (ParentNode) then begin
		Result := FindFileInsertionPoint (ParentNode);
		if Assigned (Result) then
			Result := ProjectTree.Items.InsertObject (Result, SourceFile.SourceName, SourceFile)
		else
			Result := ProjectTree.Items.AddChildObject (ParentNode, SourceFile.SourceName, SourceFile);
	end else
		Result := nil;
end;

procedure TMainForm.FileNewFile(Sender: TObject);
var
	Node: TTreeNode;
begin
	if Assigned (ProjectTree.Selected) then begin
		Node := GetSourceTypeFolder (ProjectTree.Selected);
		if Assigned (Node) then
			if Node.Index = THeaderSourceFile.GetClassTreeIndex then
				FileNewHeaderFile (Sender)
			else if Node.Index = TCSourceFile.GetClassTreeIndex then
				FileNewCSourceFile (Sender)
			else if Node.Index = TGNUAsmSourceFile.GetClassTreeIndex then
				FileNewGNUAssemblerSourceFile (Sender)
			else if (ssA68k in SpecialSupport) and (Node.Index = TAsmSourceFile.GetClassTreeIndex) then
				FileNewAssemblerSourceFile (Sender)
			else if (ssQuill in SpecialSupport) and (Node.Index = TQuillSourceFile.GetClassTreeIndex) then
				FileNewQuillSourceFile (Sender)
			else if Node.Index = TNormalTextSourceFile.GetClassTreeIndex then
				FileNewTextFile (Sender);
	end;
end;

function TMainForm.GetSelectedFolder(SourceClass: TSourceFileClass): TFolder;
var
	Node: TTreeNode;
begin
	Result := nil;
	Node := ProjectTree.Selected;
	while Assigned (Node) and (Node <> TopNode.Item [SourceClass.GetClassTreeIndex]) do begin
		if (not Assigned (Result)) and Assigned (Node.Data) and (TObject (Node.Data) is TFolder) then
			Result := Node.Data;
		Node := Node.Parent;
	end;
	if not Assigned (Node) then
		Result := nil;
end;

procedure TMainForm.ErrorListKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
var
	S: string;
	Item: TListItem;
begin
	if ((Key = VK_INSERT) or (Key = Ord('C'))) and (Shift = [ssCtrl]) then
		with ErrorList do begin
			S := '';
			Item := Selected;
			while Assigned (Item) do begin
				S := S + Item.Caption + #13#10;
				Item := GetNextItem (Item, sdAll, [isSelected]);
			end;
			Clipboard.AsText := S;
		end;
end;

{$IFDEF CODINGEXT}
procedure TMainForm.InitCodingExt;
var
	 ActionFindSymbol: TAction;
begin
	// Find Symbol declaration tool
	ActionFindSymbol := TAction.Create(Self);
	with ActionFindSymbol do
	begin
		ActionList := Actions;
		Caption := 'F&ind Symbol Declaration';
		Category := 'Extension';
		OnExecute := ActionFindSymbolExecute;
	end;

	InsertsAction(Self, [OpenFileAtCursor1, OpenFileatCursor2], ActionFindSymbol);
end;

procedure TMainForm.ActionFindSymbolExecute(Sender: TObject);
begin
	CompForm.FindSymbolDecl;
end;
{$ENDIF}

end.

