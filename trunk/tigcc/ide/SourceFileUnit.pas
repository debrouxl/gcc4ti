{
  TIGCC IDE

  Copyright (C) 2000-2004 Sebastian Reichelt
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

unit SourceFileUnit;

interface

uses
	FolderUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, ComCtrls,
	MemoComponentUnit, SourceEditUnit;

type
	TSourceFile = class;

	TSourceFileType = (ftCFile, ftGNUAsmFile, ftA68kAsmFile, ftQuillFile, ftOther);
	TBugType = (btError, btWarning, btInfo);
	TErrorCallback = procedure(const Line: string; BugTp: TBugType; SourceF: string; ErrFunc: string; ErrMessage: string; SourceLn: Integer; Offset: Integer) of object;
	TSourceFileFunction = record
		Name: string;
		PrototypeLine,
		ImplementationLine: Integer;
	end;
	TSourceFileFunctions = array of TSourceFileFunction;
	PSourceFileFunctions = ^TSourceFileFunctions;

	TSourceFileClass = class of TSourceFile;

	TSourceFiles = class(TCollection)
	public
		constructor Create;
		function FindFile(const FN: string): TSourceFile;
		function FindFileWithoutExt(const FN: string): TSourceFile;
		function FindFileNameOnly(const FN: string): TSourceFile;
		function FindFileOfType(FileType: TSourceFileClass): TSourceFile;
		function FindFileOfTypeInProject(FileType: TSourceFileClass): TSourceFile;
	end;

	TSourceFile = class(TCollectionItem)
	private
		FFileName: string;
		FParentForm: TForm;
		FTreeItem: TTreeNode;
		FInvalidated: Boolean;
		FOnError: TErrorCallback;
		FErrorList: TListView;
		FModified: Boolean;
		FFolder: TFolder;
		function GetSourceName: string;
		procedure SetSourceName(const Value: string);
		function GetEditing: Boolean;
		function GetDynamicName: string;
		procedure SetEditing(const Value: Boolean);
		procedure SetCaption(const Value: TCaption);
		function GetCaption: TCaption;
		function GetLogicalFileName: string;
		function GetFolderPath: string;
	protected
		Modifying: Boolean;
		FLastChangeTime: Integer;
		function GetInProject: Boolean; virtual;
		procedure SetFileName(const Value: string); virtual;
		procedure SetModified(const Value: Boolean); virtual;
		function GetEditor: TWinControl; virtual;
		procedure ProcessErrors(const ErrText: string); virtual;
		procedure ProcessErrorLine(Line: string); dynamic;
	public
		class function GetCompilable: Boolean; virtual;
		class function GetPrintable: Boolean; virtual;
		class function GetClassFilter: string; virtual;
		class function GetClassItemName: string; virtual;
		class function GetClassTreeIndex: Integer; virtual;
		class function GetClassImageIndex: Integer; virtual;
		destructor Destroy; override;
		class function GetAppropriateClassFromName(ItemNm: string): TSourceFileClass;
		class function GetAppropriateClassFromExt(Ext: string): TSourceFileClass;
		class function GetAppropriateClassFromTreeIndex(Index: Integer): TSourceFileClass;
		procedure WriteToFile(const FN: string = ''; SetFN: Boolean = False); virtual;
		procedure LoadFromFile(const FN: string = ''; SetFN: Boolean = False); virtual;
		procedure Save; virtual;
		procedure SaveAs; virtual;
		class function CanSave: Boolean; virtual;
		procedure Compile; virtual;
		procedure Print(Copies: Integer; PrintRange: TPrintRange; FromPage, ToPage: Integer); virtual;
		function CountPages: Integer; virtual;
		procedure Modify;
		procedure WarnIfModified;
		procedure Invalidate;
		procedure Edit; virtual;
		procedure TestChange; virtual;
		procedure UpdateProgramOutput; virtual;

		property InProject: Boolean read GetInProject;
		property Editor: TWinControl read GetEditor;
		property SourceName: string read GetSourceName write SetSourceName;
		property DynamicName: string read GetDynamicName;
	published
		property FileName: string read FFileName write SetFileName;
		property Editing: Boolean read GetEditing write SetEditing;
		property Modified: Boolean read FModified write SetModified;
		property Invalidated: Boolean read FInvalidated write FInvalidated;
		property TreeItem: TTreeNode read FTreeItem write FTreeItem;
		property ParentForm: TForm read FParentForm write FParentForm;
		property ErrorList: TListView read FErrorList write FErrorList;
		property Caption: TCaption read GetCaption write SetCaption;
		property OnError: TErrorCallback read FOnError write FOnError;
		property Folder: TFolder read FFolder write FFolder;
		property FolderPath: string read GetFolderPath;
		property LogicalFileName: string read GetLogicalFileName;
	end;

	TTextSourceFile = class(TSourceFile)
	private
		FModifyLevel: Integer;
	protected
		LineStartList: TIntegerList;
		FTempContent: string;
		function GetEditor: TWinControl; override;
		function GetTextEditor: TMemoComponent; virtual; abstract;
		function GetInternalTextEditor: TMemoComponent; virtual; abstract;
		function GetContent: string; virtual;
		procedure SetContent(const Value: string); virtual;
		procedure SetModified(const Value: Boolean); override;
		procedure Change(Sender: TObject); virtual;
		procedure ReplaceText(Sender: TObject; Pos, Change: Integer); virtual;
		procedure SetParentForm(const Value: TForm); virtual;
		property InternalTextEditor: TMemoComponent read GetInternalTextEditor;
	public
		class function GetPrintable: Boolean; override;
		destructor Destroy; override;
		procedure WriteToFile(const FN: string = ''; SetFN: Boolean = False); override;
		procedure LoadFromFile(const FN: string = ''; SetFN: Boolean = False); override;
		procedure Print(Copies: Integer; PrintRange: TPrintRange; FromPage, ToPage: Integer); override;
		function CountPages: Integer; override;
		procedure TestChange; override;
		procedure AskForReload; virtual;
		procedure UpdateEditor; virtual;
		function GetCompiledLineStart(Line: Integer): Integer; virtual;
		property TextEditor: TMemoComponent read GetTextEditor;
		property Content: string read GetContent write SetContent;
	end;

	TBinarySourceFile = class(TSourceFile)
	public
		class function CanSave: Boolean; override;
		procedure WriteToFile(const FN: string = ''; SetFN: Boolean = False); override;
	end;

	TNormalTextSourceFile = class(TTextSourceFile)
	private
		FEditor: TMemoComponent;
	protected
		function GetTextEditor: TMemoComponent; override;
		function GetInternalTextEditor: TMemoComponent; override;
	public
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
		destructor Destroy; override;
	end;

	TSourceTextSourceFile = class(TTextSourceFile)
	private
		FEditor: TSourceEdit;
		function GetSourceEditor: TSourceEdit;
	protected
		function GetTextEditor: TMemoComponent; override;
		function GetInternalTextEditor: TMemoComponent; override;
		function GetContentType: TSourceFileType; virtual;
		property InternalSourceEditor: TSourceEdit read FEditor;
	public
		destructor Destroy; override;
		procedure UpdateSyntax; virtual; abstract;
		procedure UpdateEditor; override;
		procedure SplitAndWriteToFile(const FN: string); virtual;
		function GetFunctions: TSourceFileFunctions; virtual;
		property SourceEditor: TSourceEdit read GetSourceEditor;
		property ContentType: TSourceFileType read GetContentType;
	end;

	THeaderSourceFile = class(TSourceTextSourceFile)
	protected
		procedure SetContent(const Value: string); override;
		procedure SetModified(const Value: Boolean); override;
		function GetContentType: TSourceFileType; override;
	public
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
		procedure UpdateSyntax; override;
		procedure UpdateEditor; override;
		procedure UpdateContentType(CT: TSourceFileType); virtual;
	end;

	TCSourceFile = class(TSourceTextSourceFile)
	protected
		CurErrFunction: string;
		InAssemblingState: Boolean;
		SpecialSwitches: string;
		procedure SetModified(const Value: Boolean); override;
		function GetContentType: TSourceFileType; override;
		procedure ProcessErrorLine(Line: string); override;
		procedure ProcessSFile(const SourceFile, DestFile: string);
	public
		class function GetCompilable: Boolean; override;
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
		procedure Save; override;
		procedure Compile; override;
		procedure UpdateSyntax; override;
		procedure UpdateEditor; override;
	end;

	TGNUAsmSourceFile = class(TSourceTextSourceFile)
	protected
		procedure SetModified(const Value: Boolean); override;
		function GetContentType: TSourceFileType; override;
		procedure ProcessErrorLine(Line: string); override;
	public
		class function GetCompilable: Boolean; override;
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
		procedure Save; override;
		procedure Compile; override;
		procedure UpdateSyntax; override;
		procedure UpdateEditor; override;
	end;

	TAsmSourceFile = class(TSourceTextSourceFile)
	protected
		procedure SetModified(const Value: Boolean); override;
		function GetContentType: TSourceFileType; override;
	public
		class function GetCompilable: Boolean; override;
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
		procedure Save; override;
		procedure Compile; override;
		procedure UpdateSyntax; override;
		procedure UpdateEditor; override;
	end;

	TQuillSourceFile = class(TCSourceFile)
	protected
		function GetContentType: TSourceFileType; override;
	public
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
		procedure Compile; override;
		procedure UpdateSyntax; override;
	end;

	TObjectSourceFile = class(TBinarySourceFile)
	public
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
	end;

	TArchiveSourceFile = class(TBinarySourceFile)
	public
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
	end;

	TOtherSourceFile = class(TBinarySourceFile)
	public
		class function GetClassFilter: string; override;
		class function GetClassItemName: string; override;
		class function GetClassTreeIndex: Integer; override;
		class function GetClassImageIndex: Integer; override;
	end;

	TFoundError = class(TObject)
	public
		WholeLine: string;
		BugType: TBugType;
		ErrFunction: string;
		ErrorMessage: string;
		SourceFile: TSourceFile;
		Range: TMCRange;
		destructor Destroy; override;
		procedure GoToPosition;
	end;

var
	AppNode: TTreeNode;
	NoEditor: TWinControl;

implementation

uses
	Printers,
	MasterUnit, ParsingUnit, ProcessUnit,
	UtilsDos, UtilsWin,
	HandleWaitThreadUnit, FileReadToBufferThreadUnit;

const
	CSingleSymbols: set of Char = [',', ';', '(', ')', '[', ']', '{', '}'];
	AdditionalLinesPerPage = 4;

{ TFoundError }

destructor TFoundError.Destroy;
begin
	if Assigned (Range) then begin
		Range.Free;
		Range := nil;
	end;
	inherited;
end;

procedure TFoundError.GoToPosition;
begin
	if Assigned (SourceFile) then begin
		with SourceFile do begin
			if Assigned (Editor) then begin
				Edit;
				if SourceFile is TSourceTextSourceFile then
					with SourceFile as TSourceTextSourceFile do
						with SourceEditor do begin
							if Assigned (Range) then begin
								with Selection do begin
									Assign (Range);
									ScrollInView (5);
								end;
								SetFocus;
							end;
						end;
			end;
		end;
	end else
		AppNode.Selected := True;
end;

{ TSourceFiles }

constructor TSourceFiles.Create;
begin
	inherited Create (TSourceFile);
end;

function TSourceFiles.FindFile(const FN: string): TSourceFile;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count - 1 do
		with Items [I] as TSourceFile do
			if UpperCase (FN) = UpperCase (FileName) then begin
				Result := Items [I] as TSourceFile;
				Break;
			end;
end;

function TSourceFiles.FindFileNameOnly(const FN: string): TSourceFile;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count - 1 do
		with Items [I] as TSourceFile do
			if UpperCase (FN) = UpperCase (ExtractFileName (FileName)) then begin
				Result := Items [I] as TSourceFile;
				Break;
			end;
end;

function TSourceFiles.FindFileOfType(FileType: TSourceFileClass):
	TSourceFile;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count - 1 do
		if Items [I] is FileType then begin
			Result := Items [I] as TSourceFile;
			Break;
		end;
end;

function TSourceFiles.FindFileOfTypeInProject(FileType: TSourceFileClass):
	TSourceFile;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count - 1 do
		if (Items [I] is FileType) and (Items [I] as TSourceFile).InProject then begin
			Result := Items [I] as TSourceFile;
			Break;
		end;
end;

function TSourceFiles.FindFileWithoutExt(const FN: string): TSourceFile;
var
	I: Integer;
begin
	Result := nil;
	for I := 0 to Count - 1 do
		with Items [I] as TSourceFile do
			if UpperCase (FN) = UpperCase (WithoutExt (FileName)) then begin
				Result := Items [I] as TSourceFile;
				Break;
			end;
end;

{ TSourceFile }

class function TSourceFile.CanSave: Boolean;
begin
	Result := True;
end;

procedure TSourceFile.Compile;
begin
end;

function TSourceFile.CountPages: Integer;
begin
	Result := 0;
end;

destructor TSourceFile.Destroy;
var
	I: Integer;
	F: TForm;
	N: TTreeNode;
begin
	if Assigned (ErrorList) then
		with ErrorList.Items do begin
			BeginUpdate;
			for I := Count - 1 downto 0 do
				if Assigned (Item[I].Data) then
					with TFoundError (Item[I].Data) do
						if SourceFile = Self then begin
							Range := nil;
							Item[I].Delete;
						end;
			EndUpdate;
		end;
	if Assigned (OnError) then
		OnError ('', btWarning, FileName, '', '', 0, 0);
	if Assigned (ParentForm) then begin
		F := ParentForm;
		ParentForm := nil;
		F.Free;
	end;
	if Assigned (TreeItem) then begin
		N := TreeItem;
		TreeItem := nil;
		N.Data := nil;
		N.Free;
	end;
	inherited;
end;

procedure TSourceFile.Edit;
begin
	if Assigned (TreeItem) then
		TreeItem.Selected := True
	else if Assigned (ParentForm) then begin
		ParentForm.Show;
		ParentForm.SetFocus;
	end;
end;

class function TSourceFile.GetAppropriateClassFromExt(Ext: string): TSourceFileClass;
begin
	Ext := LowerCase (Ext);
	if (Length (Ext) > 0) and (Ext [1] <> '.') then
		Ext := '.' + Ext;
	if Ext = '.h' then
		Result := THeaderSourceFile
	else if Ext = '.c' then
		Result := TCSourceFile
	else if Ext = '.s' then
		Result := TGNUAsmSourceFile
	else if Ext = '.asm' then begin
		if ssA68k in SpecialSupport then
			Result := TAsmSourceFile
		else
			Result := TGNUAsmSourceFile;
	end else if (Ext = '.qll') and (ssQuill in SpecialSupport) then
		Result := TQuillSourceFile
	else if Ext = '.o' then
		Result := TObjectSourceFile
	else if Ext = '.a' then
		Result := TArchiveSourceFile
	else if Ext = '.txt' then
		Result := TNormalTextSourceFile
	else
		Result := TOtherSourceFile;
end;

class function TSourceFile.GetAppropriateClassFromName(ItemNm: string): TSourceFileClass;
procedure CheckClass(AClass: TSourceFileClass);
begin
	if (not Assigned (Result)) and (ItemNm = UpperCase (AClass.GetClassItemName)) then
		Result := AClass;
end;
begin
	ItemNm := UpperCase (ItemNm);
	Result := nil;
	CheckClass (THeaderSourceFile);
	CheckClass (TCSourceFile);
	CheckClass (TGNUAsmSourceFile);
	CheckClass (TAsmSourceFile);
	CheckClass (TQuillSourceFile);
	CheckClass (TObjectSourceFile);
	CheckClass (TArchiveSourceFile);
	CheckClass (TNormalTextSourceFile);
	CheckClass (TOtherSourceFile);
	if (Result = TAsmSourceFile) and (not (ssA68k in SpecialSupport)) then
		Result := TGNUAsmSourceFile
	else if (Result = TQuillSourceFile) and (not (ssQuill in SpecialSupport)) then
		Result := TQuillSourceFile;
end;

class function TSourceFile.GetAppropriateClassFromTreeIndex(Index: Integer): TSourceFileClass;
procedure CheckClass(AClass: TSourceFileClass);
begin
	if (not Assigned (Result)) and (Index = AClass.GetClassTreeIndex) then
		Result := AClass;
end;
begin
	Result := nil;
	CheckClass (THeaderSourceFile);
	CheckClass (TCSourceFile);
	CheckClass (TGNUAsmSourceFile);
	if ssA68k in SpecialSupport then
		CheckClass (TAsmSourceFile);
	if ssQuill in SpecialSupport then
		CheckClass (TQuillSourceFile);
	CheckClass (TObjectSourceFile);
	CheckClass (TArchiveSourceFile);
	CheckClass (TNormalTextSourceFile);
	CheckClass (TOtherSourceFile);
end;

function TSourceFile.GetCaption: TCaption;
begin
	if Assigned (TreeItem) then
		Result := TreeItem.Text
	else if Assigned (ParentForm) then
		Result := ParentForm.Caption
	else
		Result := '';
end;

class function TSourceFile.GetClassFilter: string;
begin
	Result := 'Abstract Files (*.*)|*.*';
end;

class function TSourceFile.GetClassImageIndex: Integer;
begin
	Result := -1;
end;

class function TSourceFile.GetClassItemName: string;
begin
	Result := 'Abstract File';
end;

class function TSourceFile.GetClassTreeIndex: Integer;
begin
	Result := -1;
end;

class function TSourceFile.GetCompilable: Boolean;
begin
	Result := False;
end;

function TSourceFile.GetDynamicName: string;
var
	S: string;
begin
	S := ExtractFilePath (FileName);
	if StartsWith (ExtractFilePath (ProjectFileName), S) then
		Result := Copy (FileName, Length (ExtractFilePath (ProjectFileName)) + 1, Length (FileName))
	else
		Result := FileName;
end;

function TSourceFile.GetEditing: Boolean;
begin
	if Assigned (TreeItem) then
		Result := TreeItem.Selected
	else if Assigned (ParentForm) then
		Result := ParentForm.Visible
	else
		Result := False;
end;

function TSourceFile.GetEditor: TWinControl;
begin
	if Assigned (TreeItem) then
		Result := NoEditor
	else
		Result := nil;
end;

function TSourceFile.GetFolderPath: string;
begin
	if Assigned (Folder) then
		Result := Folder.Path
	else
		Result := '';
end;

function TSourceFile.GetInProject: Boolean;
begin
	Result := Assigned (TreeItem);
end;

function TSourceFile.GetLogicalFileName: string;
var
	Path: string;
begin
	Result := ExtractFileName (FileName);
	Path := FolderPath;
	if Length (Path) > 0 then
		Result := WithBackslash (Path) + Result;
end;

class function TSourceFile.GetPrintable: Boolean;
begin
	Result := False;
end;

function TSourceFile.GetSourceName: string;
begin
	Result := WithoutExt (ExtractFileName (FileName));
end;

procedure TSourceFile.Invalidate;
begin
	Invalidated := True;
end;

procedure TSourceFile.LoadFromFile(const FN: string; SetFN: Boolean);
begin
	if SetFN and (Length (FN) > 0) then
		FFileName := FN; 
	Modified := False;
end;

procedure TSourceFile.Modify;
begin
	Modified := True;
	Invalidate;
end;

procedure TSourceFile.Print(Copies: Integer; PrintRange: TPrintRange; FromPage, ToPage: Integer);
begin
end;

procedure TSourceFile.ProcessErrorLine(Line: string);
begin
end;

procedure TSourceFile.ProcessErrors(const ErrText: string);
var
	I: Integer;
	LL: TStringList;
begin
	UpdateProgramOutput;
	if not OperationCancelled then begin
		LL := TStringList.Create;
		with LL do try
			Text := ErrText;
			if Count > 0 then begin
				if Assigned (ErrorList) then
					ErrorList.Items.BeginUpdate;
				try
					for I := 0 to Count - 1 do
						ProcessErrorLine (Strings [I]);
				finally
					if Assigned (ErrorList) then
						ErrorList.Items.EndUpdate;
				end;
			end;
		except end;
		LL.Free;
		CompUpdate;
	end;
end;

procedure TSourceFile.Save;
begin
	if (ProjectFileName = '') and (ExtractFilePath (FileName) = 'C:\') then
		SaveAs
	else begin
		FLastChangeTime := 0;
		WriteToFile;
		Modified := False;
	end;
end;

procedure TSourceFile.SaveAs;
var
	S: string;
begin
	with TSaveDialog.Create (Application.MainForm) do try
		Title := 'Save Source File';
		S := GetClassFilter + '|All Files (*.*)|*.*';
		if S [1] = '|' then
			Delete (S, 1, 1);
		Filter := S;
		FilterIndex := 0;
		DefaultExt := Copy (ExtractFileExt (Self.FileName), 2, Length (Self.FileName));
		FileName := Self.FileName;
		Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
		if Execute then
			WriteToFile (FileName, True)
		else
			Abort;
	finally
		Free;
	end;
end;

procedure TSourceFile.SetCaption(const Value: TCaption);
begin
	if Value <> Caption then begin
		if Assigned (TreeItem) then
			TreeItem.Text := Value;
		if Assigned (ParentForm) then
			ParentForm.Caption := Value;
	end;
end;

procedure TSourceFile.SetEditing(const Value: Boolean);
begin
	if Value then
		Edit
	else
		AppNode.Selected := True;
end;

procedure TSourceFile.SetFileName(const Value: string);
begin
	if FFileName <> Value then begin
		if FileExists (Value) then
			raise EWriteError.Create ('The file "' + Value + '" already exists.')
		else
			if FileExists (FFileName) then begin
				RenameFile (FFileName, Value);
				if (LowerCase (ExtractFileExt (FFileName)) <> '.o') and FileExists (ChangeFileExt (FFileName, '.o')) then
					RenameFile (ChangeFileExt (FFileName, '.o'), ChangeFileExt (Value, '.o'));
				if (LowerCase (ExtractFileExt (FFileName)) <> '.s') and FileExists (ChangeFileExt (FFileName, '.s')) then
					RenameFile (ChangeFileExt (FFileName, '.s'), ChangeFileExt (Value, '.s'));
			end;
		FFileName := Value;
	end;
end;

procedure TSourceFile.SetModified(const Value: Boolean);
begin
	FModified := Value;
	Invalidated := False;
end;

procedure TSourceFile.SetSourceName(const Value: string);
begin
	FileName := WithBackslash (ExtractFilePath (FileName)) + Value + ExtractFileExt (FileName);
end;

procedure TSourceFile.TestChange;
begin
end;

procedure TSourceFile.UpdateProgramOutput;
begin
	if Assigned (CompUpdateProgramOutput) then
		CompUpdateProgramOutput;
end;

procedure TSourceFile.WarnIfModified;
begin
	if Modified then
		case ShowDefaultMessageBox
			('The file ''' + SourceName + ''' has been modified.  Do you want to save the changes?',
			'File Modified', mtQuestion, True) of
			idYes: Save;
			idNo: Modified := False;
			idCancel: Abort;
		end;
end;

procedure TSourceFile.WriteToFile(const FN: string; SetFN: Boolean);
begin
	if SetFN and (Length (FN) > 0) then
		FFileName := FN;
	if SetFN or (Length (FN) <= 0) then begin
		FLastChangeTime := 0;
		Caption := SourceName;
		Modified := False;
	end;
end;

{ TTextSourceFile }

procedure TTextSourceFile.AskForReload;
begin
	if ShowDefaultMessageBox ('The File "' + SourceName + '" has been changed by another program. Do you want to reload it?', 'File Changed', mtQuestion) = idYes then begin
		try
			LoadFromFile;
		except end;
		Invalidate;
	end;
end;

procedure TTextSourceFile.Change(Sender: TObject);
begin
	if (FModifyLevel <= 0) and (not Modifying) then
		Modify;
end;

function TTextSourceFile.CountPages: Integer;
var
	FontHeight,
	LinesPerPage: Integer;
begin
	if Assigned (TextEditor) then begin
		Printer.Canvas.Font.Assign (TextEditor.Font);
		FontHeight := Printer.Canvas.TextHeight ('Gg');
		LinesPerPage := Printer.PageHeight div FontHeight - AdditionalLinesPerPage;
		Result := TextEditor.LineCount div LinesPerPage + 1;
	end else
		Result := 0;
end;

destructor TTextSourceFile.Destroy;
begin
	if Assigned (LineStartList) then begin
		LineStartList.Free;
		LineStartList := nil;
	end;
	inherited;
end;

function TTextSourceFile.GetCompiledLineStart(Line: Integer): Integer;
begin
	if (Line > 0) and Assigned (TextEditor) then begin
		if Assigned (LineStartList) then
			with LineStartList do begin
				if Line >= Count then
					Result := TextEditor.TextLength + 1
				else
					Result := Items [Line];
			end
		else
			Result := TextEditor.CellToCharIdx (TextCell (Line, 1));
	end else
		Result := 0;
end;

function TTextSourceFile.GetContent: string;
begin
	if Assigned (InternalTextEditor) then
		Result := InternalTextEditor.Text
	else
		Result := FTempContent;
end;

function TTextSourceFile.GetEditor: TWinControl;
begin
	Result := TextEditor;
end;

class function TTextSourceFile.GetPrintable: Boolean;
begin
	Result := True;
end;

procedure TTextSourceFile.LoadFromFile(const FN: string; SetFN: Boolean);
begin
	if Assigned (InternalTextEditor) then
		InternalTextEditor.AllowUndo := False;
	Inc (FModifyLevel);
	try
		with TMemoryStream.Create do try
			if Length (FN) > 0 then
				LoadFromFile (FN)
			else
				LoadFromFile (FileName);
			Size := Size + 1;
			PChar (Memory) [Size - 1] := #0;
			Content := AnsiString (PChar (Memory));
		finally
			Free;
		end;
	finally
		Dec (FModifyLevel);
		if Assigned (InternalTextEditor) then
			InternalTextEditor.AllowUndo := True;
		inherited;
		if GetCompilable then begin
			if FileExists (ChangeFileExt (FileName, '.o')) then
				Invalidated := FileAge (ChangeFileExt (FileName, '.o')) < FileAge (FileName)
			else
				Invalidated := True;
		end else
			Invalidated := False;
	end;
end;

procedure TTextSourceFile.Print(Copies: Integer; PrintRange: TPrintRange; FromPage, ToPage: Integer);
var
	CurPage,
	CurPageLine,
	CurY,
	FontHeight,
	LinesPerPage: Integer;
	procedure StartPage;
	var
		PrevStyle: TFontStyles;
		PageStr: string;
	begin
		Inc (CurPage);
		if CurPage in [FromPage..ToPage] then
			with Printer.Canvas do begin
				PrevStyle := Font.Style;
				Font.Style := PrevStyle + [fsBold];
				TextOut (0, FontHeight, '  ' + ExtractFileName (FileName));
				PageStr := 'Page ' + IntToStr (CurPage) + '  ';
				TextOut (Printer.PageWidth - TextWidth (PageStr), FontHeight, PageStr);
				Font.Style := PrevStyle;
			end;
		CurY := FontHeight * 3;
		CurPageLine := 1;
	end;
var
	Remaining,
	S: string;
	P,
	Ps,
	NextPs: Integer;
begin
	if Assigned (TextEditor) then begin
		Printer.Title := 'TIGCC IDE - ' + ExtractFileName (FileName);
		Printer.Copies := Copies;
		Printer.BeginDoc;
		try
			Printer.Canvas.Font.Assign (TextEditor.Font);
			FontHeight := Printer.Canvas.TextHeight ('Gg');
			LinesPerPage := Printer.PageHeight div FontHeight - AdditionalLinesPerPage;
			if PrintRange <> prPageNums then begin
				FromPage := 1;
				ToPage := High (ToPage);
			end;
			if PrintRange = prSelection then
				Remaining := TextEditor.Selection.Text
			else
				Remaining := Content;
			CurPage := 0;
			StartPage;
			while (Length (Remaining) > 0) and (CurPage <= ToPage) do begin
				P := Pos (#13#10, Remaining);
				if P <= 0 then
					P := Length (Remaining) + 1;
				S := Copy (Remaining, 1, P - 1);
				Delete (Remaining, 1, P + 1);
				Ps := 1;
				while Ps <= Length (S) do begin
					if S [Ps] = #9 then begin
						NextPs := ((Ps - 1) div TextEditor.TabSize + 1) * TextEditor.TabSize + 1;
						System.Delete (S, Ps, 1);
						System.Insert (StringOfChar (' ', NextPs - Ps), S, Ps);
						Ps := NextPs;
					end else
						Inc (Ps);
				end;
				if CurPage in [FromPage..ToPage] then
					Printer.Canvas.TextOut (0, CurY, '  ' + S);
				Inc (CurPageLine);
				if CurPageLine > LinesPerPage then begin
					if CurPage in [FromPage..(ToPage-1)] then
						Printer.NewPage;
					StartPage;
				end else
					Inc (CurY, FontHeight);
			end;
		finally
			Printer.EndDoc;
		end;
	end;
end;

procedure TTextSourceFile.ReplaceText(Sender: TObject; Pos,
	Change: Integer);
var
	I,
	FirstChangeI,
	FirstChangePos: Integer;
begin
	if Assigned (LineStartList) then
		with LineStartList do begin
			FirstChangeI := -1;
			FirstChangePos := -1;
			for I := 0 to Count - 1 do
				if Items [I] > Pos then begin
					FirstChangeI := I;
					FirstChangePos := Items [I];
					Break;
				end;
			if FirstChangeI >= 0 then begin
				for I := FirstChangeI to Count - 1 do
					Items [I] := Items [I] + Change;
				for I := FirstChangeI to Count - 1 do begin
					if Items [I] >= FirstChangePos then
						Break;
					Items [I] := FirstChangePos;
				end;
			end;
		end;
end;

procedure TTextSourceFile.SetContent(const Value: string);
begin
	if Assigned (InternalTextEditor) then begin
		with InternalTextEditor do
			Text := Value;
	end else
		FTempContent := Value;
end;

procedure TTextSourceFile.SetModified(const Value: Boolean);
begin
	if not Modifying then begin
		Modifying := True;
		FModified := Value;
		Invalidated := False;
		Modifying := False;
	end;
end;

procedure TTextSourceFile.SetParentForm(const Value: TForm);
begin
	inherited;
	if Assigned (TextEditor) then
		with TextEditor do begin
			Parent := Value;
			if Value <> nil then
				Show;
		end;
end;

procedure TTextSourceFile.TestChange;
var
	NewTime: Integer;
	TimeChanged: Boolean;
begin
	if (Length (FileName) > 0) and FileExists (FileName) then begin
		NewTime := FileAge (FileName);
		TimeChanged := (FLastChangeTime <> 0) and (FLastChangeTime <> NewTime);
		FLastChangeTime := NewTime;
		if TimeChanged then
			AskForReload;
	end;
end;

procedure TTextSourceFile.UpdateEditor;
begin
	if Assigned (TextEditor) then
		with TextEditor do begin
			Color := EditorColor;
			Font.Assign (EditorFont);
			DragDropEditing := EditorDragDrop;
			RemoveTrailingSpaces := EditorRemoveTrSp;
		end;
end;

procedure TTextSourceFile.WriteToFile(const FN: string; SetFN: Boolean);
var
	WriteFile: string;
begin
	if Assigned (TextEditor) then try
		if Length (FN) > 0 then
			WriteFile := FN
		else
			WriteFile := FileName;
		CreatePathFor (WriteFile);
		with TFileStream.Create (WriteFile, fmCreate or fmShareExclusive) do try
			Write (PChar(TextEditor.Text)^, TextEditor.TextLength);
		finally
			Free;
		end;
		inherited;
	except
		ShowDefaultMessageBox ('Error saving source file.', 'Error', mtProgramError);
	end;
end;

{ TBinarySourceFile }

class function TBinarySourceFile.CanSave: Boolean;
begin
	Result := False;
end;

procedure TBinarySourceFile.WriteToFile(const FN: string; SetFN: Boolean);
begin
	if Length (FN) > 0 then
		CopyFile (PChar (FileName), PChar (FN), False);
	inherited;
end;

{ THeaderSourceFile }

class function THeaderSourceFile.GetClassFilter: string;
begin
	Result := 'Header Files (*.h)|*.h';
end;

class function THeaderSourceFile.GetClassImageIndex: Integer;
begin
	Result := 3;
end;

class function THeaderSourceFile.GetClassItemName: string;
begin
	Result := 'Header File';
end;

class function THeaderSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 0;
end;

function THeaderSourceFile.GetContentType: TSourceFileType;
begin
	Result := ftOther;
	if Content <> '' then begin
		if Content [1] = '/' then
			Result := ftCFile
		else if Content [1] = '|' then
			Result := ftGNUAsmFile
		else if Content [1] = ';' then
			Result := ftA68kAsmFile;
	end;
	if Result = ftOther then
		if Assigned (Collection) and (Collection is TSourceFiles) then
			with Collection as TSourceFiles do
				if Assigned (FindFileOfType (TCSourceFile)) then
					Result := ftCFile
				else if Assigned (FindFileOfType (TGNUAsmSourceFile)) then
					Result := ftGNUAsmFile
				else if Assigned (FindFileOfType (TAsmSourceFile)) then
					Result := ftA68kAsmFile
				else if Assigned (FindFileOfType (TQuillSourceFile)) then
					Result := ftQuillFile;
end;

procedure THeaderSourceFile.SetContent(const Value: string);
begin
	if Length (Value) > 0 then begin
		if Value [1] = '/' then
			UpdateContentType (ftCFile)
		else if Value [1] = '|' then
			UpdateContentType (ftGNUAsmFile)
		else if Value [1] = ';' then
			UpdateContentType (ftA68kAsmFile);
	end;
	inherited;
end;

procedure THeaderSourceFile.SetModified(const Value: Boolean);
begin
	if not Modifying then begin
		Modifying := True;
		FModified := Value;
		if Value then
			Invalidate;
		Modifying := False;
	end;
end;

procedure THeaderSourceFile.UpdateContentType(CT: TSourceFileType);
begin
	if Assigned (InternalSourceEditor) then
		with InternalSourceEditor do begin
			if CT = ftGNUAsmFile then begin
				SyntaxColoring.Assign (SyntaxAsmGNU);
				TabSize := TabSizeAsm;
			end else if CT = ftA68kAsmFile then begin
				SyntaxColoring.Assign (SyntaxAsm);
				TabSize := TabSizeAsm;
			end else if CT = ftQuillFile then begin
				SyntaxColoring.Assign (SyntaxQuill);
				TabSize := TabSizeC;
			end else begin
				SyntaxColoring.Assign (SyntaxC);
				TabSize := TabSizeC;
			end;
			AutoIndentIncrease := AutoBlocks and (CT = ftCFile);
		end;
end;

procedure THeaderSourceFile.UpdateEditor;
begin
	inherited;
	if Assigned (SourceEditor) then begin
		if ContentType = ftCFile then begin
			SourceEditor.TabSize := TabSizeC;
			SourceEditor.AutoIndentIncrease := AutoBlocks;
		end else if ContentType = ftCFile then
			SourceEditor.TabSize := TabSizeC
		else
			SourceEditor.TabSize := TabSizeAsm;
	end;
end;

procedure THeaderSourceFile.UpdateSyntax;
begin
	UpdateContentType (ContentType);
end;

{ TSourceTextSourceFile }

destructor TSourceTextSourceFile.Destroy;
var
	PrevEditor: TSourceEdit;
begin
	if Assigned (FEditor) then begin
		PrevEditor := FEditor;
		FEditor := nil;
		PrevEditor.Free;
	end;
	inherited;
end;

function TSourceTextSourceFile.GetContentType: TSourceFileType;
begin
	Result := ftOther;
end;

function TSourceTextSourceFile.GetFunctions: TSourceFileFunctions;
var
	Editor: TSourceEdit;
	TextLength,
	I,
	J,
	CurPos,
	WordIndex: Integer;
	Text,
	LastWord,
	FirstWord: string;
	InFunc,
	InAsm,
	InCompSM,
	InSpace,
	LineDone: Boolean;
begin
	SetLength (Result, 0);
	Editor := SourceEditor;
	if not Assigned (Editor) then
		Exit;
	Text := Content;
	TextLength := Length (Text);
	InFunc := False;
	InAsm := False;
	InCompSM := False;
	case ContentType of
		ftCFile:
			for I := 1 to Editor.LineCount do begin
				CurPos := Editor.CellToCharIdx (TextCell (I, 1));
				if (CurPos <= TextLength) and (Text [CurPos] <> #13) and (Text [CurPos] <> '*') then begin
					if InCompSM and (CurPos - 3 >= 1) and (Text [CurPos - 3] <> '\') then
						InCompSM := False;
					if not InCompSM then begin
						if Text [CurPos] = '{' then
							InFunc := True;
						if InFunc then begin
							if Text [CurPos] = '}' then
								InFunc := False;
						end else begin
							if (Copy (Text, CurPos, 5) = 'asm("') or (Copy (Text, CurPos, 6) = 'asm ("') then
								InAsm := True;
							if InAsm then begin
								if Pos ('")', Copy (Text, CurPos, Editor.CellToCharIdx (TextCell (I + 1, 1)) - CurPos)) > 0 then
									InAsm := False;
							end else begin
								LineDone := False;
								LastWord := '';
								WordIndex := 0;
								InSpace := True;
								while (CurPos <= TextLength) and (not LineDone) do begin
									case Text [CurPos] of
										'A'..'Z', 'a'..'z', '0'..'9', '_', '$', ':': begin
											if InSpace and (Text [CurPos] = '_') and (Copy (Text, CurPos, Length ('__attribute__')) = '__attribute__') then begin
												Inc (CurPos, Length ('__attribute__'));
												while (CurPos <= TextLength) and (Text [CurPos] in [#32, #9]) do
													Inc (CurPos);
												if (CurPos <= TextLength) and (Text [CurPos] = '(') then begin
													Inc (CurPos);
													J := 1;
													while (CurPos <= TextLength) and (J > 0) do begin
														case Text [CurPos] of
															'(': Inc (J);
															')': Dec (J);
														end;
														Inc (CurPos);
													end;
												end else
													Dec (CurPos);
											end else begin
												if InSpace then begin
													LastWord := '';
													InSpace := False;
													Inc (WordIndex);
												end;
												LastWord := LastWord + Text [CurPos];
												if WordIndex <= 1 then
													FirstWord := LastWord;
											end;
										end;
										#32, #9, #0, '*', '&': begin
											InSpace := True;
											if (Pos (':', LastWord) > 0) and (Pos ('::', LastWord) <= 0) then
												LineDone := True;
										end;
										'#': begin
											InCompSM := True;
											LineDone := True;
										end;
										else
											LineDone := True;
									end;
									Inc (CurPos);
								end;
								if (WordIndex >= 2) and (WordIndex <= 4) and (CurPos - 1 <= TextLength) and (Text [CurPos - 1] = '(') and (((FirstWord <> 'struct') and (FirstWord <> 'union') and (FirstWord <> 'enum')) or (WordIndex > 2)) then begin
									while (CurPos <= TextLength) and (Text [CurPos] <> ')') do
										Inc (CurPos);
									Inc (CurPos);
									if (CurPos <= TextLength) and (Text [CurPos] <> '(') then begin
										LineDone := (CurPos <= TextLength) and (Text [CurPos] = ';');
										if not LineDone then begin
											J := CurPos;
											while (J <= TextLength) and (not (Text [J] in [';', '{', '=', #13, #10])) do
												Inc (J);
											if (J <= TextLength) and (Text [J] = ';') then
												LineDone := True;
										end;
										if LineDone then
											for J := Low (Result) to High (Result) do
												if Result[J].Name = LastWord then begin
													LineDone := False;
													Break;
												end;
										if LineDone then begin
											SetLength (Result, Length (Result) + 1);
											with Result [High (Result)] do begin
												Name := LastWord;
												PrototypeLine := I;
												ImplementationLine := 0;
											end;
										end else begin
											for J := Low (Result) to High (Result) do
												if Result[J].Name = LastWord then begin
													Result[J].ImplementationLine := I;
													InFunc := True;
													LineDone := True;
													Break;
												end;
											if not LineDone then begin
												while (CurPos <= TextLength) and (not (Text [CurPos] in ['{', ';', '='])) do
													Inc (CurPos);
												if (CurPos <= TextLength) and (Text [CurPos] <> '=') then begin
													SetLength (Result, Length (Result) + 1);
													with Result [High (Result)] do begin
														Name := LastWord;
														PrototypeLine := 0;
														ImplementationLine := I;
														InFunc := True;
													end;
												end;
											end;
										end;
									end;
								end;
							end;
						end;
					end;
				end;
			end;
		ftGNUAsmFile, ftA68kAsmFile:
			for I := 1 to Editor.LineCount do begin
				CurPos := Editor.CellToCharIdx (TextCell (I, 1));
				LineDone := False;
				LastWord := '';
				while (CurPos <= TextLength) and (not LineDone) do begin
					if Text [CurPos] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'] then
						LastWord := LastWord + Text [CurPos]
					else
						LineDone := True;
					Inc (CurPos);
				end;
				if (CurPos - 1 <= TextLength) and (Text [CurPos - 1] = ':') then begin
					SetLength (Result, Length (Result) + 1);
					with Result [High (Result)] do begin
						Name := LastWord;
						PrototypeLine := 0;
						ImplementationLine := I;
					end;
				end;
			end;
	end;
end;

function TSourceTextSourceFile.GetInternalTextEditor: TMemoComponent;
begin
	Result := FEditor;
end;

function TSourceTextSourceFile.GetSourceEditor: TSourceEdit;
begin
	if not Assigned (FEditor) then begin
		FEditor := TSourceEdit.Create (Application.MainForm);
		with FEditor do begin
			AllowUndo := False;
			Text := FTempContent;
		end;
		UpdateEditor;
		UpdateSyntax;
		FTempContent := '';
		with FEditor do begin
			OnChangePrivate := Change;
			OnReplaceText := ReplaceText;
			AllowUndo := True;
		end;
	end;
	Result := FEditor;
end;

function TSourceTextSourceFile.GetTextEditor: TMemoComponent;
begin
	Result := SourceEditor;
end;

procedure TSourceTextSourceFile.SplitAndWriteToFile(const FN: string);
type
	TCharMode = (cmNone, cmNormalText, cmNumber, cmMultiSymbol, cmString, cmChar, cmComment, cmUnchangeableLine, cmExtUnchangeableLine, cmExtUnchangeableLineString, cmTrigraph);
var
	S: string;
	C: Char;
{$IFDEF CanSplit}
	CurPos: Integer;
	CurMode: TCharMode;
	AtLineStart: Boolean;
	Stream: TStream;
procedure InsertChar(Ch: Char);
begin
	Stream.Write (Ch, SizeOf (Char));
end;
procedure InsertString(const Str: string);
begin
	Stream.Write (PChar(Str)^, Length (Str));
end;
procedure AddLine(Offset: Integer = 0);
begin
	LineStartList.Add (CurPos + Offset);
	AtLineStart := True;
end;
procedure NewLine;
begin
	if (not (AtLineStart or (Copy (S, CurPos, 1) = #13))) or (Copy (S, CurPos, 1) = '#') then begin
		InsertString (#13#10);
		AddLine;
	end;
	CurMode := cmNone;
end;
procedure SetMultiCharMode(Mode: TCharMode);
begin
	if CurMode <> Mode then begin
		NewLine;
		CurMode := Mode;
	end;
end;
var
	B,
	NoInsert: Boolean;
{$ENDIF}
var
	I: Integer;
	EscapedRealFN: string;
begin
	try
		if DebugInfo then begin
			S := Content;
			CreatePathFor (FN);
			EscapedRealFN := '';
			for I := 1 to Length (FileName) do begin
				C := FileName [I];
				if (C = '\') then
					EscapedRealFN := EscapedRealFN + '\\'
				else
					EscapedRealFN := EscapedRealFN + C;
			end;
			with TFileStream.Create (FN, fmCreate or fmShareExclusive) do try
				case ContentType of
					ftCFile:
						S := '#line 1 "' + EscapedRealFN + '"' + #13#10 + S;
					ftQuillFile:
						S := '#line 1 "' + EscapedRealFN + '"' + #13#10 + S;
					ftGNUAsmFile:
						S := '.appfile "' + EscapedRealFN + '"; .appline 1' + #13#10 + S;
				end;
				Write (PChar(S)^, Length (S));
				if ContentType = ftCFile then
					Write (PChar(#13#10)^, 2);
			finally
				Free;
			end;
		end else
{$IFDEF CanSplit}
		if SplitFiles then begin
			case ContentType of
				ftCFile: begin
					if not Assigned (LineStartList) then
						LineStartList := TIntegerList.Create;
					LineStartList.Clear;
					LineStartList.Add (1);
					CurPos := 1;
					AddLine;
					AtLineStart := True;
					S := Content;
					CreatePathFor (FN);
					Stream := TFileStream.Create (FN, fmCreate or fmShareExclusive);
					try
						for CurPos := 1 to Length (S) do begin
							NoInsert := False;
							C := S [CurPos];
							if C = #13 then
								AddLine (2);
							case CurMode of
								cmString:
									if (C = '"') then begin
										B := True;
										I := CurPos - 1;
										while (I >= 1) and ((S [I] = '\') or (Copy (S, I - 2, 3) = '??/')) do begin
											B := not B;
											if S [I] = '\' then
												Dec (I)
											else
												Dec (I, 3);
										end;
										if B then begin
											CurMode := cmNone;
											NoInsert := True;
											InsertString (C + #13#10);
											AtLineStart := True;
											AddLine (1);
										end;
									end;
								cmChar:
									if (C = '''') then begin
										B := True;
										I := CurPos - 1;
										while (I >= 1) and ((S [I] = '\') or (Copy (S, I - 2, 3) = '??/')) do begin
											B := not B;
											if S [I] = '\' then
												Dec (I)
											else
												Dec (I, 3);
										end;
										if B then
											CurMode := cmNone;
									end;
								cmComment: begin
									if (C = '/') and (S [CurPos - 1] = '*') then
										CurMode := cmNone;
								end;
								cmUnchangeableLine:
									if C = #13 then
										CurMode := cmNone;
								cmExtUnchangeableLine:
									if (C = #13) and (S [CurPos - 1] <> '\') then
										CurMode := cmNone
									else if C = '"' then
										CurMode := cmExtUnchangeableLineString;
								cmExtUnchangeableLineString:
									if (C = '"') then begin
										B := True;
										I := CurPos - 1;
										while (I >= 1) and ((S [I] = '\') or (Copy (S, I - 2, 3) = '??/')) do begin
											B := not B;
											if S [I] = '\' then
												Dec (I)
											else
												Dec (I, 3);
										end;
										if B then
											CurMode := cmExtUnchangeableLine;
									end;
								cmTrigraph:
									if (C <> '?') and ((CurPos + 1 > Length (S)) or (S [CurPos + 1] <> '?')) then
										CurMode := cmNone;
								else begin
									if Copy (S, CurPos, 2) = '//' then
										SetMultiCharMode (cmUnchangeableLine)
									else if Copy (S, CurPos, 2) = '/*' then
										SetMultiCharMode (cmComment)
									else if Copy (S, CurPos, 3) = '??=' then
										SetMultiCharMode (cmExtUnchangeableLine)
									else if (Copy (S, CurPos, 2) = '??') and (Length (S) >= CurPos + 2) and (S [CurPos + 2] in ['(', ')', '/', '''', '<', '>', '!', '-']) then
										SetMultiCharMode (cmTrigraph)
									else
										case C of
											#32, #9:
												if (CurPos > 1) and (not (S [CurPos - 1] in [#32, #9])) then
													NewLine;
											'A'..'Z', 'a'..'z', '0'..'9', '_', '$':
												if not (CurMode in [cmNormalText, cmNumber]) then begin
													NewLine;
													if C in ['0'..'9'] then
														CurMode := cmNumber
													else
														CurMode := cmNormalText;
												end;
											'"':
												SetMultiCharMode (cmString);
											'''':
												SetMultiCharMode (cmChar);
											'#':
												SetMultiCharMode (cmExtUnchangeableLine);
											'.':
												if CurMode <> cmNumber then begin
													if (Length (S) >= CurPos + 1) and (S [CurPos + 1] in ['0'..'9']) then begin
														NewLine;
														CurMode := cmNumber;
													end else
														SetMultiCharMode (cmMultiSymbol);
												end;
											'+', '-':
												if (CurMode <> cmNumber) or (CurPos - 1 <= 1) or (not (S [CurPos - 1] in ['e', 'E', 'p', 'P'])) then
													SetMultiCharMode (cmMultiSymbol);
											else
												if C in CSingleSymbols then begin
													if CurMode <> cmNone then begin
														NewLine;
														CurMode := cmNone;
													end;
													if (CurPos + 1 <= Length (S)) and (S [CurPos + 1] <> #13) then begin
														NoInsert := True;
														InsertString (C + #13#10);
														AddLine (1);
													end;
												end else
													SetMultiCharMode (cmMultiSymbol);
										end;
								end;
							end;
							if not NoInsert then begin
								InsertChar (C);
								if not (C in [#13, #10]) then
									AtLineStart := False;
							end;
						end;
						NewLine;
					finally
						Stream.Free;
					end;
				end;
				else
					WriteToFile (FN);
			end;
		end else
{$ENDIF}
		begin
			S := Content;
			CreatePathFor (FN);
			with TFileStream.Create (FN, fmCreate or fmShareExclusive) do try
				Write (PChar(S)^, Length (S));
				if ContentType = ftCFile then
					Write (PChar(#13#10)^, 2);
			finally
				Free;
			end;
		end;
	except
		ShowDefaultMessageBox ('Error writing temporary source file.', 'Error', mtProgramError);
	end;
end;

procedure TSourceTextSourceFile.UpdateEditor;
begin
	inherited;
	if Assigned (SourceEditor) then
		with SourceEditor do
			SplitOnFly := EditorOnFly;
end;

{ TCSourceFile }

procedure TCSourceFile.Compile;
var
	Folder,
	Switches: string;
begin
	CompStartFile;
	OperationCancelled := False;
	OperationSuccessful := False;
	if FileExists (WithBackslash (TIGCCFolder) + GCCLocation + 'm68k-coff-tigcc-GCC.exe') then begin
		CompSetMessage ('Compiling File ''' + SourceName + '''');
		Folder := WithBackslash (Temp + FolderPath);
		CurErrFunction := '';
		InAssemblingState := False;
		if FileExists (Folder + 'TEMPPROG.S') then
			DeleteFile (Folder + 'TEMPPROG.S');
		if FileExists (Temp + 'TEMPPROG.O') then
			DeleteFile (Temp + 'TEMPPROG.O');
		SplitAndWriteToFile (Folder + 'TEMPPROG.C');
		MainConsole.Title := 'Compiler';
		Switches := DefaultGCCSwitches + ' ' + GCCSwitches + ' ' + SpecialSwitches;
		if UseDataVar then
			Switches := Switches + ' -mno-merge-sections';
		if AssumeUndefined then
			Switches := Switches + ' -Werror-implicit-function-declaration';
		if DebugInfo then
			Switches := Switches + ' -gdwarf-2 -g3 -fasynchronous-unwind-tables';
		if ProjectTarget = ptFargo then
			Switches := Switches + ' -DFARGO'
		else if ProjectTarget = ptFlashOS then
			Switches := Switches + ' -DFLASH_OS'
		else if Assigned (PredefinedLibOptions) and (ProjectTarget = ptRegular) then
			Switches := Switches + ' ' + PredefinedLibOptions.GetSwitches;
		CompUpdate;
		try
			MainConsole.StartProcess (WithBackslash (TIGCCFolder) + GCCLocation + 'm68k-coff-tigcc-GCC.exe', '-S -I ' + Folder + ' ' + Switches + ' "' + Folder + 'tempprog.c" -o "' + Folder + 'tempprog.s"', WithoutBackslash (WithBackslash (TIGCCFolder) + GCCLocation));
			WaitForMainConsole ('Compilation');
		except
			ShowDefaultMessageBox ('Could not start compiler.', 'Error', mtProgramError);
		end;
		OperationSuccessful := True;
		ProcessErrors (MainConsole.LastErrText);
		if OperationSuccessful then begin
			OperationSuccessful := False;
			if not OperationCancelled then
				if FileExists (Folder + 'TEMPPROG.S') then begin
					OperationSuccessful := True;
					ProcessSFile (Folder + 'TEMPPROG.S', ChangeFileExt (FileName, '.s'));
				end;
		end;
		if OperationSuccessful then begin
			OperationSuccessful := False;
			CurErrFunction := '';
			InAssemblingState := True;
			CompUpdate;
			Switches := DefaultAsSwitches + ' ' + AsSwitches;
			if CutUnusedRanges or (ProjectTarget = ptArchive) then
				Switches := Switches + ' --all-relocs';
			if OptimizeReturns or (ProjectTarget = ptArchive) then
				Switches := Switches + ' --keep-locals';
			if DebugInfo then
				Switches := Switches + ' --gdwarf2';
			try
				MainConsole.StartProcess (WithBackslash (TIGCCFolder) + AsLocation + 'm68k-coff-tigcc-As.exe', '-I ' + Folder + ' ' + Switches + ' "' + Folder + 'tempprog.s" -o ' + Temp + 'tempprog.o', WithoutBackslash (WithBackslash (TIGCCFolder) + AsLocation));
				WaitForMainConsole ('Compilation');
			except
				ShowDefaultMessageBox ('Could not start assembler.', 'Error', mtProgramError);
			end;
			if FileExists (Temp + 'TEMPPROG.O') then begin
				if not OperationCancelled then begin
					CopyFile (PChar (Temp + 'TEMPPROG.O'), PChar (ChangeFileExt (FileName, '.o')), False);
					OperationSuccessful := True;
				end;
				DeleteFile (Temp + 'TEMPPROG.O');
			end;
			ProcessErrors (MainConsole.LastErrText);
		end;
		CompUpdate;
		try
			if FileExists (Folder + 'TEMPPROG.C') then
				DeleteFile (Folder + 'TEMPPROG.C');
			if FileExists (Folder + 'TEMPPROG.S') then
				DeleteFile (Folder + 'TEMPPROG.S');
			RemovePath (Folder, Temp);
		except end;
		CompUpdate;
	end else
		ShowDefaultMessageBox ('Cannot find compiler.', 'Error', mtProgramError);
	if OperationSuccessful and not OperationCancelled then
		Invalidated := False;
end;

class function TCSourceFile.GetClassFilter: string;
begin
	Result := 'C Files (*.c)|*.c';
end;

class function TCSourceFile.GetClassImageIndex: Integer;
begin
	Result := 4;
end;

class function TCSourceFile.GetClassItemName: string;
begin
	Result := 'C File';
end;

class function TCSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 1;
end;

class function TCSourceFile.GetCompilable: Boolean;
begin
	Result := True;
end;

function TCSourceFile.GetContentType: TSourceFileType;
begin
	Result := ftCFile;
end;

procedure TCSourceFile.ProcessErrorLine(Line: string);
var
	Whole: string;
	P,
	Ofs: Integer;
	S: string;
	FN: string;
	Tp: TBugType;
	CurErrFile: string;
begin
	if not Assigned (OnError) then
		Exit;
	P := Pos (#10, Line);
	if P > 0 then begin
		ProcessErrorLine (Copy (Line, 1, P - 1));
		ProcessErrorLine (Copy (Line, P + 1, Length (Line)));
	end else begin
		repeat
			P := Pos ('`', Line);
			if P > 0 then
				Line [P] := '''';
		until P <= 0;
		repeat
			P := Pos ('´', Line);
			if P > 0 then
				Line [P] := '''';
		until P <= 0;
		repeat
			P := Pos ('"', Line);
			if P > 0 then
				Line [P] := '''';
		until P <= 0;
		Line := Trim (Line);
		if (Pos ('ASSEMBLER MESSAGES:', UpperCase (Line)) > 0) or
			StartsWith ('FROM ', Line) or
			(AssumeUndefined and
			((Pos ('PREVIOUS IMPLICIT DECLARATION', UpperCase (Line)) > 0) or
			(Pos ('PREVIOUSLY IMPLICITLY DECLARED', UpperCase (Line)) > 0))) then
			Exit;
		Whole := Line;
		FN := '';
		CurErrFile := '';
		if StartsWith ('IN FILE', Line) then
			CurErrFunction := ''
		else begin
			P := Pos (':', Line);
			while (P > 0) and (Length (Line) > P) and (Line [P + 1] in ['\', '/']) do begin
				Ofs := Pos (':', Copy (Line, P + 1, Length (Line)));
				if Ofs > 0 then
					Inc (P, Ofs)
				else begin
					P := 0;
					Break;
				end;
			end;
			if P > 0 then
				FN := Copy (Line, 1, P - 1);
			if Length (FN) > 0 then begin
				Delete (Line, 1, Length (FN) + 1);
				repeat
					P := Pos ('/', FN);
					if P > 0 then
						FN [P] := '\';
				until P <= 0;
				if InAssemblingState then begin
					if Pos ('TEMPPROG', UpperCase (FN)) > 0 then
						CurErrFile := ChangeFileExt (FileName, '.s')
					else
						CurErrFile := ExtractFileName (FN);
				end else begin
					if UpperCase (ExtractFileExt (FN)) = '.S' then begin
						FN := '';
						CurErrFunction := '';
					end else begin
						if Pos ('TEMPPROG', UpperCase (FN)) > 0 then
							CurErrFile := FileName
						else
							CurErrFile := ExtractFileName (FN);
					end;
				end;
				P := 0;
				Ofs := 0;
				if (Length (Line) > 0) and (Line [1] in ['0'..'9']) then begin
					try
						S := Copy (Line, 1, Pos (':', Line) - 1);
						P := StrToInt (S);
						Delete (Line, 1, Length (S) + 1);
						if (Length (Line) > 0) and (Line [1] in ['0'..'9']) then try
							S := Copy (Line, 1, Pos (':', Line) - 1);
							Ofs := StrToInt (S) - 1;
							Delete (Line, 1, Length (S) + 1);
						except end;
					except end;
					Line := Trim (Line);
					if StartsWith ('WARNING:', Line, False, True) then
						Tp := btWarning
					else begin
						StartsWith ('ERROR:', Line, False, True);
						Tp := btError;
						OperationSuccessful := False;
					end;
					Line := Trim (Line);
					if StartsWith ('#WARNING ', Line, False, True) then
						Tp := btWarning;
					StartsWith ('#ERROR ', Line, False, True);
					if StartsWith ('PREVIOUS DECLARATION OF ', Line) or
						StartsWith ('POSSIBLE REAL START OF ', Line) or
						StartsWith ('UNUSED VARIABLE ', Line) or
						StartsWith ('UNUSED PARAMETER ', Line) or
						(Pos ('PREVIOUSLY DECLARED HERE', UpperCase (Line)) > 0) or
						(Pos ('LOCATION OF THE PREVIOUS DEFINITION', UpperCase (Line)) > 0) then
						Tp := btInfo;
					if AssumeUndefined and StartsWith ('Implicit declaration of ', Line, False, True) then begin
						Insert ('Undefined reference to ', Line, 1);
						Tp := btError;
						OperationSuccessful := False;
					end;
					if (Length (CurErrFile) <= 0) and (P > 0) and (Length (FN) > 0) then
						Line := UpperCase (ExtractFileName (FN)) + ' Line ' + IntToStr (P) + ' - ' + Line;
					Line := Trim (Line);
					if (Length (Line) > 0) and (Line [Length (Line)] <> '.') then
						Line := Line + '.';
					OnError (Whole, Tp, CurErrFile, CurErrFunction, Line, P, Ofs);
					if Tp = btError then
						OperationSuccessful := False;
				end else begin
					if StartsWith (' IN FUNCTION ''', Line, False, True) then
						CurErrFunction := Copy (Line, 1, Pos ('''', Line) - 1)
					else if StartsWith (' AT TOP LEVEL', Line) then
						CurErrFunction := ''
					else
						OnError (Whole, btError, FileName, '', Line, 0, 0);
				end;
			end else begin
				Tp := btError;
				if StartsWith ('PLEASE FILL OUT ', Line) then
					Tp := btInfo;
				OnError (Whole, Tp, '', '', Whole, 0, 0);
			end;
		end;
	end;
end;

procedure TCSourceFile.ProcessSFile(const SourceFile, DestFile: string);
var
	L: TStringList;
begin
	L := TStringList.Create;
	with L do try
		LoadFromFile (SourceFile);
		if (Count > 0) and (Copy (LowerCase (Strings [0]), 1, Length (#9'.file')) = #9'.file') then
			Strings [0] := #9'.file'#9'"' + ExtractFileName (FileName) + '"';
		try
			ParseSFile (L);
		except
			OperationSuccessful := False;
		end;
		SaveToFile (SourceFile);
		SaveToFile (DestFile);
	finally
		Free;
	end;
end;

procedure TCSourceFile.Save;
var
	FH: THandle;
begin
	inherited;
	if (not Invalidated) and FileExists (ChangeFileExt (FileName, '.o')) then begin
		FH := FileOpen (ChangeFileExt (FileName, '.o'), fmOpenReadWrite + fmShareExclusive);
		FileSetDate (FH, FileAge (FileName));
		FileClose (FH);
	end;
end;

procedure TCSourceFile.SetModified(const Value: Boolean);
begin
	if not Modifying then begin
		Modifying := True;
		FModified := Value;
		if Value then
			Invalidate;
		Modifying := False;
	end;
end;

procedure TCSourceFile.UpdateEditor;
begin
	inherited;
	if Assigned (SourceEditor) then begin
		SourceEditor.TabSize := TabSizeC;
		SourceEditor.AutoIndentIncrease := AutoBlocks;
	end;
end;

procedure TCSourceFile.UpdateSyntax;
begin
	if Assigned (SourceEditor) then
		with SourceEditor do
			SyntaxColoring.Assign (SyntaxC);
end;

{ TGNUAsmSourceFile }

procedure TGNUAsmSourceFile.Compile;
var
	Folder,
	Switches: string;
begin
	CompStartFile;
	OperationCancelled := False;
	OperationSuccessful := False;
	if FileExists (WithBackslash (TIGCCFolder) + AsLocation + 'm68k-coff-tigcc-As.exe') then begin
		CompSetMessage ('Assembling File ''' + SourceName + '''');
		Folder := WithBackslash (Temp + FolderPath);
		if FileExists (Temp + 'TEMPPROG.O') then
			DeleteFile (Temp + 'TEMPPROG.O');
		SplitAndWriteToFile (Folder + 'TEMPPROG.S');
		Switches := DefaultAsSwitches + ' ' + AsSwitches;
		if CutUnusedRanges or (ProjectTarget = ptArchive) then
			Switches := Switches + ' --all-relocs';
		if OptimizeReturns or (ProjectTarget = ptArchive) then
			Switches := Switches + ' --keep-locals';
		if DebugInfo then
			Switches := Switches + ' --gdwarf2';
		MainConsole.Title := 'Assembler';
		try
			MainConsole.StartProcess (WithBackslash (TIGCCFolder) + AsLocation + 'm68k-coff-tigcc-As.exe', '-I ' + Folder + ' ' + Switches + ' "' + Folder + 'tempprog.s" -o ' + Temp + 'tempprog.o', WithoutBackslash (WithBackslash (TIGCCFolder) + AsLocation));
			WaitForMainConsole ('Assembling');
		except
			ShowDefaultMessageBox ('Could not start assembler.', 'Error', mtProgramError);
		end;
		if FileExists (Temp + 'TEMPPROG.O') then begin
			if not OperationCancelled then begin
				CopyFile (PChar (Temp + 'TEMPPROG.O'), PChar (ChangeFileExt (FileName, '.o')), False);
				OperationSuccessful := True;
			end;
			DeleteFile (Temp + 'TEMPPROG.O');
		end;
		ProcessErrors (MainConsole.LastErrText);
		try
			if FileExists (Folder + 'TEMPPROG.S') then
				DeleteFile (Folder + 'TEMPPROG.S');
			RemovePath (Folder, Temp);
		except end;
		CompUpdate;
	end else
		ShowDefaultMessageBox ('Cannot find assembler.', 'Error', mtProgramError);
	if OperationSuccessful and not OperationCancelled then
		Invalidated := False;
end;

class function TGNUAsmSourceFile.GetClassFilter: string;
begin
	Result := 'GNU Assembly Files (*.s)|*.s';
end;

class function TGNUAsmSourceFile.GetClassImageIndex: Integer;
begin
	Result := 5;
end;

class function TGNUAsmSourceFile.GetClassItemName: string;
begin
	Result := 'GNU Assembler File';
end;

class function TGNUAsmSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 2;
end;

class function TGNUAsmSourceFile.GetCompilable: Boolean;
begin
	Result := True;
end;

function TGNUAsmSourceFile.GetContentType: TSourceFileType;
begin
	Result := ftGNUAsmFile;
end;

procedure TGNUAsmSourceFile.ProcessErrorLine(Line: string);
var
	Whole: string;
	P,
	Ofs: Integer;
	S: string;
	FN: string;
	Tp: TBugType;
	CurErrFile: string;
begin
	P := Pos (#10, Line);
	if P > 0 then begin
		ProcessErrorLine (Copy (Line, 1, P - 1));
		ProcessErrorLine (Copy (Line, P + 1, Length (Line)));
	end else begin
		repeat
			P := Pos ('`', Line);
			if P > 0 then
				Line [P] := '''';
		until P <= 0;
		repeat
			P := Pos ('´', Line);
			if P > 0 then
				Line [P] := '''';
		until P <= 0;
		repeat
			P := Pos ('"', Line);
			if P > 0 then
				Line [P] := '''';
		until P <= 0;
		if (Pos ('ASSEMBLER MESSAGES:', UpperCase (Line)) > 0) or
			(Pos ('IN FILE', UpperCase (Line)) > 0) then
			Exit;
		Whole := Line;
		FN := '';
		CurErrFile := '';
		P := Pos (':', Line);
		while (Length (Line) > P) and (Line [P + 1] in ['\', '/']) do
			Inc (P, Pos (':', Copy (Line, P + 1, Length (Line))));
		FN := Copy (Line, 1, P - 1);
		if Length (FN) > 0 then begin
			Delete (Line, 1, Length (FN) + 1);
			repeat
				P := Pos ('/', FN);
				if P > 0 then
					FN [P] := '\';
			until P <= 0;
			if (UpperCase (ExtractFileExt (FN)) = '.C') or (Pos ('TEMPPROG', UpperCase (FN)) > 0) then
				CurErrFile := FileName
			else
				CurErrFile := ExtractFileName (FN);
			P := 0;
			Ofs := 0;
			if (Length (Line) > 0) and (Line [1] in ['0'..'9']) then begin
				try
					S := Copy (Line, 1, Pos (':', Line) - 1);
					P := StrToInt (S);
					Delete (Line, 1, Length (S) + 1);
					if (Length (Line) > 0) and (Line [1] in ['0'..'9']) then try
						S := Copy (Line, 1, Pos (':', Line) - 1);
						Ofs := StrToInt (S) - 1;
						Delete (Line, 1, Length (S) + 1);
					except end;
				except end;
				Line := Trim (Line);
				if StartsWith ('WARNING:', Line, False, True) then
					Tp := btWarning
				else begin
					StartsWith ('ERROR:', Line, False, True);
					Tp := btError;
					OperationSuccessful := False;
				end;
				Line := Trim (Line);
				if (Length (CurErrFile) <= 0) and (P > 0) and (Length (FN) > 0) then
					Line := UpperCase (ExtractFileName (FN)) + ' Line ' + IntToStr (P) + ' - ' + Line;
				if Assigned (OnError) then
					OnError (Whole, Tp, CurErrFile, '', Line, P, Ofs);
			end else begin
				if Assigned (OnError) then
					OnError (Whole, btError, FileName, '', Line, 0, 0);
			end;
		end else
			if Assigned (OnError) then
				OnError (Whole, btError, '', '', Whole, 0, 0);
	end;
end;

procedure TGNUAsmSourceFile.Save;
var
	FH: THandle;
begin
	inherited;
	if (not Invalidated) and FileExists (ChangeFileExt (FileName, '.o')) then begin
		FH := FileOpen (ChangeFileExt (FileName, '.o'), fmOpenReadWrite + fmShareExclusive);
		FileSetDate (FH, FileAge (FileName));
		FileClose (FH);
	end;
end;

procedure TGNUAsmSourceFile.SetModified(const Value: Boolean);
begin
	if not Modifying then begin
		Modifying := True;
		FModified := Value;
		if Value then
			Invalidate;
		Modifying := False;
	end;
end;

procedure TGNUAsmSourceFile.UpdateEditor;
begin
	inherited;
	if Assigned (SourceEditor) then begin
		SourceEditor.TabSize := TabSizeAsm;
		SourceEditor.AutoIndentIncrease := False;
	end;
end;

procedure TGNUAsmSourceFile.UpdateSyntax;
begin
	if Assigned (SourceEditor) then
		with SourceEditor do begin
			SyntaxColoring.Assign (SyntaxAsmGNU);
			TabSize := TabSizeAsm;
			AutoIndentIncrease := False;
		end;
end;

{ TAsmSourceFile }

procedure TAsmSourceFile.Compile;
var
	Folder,
	Switches: string;
	FPos: Integer;
	CurErrFile,
	S: string;
	CurErrLine,
	P: Integer;
	EmptyLn: Boolean;
	LL: TStringList;
begin
	CompStartFile;
	OperationCancelled := False;
	OperationSuccessful := False;
	if FileExists (WithBackslash (TIGCCFolder) + A68kLocation + 'A68k.exe') then begin
		CompSetMessage ('Assembling File ''' + SourceName + '''');
		Folder := WithBackslash (Temp + FolderPath);
		if FileExists (WithBackslash (TIGCCFolder) + GCCLocation + 'TEMPPROG.O') then
			DeleteFile (WithBackslash (TIGCCFolder) + GCCLocation + 'TEMPPROG.O');
		if FileExists (Temp + 'TEMPPROG.O') then
			DeleteFile (Temp + 'TEMPPROG.O');
		SplitAndWriteToFile (Folder + 'TEMPPROG.ASM');
		Switches := DefaultA68kSwitches + ' ' + AsmSwitches;
		if CutUnusedRanges or (ProjectTarget = ptArchive) then
			Switches := Switches + ' -a';
		if OptimizeReturns or (ProjectTarget = ptArchive) then
			Switches := Switches + ' -d';
		MainConsole.Title := 'Assembler';
		try
			MainConsole.StartProcess (WithBackslash (TIGCCFolder) + A68kLocation + 'A68k.exe', '"' + Folder + 'tempprog.asm" ' + Switches, WithoutBackslash (Temp));
			WaitForMainConsole ('Assembling');
		except
			ShowDefaultMessageBox ('Could not start assembler.', 'Error', mtProgramError);
		end;
		UpdateProgramOutput;
		if not OperationCancelled then begin
			if Assigned (ErrorList) then
				ErrorList.Items.BeginUpdate;
			try
				LL := TStringList.Create;
				try
					LL.Text := MainConsole.LastOutText;
					EmptyLn := False;
					FPos := 0;
					while FPos < LL.Count do begin
						S := LL [FPos];
						Inc (FPos);
						CurErrLine := 0;
						if (Length (S) <= 0) or (Pos ('ASSEMBLING', UpperCase (S)) > 0) then
							EmptyLn := True
						else begin
							if EmptyLn then begin
								EmptyLn := False;
								while (FPos < LL.Count) and (Pos ('TEMPPROG.ASM LINE ', UpperCase (S)) <= 0) do begin
									if (Length (S) > 0) and (Pos ('(USER MACRO)', UpperCase (S)) <= 0) then begin
										if Length (CurErrFile) <= 0 then begin
											while Pos ('/', S) > 0 do
												S [Pos ('/', S)] := '\';
											P := Pos ('LINE ', UpperCase (S));
											if P > 0 then begin
												CurErrFile := ExtractFileName (Copy (S, 1, P - 2));
												try
													CurErrLine := StrToInt (Copy (S, P + Length ('LINE '), Length (S)));
												except
													CurErrLine := 0;
												end;
											end;
										end;
									end;
									S := LL [FPos];
									Inc (FPos);
								end;
								if FPos < LL.Count then begin
									if Length (CurErrFile) <= 0 then begin
										Delete (S, 1, Pos ('TEMPPROG.ASM LINE ', UpperCase (S)) + Length ('TEMPPROG.ASM LINE ') - 1);
										try
											CurErrLine := StrToInt (S);
										except
											CurErrLine := 0;
										end;
									end;
									Inc (FPos);
									if FPos < LL.Count then begin
										S := LL [FPos];
										Inc (FPos);
										S := Copy (S, FirstNonWhiteSpace (S), Length (S));
										StartsWith ('^ ', S, True, True);
										if S [Length (S)] = '.' then
											Delete (S, Length (S), 1);
										OperationSuccessful := False;
										if Length (CurErrFile) <= 0 then
											CurErrFile := FileName;
										if Assigned (OnError) then
											OnError (IntToStr (CurErrLine) + ': ' + S, btError, CurErrFile, '', S, CurErrLine, 0);
										CurErrFile := '';
									end;
								end;
							end;
						end;
					end;
				except end;
				LL.Free;
			finally
				if Assigned (ErrorList) then
					ErrorList.Items.EndUpdate;
			end;
		end;
		CompUpdate;
		if FileExists (Temp + 'TEMPPROG.O') then begin
			if not OperationCancelled then begin
				CopyFile (PChar (Temp + 'TEMPPROG.O'), PChar (ChangeFileExt (FileName, '.o')), False);
				OperationSuccessful := True;
			end;
			DeleteFile (Temp + 'TEMPPROG.O');
		end;
		try
			if FileExists (Folder + 'TEMPPROG.ASM') then
				DeleteFile (Folder + 'TEMPPROG.ASM');
			RemovePath (Folder, Temp);
		except end;
		CompUpdate;
	end else
		ShowDefaultMessageBox ('Cannot find assembler.', 'Error', mtProgramError);
	if OperationSuccessful and not OperationCancelled then
		Invalidated := False;
end;

class function TAsmSourceFile.GetClassFilter: string;
begin
	Result := 'A68k Assembly Files (*.asm)|*.asm';
end;

class function TAsmSourceFile.GetClassImageIndex: Integer;
begin
	Result := 5;
end;

class function TAsmSourceFile.GetClassItemName: string;
begin
	Result := 'Assembler File';
end;

class function TAsmSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 3;
end;

class function TAsmSourceFile.GetCompilable: Boolean;
begin
	Result := True;
end;

function TAsmSourceFile.GetContentType: TSourceFileType;
begin
	Result := ftA68kAsmFile;
end;

procedure TAsmSourceFile.Save;
var
	FH: THandle;
begin
	inherited;
	if (not Invalidated) and FileExists (ChangeFileExt (FileName, '.o')) then begin
		FH := FileOpen (ChangeFileExt (FileName, '.o'), fmOpenReadWrite + fmShareExclusive);
		FileSetDate (FH, FileAge (FileName));
		FileClose (FH);
	end;
end;

procedure TAsmSourceFile.SetModified(const Value: Boolean);
begin
	if not Modifying then begin
		Modifying := True;
		FModified := Value;
		if Value then
			Invalidate;
		Modifying := False;
	end;
end;

procedure TAsmSourceFile.UpdateEditor;
begin
	inherited;
	if Assigned (SourceEditor) then begin
		SourceEditor.TabSize := TabSizeAsm;
		SourceEditor.AutoIndentIncrease := False;
	end;
end;

procedure TAsmSourceFile.UpdateSyntax;
begin
	if Assigned (SourceEditor) then
		with SourceEditor do begin
			SyntaxColoring.Assign (SyntaxAsm);
			TabSize := TabSizeAsm;
			AutoIndentIncrease := False;
		end;
end;

{ TQuillSourceFile }

procedure TQuillSourceFile.Compile;
var
	QuillDrv: string;
begin
	QuillDrv := WithBackslash (TIGCCFolder) + QuillIncludeLocation + 'Quill.drv';
	if not FileExists (QuillDrv) then begin
		QuillDrv := WithBackslash (TIGCCFolder) + CIncludeLocation + 'Quill.drv';
		if not FileExists (QuillDrv) then begin
			QuillDrv := WithBackslash (TIGCCFolder) + GCCLocation + 'Quill.drv';
			if not FileExists (QuillDrv) then
				QuillDrv := 'Quill.drv';
		end;
	end;
	SpecialSwitches := SpecialQuillGCCSwitches + ' -include "' + QuillDrv + '"';
	inherited;
end;

class function TQuillSourceFile.GetClassFilter: string;
begin
	Result := 'Quill Files (*.qll)|*.qll';
end;

class function TQuillSourceFile.GetClassImageIndex: Integer;
begin
	Result := 4;
end;

class function TQuillSourceFile.GetClassItemName: string;
begin
	Result := 'Quill File';
end;

class function TQuillSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 3;
	if ssA68k in SpecialSupport then
		Inc (Result);
end;

function TQuillSourceFile.GetContentType: TSourceFileType;
begin
	Result := ftQuillFile;
end;

procedure TQuillSourceFile.UpdateSyntax;
begin
	if Assigned (SourceEditor) then
		with SourceEditor do
			SyntaxColoring.Assign (SyntaxQuill);
end;

{ TObjectSourceFile }

class function TObjectSourceFile.GetClassFilter: string;
begin
	Result := 'Object Files (*.o)|*.o';
end;

class function TObjectSourceFile.GetClassImageIndex: Integer;
begin
	Result := 6;
end;

class function TObjectSourceFile.GetClassItemName: string;
begin
	Result := 'Object File';
end;

class function TObjectSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 3;
	if ssA68k in SpecialSupport then
		Inc (Result);
	if ssQuill in SpecialSupport then
		Inc (Result);
end;

{ TArchiveSourceFile }

class function TArchiveSourceFile.GetClassFilter: string;
begin
	Result := 'Archive Files (*.a)|*.a';
end;

class function TArchiveSourceFile.GetClassImageIndex: Integer;
begin
	Result := 6;
end;

class function TArchiveSourceFile.GetClassItemName: string;
begin
	Result := 'Archive File';
end;

class function TArchiveSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 4;
	if ssA68k in SpecialSupport then
		Inc (Result);
	if ssQuill in SpecialSupport then
		Inc (Result);
end;

{ TNormalTextSourceFile }

destructor TNormalTextSourceFile.Destroy;
var
	PrevEditor: TMemoComponent;
begin
	if Assigned (FEditor) then begin
		PrevEditor := FEditor;
		FEditor := nil;
		PrevEditor.Free;
	end;
	inherited;
end;

class function TNormalTextSourceFile.GetClassFilter: string;
begin
	Result := 'Text Files (*.txt)|*.txt';
end;

class function TNormalTextSourceFile.GetClassImageIndex: Integer;
begin
	Result := 7;
end;

class function TNormalTextSourceFile.GetClassItemName: string;
begin
	Result := 'Text File';
end;

class function TNormalTextSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 5;
	if ssA68k in SpecialSupport then
		Inc (Result);
	if ssQuill in SpecialSupport then
		Inc (Result);
end;

function TNormalTextSourceFile.GetInternalTextEditor: TMemoComponent;
begin
	Result := FEditor;
end;

function TNormalTextSourceFile.GetTextEditor: TMemoComponent;
begin
	if not Assigned (FEditor) then begin
		FEditor := TMemoComponent.Create (Application.MainForm);
		with FEditor do begin
			AllowUndo := False;
			TabSize := 8;
			Text := FTempContent;
		end;
		UpdateEditor;
		FTempContent := '';
		with FEditor do begin
			OnChangePrivate := Change;
			OnReplaceText := ReplaceText;
			AllowUndo := True;
		end;
	end;
	Result := FEditor;
end;

{ TOtherSourceFile }

class function TOtherSourceFile.GetClassFilter: string;
begin
	Result := '';
end;

class function TOtherSourceFile.GetClassImageIndex: Integer;
begin
	Result := 8;
end;

class function TOtherSourceFile.GetClassItemName: string;
begin
	Result := 'Other File';
end;

class function TOtherSourceFile.GetClassTreeIndex: Integer;
begin
	Result := 6;
	if ssA68k in SpecialSupport then
		Inc (Result);
	if ssQuill in SpecialSupport then
		Inc (Result);
end;

end.

