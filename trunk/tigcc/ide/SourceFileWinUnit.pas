{
  TIGCC IDE

  Copyright (C) 2000-2004 Sebastian Reichelt

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

unit SourceFileWinUnit;

interface

uses
	SourceFileUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, ActnList, ToolWin, MemoComponentUnit, ExtCtrls{$IFDEF CODINGEXT}, CompletionForm{$ENDIF};

type
	TSourceFileForm = class(TForm)
    Actions: TActionList;
    ActionFilePrint: TAction;
    ActionFilePrintQuickly: TAction;
		ActionFileClose: TAction;
		ActionEditUndo: TAction;
    ActionEditRedo: TAction;
    ActionEditDelete: TAction;
    ActionEditCut: TAction;
    ActionEditCopy: TAction;
    ActionEditPaste: TAction;
    ActionEditSelectAll: TAction;
    ActionFindFind: TAction;
    ActionFindReplace: TAction;
    ActionFindOpenFile: TAction;
    ActionFindFunctions: TAction;
    ActionEditIncreaseIndent: TAction;
    ActionEditDecreaseIndent: TAction;
		MainMenu: TMainMenu;
    File1: TMenuItem;
		Print1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N2: TMenuItem;
    Delete1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    N9: TMenuItem;
    SelectAll1: TMenuItem;
    N18: TMenuItem;
		IncreaseIndent1: TMenuItem;
    DecreaseIndent1: TMenuItem;
    Find1: TMenuItem;
		Find2: TMenuItem;
    Replace1: TMenuItem;
    N13: TMenuItem;
    Functions1: TMenuItem;
    N15: TMenuItem;
    OpenFileAtCursor1: TMenuItem;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    ActionFileCompile: TAction;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
		N1: TMenuItem;
		Compile1: TMenuItem;
		N3: TMenuItem;
		N4: TMenuItem;
		Close1: TMenuItem;
		StatusBar: TStatusBar;
    EditorToolBar: TToolBar;
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
    ToolButton19: TToolButton;
    ToolButton11: TToolButton;
		ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton25: TToolButton;
    ToolButton22: TToolButton;
    ToolButton13: TToolButton;
    ToolBarBevel: TBevel;
    ToolButton1: TToolButton;
    EditorPopup: TPopupMenu;
    Undo2: TMenuItem;
		Redo2: TMenuItem;
    N16: TMenuItem;
    Clear1: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
		N17: TMenuItem;
    SelectAll2: TMenuItem;
    N20: TMenuItem;
    IncreaseIndent2: TMenuItem;
    DecreaseIndent2: TMenuItem;
		N5: TMenuItem;
		OpenFileatCursor2: TMenuItem;
		FindDlg: TFindDialog;
		ReplaceDlg: TReplaceDialog;
		FunctionPopup: TPopupMenu;
		NoFunctionsItem: TMenuItem;
		ActionFileAddToProject: TAction;
		AddtoProject1: TMenuItem;
		N6: TMenuItem;
		ToolButton2: TToolButton;
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure FormCreate(Sender: TObject);
		procedure ActionFileCloseExecute(Sender: TObject);
		procedure FileSave(Sender: TObject);
		procedure FileSaveAs(Sender: TObject);
		procedure FileCompile(Sender: TObject);
		procedure FilePrint(Sender: TObject);
		procedure FilePrintQuickly(Sender: TObject);
		procedure EditorChange(Sender: TObject);
		procedure EditUndo(Sender: TObject);
		procedure EditRedo(Sender: TObject);
		procedure EditDelete(Sender: TObject);
		procedure EditCut(Sender: TObject);
		procedure EditCopy(Sender: TObject);
		procedure EditPaste(Sender: TObject);
		procedure EditSelectAll(Sender: TObject);
		procedure EditIncreaseIndent(Sender: TObject);
		procedure EditDecreaseIndent(Sender: TObject);
		procedure FindDlgFind(Sender: TObject);
		procedure ReplaceDlgReplace(Sender: TObject);
		procedure FindString(Sender: TObject);
		procedure FindFind(Sender: TObject);
		procedure FindReplace(Sender: TObject);
		procedure FindOpenFile(Sender: TObject);
		procedure FunctionPopupPopup(Sender: TObject);
		procedure FindFunctions(Sender: TObject);
		procedure FindFunctionFromPopup(Sender: TObject);
		procedure FileAddToProject(Sender: TObject);
		procedure FormEnable(Sender: TObject);
		procedure FormShow(Sender: TObject);
    procedure FindDlgClose(Sender: TObject);
	private
		FSourceFile: TSourceFile;
		procedure SetSourceFile(const Value: TSourceFile);
		function GetEditor: TMemoComponent;
	protected
		procedure CreateParams(var Params: TCreateParams); override;
	public
		OriginalCaption: string;
		EnableTimer: TTimer;
		Funcs: TSourceFileFunctions;
		procedure UpdateCaption;
		procedure UpdateEditButtons;
		procedure UpdateStatusBar;
		procedure UpdateFuncs;
		property SourceFile: TSourceFile read FSourceFile write SetSourceFile;
		property Editor: TMemoComponent read GetEditor;
{$IFDEF CODINGEXT}
		procedure InitCodingExt;
		procedure ActionFindSymbolExecute(Sender: TObject);
{$ENDIF}
	end;

implementation

{$R *.DFM}

uses
	Printers,
	UtilsDos, UtilsWin,
	MasterUnit,
	MainUnit, FunctionsWinUnit;

procedure TSourceFileForm.SetSourceFile(const Value: TSourceFile);
begin
	FSourceFile := Value;
	UpdateCaption;
	if Assigned (SourceFile) and (SourceFile is TTextSourceFile) then
		with SourceFile as TTextSourceFile do
			if Assigned (TextEditor) then
				with TextEditor do begin
					OnKeyDown := MainForm.EditorKeyDown;
					OnChange := EditorChange;
					OnSelectionChange := EditorChange;
					PopupMenu := EditorPopup;
				end;
	ActionFileCompile.Enabled := Assigned (SourceFile) and SourceFile.Compilable;
	ActionFilePrint.Enabled := Assigned (SourceFile) and SourceFile.Printable and (Printer.Printers.Count > 0);
	ActionFilePrintQuickly.Enabled := ActionFilePrint.Enabled;
	ActionFindFunctions.Enabled := Assigned (SourceFile) and (SourceFile is TSourceTextSourceFile);
	EditorChange (Editor);
end;

procedure TSourceFileForm.UpdateCaption;
begin
	if Assigned (SourceFile) then begin
		Caption := OriginalCaption + ' - ' + SourceFile.SourceName;
		if (MainForm.ProjectFile = '') and (ExtractFilePath (SourceFile.FileName) = 'C:\') then
			StatusBar.Panels.Items[5].Text := ExtractFileName (SourceFile.FileName)
		else
			StatusBar.Panels.Items[5].Text := SourceFile.FileName;
	end else
		Caption := OriginalCaption;
end;

function TSourceFileForm.GetEditor: TMemoComponent;
begin
	if Assigned (SourceFile) and (SourceFile is TTextSourceFile) then
		Result := (SourceFile as TTextSourceFile).TextEditor
	else
		Result := nil;
end;

procedure TSourceFileForm.FormClose(Sender: TObject;
	var Action: TCloseAction);
begin
	if Assigned (SourceFile) and (SourceFile.ParentForm = Self) then begin
		SourceFile.ParentForm := nil;
		SourceFile.Free;
	end;
	Action := caFree;
end;

procedure TSourceFileForm.FormCloseQuery(Sender: TObject;
	var CanClose: Boolean);
begin
	if Compiling then
		CanClose := False
	else if Assigned (SourceFile) and (SourceFile.ParentForm = Self) then
		SourceFile.WarnIfModified;
end;

procedure TSourceFileForm.FormCreate(Sender: TObject);
begin
	OriginalCaption := Caption;
{$IFDEF CODINGEXT}
  InitCodingExt;
{$ENDIF}
end;

procedure TSourceFileForm.ActionFileCloseExecute(Sender: TObject);
begin
	if not Compiling then
		Close;
end;

procedure TSourceFileForm.FileSave(Sender: TObject);
begin
	if Assigned (SourceFile) then begin
		SourceFile.Save;
		UpdateCaption;
	end;
end;

procedure TSourceFileForm.FileSaveAs(Sender: TObject);
begin
	if Assigned (SourceFile) then begin
		SourceFile.SaveAs;
		UpdateCaption;
		UpdateStatusBar;
	end;
end;

procedure TSourceFileForm.FileCompile(Sender: TObject);
begin
	if (not Compiling) and Assigned (SourceFile) then
		with MainForm do begin
			BeginCompilation;
			CopyHeaders;
			Self.SourceFile.Compile;
			DeleteHeaders;
			EndCompilation;
		end;
end;

procedure TSourceFileForm.FilePrint(Sender: TObject);
var
	Cp: Integer;
begin
	if Assigned (SourceFile) and (Printer.Printers.Count > 0) then
		with MainForm do try
			PrintDlg.PrintRange := prAllPages;
			PrintDlg.MaxPage := TSourceFile(ProjectTree.Selected.Data).CountPages;
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
			SourceFile.Print (Cp, PrintDlg.PrintRange, PrintDlg.FromPage, PrintDlg.ToPage);
		except
			Abort;
		end;
end;

procedure TSourceFileForm.FilePrintQuickly(Sender: TObject);
begin
	FilePrint (nil);
end;

procedure TSourceFileForm.EditorChange(Sender: TObject);
begin
	UpdateEditButtons;
	UpdateStatusBar;
end;

procedure TSourceFileForm.UpdateEditButtons;
var
	TextSel: Boolean;
begin
	if Assigned (Editor) then begin
		TextSel := Editor.SelLength > 0;
		ActionEditDelete.Enabled := TextSel;
		ActionEditCut.Enabled := TextSel;
		ActionEditCopy.Enabled := TextSel;
		ActionEditUndo.Enabled := Editor.CanUndo;
		ActionEditRedo.Enabled := Editor.CanRedo;
	end;
end;

procedure TSourceFileForm.UpdateStatusBar;
var
	S: string;
	I: Integer;
	Cell: TTextCell;
begin
	with StatusBar.Panels do begin
		if Assigned (Editor) then begin
			with Editor do begin
				if Enabled then begin
					if Selection.RLength > 0 then begin
						I := Selection.RLength + Selection.StartRowCol.Row - Selection.EndRowCol.Row;
						if Selection.EndRowCol.Col > LineLength [Selection.EndRowCol.Row] then
							Dec (I);
						S := IntToStr (I);
						if Items[0].Text <> S then
							Items[0].Text := S;
						if Items[1].Text <> '' then
							Items[1].Text := '';
						if Items[0].Width <> 60 then
							Items[0].Width := 60;
						if Items[1].Width <> 0 then
							Items[1].Width := 0;
					end else begin
						Cell := Selection.StartRowCol;
						S := IntToStr (Cell.Row);
						if Items[0].Text <> S then
							Items[0].Text := S;
						S := IntToStr (Cell.Col);
						if Items[1].Text <> S then
							Items[1].Text := S;
						if Items[0].Width <> 30 then
							Items[0].Width := 30;
						if Items[1].Width <> 30 then
							Items[1].Width := 30;
					end;
					I := TextLength - LineCount + 1;
					S := IntToStr (I) + ' Character';
					if I <> 1 then
						S := S + 's';
					if Items[3].Text <> S then
						Items[3].Text := S;
					if Items[2].Width <> 1 then
						Items[2].Width := 1;
					if Items[3].Width <> 93 then
						Items[3].Width := 93;
					if Items[2].Width <> 1 then
						Items[2].Width := 1;
				end;
			end;
		end else begin
			if Items[0].Text <> '' then
				Items[0].Text := '';
			if Items[1].Text <> '' then
				Items[1].Text := '';
			if Items[3].Text <> '' then
				Items[3].Text := '';
			if Items[0].Width <> 0 then
				Items[0].Width := 0;
			if Items[1].Width <> 0 then
				Items[1].Width := 0;
			if Items[2].Width <> 0 then
				Items[2].Width := 0;
			if Items[3].Width <> 0 then
				Items[3].Width := 0;
			if Items[4].Width <> 0 then
				Items[4].Width := 0;
		end;
	end;
	Update;
end;

procedure TSourceFileForm.EditUndo(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.Undo;
end;

procedure TSourceFileForm.EditRedo(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.Redo;
end;

procedure TSourceFileForm.EditDelete(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.ClearSelection;
end;

procedure TSourceFileForm.EditCut(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.CutToClipboard;
end;

procedure TSourceFileForm.EditCopy(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.CopyToClipboard;
end;

procedure TSourceFileForm.EditPaste(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.PasteFromClipboard;
end;

procedure TSourceFileForm.EditSelectAll(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.SelectAll;
end;

procedure TSourceFileForm.EditIncreaseIndent(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.ChangeIndent (1);
end;

procedure TSourceFileForm.EditDecreaseIndent(Sender: TObject);
begin
	if Assigned (Editor) then
		Editor.ChangeIndent (-1);
end;

procedure TSourceFileForm.FindDlgFind(Sender: TObject);
begin
	try
		FindString (Sender);
	except
		ShowDefaultMessageBox ('Text ''' + (Sender as TFindDialog).FindText + ''' not found.', 'Search Failed', mtProgramError);
	end;
end;

procedure TSourceFileForm.ReplaceDlgReplace(Sender: TObject);
begin
	if Assigned (Editor) then
		with Sender as TReplaceDialog do
			repeat
				if UpperCase (Editor.Selection.Text) = UpperCase (FindText) then
					Editor.Selection.Text := ReplaceText;
				Options := Options + [frFindNext];
				FindString (Sender);
			until not (frReplaceAll in Options);
end;

procedure TSourceFileForm.FindString(Sender: TObject);
var
	P: Integer;
	S,
	T: string;
	Valid: Boolean;
	FPos: Integer;
begin
	if Assigned (Editor) then begin
		with Sender as TFindDialog do begin
			if frFindNext in Options then
				P := Editor.SelStart + 1
			else
				if frDown in Options then
					P := 1
				else
					P := Length (Editor.Text);
			repeat
				Valid := False;
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
				with Editor.Selection do begin
					DoChanging;
					RStart := P + FPos;
					RLength := Length (S);
					DoChange;
					ScrollInView (2);
				end;
				SetFocus;
			end else
				Abort;
		end;
	end;
end;

procedure TSourceFileForm.FindFind(Sender: TObject);
begin
	FindDlg.Execute;
	SetFocus;
end;

procedure TSourceFileForm.FindReplace(Sender: TObject);
begin
	ReplaceDlg.Execute;
	SetFocus;
end;

procedure TSourceFileForm.FindOpenFile(Sender: TObject);
var
	I: Integer;
	S: string;
	QuotesInLine: Boolean;
	SourceFile: TSourceFile;
begin
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
			SourceFile := MainForm.SourceFiles.FindFileNameOnly (S);
			if Assigned (SourceFile) then
				SourceFile.Edit
			else begin
				SourceFile := Self.SourceFile;
				if FileExists (ExpandFileName (S)) then
					MainForm.AddSourceFile (ExpandFileName (S), True)
				else if Assigned (SourceFile) and (SourceFile is TGNUAsmSourceFile) and FileExists (WithBackslash (TIGCCFolder) + GASIncludeLocation + S) then
					MainForm.AddSourceFile (WithBackslash (TIGCCFolder) + GASIncludeLocation + S, True)
				else if Assigned (SourceFile) and (SourceFile is TAsmSourceFile) and FileExists (WithBackslash (TIGCCFolder) + ASMIncludeLocation + S) then
					MainForm.AddSourceFile (WithBackslash (TIGCCFolder) + ASMIncludeLocation + S, True)
				else if FileExists (WithBackslash (TIGCCFolder) + CIncludeLocation + S) then
					MainForm.AddSourceFile (WithBackslash (TIGCCFolder) + CIncludeLocation + S, True)
				else
					ShowDefaultMessageBox ('File ''' + S + ''' not found.', 'Search Failed', mtProgramError);
			end;
		end;
	end;
end;

procedure TSourceFileForm.FunctionPopupPopup(Sender: TObject);
var
	I: Integer;
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
			if (Pos ('main', Funcs[I].Name) > 0) and (Pos ('main', Funcs[I].Name) = Length (Funcs[I].Name) - Length ('main') + 1) then
				Insert (0, M)
			else
				Add (M);
		end;
		if Count <= 0 then
			Add (NoFunctionsItem);
	end;
end;

procedure TSourceFileForm.FindFunctions(Sender: TObject);
var
	I: Integer;
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
				if Assigned (Editor) then begin
					Editor.Selection.NoSelAtPos (Editor.CellToCharIdx (TextCell (Self.Funcs[Integer(FuncList.Items.Objects[FuncList.ItemIndex])].PrototypeLine, 1)));
					Editor.Selection.ScrollInView (5);
				end;
			end;
			mrNo: begin
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

procedure TSourceFileForm.UpdateFuncs;
begin
	if Assigned (SourceFile) and (SourceFile is TSourceTextSourceFile) then
		with SourceFile as TSourceTextSourceFile do
			Funcs := GetFunctions
	else
		SetLength (Funcs, 0);
end;

procedure TSourceFileForm.FindFunctionFromPopup(Sender: TObject);
var
	L: Integer;
begin
	if Sender is TMenuItem then
		with Sender as TMenuItem do begin
			if Funcs[Tag].ImplementationLine > 0 then
				L := Funcs[Tag].ImplementationLine
			else
				L := Funcs[Tag].PrototypeLine;
			if L > 0 then begin
				if Assigned (Editor) then begin
					Editor.Selection.NoSelAtPos (Editor.CellToCharIdx (TextCell (L, 1)));
					Editor.Selection.ScrollInView (5);
				end;
			end;
		end;
end;

procedure TSourceFileForm.FileAddToProject(Sender: TObject);
var
	N: TTreeNode;
begin
	if (not Compiling) and Assigned (SourceFile) then
		with MainForm, SourceFile do begin
			ParentForm := nil;
			N := ProjectTree.Items.AddChildObject (TopNode.Item [ClassTreeIndex], SourceName, Pointer (SourceFile));
			with N do begin
				ImageIndex := ClassImageIndex;
				SelectedIndex := ImageIndex;
			end;
			TreeItem := N;
			if Assigned (Editor) then
				with Editor do begin
					Hide;
					Parent := EditorPanel;
					Align := alClient;
				end;
			if SourceFile is TTextSourceFile then
				with SourceFile as TTextSourceFile do
					if Assigned (TextEditor) then
						with TextEditor do begin
							OnEnter := EditorEnter;
							OnExit := EditorExit;
							OnKeyDown := EditorKeyDown;
							OnChange := EditorChange;
							OnSelectionChange := EditorChange;
							PopupMenu := EditorPopup;
						end;
			SortFiles;
			MainForm.Modify;
			ProjectTree.Selected := N;
			SourceFile := nil;
			Self.Close;
		end;
end;

procedure TSourceFileForm.FormEnable(Sender: TObject);
begin
	if Assigned (EnableTimer) then begin
		EnableTimer.Free;
		EnableTimer := nil;
	end;
	Enabled := True;
	if Assigned (Editor) then begin
		Editor.Enabled := False;
		Editor.Enabled := True;
		ActiveControl := Editor;
	end;
	SetFocus;
end;

procedure TSourceFileForm.FormShow(Sender: TObject);
begin
	if not (Enabled or Assigned (EnableTimer)) then begin
		EnableTimer := TTimer.Create (Self);
		EnableTimer.OnTimer := FormEnable;
		EnableTimer.Interval := 100;
		EnableTimer.Enabled := True;
	end;
end;

procedure TSourceFileForm.CreateParams(var Params: TCreateParams);
begin
	inherited;
	Params.WndParent := GetDesktopWindow;
end;

{$IFDEF CODINGEXT}
procedure TSourceFileForm.InitCodingExt;
var
	ActionFindSymbol: TAction;
begin
	// Find Symbol declaration tool
	ActionFindSymbol := TAction.Create(Self);
	with ActionFindSymbol do begin
		ActionList := Actions;
		Caption := 'Find Symbol Declaration';
		Category := 'Extension';
		OnExecute := ActionFindSymbolExecute;
	end;

	InsertsAction(Self, [OpenFileAtCursor1, OpenFileatCursor2], ActionFindSymbol);
end;

procedure TSourceFileForm.ActionFindSymbolExecute(Sender: TObject);
begin
	CompForm.FindSymbolDecl;
end;
{$ENDIF}

procedure TSourceFileForm.FindDlgClose(Sender: TObject);
begin
	SetFocus;
end;

end.

