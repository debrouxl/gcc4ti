unit CompletionForm;

interface

uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, MasterUnit,
	Dialogs, StdCtrls, ComCtrls, CodeCompletion, SourceFileUnit, UtilsDos, MemoComponentUnit,
	HtFormatting, Menus, ActnList;

const
	SymbolFileNotFound = 'Symbol %s is declared in %s, but file can''t be found!';
	AddTemplatesMsg = 'To add templates, go to Preferences->Coding';

type
	TCompForm = class(TForm)
		CompList: TListBox;
		CompStatus: TStatusBar;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormDeactivate(Sender: TObject);
		procedure CompListDrawItem(Control: TWinControl; Index: Integer;
			Rect: TRect; State: TOwnerDrawState);
		procedure CompListDblClick(Sender: TObject);
		procedure CompListKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure CompListKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure CompListKeyPress(Sender: TObject; var Key: Char);
	private
		procedure SetEditor(const Value: TMemoComponent);
	protected
		{ Déclarations protégées }
		C: TCompletionList;
		CompRow: Integer;
		StopKey: Boolean;
		HintWindow: THtHintWindow;

		FSourceFile: TSourceFile;
		FEditor: TMemoComponent;

		procedure CreateParams(var Params: TCreateParams); override;
		function GetFileData(const F: string): string;
		procedure NeedFile(const FileName: string; out CCFData: string);

	protected
		// Be informed when active control change :)

		OldActiveControlChange: TNotifyEvent;
		procedure ActiveControlChange(Sender: TObject);
	public
		{ Déclarations publiques }

		// Get & Replace word under caret
		procedure DelimitateWord(const Line: string; var StartPos: Integer; out EndPos: Integer);
		function GetWord(Full, RemoveSpace: Boolean): string;
		procedure SetWord(const Value: string);

		// Show Completion Windows
		property List: TCompletionList read C;
		procedure RebuildList;
		procedure ShowCompletion;
		procedure CloseCompletion;
		procedure ApplyCompletion;

		// Show an hint window about the symbol Symbol, or symbol under caret if ''
		function ShowSymbolInfo(Symbol: string = ''): Boolean;
		procedure CloseSymbolInfo;

		// Find symbol declaration
		procedure FindSymbolDecl;

	protected
		OldChange: TNotifyEvent;
		OldKeyDown: TKeyEvent;
		OldKeyPress: TKeyPressEvent;

		// Redirect Event from Editor
		procedure Editor_Enter(Sender: TMemoComponent);
		procedure Editor_Exit(Sender: TObject);
		procedure Editor_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure Editor_KeyPress(Sender: TObject; var Key: Char);
		procedure Editor_Change(Sender: TObject);

	public
		property Editor: TMemoComponent read FEditor write SetEditor;
	end;

var
	CompForm: TCompForm;

	// TIGCC integration
function SourceFileFromEditor(M: TMemoComponent): TSourceFile;

procedure InsertAction(Owner: TComponent; After: TMenuItem; Action: TBasicAction);
procedure InsertsAction(Owner: TComponent; const Afters: array of TMenuItem; Action: TBasicAction);

implementation

uses Types, StrUtils, Math, TmpltForm, MainUnit;

{$R *.dfm}

function SourceFileFromEditor(M: TMemoComponent): TSourceFile;
var
	i: Integer;
	Lst: TSourceFiles;
	Src: TSourceFile;
begin
	Lst := MainForm.SourceFiles;
	for i := 0 to Lst.Count - 1 do
	begin
		Src := TSourceFile(Lst.Items[i]);
		if Src.Editor = M then
		begin
			Result := Src;
			Exit;
		end;
	end;
	Result := nil;
end;

procedure InsertAction(Owner: TComponent; After: TMenuItem; Action: TBasicAction);
var
	Item: TMenuItem;
begin
	Item := TMenuItem.Create(Owner);
	Item.Action := Action;
	with After.Parent do
		Insert(IndexOf(After) + 1, Item);
end;

procedure InsertsAction(Owner: TComponent; const Afters: array of TMenuItem; Action: TBasicAction);
var
	i: Integer;
begin
	for i := Low(Afters) to High(Afters) do
		InsertAction(Owner, Afters[i], Action);
end;

// GUI Methods

procedure TCompForm.FormCreate(Sender: TObject);
begin
	C := TCompletionList.Create;
	C.OnNeedFile := NeedFile;
	HintWindow := THtHintWindow.Create(Self);
	OldActiveControlChange := Screen.OnActiveControlChange;
	Screen.OnActiveControlChange := ActiveControlChange;
end;

procedure TCompForm.FormDestroy(Sender: TObject);
begin
	if Assigned(Screen.OnActiveControlChange) then
		Screen.OnActiveControlChange := OldActiveControlChange;
	HintWindow.Free;
	C.Free;
end;

procedure TCompForm.FormDeactivate(Sender: TObject);
var
	M: TMemoComponent;
begin
	M := Editor;
	if (M = nil) or (not M.Focused) then
		CloseCompletion;
end;

procedure TCompForm.CreateParams(var Params: TCreateParams);
begin
	inherited;
	Params.Style := WS_POPUP or WS_SYSMENU or WS_THICKFRAME;
end;

procedure TCompForm.CompListDrawItem(Control: TWinControl; Index: Integer;
	Rect: TRect; State: TOwnerDrawState);
const
	MinWidth = 62;
var
	C: TCompletionItem;
	S: string;
	Cnv: TCanvas;
	Selected: Boolean;
	W: Integer;
begin
	if (Index >= 0) and (Index < List.Count) then
	begin
		Cnv := CompList.Canvas;
		Selected := odSelected in State;
		C := List.Completion[Index];
		S := List[Index];
		if Assigned(C) then
		begin
			Cnv.Font.Style := [];
			DrawHtText(C.Left, Rect, Cnv, W, Selected);
			Inc(Rect.Left, W);
		end;
		if Rect.Left < MinWidth then
			Rect.Left := MinWidth;
		Inc(Rect.Left, 2);
		Cnv.Font.Style := [fsBold];
		DrawHtText(S, Rect, Cnv, W, Selected);
		Inc(Rect.Left, W + 2);
		if Assigned(C) then
		begin
			Cnv.Font.Style := [];
			DrawHtText(C.Right, Rect, CompList.Canvas, Rect.Left, Selected);
			if Selected then
				CompStatus.SimpleText := C.Description;
		end;
	end;
end;

procedure TCompForm.CompListDblClick(Sender: TObject);
begin
	ApplyCompletion;
end;

// Completion Tools

procedure TCompForm.RebuildList;
var
	S: TSourceFile;
begin
	List.Clear;
	S := SourceFileFromEditor(Editor);
	if Assigned(S) then
	begin
		if S is TCSourceFile then
			List.AddCompletion(MakeCCF_C(ExtractFileName(S.FileName), TCSourceFile(S)))
		else if S is THeaderSourceFile then
			List.AddCompletion(MakeCCF_H(ExtractFileName(S.FileName), THeaderSourceFile(S).SourceEditor.Lines));
	end;
	CompList.Count := List.Count;
end;

procedure TCompForm.ShowCompletion;
var
	M: TMemoComponent;
	P: TPoint;
	i: Integer;
begin
	M := Editor;
	CloseSymbolInfo;
	if Assigned(M) then
	begin
		RebuildList;
		CompRow := M.Selection.StartRowCol.Row;
		P := M.Selection.StartPoint;
		P := M.ClientToScreen(P);
		Left := P.X + 4;
		Top := P.Y + 20;
		List.Find(GetWord(True, False), i);
		CompList.ItemIndex := i;
		LockWindowUpdate(M.ParentWindow);
		Show;
		M.SetFocus;
		LockWindowUpdate(0);
	end;
end;

procedure TCompForm.CloseCompletion;
begin
	Hide;
end;

function TCompForm.ShowSymbolInfo(Symbol: string = ''): Boolean;
	function Similarity(const A, B: string): Integer;
	var
		i, l: Integer;
	begin
		l := Min(Length(A), Length(B));
		i := 1;
		while (i <= l) and (UpCase(A[i]) = UpCase(B[i])) do
			Inc(i);
		Result := Abs(i - l);
	end;
	function Similar(const A, B: string): Boolean;
	var
		l: Integer;
	begin
		l := Min(Length(A), Length(B));
		Result := Similarity(A, B) < (l shr 1);
	end;
var
	M: TMemoComponent;
	P: TPoint;
	R: TRect;
	i: Integer;
	C: TCompletionItem;
	T: string;
begin
	CloseCompletion;
	Result := False;
	if Symbol = '' then
		Symbol := GetWord(False, True);
	if Symbol = '' then
	begin
		CloseSymbolInfo;
		Exit;
	end;

	M := Editor;
	if Assigned(M) then
	begin
		RebuildList;

		// Find Symbol
		if List.Find(Symbol, i) then
			T := ''
		else if (i >= 0) and (i < List.Count) then
		begin
			if i = 0 then
			begin
				if Similar(Symbol, List[i]) then
					T := List[i] + ' ?'#13#10
				else
					Exit;
			end
			else
			begin
				if Similarity(Symbol, List[i - 1]) > Similarity(Symbol, List[i]) then
					Dec(i);
				if Similar(Symbol, List[i]) then
					T := List[i] + ' ?'#13#10
				else
					Exit;
			end;
		end
		else
			Exit;

		// Show Symbol info
		C := List.Completion[i];
		if Assigned(C) then
		begin
			T := T + C.Left + ' ' + C.Right + #13#10 + C.Description;
			// Calc hint window position
			CompRow := M.Selection.StartRowCol.Row;
			P := M.ClientToScreen(M.Selection.StartPoint);
			R := HintWindow.CalcHintRect(Screen.Width, T, nil);
			Inc(R.Left, P.X + 4);
			Inc(R.Right, P.X + 4);
			Inc(R.Top, P.Y + 20);
			Inc(R.Bottom, P.Y + 20);
			LockWindowUpdate(M.ParentWindow);
			HintWindow.ActivateHint(R, T);
			Result := True;
			M.SetFocus;

			// Bug with TSourceFileForm ??!!
			if Assigned(M.Parent) and (M.Parent is TForm) then
				M.Parent.BringToFront;
			LockWindowUpdate(0);
		end;
	end;
end;

procedure TCompForm.CloseSymbolInfo;
begin
	if IsWindowVisible(HintWindow.Handle) then
		ShowWindow(HintWindow.Handle, SW_HIDE);
end;

procedure TCompForm.ApplyCompletion;
var
	i: Integer;
begin
	i := CompList.ItemIndex;
	if (i >= 0) and (i < List.Count) then
	begin
		SetWord(List[i]);
		CloseCompletion;
	end;
end;

// Find Files

function TCompForm.GetFileData(const F: string): string;
var
	S: TSourceFile;
	Folder: string;
begin
	S := MainForm.SourceFiles.FindFileNameOnly(F);
	Folder := WithBackslash(TIGCCFolder);
	if Assigned(S) and (S is TSourceTextSourceFile) then
		Result := TSourceTextSourceFile(S).Content
	else if FileExists(ExpandFileName(F)) then
		Result := FileToString(ExpandFileName(F))
	else if FileExists(Folder + CIncludeLocation + F) then
		Result := FileToString(Folder + CIncludeLocation + F)
	else if FileExists(Folder + ASMIncludeLocation + F) then
		Result := FileToString(Folder + ASMIncludeLocation + F)
	else
		Result := '';
end;

procedure TCompForm.NeedFile(const FileName: string; out CCFData: string);
var
	CCFName: string;
	Lst: TStringList;
begin
	// Search CCF file
	CCFName := WithBackslash(TIGCCFolder) + CompletionLocation + ChangeFileExt(FileName, '.ccf');
	if FileExists(CCFName) then
		CCFData := FileToString(CCFName)
	else
	begin
		// No CCF ? Generate info from header...
		if UpperCase(ExtractFileExt(FileName)) = '.H' then
		begin
			Lst := TStringList.Create;
			Lst.Text := GetFileData(FileName);
			if Lst.Count > 0 then
				CCFData := MakeCCF_H(FileName, Lst);
			Lst.Free;
		end;
	end;
end;

// Word manipulations

procedure TCompForm.DelimitateWord(const Line: string; var StartPos: Integer; out EndPos: Integer);
const
	ValidChars = ['A'..'Z', 'a'..'z', '0'..'9', '_', '$', '#'];
var
	i, j, l: Integer;
begin
	i := StartPos - 1;
	j := StartPos - 1;
	l := Length(Line);
	// Find start pos
	while (i > 0) and (i <= l) and (Line[i] in ValidChars) do
		Dec(i);
	// Find end pos
	while (j > 0) and (j <= l) and (Line[j] in ValidChars) do
		Inc(j);
	if j = StartPos - 1 then
	begin
		j := StartPos;
		while (j > 0) and (j <= l) and (Line[j] in ValidChars) do
			Inc(j);
	end;
	Dec(j);
	Inc(i);
	StartPos := i;
	EndPos := j;
end;

function TCompForm.GetWord(Full, RemoveSpace: Boolean): string;
var
	M: TMemoComponent;
	S: string;
	T: TTextCell;
	StPos, EndPos, l: Integer;
begin
	M := Editor;
	Result := '';
	if Assigned(M) then
	begin
		T := M.Selection.StartRowCol;
		if (T.Row > 0) and (T.Row <= M.LineCount) then
		begin
			S := M.Lines[T.Row - 1];
			StPos := T.Col;
			l := Length(S);
			if RemoveSpace and (StPos > 1) and (StPos <= l) then
			begin
				if S[StPos - 1] = ' ' then
					Dec(StPos);
				while (StPos > 1) and (S[StPos] in ['(', ' ']) do
					Dec(StPos);
			end;
			DelimitateWord(S, StPos, EndPos);
			if Full then
				Result := Trim(Copy(S, StPos, EndPos - StPos + 1))
			else
				Result := Trim(Copy(S, StPos, T.Col - StPos));
		end;
	end;
end;

procedure TCompForm.SetWord(const Value: string);
var
	M: TMemoComponent;
begin
	M := Editor;
	if Assigned(M) then
	begin
		with M.Selection do
		begin
			SelectWord;
			Text := Value;
		end;
	end;
end;

// Editor Events

procedure TCompForm.Editor_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	procedure Select(Direction, Page: Boolean);
	var
		HowMany, i, c: Integer;
	begin
		i := CompList.ItemIndex;
		c := List.Count;
		if Page then
			HowMany := CompList.Height div CompList.ItemHeight
		else
			HowMany := 1;
		if Direction then
			Inc(i, HowMany)
		else
			Dec(i, HowMany);
		if i <= 0 then
			CompList.ItemIndex := 0
		else if i >= c then
			CompList.ItemIndex := c - 1
		else
			CompList.ItemIndex := i;
	end;
var
	NewKey: Word;
begin
	if Assigned(OldKeyDown) then
		OldKeyDown(Sender, Key, Shift);
	if Visible then
	begin
		NewKey := 0;
		case Key of
			VK_ESCAPE: Close;
			VK_RETURN:
				begin
					ApplyCompletion;
					StopKey := True;
				end;
			VK_UP: Select(False, False);
			VK_DOWN: Select(True, False);
			VK_PRIOR: Select(False, True);
			VK_NEXT: Select(True, True);
			VK_HOME: CompList.ItemIndex := 0;
			VK_END: CompList.ItemIndex := List.Count - 1;
			VK_LEFT, VK_RIGHT:
				begin
					Editor_Change(Sender);
					NewKey := Key;
				end;
		else
			NewKey := Key;
		end;
		Key := NewKey;
	end
	else if (ssCtrl in Shift) and (Key = VK_SPACE) then
	begin
		Key := 0;
		StopKey := True;
		ShowCompletion;
	end
	else if (ssCtrl in Shift) and (Key = Ord('J')) then
	begin
		Key := 0;
		StopKey := True;
		if TemplateForm.Templates.Count > 0 then
			TemplateForm.Show
		else
			ShowMessage(AddTemplatesMsg);
	end
	else if (Key in [VK_BACK, VK_ESCAPE]) then // or not (Key in [Ord('A')..Ord('Z'), Ord('0')..Ord('9')
		CloseSymbolInfo;
end;

procedure TCompForm.Editor_KeyPress(Sender: TObject; var Key: Char);
begin
	if Assigned(OldKeyPress) then
		OldKeyPress(Sender, Key);
	if StopKey then
	begin
		Key := #0;
		StopKey := False;
	end
	else
	begin
		case Key of
			'(': if Visible or not IsWindowVisible(HintWindow.Handle) then ShowSymbolInfo;
			')': CloseSymbolInfo;
		end;
	end;
end;

procedure TCompForm.Editor_Change(Sender: TObject);
var
	i: Integer;
	M: TMemoComponent;
begin
	if Assigned(OldChange) then
		OldChange(Sender);
	M := Editor;
	if Visible then
	begin
		if Assigned(M) then
		begin
			if CompRow <> M.Selection.StartRowCol.Row then
				CloseCompletion
			else
			begin
				List.Find(GetWord(False, False), i);
				CompList.ItemIndex := i;
			end;
		end
		else
			CloseCompletion;
	end
	else if Assigned(M) and (CompRow <> M.Selection.StartRowCol.Row) then
		CloseSymbolInfo;
end;

procedure TCompForm.Editor_Enter(Sender: TMemoComponent);
begin
	if Assigned(Sender) then
	begin
		FEditor := Sender;

		CloseCompletion;
		CloseSymbolInfo;

		OldKeyDown := Editor.OnKeyDown;
		OldKeyPress := Editor.OnKeyPress;
		OldChange := Editor.OnChange;

		Editor.OnKeyDown := Editor_KeyDown;
		Editor.OnKeyPress := Editor_KeyPress;
		Editor.OnChange := Editor_Change;
		Editor.OnSelectionChange := Editor_Change;
	end
	else
	begin
		FEditor := nil;
		OldKeyDown := nil;
		OldKeyPress := nil;
		OldChange := nil;
	end;
end;

procedure TCompForm.Editor_Exit(Sender: TObject);
begin
	if Assigned(Editor) then
	begin
		Editor.OnKeyDown := OldKeyDown;
		Editor.OnKeyPress := OldKeyPress;
		Editor.OnChange := OldChange;
		Editor.OnSelectionChange := OldChange;
	end;
	CloseSymbolInfo;
	if not Focused then
	begin
		FEditor := nil;
		CloseCompletion;
	end;
end;

procedure TCompForm.CompListKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
var
	M: TMemoComponent;
begin
	M := Editor;
	if Assigned(M) then
	begin
		M.HandleKeyDown(Key, Shift);
		M.SetFocus;
		Key := 0;
	end
	else
		CloseCompletion;
end;

procedure TCompForm.CompListKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
var
	M: TMemoComponent;
begin
	M := Editor;
	if Assigned(M) then
	begin
		M.HandleKeyUp(Key, Shift);
		M.SetFocus;
		Key := 0;
	end
	else
		CloseCompletion;
end;

procedure TCompForm.CompListKeyPress(Sender: TObject; var Key: Char);
var
	M: TMemoComponent;
begin
	M := Editor;
	if Assigned(M) then
	begin
		M.HandleKeyPress(Key);
		M.SetFocus;
	end;
end;

procedure TCompForm.FindSymbolDecl;
var
	i, p: Integer;
	Symbol, S: string;
	C: TCompletionItem;
	SourceFile: TSourceFile;
begin
	RebuildList;
	Symbol := GetWord(True, True);
	if (Symbol <> '') and List.Find(Symbol, i) then
	begin
		C := List.Completion[i];
		if Assigned(C) and (C.SourceFileName <> '') then
		begin
			SourceFile := MainForm.SourceFiles.FindFileNameOnly(C.SourceFileName);
			if SourceFile = nil then
			begin
				if FileExists(ExpandFileName(C.SourceFileName)) then
					SourceFile := MainForm.AddSourceFile(ExpandFileName(C.SourceFileName), True)
				else if FileExists(WithBackslash(TIGCCFolder) + CIncludeLocation + C.SourceFileName) then
					SourceFile := MainForm.AddSourceFile(WithBackslash(TIGCCFolder) + CIncludeLocation + C.SourceFileName, True)
				else if FileExists(WithBackslash(TIGCCFolder) + ASMIncludeLocation + C.SourceFileName) then
					SourceFile := MainForm.AddSourceFile(WithBackslash(TIGCCFolder) + ASMIncludeLocation + C.SourceFileName, True)
			end;
			if Assigned(SourceFile) and (SourceFile is TTextSourceFile) then
				with TTextSourceFile(SourceFile).TextEditor do
				begin
					if (C.Line < Cardinal(Lines.Count)) then
					begin
						S := Lines[C.Line];
						p := Pos(Symbol, S);
						if p = 0 then
							p := 1;
					end
					else
						p := 1;
					Selection.StartRowCol := TextCell(C.Line + 1, p);
					Selection.RLength := 0;
					Selection.ScrollInView(4);
				end
			else
				ShowMessageFmt(SymbolFileNotFound, [Symbol, C.SourceFileName]);
		end;
	end;
end;

procedure TCompForm.ActiveControlChange(Sender: TObject);
var
	W: TWinControl;
begin
	W := Screen.ActiveControl;
	if (W = Editor) or (W = Self) or (W = CompList) or (W = TemplateForm) or (W = TemplateForm.TmpltList) then
		Exit;
	if Assigned(W) and (W is TMemoComponent) then
		Editor := TMemoComponent(W)
	else
		Editor := nil;
	if Assigned(OldActiveControlChange) then
		OldActiveControlChange(Sender);
end;

procedure TCompForm.SetEditor(const Value: TMemoComponent);
begin
	if Value <> FEditor then
	begin
		Editor_Exit(Self);
		Editor_Enter(Value);
		FEditor := Value;
	end;
end;

end.

