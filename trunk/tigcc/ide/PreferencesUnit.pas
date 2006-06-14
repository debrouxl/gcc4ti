unit PreferencesUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, SourceEditUnit{$IFDEF CODINGEXT}, PrefFrame{$ENDIF};

type
  TPreferencesForm = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
		ColorDlg: TColorDialog;
    FontDlg: TFontDialog;
    PageController: TPageControl;
		GeneralSheet: TTabSheet;
    StopCompilationBox: TCheckBox;
		FlatButtonsCheckBox: TCheckBox;
    MenuBitmapsCheckBox: TCheckBox;
    AutoSaveCheckBox: TCheckBox;
    EditorSheet: TTabSheet;
		Label2: TLabel;
		CTabSizeEdit: TEdit;
    ColorCheckBox: TCheckBox;
    ChangeColorButton: TButton;
		Label3: TLabel;
		ChangeFontButton: TButton;
    FontLabel: TPanel;
    SyntaxHighlightingSheet: TTabSheet;
    Label4: TLabel;
		ASMTabSizeEdit: TEdit;
    LanguageSelectionBox: TComboBox;
    Label5: TLabel;
    SyntaxEnabledBox: TCheckBox;
		Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
		Button6: TButton;
    ObjectTree: TTreeView;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    OKTimer: TTimer;
		OpenFolderBox: TCheckBox;
		DeleteObjectFilesBox: TCheckBox;
    ResetButton: TButton;
    OnFlyCheckBox: TCheckBox;
    DeleteErrorsCheckBox: TCheckBox;
    AutoBlockCheckBox: TCheckBox;
    SplitFilesCheckBox: TCheckBox;
    DeleteAssemblyFilesBox: TCheckBox;
    AllowImplicitCheckBox: TCheckBox;
    AutoNewsCheckBox: TCheckBox;
    TransferSheet: TTabSheet;
    TargetBox: TGroupBox;
    VTIBox: TRadioButton;
    RealCalcBox: TRadioButton;
    PortBox: TGroupBox;
    PortCOM1Box: TRadioButton;
    PortCOM2Box: TRadioButton;
    PortCOM3Box: TRadioButton;
    PortCOM4Box: TRadioButton;
    CableBox: TGroupBox;
    CableBlackBox: TRadioButton;
    CableGrayBox: TRadioButton;
    NoneBox: TRadioButton;
    VTIPathEdit: TEdit;
    VTIPathBrowseButton: TButton;
    BrowseDlg: TOpenDialog;
    DragDropEditCheckBox: TCheckBox;
    RemoveTrailingSpcCheckBox: TCheckBox;
    JumpToErrorBox: TCheckBox;
		procedure ColorCheckBoxClick(Sender: TObject);
		procedure ChangeColorButtonClick(Sender: TObject);
		procedure ChangeFontButtonClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure LanguageSelectionBoxChange(Sender: TObject);
		procedure ObjectTreeChange(Sender: TObject; Node: TTreeNode);
		procedure SyntaxEnabledBoxClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ObjectTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
		procedure EditItem(Sender: TObject);
		procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure ObjectTreeEdited(Sender: TObject; Node: TTreeNode;
			var S: String);
		procedure ObjectTreeEditing(Sender: TObject; Node: TTreeNode;
			var AllowEdit: Boolean);
		procedure OKTimerTimer(Sender: TObject);
		procedure ResetButtonClick(Sender: TObject);
		procedure PageControllerChange(Sender: TObject);
    procedure TargetBoxClick(Sender: TObject);
    procedure VTIPathBrowseButtonClick(Sender: TObject);
	private
	public
		BackColor: TColor;
		EditorFont: TFont;
		SyntaxC: TSyntaxColoring;
		SyntaxAsmGNU: TSyntaxColoring;
		SyntaxAsm: TSyntaxColoring;
		SyntaxQuill: TSyntaxColoring;
		Syntax: TSyntaxColoring;
		SyntaxTabClicked: Boolean;

{$IFDEF CODINGEXT}
    procedure InitCodingExt;
{$ENDIF}
	end;

implementation

{$R *.DFM}

uses
	MasterUnit,
	UtilsDos, UtilsWin,
	MainUnit, StyleSelectionUnit, ColorsUnit, CustomStyleUnit, WordListUnit;

procedure TPreferencesForm.ColorCheckBoxClick(Sender: TObject);
begin
	ChangeColorButton.Enabled := ColorCheckBox.Checked;
end;

procedure TPreferencesForm.ChangeColorButtonClick(Sender: TObject);
begin
	with ColorDlg do begin
		Color := BackColor;
		if Execute then
			BackColor := Color;
	end;
end;

procedure TPreferencesForm.ChangeFontButtonClick(Sender: TObject);
begin
	with FontDlg do begin
		Font.Assign (EditorFont);
		if Execute then begin
			EditorFont.Assign (Font);
			FontLabel.Caption := EditorFont.Name;
			FontLabel.Font.Assign (EditorFont);
		end;
	end;
end;

procedure TPreferencesForm.FormCreate(Sender: TObject);
begin
	EditorFont := TFont.Create;
	SyntaxC := TSyntaxColoring.Create (nil);
	SyntaxAsm := TSyntaxColoring.Create (nil);
	SyntaxAsmGNU := TSyntaxColoring.Create (nil);
	SyntaxQuill := TSyntaxColoring.Create (nil);
	if ssA68k in SpecialSupport then
		LanguageSelectionBox.Items.Add ('A68k ASM Files');
	if ssQuill in SpecialSupport then
		LanguageSelectionBox.Items.Add ('Quill Files');
	LanguageSelectionBox.ItemIndex := 0;

{$IFDEF CODINGEXT}
  InitCodingExt;
{$ENDIF}
end;

procedure TPreferencesForm.FormDestroy(Sender: TObject);
begin
	SyntaxQuill.Free;
	SyntaxAsmGNU.Free;
	SyntaxAsm.Free;
	SyntaxC.Free;
	EditorFont.Free;
end;

procedure TPreferencesForm.FormShow(Sender: TObject);
begin
	SyntaxTabClicked := PageController.ActivePage = SyntaxHighlightingSheet;
	TargetBoxClick (Sender);
	ColorCheckBoxClick (Sender);
	FontLabel.Caption := EditorFont.Name;
	FontLabel.Font.Assign (EditorFont);
	LanguageSelectionBoxChange (Sender);
end;

procedure TPreferencesForm.LanguageSelectionBoxChange(Sender: TObject);
label
	Done;
var
	I: Integer;
begin
	I := LanguageSelectionBox.ItemIndex;
	if I <= 0 then begin
		Syntax := SyntaxC;
		goto Done;
	end;
	Dec (I);
	if I <= 0 then begin
		Syntax := SyntaxAsmGNU;
		goto Done;
	end;
	if ssA68k in SpecialSupport then begin
		Dec (I);
		if I <= 0 then begin
			Syntax := SyntaxAsm;
			goto Done;
		end;
	end;
	if ssQuill in SpecialSupport then begin
		Dec (I);
		if I <= 0 then begin
			Syntax := SyntaxQuill;
			goto Done;
		end;
	end;
	Done:
	SyntaxEnabledBox.Checked := Syntax.Enabled;
	SyntaxEnabledBoxClick (Sender);
	ObjectTree.Items.BeginUpdate;
	with ObjectTree.Items [0] do begin
		Item[0].DeleteChildren;
		Item[1].DeleteChildren;
	end;
	with Syntax do begin
		with CustomStyles do
			for I := 0 to Count - 1 do
				ObjectTree.Items.AddChildObject (ObjectTree.Items[0].Item[0], Items[I].Caption, Pointer (Items [I]));
		with WordLists do
			for I := 0 to Count - 1 do
				ObjectTree.Items.AddChildObject (ObjectTree.Items[0].Item[1], Items[I].Caption, Pointer (Items [I]));
	end;
	ObjectTree.FullExpand;
	ObjectTree.Items.EndUpdate;
	ObjectTreeChange (Sender, nil);
end;

procedure TPreferencesForm.ObjectTreeChange(Sender: TObject;
	Node: TTreeNode);
begin
	Button9.Enabled := SyntaxEnabledBox.Checked and Assigned (ObjectTree.Selected) and Assigned (ObjectTree.Selected.Data);
end;

procedure TPreferencesForm.SyntaxEnabledBoxClick(Sender: TObject);
begin
	Syntax.Enabled := SyntaxEnabledBox.Checked;
	Button1.Enabled := SyntaxEnabledBox.Checked;
	Button2.Enabled := SyntaxEnabledBox.Checked;
	Button3.Enabled := SyntaxEnabledBox.Checked;
	Button4.Enabled := SyntaxEnabledBox.Checked;
	Button5.Enabled := SyntaxEnabledBox.Checked;
	Button6.Enabled := SyntaxEnabledBox.Checked;
	Button7.Enabled := SyntaxEnabledBox.Checked;
	Button8.Enabled := SyntaxEnabledBox.Checked;
	ObjectTree.Enabled := SyntaxEnabledBox.Checked;
	ObjectTreeChange (Sender, nil);
end;

procedure TPreferencesForm.Button1Click(Sender: TObject);
begin
	ColorDlg.Color := Syntax.NumberColor;
	if ColorDlg.Execute then
		Syntax.NumberColor := ColorDlg.Color;
end;

procedure TPreferencesForm.Button3Click(Sender: TObject);
begin
	ColorDlg.Color := Syntax.SymbolColor;
	if ColorDlg.Execute then
		Syntax.SymbolColor := ColorDlg.Color;
end;

procedure TPreferencesForm.Button2Click(Sender: TObject);
begin
	with TStyleSelectionForm.Create (Self) do try
		CustomStyle := Syntax.NumberCustomStyle;
		Style := Syntax.NumberStyle;
		if ShowModal = mrOK then begin
			Syntax.NumberCustomStyle := CustomStyle;
			Syntax.NumberStyle := Style;
		end;
	finally
		Free;
	end;
end;

procedure TPreferencesForm.Button4Click(Sender: TObject);
begin
	with TStyleSelectionForm.Create (Self) do try
		CustomStyle := Syntax.SymbolCustomStyle;
		Style := Syntax.SymbolStyle;
		if ShowModal = mrOK then begin
			Syntax.SymbolCustomStyle := CustomStyle;
			Syntax.SymbolStyle := Style;
		end;
	finally
		Free;
	end;
end;

procedure TPreferencesForm.Button6Click(Sender: TObject);
begin
	with TStyleSelectionForm.Create (Self) do try
		CustomStyle := Syntax.ParenthesisCustomStyle;
		Style := Syntax.ParenthesisStyle;
		if ShowModal = mrOK then begin
			Syntax.ParenthesisCustomStyle := CustomStyle;
			Syntax.ParenthesisStyle := Style;
		end;
	finally
		Free;
	end;
end;

procedure TPreferencesForm.Button5Click(Sender: TObject);
begin
	with TMultipleColorsForm.Create (Self) do try
		ColorBox.Items.Assign (Syntax.ParenthesisColors);
		if ShowModal = mrOK then
			Syntax.ParenthesisColors.Assign (ColorBox.Items);
	finally
		Free;
	end;
end;

procedure TPreferencesForm.ObjectTreeKeyDown(Sender: TObject;
	var Key: Word; Shift: TShiftState);
var
	ItemData: TCollectionItem;
begin
	if Key = vk_Delete then begin
		if Assigned (ObjectTree.Selected) and Assigned (ObjectTree.Selected.Data) then begin
			ItemData := TCollectionItem (ObjectTree.Selected.Data);
			ObjectTree.Selected.Delete;
			ItemData.Free;
		end;
	end;
end;

procedure TPreferencesForm.EditItem(Sender: TObject);
var
	ItemData: TCollectionItem;
begin
	if Assigned (ObjectTree.Selected) and Assigned (ObjectTree.Selected.Data) then begin
		ItemData := TCollectionItem (ObjectTree.Selected.Data);
		if ItemData is TCustomStyle then
			with TCustomStyleForm.Create (Self) do try
				Style.Assign (ItemData);
				if ShowModal = mrOK then
					ItemData.Assign (Style);
			finally
				Free;
			end
		else if ItemData is TWordList then
			with TWordListForm.Create (Self) do try
				List.Assign (ItemData);
				if ShowModal = mrOK then
					ItemData.Assign (List);
			finally
				Free;
			end;
	end;
end;

procedure TPreferencesForm.Button7Click(Sender: TObject);
var
	ItemData: TCustomStyle;
begin
	if ObjectTree.Enabled then
		ObjectTree.SetFocus;
	ItemData := Syntax.CustomStyles.Add;
	ItemData.Caption := 'New Style';
	with ObjectTree.Items.AddChildObject (ObjectTree.Items[0].Item[0], ItemData.Caption, Pointer (ItemData)) do begin
		MakeVisible;
		EditText;
	end;
end;

procedure TPreferencesForm.Button8Click(Sender: TObject);
var
	ItemData: TWordList;
begin
	if ObjectTree.Enabled then
		ObjectTree.SetFocus;
	ItemData := Syntax.WordLists.Add;
	ItemData.Caption := 'New List';
	with ObjectTree.Items.AddChildObject (ObjectTree.Items[0].Item[1], ItemData.Caption, Pointer (ItemData)) do begin
		MakeVisible;
		EditText;
	end;
end;

procedure TPreferencesForm.ObjectTreeEdited(Sender: TObject;
	Node: TTreeNode; var S: String);
var
	ItemData: TCollectionItem;
begin
	OKButton.Enabled := True;
	if Assigned (Node.Data) then begin
		ItemData := TCollectionItem (Node.Data);
		if ItemData is TCustomStyle then
			TCustomStyle(ItemData).Caption := S;
		if ItemData is TWordList then
			TWordList(ItemData).Caption := S;
	end;
	ObjectTree.Selected := Node;
end;

procedure TPreferencesForm.ObjectTreeEditing(Sender: TObject;
	Node: TTreeNode; var AllowEdit: Boolean);
begin
	AllowEdit := Assigned (Node.Data);
	if AllowEdit then begin
		OKButton.Enabled := False;
		OKTimer.Enabled := True;
	end;
end;

procedure TPreferencesForm.OKTimerTimer(Sender: TObject);
begin
	if not ObjectTree.IsEditing then begin
		OKButton.Enabled := True;
		OKTimer.Enabled := False;
	end;
end;

procedure TPreferencesForm.ResetButtonClick(Sender: TObject);
begin
	if Syntax = SyntaxC then
		Syntax.Assign (MainForm.SyntaxCBackup)
	else if Syntax = SyntaxAsmGNU then
		Syntax.Assign (MainForm.SyntaxAsmGNUBackup)
	else if Syntax = SyntaxAsm then
		Syntax.Assign (MainForm.SyntaxAsmBackup)
	else if Syntax = SyntaxQuill then
		Syntax.Assign (MainForm.SyntaxQuillBackup);
	LanguageSelectionBoxChange (Sender);
end;

procedure TPreferencesForm.PageControllerChange(Sender: TObject);
begin
	if PageController.ActivePage = SyntaxHighlightingSheet then
		SyntaxTabClicked := True;
end;

procedure TPreferencesForm.TargetBoxClick(Sender: TObject);
begin
	VTIPathEdit.Enabled := VTIBox.Checked;
	VTIPathBrowseButton.Enabled := VTIPathEdit.Enabled;
	PortBox.Enabled := RealCalcBox.Checked;
	PortCOM1Box.Enabled := PortBox.Enabled;
	PortCOM2Box.Enabled := PortBox.Enabled;
	PortCOM3Box.Enabled := PortBox.Enabled;
	PortCOM4Box.Enabled := PortBox.Enabled;
	CableBox.Enabled := PortBox.Enabled;
	CableBlackBox.Enabled := CableBox.Enabled;
	CableGrayBox.Enabled := CableBox.Enabled;
end;

procedure TPreferencesForm.VTIPathBrowseButtonClick(Sender: TObject);
begin
	with BrowseDlg do begin
		FileName := VTIPathEdit.Text;
		if Execute then
			VTIPathEdit.Text := FileName;
	end;
end;

{$IFDEF CODINGEXT}
procedure TPreferencesForm.InitCodingExt;
var
  Tab: TTabSheet;
  Frame: TCodingExt;
begin
  Tab := TTabSheet.Create(Self);
  Tab.PageControl := PageController;
  Tab.Caption := 'Co&ding';

  Frame := TCodingExt.Create(Self);
  Frame.Parent := Tab;
  Tab.OnShow := Frame.OnShow;
end;
{$ENDIF}

end.

