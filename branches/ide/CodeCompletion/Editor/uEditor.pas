{
  TIGCC IDE

  Copyright (C) 2004 Fréderic Bour

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

unit uEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Grids, ValEdit, Menus, FileCtrl,
  ComCtrls, CodeCompletion, HtFormatting;

const
  DefautCaption = 'Completion Editor';
  Cell_Left = ' Left';
  Cell_Right = ' Right';
  Cell_Name = ' Name';
  Cell_Desc = ' Description';
  Cell_Line = ' Line';
  SaveModif = 'Save modifications ?';
  SaveError = 'Error %s:'#13#10'%s';
  PreviewLBl = 'Preview:';
  BrowseHSF = 'Select Directory where HSF files are stored';
  LoadingError = 'Error while loading! (%s error: %s)';

type
  TCEditorForm = class(TForm)
    MenuPnl: TPanel;
    NewBtn: TSpeedButton;
    SaveBtn: TSpeedButton;
    OpenBtn: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolsBtn: TSpeedButton;
    ToolsMenu: TPopupMenu;
    ParseHSF: TMenuItem;
    MainPageControl: TPageControl;
    InfoTab: TTabSheet;
    ItemsTab: TTabSheet;
    ItemsLbl: TLabel;
    ItemsList: TListBox;
    ItemBox: TGroupBox;
    ItemEditor: TValueListEditor;
    ApplyBtn: TButton;
    UNameLbl: TLabel;
    UNameEdit: TEdit;
    IncludeLbl: TLabel;
    IncludeMemo: TMemo;
    DelBtn: TButton;
    ParseHeader: TMenuItem;
    OpenHeaderDialog: TOpenDialog;
    N1: TMenuItem;
    Batch1: TMenuItem;
    PreviewBox: TPaintBox;
    procedure NewBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure ItemsListClick(Sender: TObject);
    procedure ItemEditorStringsChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ToolsBtnClick(Sender: TObject);
    procedure ParseHSFClick(Sender: TObject);
    procedure ParseHeaderClick(Sender: TObject);
    procedure PreviewBoxPaint(Sender: TObject);
    procedure ItemsListData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure Batch1Click(Sender: TObject);
    procedure SetModified(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    Filename: string;
    Locked, Modified: Boolean;
    Lst, LastHeader: TStringList;
    HSFDir: string;
    procedure UpdatePreview;
    procedure SetItem(const Name, Left, Right, Description: string; Line: Cardinal);

    // Stream methods
    procedure Clear;
    procedure LoadFromStream(F: TStream);
    procedure SaveToStream(F: TStream);

    // Tools Method
    function ParseH(const FileName: string): Boolean;

    // Add Symbol Line
    function GetLine(const Symbol: string): Integer;
  end;

var
  CEditorForm: TCEditorForm;

procedure ClearCompletion(Lst: TStrings);

implementation

{$R *.dfm}

uses uHSFParser, Math, uBatch;

procedure ClearCompletion(Lst: TStrings);
var
  i: Integer;
begin
  for i := 0 to Lst.Count - 1 do
    Lst.Objects[i].Free;
  Lst.Clear;
end;

procedure TCEditorForm.UpdatePreview;
begin
  PreviewBox.Repaint;
end;

procedure TCEditorForm.NewBtnClick(Sender: TObject);
begin
  Clear;
end;

procedure TCEditorForm.FormCreate(Sender: TObject);
begin
  LastHeader := TStringList.Create;
  Lst := TStringList.Create;
  Lst.Sorted := True;
  ItemEditor.ColWidths[0] := 96;
  ItemEditor.Strings.Clear;
  ItemEditor.Strings.Add(Cell_Name + '=');
  ItemEditor.Strings.Add(Cell_Left + '=');
  ItemEditor.Strings.Add(Cell_Right + '=');
  ItemEditor.Strings.Add(Cell_Desc + '=');
  ItemEditor.Strings.Add(Cell_Line + '=');
  Modified := False;
  Clear;
end;

procedure TCEditorForm.FormDestroy(Sender: TObject);
begin
  ClearCompletion(Lst);
  Lst.Free;
  LastHeader.Free;
end;

procedure TCEditorForm.ApplyBtnClick(Sender: TObject);
begin
  SetItem(ItemEditor.Values[Cell_Name],
    ItemEditor.Values[Cell_Left],
    ItemEditor.Values[Cell_Right],
    ItemEditor.Values[Cell_Desc],
    StrToIntDef(ItemEditor.Values[Cell_Line], 0));
end;

procedure TCEditorForm.OpenBtnClick(Sender: TObject);
var
  F: TFileStream;
begin
  if CloseQuery and OpenDialog.Execute then
  try
    Lst.Clear;
    IncludeMemo.Clear;
    F := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    try
      LoadFromStream(F);
      Filename := OpenDialog.FileName;
      Caption := DefautCaption + ' - ' + ExtractFileName(Filename);
    finally
      F.Free;
    end;
  except
    on E: Exception do
      ShowMessageFmt(LoadingError, [E.ClassName, E.Message]);
  end;
  ItemsList.Count := Lst.Count;
end;

procedure TCEditorForm.SaveBtnClick(Sender: TObject);
var
  F: TFileStream;
begin
  if FileExists(Filename) or SaveDialog.Execute then
  try
    F := TFileStream.Create(SaveDialog.FileName, fmCreate);
    SaveToStream(F);
    F.Free;
    Filename := SaveDialog.FileName;
    Caption := DefautCaption + ' - ' + ExtractFileName(Filename);
    Modified := False;
  except
    on E: Exception do
      ShowMessageFmt(SaveError, [E.ClassName, E.Message]);
  end;
end;

procedure TCEditorForm.DelBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := Lst.Count - 1 downto 0 do
    if ItemsList.Selected[i] then
    begin
      Lst.Objects[i].Free;
      Lst.Delete(i);
    end;
  ItemsList.Count := Lst.Count;
end;

procedure TCEditorForm.ItemsListClick(Sender: TObject);
var
  i: Integer;
  O: TCompletionItem;
begin
  if ItemsList.SelCount > 0 then
  begin
    i := ItemsList.ItemIndex;
    ItemEditor.Strings.BeginUpdate;
    ItemEditor.Values[Cell_Name] := Lst[i];
    O := TCompletionItem(Lst.Objects[i]);
    if Assigned(O) then
    begin
      ItemEditor.Values[Cell_Left] := O.Left;
      ItemEditor.Values[Cell_Right] := O.Right;
      ItemEditor.Values[Cell_Desc] := O.Description;
      ItemEditor.Values[Cell_Line] := IntToStr(O.Line);
    end;
    ItemEditor.Strings.EndUpdate;
    UpdatePreview;
  end;
end;

procedure TCEditorForm.ItemEditorStringsChange(Sender: TObject);
begin
  UpdatePreview;
  SetModified(Sender);
end;

procedure TCEditorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if Modified then
    case MessageDlg(SaveModif, mtConfirmation, mbYesNoCancel, 0) of
      mrYes: SaveBtn.Click;
      mrNo: ;
    else
      CanClose := False;
    end;
end;

procedure TCEditorForm.ToolsBtnClick(Sender: TObject);
var
  P: TPoint;
begin
  P := Point(0, ToolsBtn.Height);
  P := ToolsBtn.ClientToScreen(P);
  ToolsMenu.Popup(P.X, P.Y);
end;

procedure TCEditorForm.ParseHeaderClick(Sender: TObject);
begin
  if OpenHeaderDialog.Execute then
    ParseH(OpenHeaderDialog.FileName);
end;

procedure TCEditorForm.ParseHSFClick(Sender: TObject);
begin
  if SelectDirectory(BrowseHSF, '', HSFDir) then
  begin
    Locked := True;
    Lst.BeginUpdate;
    ImportDir(HSFDir, Self, GetLine);
    Lst.EndUpdate;
    Locked := False;
    ItemsList.Count := Lst.Count;
  end;
end;

procedure TCEditorForm.SetItem(const Name, Left, Right, Description: string; Line: Cardinal);
var
  i: Integer;
  O: TCompletionItem;
begin
  if (Name <> '') then
  begin
    i := Lst.IndexOf(Name);
    if i = -1 then
    begin
      O := TCompletionItem.Create;
      i := Lst.AddObject(Name, O);
    end
    else
      O := TCompletionItem(Lst.Objects[i]);
    O.Left := Left;
    O.Right := Right;
    O.Description := Description;
    O.Line := Line;
    if not Locked then
    begin
      ItemsList.Count := Lst.Count;
      ItemsList.ItemIndex := i;
    end;
    Modified := True;
  end;
end;

procedure TCEditorForm.LoadFromStream(F: TStream);
var
  S: string;
  i, W: Word;
  C: TCompletionItem;
  B: Byte;
begin
  if (F.Read(B, 1) = 1) and (B = CCFVersion) and ReadString(F, S) then
  begin
    // Set Unit Name
    UNameEdit.Text := S;
    if F.Read(W, 2) = 2 then
    begin
      // Load Includes List
      IncludeMemo.Lines.Clear;
      for i := 1 to W do
      begin
        if ReadString(F, S) then
          IncludeMemo.Lines.Add(S)
        else
          Break;
      end;
      // Load Symbols List
      while ReadString(F, S) do
      begin
        C := TCompletionItem.Create;
        C.ReadFromStream(F);
        Lst.AddObject(S, C);
      end;
    end;
    ItemsList.Count := Lst.Count;
  end;
end;

procedure TCEditorForm.PreviewBoxPaint(Sender: TObject);
var
  H, W: Integer;
  C: TCanvas;
  R: TRect;
begin
  C := PreviewBox.Canvas;
  R := PreviewBox.ClientRect;
  H := C.TextHeight(PreviewLbl) + 2;

  try
    C.FillRect(R);
    // Draw Preview
    C.TextRect(R, R.Left, R.Top, PreviewLbl);
    Inc(R.Top, H);

    // Draw Left
    DrawHtText(ItemEditor.Values[Cell_Left], R, C, W, False);
    Inc(R.Top, H);

    // Draw Name
    C.Font.Style := [fsBold];
    C.TextRect(R, R.Left, R.Top, ItemEditor.Values[Cell_Name]);
    C.Font.Style := [];
    Inc(R.Top, H);

    // Draw Right
    DrawHtText(ItemEditor.Values[Cell_Right], R, C, W, False);
    Inc(R.Top, H);

    // Draw Description
    C.TextRect(R, R.Left, R.Top, ItemEditor.Values[Cell_Desc]);
    Inc(R.Top, H);

  except
    C.Font.Style := [];
  end;
end;

procedure TCEditorForm.SaveToStream(F: TStream);
var
  i: Integer;
  W: Word;
begin
  try
    // Write CCF Version
    F.Write(CCFVersion, SizeOf(CCFVersion));
    // Write Unit Name
    WriteString(F, UNameEdit.Text);
    // Write Included Units
    W := IncludeMemo.Lines.Count;
    if F.Write(W, 2) = 2 then
      for i := 0 to W - 1 do
        if not WriteString(F, Trim(IncludeMemo.Lines[i])) then
          Break;
    // Write Symbols
    for i := 0 to Lst.Count - 1 do
    begin
      if WriteString(F, Lst[i]) then
        TCompletionItem(Lst.Objects[i]).WriteToStream(F)
      else
        Break;
    end;
  except
  end;
end;

function TCEditorForm.ParseH(const FileName: string): Boolean;
var
  Src: TStringList;
  S: TStream;
begin
  Src := TStringList.Create;
  try
    try
      Src.LoadFromFile(FileName);
      S := TStringStream.Create(MakeCCF_H(ExtractFileName(FileName), Src, LastHeader));
      try
        LoadFromStream(S);
      finally
        S.Free;
      end;
    finally
      Src.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TCEditorForm.Clear;
begin
  if CloseQuery then
  begin
    LastHeader.Clear;
    Caption := DefautCaption;
    Filename := '';
    UNameEdit.Text := '';
    IncludeMemo.Text := '';
    ClearCompletion(Lst);
    ItemsList.Count := 0;
  end;
end;

procedure TCEditorForm.ItemsListData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  if (Index >= 0) and (Index < Lst.Count) then
    Data := Lst[Index]
  else
    ItemsList.Count := Lst.Count;
end;

procedure TCEditorForm.Batch1Click(Sender: TObject);
var
  B: TBatchEdit;
begin
  Locked := True;
  B := TBatchEdit.Create(Self);
  B.Editor := Self;
  B.ShowModal;
  B.Free;
  Locked := False;
end;

procedure TCEditorForm.SetModified(Sender: TObject);
begin
  Modified := True;
end;

function TCEditorForm.GetLine(const Symbol: string): Integer;
var
  i, p: Integer;
  S: string;
begin
  for i := 0 to LastHeader.Count - 1 do
  begin
    S := LastHeader[i];
    p := Pos(Symbol, S);
    if not ((p = 0) or ((p > 1) and (S[p - 1] in ['a'..'z', 'A'..'Z', '0'..'9', '$', '_']))) then
      if GetIdent(S, p) = Symbol then
      begin
        Result := i;
        Exit;
      end;
  end;
  Result := 0;
end;

end.
