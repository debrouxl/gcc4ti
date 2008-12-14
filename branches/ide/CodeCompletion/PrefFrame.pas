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

unit PrefFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TmpltForm;

type
  TCodingExt = class(TFrame)
    TemplateBox: TGroupBox;
    CompBox: TGroupBox;
    CompEditor: TButton;
    TmpltList: TListBox;
    IdLbl: TLabel;
    CodeLbl: TLabel;
    IdEdit: TEdit;
    CodeMemo: TMemo;
    ClearBtn: TButton;
    ApplyBtn: TButton;
    procedure CompEditorClick(Sender: TObject);
    procedure TmpltListData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure ApplyBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure TmpltListClick(Sender: TObject);
    procedure OnShow(Sender: TObject);
  public
    { Déclarations publiques }

  end;

implementation

{$R *.dfm}

uses uEditor;

procedure TCodingExt.CompEditorClick(Sender: TObject);
var
  Form: TCEditorForm;
begin
  Form := TCEditorForm.Create(Self);
  Form.ShowModal;
  Form.Free;
end;

procedure TCodingExt.TmpltListData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  if (Index >= 0) and (Index < TemplateForm.Templates.Count) then
    Data := TemplateForm.Templates[Index];
end;

procedure TCodingExt.ApplyBtnClick(Sender: TObject);
var
  Id: string;
  T: TTemplate;
  i: Integer;
begin
  Id := Trim(IdEdit.Text);
  if Id <> '' then
  begin
    if TemplateForm.Templates.Find(Id, i) then
      TTemplate(TemplateForm.Templates.Objects[i]).Text := CodeMemo.Text
    else
    begin
      T := TTemplate.Create;
      T.Text := CodeMemo.Text;
      TemplateForm.Templates.AddObject(Id, T);
    end;
  end;
  TmpltList.Count := TemplateForm.Templates.Count;
end;

procedure TCodingExt.ClearBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i := TemplateForm.Templates.Count - 1 downto 0 do
    if TmpltList.Selected[i] then
    begin
      TemplateForm.Templates.Objects[i].Free;
      TemplateForm.Templates.Delete(i);
    end;
  TmpltList.Count := TemplateForm.Templates.Count;
end;

procedure TCodingExt.TmpltListClick(Sender: TObject);
var
  Index: Integer;
begin
  if TmpltList.SelCount > 0 then
  begin
    Index := TmpltList.ItemIndex;
    IdEdit.Text := TemplateForm.Templates[Index];
    CodeMemo.Text := TTemplate(TemplateForm.Templates.Objects[Index]).Text;
  end;
end;

procedure TCodingExt.OnShow(Sender: TObject);
begin
  if TmpltList.Count <> TemplateForm.Templates.Count then
    TmpltList.Count := TemplateForm.Templates.Count;
end;

end.

