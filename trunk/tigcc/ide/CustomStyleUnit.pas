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

unit CustomStyleUnit;

interface

uses
	SourceEditUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls;

type
	TCustomStyleForm = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Bevel1: TBevel;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		BeginEdit: TEdit;
		EndEdit: TEdit;
		IgnoreEdit: TEdit;
		Bevel2: TBevel;
		Button3: TButton;
		Button4: TButton;
    ColorDlg: TColorDialog;
    SwitchableCheckBox: TCheckBox;
    LineStartOnlyCheckBox: TCheckBox;
		procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BeginEditChange(Sender: TObject);
    procedure EndEditChange(Sender: TObject);
		procedure IgnoreEditChange(Sender: TObject);
		procedure SwitchableCheckBoxClick(Sender: TObject);
		procedure LineStartOnlyCheckBoxClick(Sender: TObject);
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure FormShow(Sender: TObject);
	private
	public
		Style: TCustomStyle;
	end;

implementation

{$R *.DFM}

uses
	StyleSelectionUnit;

procedure TCustomStyleForm.FormCreate(Sender: TObject);
begin
	Style := TCustomStyle.Create (nil);
end;

procedure TCustomStyleForm.FormDestroy(Sender: TObject);
begin
	Style.Free;
end;

procedure TCustomStyleForm.BeginEditChange(Sender: TObject);
begin
	Style.BeginText := BeginEdit.Text;
end;

procedure TCustomStyleForm.EndEditChange(Sender: TObject);
begin
	Style.EndText := EndEdit.Text;
end;

procedure TCustomStyleForm.IgnoreEditChange(Sender: TObject);
begin
	Style.IgnoreChar := IgnoreEdit.Text;
end;

procedure TCustomStyleForm.SwitchableCheckBoxClick(Sender: TObject);
begin
	Style.Switchable := SwitchableCheckBox.Checked;
end;

procedure TCustomStyleForm.LineStartOnlyCheckBoxClick(Sender: TObject);
begin
	Style.LineStartOnly := LineStartOnlyCheckBox.Checked;
end;

procedure TCustomStyleForm.Button1Click(Sender: TObject);
begin
	ColorDlg.Color := Style.Color;
	if ColorDlg.Execute then begin
		Style.Color := ColorDlg.Color;
		Style.CustomColor := True;
	end;
end;

procedure TCustomStyleForm.Button2Click(Sender: TObject);
begin
	with TStyleSelectionForm.Create (Self) do try
		CustomStyle := Self.Style.CustomStyle;
		Style := Self.Style.Style;
		if ShowModal = mrOK then begin
			Self.Style.CustomStyle := CustomStyle;
			Self.Style.Style := Style;
		end;
	finally
		Free;
	end;
end;

procedure TCustomStyleForm.FormShow(Sender: TObject);
begin
	if Style.BeginText = #13 then
		BeginEdit.Text := '#13'
	else
		BeginEdit.Text := Style.BeginText;
	if Style.EndText = #13 then
		EndEdit.Text := '#13'
	else
		EndEdit.Text := Style.EndText;
	IgnoreEdit.Text := Style.IgnoreChar;
	SwitchableCheckBox.Checked := Style.Switchable;
	LineStartOnlyCheckBox.Checked := Style.LineStartOnly;
end;

end.
