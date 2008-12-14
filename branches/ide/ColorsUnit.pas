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

unit ColorsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TMultipleColorsForm = class(TForm)
    ColorBox: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Bevel1: TBevel;
    Button4: TButton;
    Button5: TButton;
    ColorDlg: TColorDialog;
    procedure ColorBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure EditColor(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ColorBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UpdateButtons(Sender: TObject);
  private
	public
	end;

implementation

{$R *.DFM}

procedure TMultipleColorsForm.ColorBoxDrawItem(Control: TWinControl; Index: Integer;
	Rect: TRect; State: TOwnerDrawState);
begin
	with Control as TListBox do begin
		if odSelected in State then
			Canvas.Pen.Color := clHighlight
		else
			Canvas.Pen.Color := Color;
		Canvas.MoveTo (Rect.Left, Rect.Top);
		Canvas.LineTo (Rect.Right, Rect.Top);
		Canvas.MoveTo (Rect.Left, Rect.Top + 1);
		Canvas.LineTo (Rect.Right, Rect.Top + 1);
		Canvas.MoveTo (Rect.Left, Rect.Bottom - 1);
		Canvas.LineTo (Rect.Right, Rect.Bottom - 1);
		Canvas.MoveTo (Rect.Left, Rect.Bottom - 2);
		Canvas.LineTo (Rect.Right, Rect.Bottom - 2);
		Canvas.MoveTo (Rect.Left, Rect.Top);
		Canvas.LineTo (Rect.Left, Rect.Bottom);
		Canvas.MoveTo (Rect.Left + 1, Rect.Top);
		Canvas.LineTo (Rect.Left + 1, Rect.Bottom);
		Canvas.MoveTo (Rect.Right - 1, Rect.Top);
		Canvas.LineTo (Rect.Right - 1, Rect.Bottom);
		Canvas.MoveTo (Rect.Right - 2, Rect.Top);
		Canvas.LineTo (Rect.Right - 2, Rect.Bottom);
		Inc (Rect.Left, 2);
		Inc (Rect.Top, 2);
		Dec (Rect.Right, 2);
		Dec (Rect.Bottom, 2);
		try
			Canvas.Brush.Color := StrToInt (Items [Index]);
		except
			Canvas.Brush.Color := clBlack;
		end;
		Canvas.Brush.Style := bsSolid;
		Canvas.FillRect (Rect);
	end;
end;

procedure TMultipleColorsForm.EditColor(Sender: TObject);
begin
	if Button3.Enabled then begin
		try
			ColorDlg.Color := StrToInt (ColorBox.Items [ColorBox.ItemIndex]);
		except
			ColorDlg.Color := clBlack;
		end;
		if ColorDlg.Execute then
			ColorBox.Items [ColorBox.ItemIndex] := '$' + UpperCase (IntToHex (ColorDlg.Color, 6));
	end;
end;

procedure TMultipleColorsForm.Button1Click(Sender: TObject);
var
	I: Integer;
begin
	if ColorDlg.Execute then begin
		I := ColorBox.Items.Add (UpperCase (IntToHex (ColorDlg.Color, 6)));
		if ColorBox.Items [I] [1] <> '$' then
			ColorBox.Items [I] := '$' + ColorBox.Items [I];
	end;
	UpdateButtons (Sender);
end;

procedure TMultipleColorsForm.Button2Click(Sender: TObject);
begin
	if Button2.Enabled then
		ColorBox.Items.Delete (ColorBox.ItemIndex);
	UpdateButtons (Sender);
	if Button2.Enabled then
		ColorBox.ItemIndex := ColorBox.Items.Count - 1;
end;

procedure TMultipleColorsForm.ColorBoxKeyDown(Sender: TObject;
	var Key: Word; Shift: TShiftState);
begin
	if Key = vk_Delete then
		Button2.Click;
end;

procedure TMultipleColorsForm.UpdateButtons(Sender: TObject);
begin
	Button2.Enabled := ColorBox.Items.Count > 0;
	Button3.Enabled := Button2.Enabled;
end;

end.
