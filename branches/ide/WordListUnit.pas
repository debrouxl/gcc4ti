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

unit WordListUnit;

interface

uses
	SourceEditUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
	TWordListForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ColorDlg: TColorDialog;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button3: TButton;
    Button4: TButton;
    ListStrings: TMemo;
    CaseSensitiveBox: TCheckBox;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CaseSensitiveBoxClick(Sender: TObject);
	private
	public
		List: TWordList;
	end;

var
	WordListForm: TWordListForm;

implementation

{$R *.DFM}

uses
	StyleSelectionUnit;

procedure TWordListForm.FormCreate(Sender: TObject);
begin
	List := TWordList.Create (nil);
end;

procedure TWordListForm.FormDestroy(Sender: TObject);
begin
	List.Free;
end;

procedure TWordListForm.Button1Click(Sender: TObject);
begin
	ColorDlg.Color := List.Color;
	if ColorDlg.Execute then begin
		List.Color := ColorDlg.Color;
		List.CustomColor := True;
	end;
end;

procedure TWordListForm.Button2Click(Sender: TObject);
begin
	with TStyleSelectionForm.Create (Self) do try
		CustomStyle := List.CustomStyle;
		Style := List.Style;
		if ShowModal = mrOK then begin
			List.CustomStyle := CustomStyle;
			List.Style := Style;
		end;
	finally
		Free;
	end;
end;

procedure TWordListForm.FormShow(Sender: TObject);
begin
	ListStrings.Lines.Assign (List.Words);
	CaseSensitiveBox.Checked := List.CaseSensitive;
end;

procedure TWordListForm.FormClose(Sender: TObject;
	var Action: TCloseAction);
begin
	List.Words.Assign (ListStrings.Lines);
end;

procedure TWordListForm.CaseSensitiveBoxClick(Sender: TObject);
begin
	List.CaseSensitive := CaseSensitiveBox.Checked;
end;

end.
