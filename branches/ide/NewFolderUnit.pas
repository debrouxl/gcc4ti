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

unit NewFolderUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TNewFolderForm = class(TForm)
    Label1: TLabel;
    InputBox: TEdit;
    Bevel1: TBevel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure InputBoxChange(Sender: TObject);
  private
	public
	end;

var
  NewFolderForm: TNewFolderForm;

implementation

{$R *.DFM}

procedure TNewFolderForm.InputBoxChange(Sender: TObject);
begin
	ButtonOK.Enabled := InputBox.Text <> '';
end;

end.
