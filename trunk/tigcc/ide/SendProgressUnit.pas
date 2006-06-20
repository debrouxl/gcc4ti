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

unit SendProgressUnit;

interface

uses
	LinkUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls;

type
	TSendProgressForm = class(TForm)
		ProgressBar: TProgressBar;
		FileNameLabel: TLabel;
		CancelButton: TButton;
		procedure CancelButtonClick(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	private
	public
		Cancelled: Boolean;
		CloseNow: Boolean;
	end;

function ProgressProg(ID: Pointer; Progress: PDWord): Boolean;

implementation

{$R *.DFM}

function ProgressProg(ID: Pointer; Progress: PDWord): Boolean;
begin
	with TSendProgressForm (ID) do begin
		ProgressBar.Position := Progress^;
		Result := not Cancelled;
	end;
	Application.ProcessMessages;
end;

procedure TSendProgressForm.CancelButtonClick(Sender: TObject);
begin
	Cancelled := True;
end;

procedure TSendProgressForm.FormCloseQuery(Sender: TObject;
	var CanClose: Boolean);
begin
	CanClose := CloseNow;
	if not CanClose then
		Cancelled := True;
end;

end.
