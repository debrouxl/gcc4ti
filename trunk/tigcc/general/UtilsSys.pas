{
  This Delphi unit is part of TIGCC.

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

unit UtilsSys;

interface

uses
	Windows, SysUtils, Classes;

function LastPos (const Substr, S: string): Integer;

var
	AppLocationDir: string; // the directory where the program was started from, with a '\' at the end

implementation

function LastPos;
var
	I: Integer;
begin
	for I := Length (S) - Length (Substr) + 1 downto 1 do begin
		if Copy (S, I, Length (SubStr)) = Substr then begin
			Result := I;
			Exit;
		end;
	end;
	Result := 0;
end;

initialization
	AppLocationDir := ExtractFilePath (ParamStr (0));
end.
