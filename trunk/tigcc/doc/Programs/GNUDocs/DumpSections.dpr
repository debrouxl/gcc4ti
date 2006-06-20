{
  TIGCC Documentation Tools

  Copyright (C) 2002-2004 Sebastian Reichelt

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

program DumpSections;

uses
	UtilsDos,
	SysUtils,
	Classes;

var
	I,
	J: Integer;
	Contents: TStringList;
	Title: string;

begin
	Contents := TStringList.Create;
	try
		with TFileReferences.Create do try
			SearchForFiles (ParamStr (1) + '*.hss', atStd);
			for I := 0 to Count - 1 do
				with TStringList.Create do try
					LoadFromFile (Items[I].FullName);
					for J := 0 to Count - 1 do
						if Copy (Strings [J], 1, Length ('Title=')) = 'Title=' then begin
							Title := Copy (Strings [J], Length ('Title=') + 1, Length (Strings [J]));
							if Length (Title) > 0 then begin
								if Title [1] = '.' then
									Contents.Add (#9'else if Pos (''' + Title + ''', Ref) > 0 then'#13#10#9#9'Result := ''' + ExtractFileName (WithoutBackslash (ParamStr (1))) + '/' + WithoutExt (Items[I].FileName) + '''')
								else if Pos ('cpp', ParamStr (1)) > 0 then
									Contents.Add (#9'else if Ref = ''' + Title + ''' then'#13#10#9#9'Result := ''' + ExtractFileName (WithoutBackslash (ParamStr (1))) + '/' + WithoutExt (Items[I].FileName) + '''')
								else
									Contents.Add (#9'else if Ref = ''' + Title + ',,' + Title + ''' then'#13#10#9#9'Result := ''' + ExtractFileName (WithoutBackslash (ParamStr (1))) + '/' + WithoutExt (Items[I].FileName) + '''');
							end;
						end;
				finally
					Free;
				end;
		finally
			Free;
		end;
		Contents.SaveToFile ('DumpSections.out');
	finally
		Contents.Free;
	end;
end.
