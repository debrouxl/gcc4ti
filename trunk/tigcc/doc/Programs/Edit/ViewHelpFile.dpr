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

program ViewHelpFile;

uses
	Windows,
	SysUtils,
	Classes,
	UtilsDos,
	UtilsWin,
	ShellAPI,
	HelpSystemMasterUnit in '..\HelpSystemMasterUnit.pas';

function URLEncode(const Input: string): string;
var
	I: Integer;
begin
	Result := '';
	for I := 1 to Length (Input) do
		if Input [I] in ['A'..'Z', 'a'..'z', '0'..'9', '.'] then
			Result := Result + Input [I]
		else
			Result := Result + '%' + IntToHex (Byte (Input [I]), 2);
end;

var
	Ln: Integer;
	LineBreakAfterComment: Boolean;
	Sec,
	Line,
	S,
	FileName,
	FileContents,
	Contents,
	Title,
	Category,
	SubType,
	HeaderFiles,
	Definition,
	SeeAlso,
	SeeAlsoRes,
	Str,
	Desc: string;
	TempPath: array [0..MAX_PATH] of Char;

procedure AddContents(const Section: string; Bold: Boolean = False);
var
	P,
	Q: Integer;
	S,
	Body: string;
begin
	S := FileContents;
	DeleteToAfterFirst ('[' + Section + ']', S);
	Body := Trim (DeleteToFirst (#13#10'[', S));
	if Length (Body) > 0 then begin
		if SingleTags.Count > 0 then
			CheckHTML (Body, SingleTags, ClosingTags);
		Body := StringReplace (Body, '<A ', '<U><FONT COLOR="#0000FF" ', [rfReplaceAll]);
		Body := StringReplace (Body, '</A>', '</FONT></U>', [rfReplaceAll]);
		repeat
			P := Pos ('$$EXAMPLE', Body);
			if P > 0 then begin
				Delete (Body, P, Length ('$$EXAMPLE'));
				Insert ('<PRE>', Body, P);
				Q := Pos (')', Copy (Body, P, Length (Body)));
				Insert (#13#10'</PRE>', Body, P + Q);
			end;
		until P <= 0;
		PrepareBody (Body, False, '', '', True, Bold, False);
		Contents := Contents + Body + #13#10;
	end;
end;

begin
	try
		if ParamCount > 0 then begin
			FileName := ParamStr (1);
			if FileExists (FileName) then begin
				Title := '';
				Category := '';
				SubType := '';
				HeaderFiles := '';
				Definition := '';
				SeeAlso := '';
				with TStringList.Create do try
					LoadFromFile (FileName);
					Sec := '';
					for Ln := 0 to Count - 1 do begin
						Line := Strings [Ln];
						if Length (Line) > 0 then begin
							IniReadSec (Line, Sec);
							if Sec = 'main' then begin
								if IniReadString (Line, 'Name') then
									Title := Line
								else if IniReadString (Line, 'Title') then
									Title := HTMLize (Line)
								else if IniReadString (Line, 'Type') then
									Category := Line
								else if IniReadString (Line, 'Subtype') then
									SubType := Line
								else if IniReadString (Line, 'Header Files') then
									HeaderFiles := Line
								else if IniReadString (Line, 'Definition') then
									Definition := Line
								else if IniReadString (Line, 'See Also') then
									SeeAlso := Line;
							end;
						end;
					end;
				finally
					Free;
				end;
				if not BalancedParentheses (Definition) then
					raise Exception.Create ('Definition contains unbalanced parentheses');
				if Pos ('{ ', Definition) > 0 then begin
					S := Definition;
					Definition := '';
					LineBreakAfterComment := False;
					while Length (S) > 0 do begin
						if Copy (S, 1, 2) = '{ ' then begin
							System.Delete (S, 1, 2);
							Definition := Definition + '{'#13#10
								+	'<TABLE><TR><TD WIDTH="12"></TD><TD><CODE>'#13#10;
						end else if Copy (S, 1, 2) = ' }' then begin
							System.Delete (S, 1, 2);
							Definition := Definition + '</CODE></TD></TR></TABLE>'#13#10'}';
						end else if Copy (S, 1, 2) = '; ' then begin
							System.Delete (S, 1, 1);
							if Copy (S, 1, 2) <> ' }' then
								System.Delete (S, 1, 1);
							if Copy (S, 1, 2) = '/*' then begin
								Definition := Definition + '; ';
								LineBreakAfterComment := True;
							end else begin
								Definition := Definition + ';<BR>'#13#10;
							end;
						end else if Copy (S, 1, 2) = '/*' then begin
							System.Delete (S, 1, 2);
							Definition := Definition + '<I>/*';
						end else if Copy (S, 1, 2) = '*/' then begin
							System.Delete (S, 1, 2);
							Definition := Definition + '*/</I>';
							if LineBreakAfterComment then begin
								if (Copy (S, 1, 1) = ' ') and (Copy (S, 1, 2) <> ' }') then
									System.Delete (S, 1, 1);
								Definition := Definition + '<BR>'#13#10;
								LineBreakAfterComment := False;
							end;
						end else begin
							Definition := Definition + S [1];
							System.Delete (S, 1, 1);
						end;
					end;
				end;
				Contents := '<HTML>'#13#10
					+ '<HEAD>'#13#10
					+ '<TITLE>' + Title + '</TITLE>'#13#10
					+ '<STYLE TYPE="TEXT/CSS">'#13#10
					+ '<!--'#13#10
					+ 'BODY { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; BACKGROUND-COLOR: #FFFFF8; }'#13#10
					+ 'P { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
					+ 'H2 { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
					+ '-->'#13#10
					+ '</STYLE>'#13#10
					+ '</HEAD>'#13#10
					+ '<BODY BGCOLOR="#FFFFF8">'#13#10
					+ '<H2><B>' + Title + '</B></H2>'#13#10;
				if Length (Category) > 0 then begin
					Contents := Contents
						+ '<P>' + Category;
					if Length (SubType) > 0 then
						Contents := Contents + ' (' + SubType + ')';
					Contents := Contents
						+ '</P>'#13#10;
				end;
				if Length (HeaderFiles) > 0 then begin
					Contents := Contents
						+ '<P>' + HeaderFiles + '</P>'#13#10;
				end;
				Contents := Contents + '<HR>'#13#10;
				if Length (Definition) > 0 then begin
					Contents := Contents
						+ '<P><TABLE BORDER="1" CELLPADDING="2" BGCOLOR="#C0C0FF"><TR BGCOLOR="#FFFFFF"><TD><CODE>' + Definition + '</CODE></TD></TR></TABLE></P>'#13#10
						+ '<HR>'#13#10;
				end;
				FileContents := LoadFile (FileName);
				AddContents ('Description', True);
				AddContents ('Explanation');
				AddContents ('Top');
				AddContents ('Bottom');
				if Length (SeeAlso) > 0 then begin
					SeeAlsoRes := '';
					repeat
						Str := Trim (DeleteToAfterFirst (',', SeeAlso));
						if Length (Str) > 0 then begin
							if Pos (':', Str) > 0 then
								Desc := Trim (Copy (Str, Pos (':', Str) + 1, Length (Str)))
							else begin
								if Str [Length (Str)] = '/' then
									Delete (Str, Length (Str), 1);
								if Pos ('/', Str) > 0 then
									Desc := Copy (Str, LastPos ('/', Str) + 1, Length (Str))
								else
									Desc := Str;
							end;
							if Length (SeeAlsoRes) > 0 then
								SeeAlsoRes := SeeAlsoRes + ', ';
							SeeAlsoRes := SeeAlsoRes + '<U><FONT COLOR="#0000FF">' + HTMLize (Desc) + '</FONT></U>';
						end;
					until Length (Str) <= 0;
					Contents := Contents
						+ '<P><HR>See also: ' + SeeAlsoRes + '</P>'#13#10;
				end;
				Contents := Contents
					+ '</BODY>'#13#10
					+ '</HTML>'#13#10;
				GetTempPath (MAX_PATH, TempPath);
				FileName := WithBackslash (TempPath) + ChangeFileExt (ExtractFileName (FileName), '.html');
				WriteFile (FileName, Contents);
				ShellExecute (0, nil, PChar (FileName), nil, nil, sw_ShowMaximized);
			end else
				ShowDefaultMessageBox ('File "' + FileName + '" not found.', 'Error', mtProgramError);
		end else
			ShowDefaultMessageBox ('No parameters in command line.', 'Error', mtProgramError);
	except
		on E: Exception do
			if not (E is EAbort) then
				MessageBox (0, PChar (E.Message), 'Error', mb_OK or mb_IconError);
	end;
end.
