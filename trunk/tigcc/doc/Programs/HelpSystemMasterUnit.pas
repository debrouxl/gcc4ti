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

unit HelpSystemMasterUnit;

interface

uses
	SysUtils, Classes;

type
	ESubStrNotFound = class(Exception);
	EFileNotFound = class(Exception);

var
	SystemFolder,
	SourceFolder,
	DestFolder,
	WebFolder,
	IncludeFolder,
	ExamplesFolder: string;
	SingleTags,
	ClosingTags,
	DependentTags: TStringList;

function LoadFile(const FileName: string): string;
procedure WriteFile(const FileName, Contents: string);

procedure IniReadSec(const Line: string; var Sec: string);
function IniReadString(var Line: string; const Ident: string): Boolean;

function HTMLize(const Input: string): string;
function DeHTMLize(Input: string): string;
function HTMLizeChar(Input: Char): string;

function BalancedParentheses(const S: string): Boolean;

procedure CheckHTML(S: string; SingleTags, ClosingTags: TStringList; const HeaderFile: string = ''; const Section: string = '');

procedure PrepareBody(var Body: string; CHMSystem: Boolean; const HeaderFile: string = ''; const Section: string = ''; EncloseInP: Boolean = False; Bold: Boolean = False; ResolveLinks: Boolean = True);
function EncodeLink(const LinkValue: string; CHMSystem: Boolean; const HeaderFile: string = ''; const Section: string = ''; const LinkText: string = ''): string;
function EncodeInfoLink(const LinkValue: string; CHMSystem: Boolean; const HeaderFile: string = ''; const Section: string = ''): string;
function CreateHTMLLink(const MainFile, Section, LinkWord: string; CHMSystem: Boolean; const FromFile, FromSection: string): string;

function GetSeeAlsoText(Input: string; CHMSystem: Boolean; const HeaderFile: string = ''; const Section: string = ''): string;

function DeleteToFirst(const SubStr: string; var Str: string; MaxLen: Integer = -1): string;
function DeleteToAfterFirst(const SubStr: string; var Str: string; MaxLen: Integer = -1): string;
procedure DeleteWhiteSpace(var Str: string);
procedure TrimWhiteSpace(var Str: string);
procedure TrimHTMLWhiteSpace(var Str: string);

function StrToBool(const S: string): Boolean;

function StartsWith(const SubStr: string; var Str: string; CaseSensitive: Boolean = True; StartPos: Integer = 1; Remove: Boolean = False): Boolean;

function GetGenericFileStart(const Title, Heading: string; const TypeStr: string = ''; const IconFile: string = ''; const LeftNav: string = ''; const CenterNav: string = ''; const RightNav: string = ''; NavWidth: Integer = 60): string;
function GetGenericFileEnd: string;

function GetWebFileStart(const Title, Heading: string; NarrowTables: Boolean = False): string;
function GetWebFileEnd: string;

implementation

uses
	Windows,
	UtilsDos,
	UtilsWin;

function LoadFile(const FileName: string): string;
begin
	with TMemoryStream.Create do try
		LoadFromFile (FileName);
		Size := Size + 1;
		PChar(Memory) [Size - 1] := #0;
		Result := PChar (Memory);
	finally
		Free;
	end;
end;

procedure WriteFile(const FileName, Contents: string);
begin
	with TFileStream.Create (FileName, fmCreate) do try
		Write (PChar(Contents)^, Length (Contents));
	finally
		Free;
	end;
end;

procedure IniReadSec(const Line: string; var Sec: string);
begin
	if (Length (Line) > 2) and (Line [1] = '[') and (Line [Length (Line)] = ']') then
		Sec := LowerCase (Copy (Line, 2, Length (Line) - 2));
end;

function IniReadString(var Line: string; const Ident: string): Boolean;
begin
	Result := StartsWith (Ident + '=', Line, False, 1, True);
end;

function HTMLize(const Input: string): string;
begin
	Result := Input;
	Result := StringReplace (Result, '&#', '$$$#', [rfReplaceAll]);
	Result := StringReplace (Result, '&', '&amp;', [rfReplaceAll]);
	Result := StringReplace (Result, '$$$#', '&#', [rfReplaceAll]);
	Result := StringReplace (Result, '·', '&nbsp;', [rfReplaceAll]);
	Result := StringReplace (Result, '<', '&lt;', [rfReplaceAll]);
	Result := StringReplace (Result, '>', '&gt;', [rfReplaceAll]);
	Result := StringReplace (Result, '"', '&quot;', [rfReplaceAll]);
end;

function DeHTMLize(Input: string): string;
var
	P: Integer;
	Code: string;
begin
	Result := '';
	repeat
		P := Pos ('&', Input);
		if P > 0 then begin
			Result := Result + Copy (Input, 1, P - 1);
			Delete (Input, 1, P);
			Code := LowerCase (Copy (Input, 1, Pos (';', Input) - 1));
			Delete (Input, 1, Length (Code) + 1);
			if Code = 'amp' then
				Result := Result + '&'
			else if Code = 'nbsp' then
				Result := Result + '·'
			else if Code = 'lt' then
				Result := Result + '<'
			else if Code = 'gt' then
				Result := Result + '>'
			else if Code = 'quot' then
				Result := Result + '"';
		end;
	until P <= 0;
	Result := Result + Input;
end;

function HTMLizeChar(Input: Char): string;
begin
	case Input of
		'&':
			Result := '&amp;';
		'<':
			Result := '&lt;';
		'>':
			Result := '&gt;';
		'"':
			Result := '&quot;';
		else
			Result := Input;
	end;
end;

function BalancedParentheses(const S: string): Boolean;
var
	P: Integer;
	Level: Integer;
begin
	Level := 0;
	for P := 1 to Length (S) do
		if S [P] = '(' then
			Inc (Level)
		else if S [P] = ')' then begin
			Dec (Level);
			if Level < 0 then begin
				Result := False;
				Exit;
			end;
		end;
	Result := Level = 0;
end;

procedure CheckHTML(S: string; SingleTags, ClosingTags: TStringList; const HeaderFile: string = ''; const Section: string = '');
var
	OpenedTags,
	DTags: TStringList;
	Tag,
	OTag: string;
begin
	OpenedTags := TStringList.Create;
	DTags := TStringList.Create;
	try
		while Pos ('<', S) > 0 do begin
			DeleteToAfterFirst ('<', S);
			Tag := '';
			while (Length (S) > 0) and (S [1] in ['/', 'A'..'Z', 'a'..'z', '0'..'9', '_']) do begin
				Tag := Tag + S [1];
				Delete (S, 1, 1);
			end;
			if Length (S) > 0 then begin
				while (Length (S) > 0) and (S [1] <> '>') do begin
					if S [1] = '"' then begin
						Delete (S, 1, 1);
						if Pos ('"', S) > 0 then
							DeleteToAfterFirst ('"', S)
						else
							raise ESubStrNotFound.CreateFmt ('Unclosed quotes in HTML tag "<' + Tag + '>" in %s/%s', [HeaderFile, Section]);
					end else
						Delete (S, 1, 1);
				end;
				if Length (S) > 0 then begin
					Delete (S, 1, 1);
					if Tag <> UpperCase (Tag) then begin
						if ShowDefaultMessageBox (Format ('Non-uppercase HTML tag "<' + Tag + '>" in %s/%s', [HeaderFile, Section]), 'Information', mtInformation, True) = idCancel then
							Abort;
						Tag := UpperCase (Tag);
					end;
					if (OpenedTags.Count > 0) and ('/' + OpenedTags.Strings [OpenedTags.Count - 1] = Tag) then
						OpenedTags.Delete (OpenedTags.Count - 1)
					else begin
						DTags.CommaText := DependentTags.Values [Tag];
						if OpenedTags.Count > 0 then
							OTag := OpenedTags.Strings [OpenedTags.Count - 1]
						else
							OTag := '';
						if (DTags.Count > 0) and (DTags.IndexOf (OTag) < 0) then begin
							if ShowDefaultMessageBox (Format ('Unexpected HTML tag "<' + Tag + '>" in "<' + OTag + '>" instead of "<' + DTags.CommaText + '>" in %s/%s', [HeaderFile, Section]), 'Warning', mtInformation, True) = idCancel then
								Abort;
						end else begin
							if SingleTags.IndexOf (Tag) < 0 then begin
								if ClosingTags.IndexOf (Tag) >= 0 then
									OpenedTags.Add (Tag)
								else begin
									if (Length (Tag) > 0) and (Tag [1] = '/') then begin
										if ShowDefaultMessageBox (Format ('Unexpected HTML closing tag "<' + Tag + '>" in %s/%s', [HeaderFile, Section]), 'Warning', mtInformation, True) = idCancel then
											Abort;
									end else begin
										if ShowDefaultMessageBox (Format ('Unsupported HTML tag "<' + Tag + '>" in %s/%s', [HeaderFile, Section]), 'Warning', mtInformation, True) = idCancel then
											Abort;
									end;
								end;
							end;
						end;
					end;
				end else
					raise ESubStrNotFound.CreateFmt ('Unfinished HTML tag "<' + Tag + '>" in %s/%s', [HeaderFile, Section]);
			end else
				raise ESubStrNotFound.CreateFmt ('Unfinished HTML tag "<' + Tag + '>" in %s/%s', [HeaderFile, Section]);
		end;
		if OpenedTags.Count > 0 then
			if ShowDefaultMessageBox (Format ('Unclosed HTML tags in %s/%s: ' + OpenedTags.CommaText, [HeaderFile, Section]), 'Warning', mtInformation, True) = idCancel then
				Abort;
	finally
		DTags.Free;
		OpenedTags.Free;
	end;
end;

procedure PrepareBody(var Body: string; CHMSystem: Boolean; const HeaderFile, Section: string; EncloseInP, Bold, ResolveLinks: Boolean);
var
	P,
	NewP: Integer;
	S1,
	LinkValue,
	LinkText: string;
begin
	if CHMSystem then begin
		if SingleTags.Count > 0 then
			CheckHTML (Body, SingleTags, ClosingTags, HeaderFile, Section);
		Body := StringReplace (Body, #13#10'</LI>', '', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '</LI>', '', [rfReplaceAll, rfIgnoreCase]);
	end;
	if Pos ('NOTE:', Body) > 0 then
		raise ESubStrNotFound.CreateFmt ('"NOTE:" found in %s/%s', [HeaderFile, Section]);
	P := 0;
	repeat
		NewP := Pos ('<A HREF="', Copy (Body, P + 1, Length (Body)));
		if NewP > 0 then begin
			Inc (P, NewP);
			if not (StartsWith ('$$', Body, True, P + Length ('<A HREF="')) or StartsWith ('http://', Body, True, P + Length ('<A HREF="')) or StartsWith ('ftp://', Body, True, P + Length ('<A HREF="')) or StartsWith ('mailto:', Body, True, P + Length ('<A HREF="')) or StartsWith ('javascript:', Body, True, P + Length ('<A HREF="'))) then
				raise ESubStrNotFound.CreateFmt ('Invalid internal link in %s/%s', [HeaderFile, Section]);
		end;
	until NewP <= 0;
	if ResolveLinks then
		repeat
			P := Pos ('$$', Body);
			if P > 0 then begin
				Delete (Body, P, 2);
				if StartsWith ('LINK', Body, False, P, True) then begin
					while (Length (Body) >= P) and (Body [P] = ' ') do
						Delete (Body, P, 1);
					if StartsWith ('(', Body, False, P, True) then begin
						LinkValue := '';
						while (Length (Body) >= P) and (Body [P] <> ')') do begin
							LinkValue := LinkValue + Body [P];
							Delete (Body, P, 1);
						end;
						if Length (Body) >= P then begin
							Delete (Body, P, 1);
							S1 := Copy (Body, P + 2, 50);
							LinkText := DeleteToFirst ('<', S1);
							Insert (EncodeLink (LinkValue, CHMSystem, HeaderFile, Section, LinkText), Body, P);
						end else
							raise ESubStrNotFound.CreateFmt ('Missing closing parenthesis for $$LINK in %s/%s', [HeaderFile, Section]);
					end else
						raise ESubStrNotFound.CreateFmt ('Missing opening parenthesis for $$LINK in %s/%s', [HeaderFile, Section]);
				end else if StartsWith ('INFOLINK', Body, False, P, True) then begin
					while (Length (Body) >= P) and (Body [P] = ' ') do
						Delete (Body, P, 1);
					if StartsWith ('(', Body, False, P, True) then begin
						LinkValue := '';
						while (Length (Body) >= P) and (Body [P] <> ')') do begin
							LinkValue := LinkValue + Body [P];
							Delete (Body, P, 1);
						end;
						if Length (Body) >= P then begin
							Delete (Body, P, 1);
							Insert (EncodeInfoLink (LinkValue, CHMSystem, HeaderFile, Section), Body, P);
						end else
							raise ESubStrNotFound.CreateFmt ('Missing closing parenthesis for $$INFOLINK in %s/%s', [HeaderFile, Section]);
					end else
						raise ESubStrNotFound.CreateFmt ('Missing opening parenthesis for $$INFOLINK in %s/%s', [HeaderFile, Section]);
				end else if StartsWith ('EXAMPLE', Body, False, P, True) then begin
					while (Length (Body) >= P) and (Body [P] = ' ') do
						Delete (Body, P, 1);
					if StartsWith ('(', Body, False, P, True) then begin
						LinkValue := '';
						while (Length (Body) >= P) and (Body [P] <> ')') do begin
							LinkValue := LinkValue + Body [P];
							Delete (Body, P, 1);
						end;
						if Length (Body) >= P then begin
							Delete (Body, P, 1);
							if not FileExists (ExamplesFolder + '\' + LinkValue) then
								raise ESubStrNotFound.CreateFmt ('Unresolvable link to %s in %s/%s', [LinkValue, HeaderFile, Section]);
							S1 := LoadFile (ExamplesFolder + '\' + LinkValue);
							if Pos (#9, S1) > 0 then
								raise ESubStrNotFound.CreateFmt ('Tab character found in %s example', [LinkValue]);
							Insert ('<PRE>' + HTMLize (Trim (S1)) + #13#10'</PRE>', Body, P);
						end else
							raise ESubStrNotFound.CreateFmt ('Missing closing parenthesis for $$EXAMPLE in %s/%s', [HeaderFile, Section]);
					end else
						raise ESubStrNotFound.CreateFmt ('Missing opening parenthesis for $$EXAMPLE in %s/%s', [HeaderFile, Section]);
				end else
					raise ESubStrNotFound.CreateFmt ('"$$" with unknown indentifier in %s/%s', [HeaderFile, Section]);
			end;
		until P <= 0;
	if EncloseInP then begin
		Body := '<P>' + Body + '</P>';
		Body := StringReplace (Body, '<PRE', '</P>'#13#10'<PRE', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '</PRE>', '</PRE>'#13#10'<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<UL', '</P>'#13#10'<UL', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '</UL>', '</UL>'#13#10'<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<OL', '</P>'#13#10'<OL', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '</OL>', '</OL>'#13#10'<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<LI>', '<LI><P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '</LI>', '</P></LI>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<DL>', '</P>'#13#10'<DL>'#13#10'<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<DL COMPACT>', '</P>'#13#10'<DL COMPACT>'#13#10'<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '</DL>', '</P>'#13#10'</DL>'#13#10'<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<DT>', '</P><DT><P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<DD>', '</P><DD><P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<P></P>', '', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<P>'#13#10'</P>', #13#10, [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<P>'#13#10, '<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, #13#10'</P>', '</P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<P><BR>', '<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<P><BR>', '<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<P>'#13#10'<BR>', '<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<P>'#13#10'<BR>', '<P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<BR></P>', '</P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<BR></P>', '</P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<BR>'#13#10'</P>', '</P>', [rfReplaceAll, rfIgnoreCase]);
		Body := StringReplace (Body, '<BR>'#13#10'</P>', '</P>', [rfReplaceAll, rfIgnoreCase]);
		if Bold then begin
			Body := StringReplace (Body, '<P>', '<P><B>', [rfReplaceAll, rfIgnoreCase]);
			Body := StringReplace (Body, '</P>', '</B></P>', [rfReplaceAll, rfIgnoreCase]);
		end;
		Body := Trim (Body) + #13#10;
	end else if Bold then
		Body := '<B>' + Body + '</B>';
end;

function EncodeLink(const LinkValue: string; CHMSystem: Boolean; const HeaderFile, Section, LinkText: string): string;
var
	P: Integer;
	S,
	LinkHeaderFile,
	LinkSection,
	LinkWord: string;
begin
	if Length (LinkValue) <= 0 then
		raise EFileNotFound.CreateFmt ('Empty link in %s/%s', [HeaderFile, Section]);
	if (LinkValue = HeaderFile + '/' + Section) or (LinkValue = Section) then
		raise ESubStrNotFound.CreateFmt ('Link to self in %s/%s', [HeaderFile, Section]);
	if Copy (LinkValue, 1, Length ('info/')) = 'info/' then
		Result := EncodeInfoLink (Copy (LinkValue, Length ('info/') + 1, Length (LinkValue)), CHMSystem, HeaderFile, Section)
	else begin
		P := Pos ('/', LinkValue);
		if P > 0 then begin
			LinkHeaderFile := Copy (LinkValue, 1, P - 1);
			LinkSection := Copy (LinkValue, P + 1, Length (LinkValue));
		end else begin
			if Copy (HeaderFile, 1, Length ('info/')) = 'info/' then begin
				Result := EncodeInfoLink (Copy (HeaderFile, Length ('info/') + 1, Length (HeaderFile)) + '/' + LinkValue, CHMSystem, HeaderFile, Section);
				Exit;
			end;
			LinkHeaderFile := HeaderFile;
			LinkSection := LinkValue;
		end;
		if Length (LinkSection) > 0 then begin
			if not FileExistsWithCase (SystemFolder + '\Include\' + LinkHeaderFile + '\' + LinkSection + '.hsf') then
				raise EFileNotFound.CreateFmt ('Unresolvable link to "%s" in %s/%s', [LinkValue, HeaderFile, Section]);
		end else begin
			if not DirExistsWithCase (SystemFolder + '\Include\' + LinkHeaderFile) then
				raise EFileNotFound.CreateFmt ('Unresolvable link to "%s" in %s/%s', [LinkValue, HeaderFile, Section]);
		end;
		LinkWord := '';
		if (Length (LinkText) > 0) and (Length (LinkSection) > 0) and (Pos (LinkText, LinkSection) <= 0) and (Pos (' ', LinkText) <= 0) then begin
			try
				if (Length (LinkHeaderFile) > 0) and (Length (LinkSection) > 0) then begin
					S := LoadFile (SystemFolder + '\Include\' + LinkHeaderFile + '\' + LinkSection + '.hsf');
					if (Pos ('>' + LinkText + '</TD>', S) > 0) or (Pos ('>' + LinkText + '&nbsp;&nbsp;', S) > 0) then
						LinkWord := LinkText;
				end;
			except end;
		end;
		Result := CreateHTMLLink (ChangeFileExt (LinkHeaderFile, '.html'), LinkSection, LinkWord, CHMSystem, ChangeFileExt (HeaderFile, '.html'), Section);
	end;
end;

function EncodeInfoLink(const LinkValue: string; CHMSystem: Boolean; const HeaderFile, Section: string): string;
var
	P: Integer;
	LinkFile,
	LinkSection,
	CurrentFile: string;
begin
	if Length (LinkValue) <= 0 then
		raise EFileNotFound.CreateFmt ('Empty link in %s/%s', [HeaderFile, Section]);
	if 'info/' + LinkValue = HeaderFile + '/' + Section then
		raise ESubStrNotFound.CreateFmt ('Link to self in %s/%s', [HeaderFile, Section]);
	if Copy (HeaderFile, 1, Length ('info/')) = 'info/' then
		CurrentFile := Copy (HeaderFile, Length ('info/') + 1, Length (HeaderFile)) + '.html'
	else
		CurrentFile := ChangeFileExt (HeaderFile, '.html');
	P := Pos ('/', LinkValue);
	if P > 0 then begin
		LinkFile := Copy (LinkValue, 1, P - 1);
		LinkSection := Copy (LinkValue, P + 1, Length (LinkValue));
		if LinkSection = LinkFile then
			LinkSection := '';
	end else begin
		LinkFile := LinkValue;
		LinkSection := '';
	end;
	if LinkFile = 'hdrindex' then
		Result := CreateHTMLLink ('hdrindex.html', '', '', CHMSystem, CurrentFile, Section)
	else if LinkFile = 'keywords' then begin
		if Length (LinkSection) > 0 then begin
			if FileExistsWithCase (SystemFolder + '\Keywords\' + LinkSection + '.ref') then
				LinkSection := Trim (LoadFile (SystemFolder + '\Keywords\' + LinkSection + '.ref'));
			if not FileExistsWithCase (SystemFolder + '\Keywords\' + LinkSection + '.hsk') then
				raise EFileNotFound.CreateFmt ('Unresolvable link to "%s" in %s/%s', [LinkValue, HeaderFile, Section]);
		end;
	end else begin
		if Length (LinkSection) > 0 then begin
			if not FileExistsWithCase (SystemFolder + '\Info\' + LinkFile + '\' + LinkSection + '.hss') then
				raise EFileNotFound.CreateFmt ('Unresolvable link to "%s" in %s/%s', [LinkValue, HeaderFile, Section]);
		end else begin
			if not FileExistsWithCase (SystemFolder + '\Info\' + LinkFile + '\' + LinkFile + '.hss') then
				raise EFileNotFound.CreateFmt ('Unresolvable link to "%s" in %s/%s', [LinkValue, HeaderFile, Section]);
		end;
	end;
	Result := CreateHTMLLink (LinkFile + '.html', LinkSection, '', CHMSystem, CurrentFile, Section)
end;

function CreateHTMLLink(const MainFile, Section, LinkWord: string; CHMSystem: Boolean; const FromFile, FromSection: string): string;
begin
	if Length (Section) > 0 then begin
		if CHMSystem then begin
			Result := WithoutExt (MainFile) + '_' + Section + '.html';
			if Length (LinkWord) > 0 then
				Result := Result + '#' + LinkWord;
		end else begin
			if Length (LinkWord) > 0 then begin
				if MainFile = FromFile then
					Result := '#' + LinkWord
				else
					Result := MainFile + '#' + LinkWord;
			end else begin
				if MainFile = FromFile then
					Result := '#' + Section
				else
					Result := MainFile + '#' + Section;
			end;
		end;
	end else
		Result := MainFile;
end;

function GetSeeAlsoText(Input: string; CHMSystem: Boolean; const HeaderFile, Section: string): string;
var
	Str,
	Link,
	Desc: string;
begin
	Result := '';
	repeat
		Str := Trim (DeleteToAfterFirst (',', Input));
		if Length (Str) > 0 then begin
			if Pos (':', Str) > 0 then begin
				Link := Trim (Copy (Str, 1, Pos (':', Str) - 1));
				Desc := Trim (Copy (Str, Pos (':', Str) + 1, Length (Str)));
			end else begin
				Link := Str;
				if Str [Length (Str)] = '/' then
					Delete (Str, Length (Str), 1);
				if Pos ('/', Str) > 0 then
					Desc := Copy (Str, LastPos ('/', Str) + 1, Length (Str))
				else
					Desc := Str;
				if StartsWith ('_ROM_CALL_', Desc, True, 1, True) then
					Desc := 'ROM Call 0x' + Desc;
			end;
			if Length (Result) > 0 then
				Result := Result + ', ';
			Result := Result + '<A HREF="' + EncodeLink (Link, CHMSystem, HeaderFile, Section, Desc) + '">' + HTMLize (Desc) + '</A>';
		end;
	until Length (Str) <= 0;
end;

function DeleteToFirst(const SubStr: string; var Str: string; MaxLen: Integer): string;
var
	P: Integer;
begin
	if MaxLen < 0 then
		MaxLen := Length (Str)
	else
		Inc (MaxLen, Length (SubStr));
	P := Pos (SubStr, Copy (Str, 1, MaxLen));
	if P <= 0 then begin
		if MaxLen < Length (Str) then
			raise ESubStrNotFound.CreateFmt ('Substring "%s" not found in "%s"', [SubStr, Copy (Str, 1, MaxLen)])
		else
			P := Length (Str) + 1;
	end;
	Result := Copy (Str, 1, P - 1);
	Delete (Str, 1, P - 1)
end;

function DeleteToAfterFirst(const SubStr: string; var Str: string; MaxLen: Integer): string;
begin
	Result := DeleteToFirst (SubStr, Str, MaxLen);
	if Length (Str) > 0 then
		Delete (Str, 1, Length (SubStr));
end;

procedure DeleteWhiteSpace(var Str: string);
begin
	while (Length (Str) > 0) and (Str [1] in [' ', #9, #13, #10]) do
		Delete (Str, 1, 1);
end;

procedure TrimWhiteSpace(var Str: string);
begin
	DeleteWhiteSpace (Str);
	while (Length (Str) > 0) and (Str [Length (Str)] in [' ', #9, #13, #10]) do
		Delete (Str, Length (Str), 1);
end;

procedure TrimHTMLWhiteSpace(var Str: string);
var
	Stop: Boolean;
begin
	repeat
		Stop := True;
		TrimWhiteSpace (Str);
		if Length (Str) >= 4 then begin
			if UpperCase (Copy (Str, 1, 4)) = '<BR>' then begin
				Delete (Str, 1, 4);
				Stop := False;
			end else if UpperCase (Copy (Str, 1, 3)) = '<P>' then begin
				Delete (Str, 1, 3);
				Stop := False;
			end else if UpperCase (Copy (Str, Length (Str) - 3, 4)) = '<BR>' then begin
				Delete (Str, Length (Str) - 3, 4);
				Stop := False;
			end else if UpperCase (Copy (Str, Length (Str) - 3, 4)) = '</P>' then begin
				Delete (Str, Length (Str) - 3, 4);
				Stop := False;
			end;
		end;
	until Stop;
end;

function StrToBool(const S: string): Boolean;
begin
	Result := StrToInt (S) <> 0;
end;

function StartsWith(const SubStr: string; var Str: string; CaseSensitive: Boolean; StartPos: Integer; Remove: Boolean): Boolean;
begin
	if CaseSensitive then
		Result := Copy (Str, StartPos, Length (SubStr)) = SubStr
	else
		Result := UpperCase (Copy (Str, StartPos, Length (SubStr))) = UpperCase (SubStr);
	if Remove and Result then
		Delete (Str, StartPos, Length (SubStr));
end;

function GetGenericFileStart(const Title, Heading, TypeStr, IconFile, LeftNav, CenterNav, RightNav: string; NavWidth: Integer): string;
begin
	Result := '<HTML>'#13#10
		+ '<HEAD>'#13#10;
	if Length (Title) > 0 then
		Result := Result + '<TITLE>' + Title + '</TITLE>'#13#10;
	Result := Result + '<LINK REL="STYLESHEET" TYPE="TEXT/CSS" HREF="style.css">'#13#10
		+ '</HEAD>'#13#10
		+ '<BODY BGCOLOR="#FFFFF8">'#13#10
		+ '<TABLE CLASS="INVTABLE" WIDTH="100%">'#13#10
		+ '<TR>'#13#10;
	if Length (IconFile) > 0 then
		Result := Result + '<TD CLASS="NOBORDER" WIDTH="40"><IMG SRC="' + IconFile + '.gif" WIDTH="32" HEIGHT="32" BORDER="0"></TD>'#13#10;
	if Length (Heading) > 0 then
		Result := Result + '<TD CLASS="TITLE">' + Heading + '</TD>'#13#10;
	if Length (TypeStr) > 0 then
		Result := Result + '<TD CLASS="DESCRIPTION">' + TypeStr + '</TD>'#13#10;
	Result := Result + '</TR>'#13#10
		+ '</TABLE>'#13#10
		+ '<HR>'#13#10;
	if (Length (LeftNav) > 0) or (Length (CenterNav) > 0) or (Length (RightNav) > 0) then begin
		Result := Result + '<TABLE CLASS="NOBORDER" WIDTH="100%"><TR>'#13#10;
		if (Length (CenterNav) > 0) and ((Length (LeftNav) > 0) or (Length (RightNav) > 0)) then
			Result := Result + '<TD CLASS="HEADER" ALIGN="LEFT" WIDTH="' + IntToStr (NavWidth) + '">' + LeftNav + '</TD>'#13#10
		else if Length (LeftNav) > 0 then
			Result := Result + '<TD CLASS="HEADER" ALIGN="LEFT">' + LeftNav + '</TD>'#13#10;
		if Length (CenterNav) > 0 then
			Result := Result + '<TD CLASS="HEADER" ALIGN="CENTER">' + CenterNav + '</TD>'#13#10;
		if (Length (CenterNav) > 0) and ((Length (LeftNav) > 0) or (Length (RightNav) > 0)) then
			Result := Result + '<TD CLASS="HEADER" ALIGN="RIGHT" WIDTH="' + IntToStr (NavWidth) + '">' + RightNav + '</TD>'#13#10
		else if Length (RightNav) > 0 then
			Result := Result + '<TD CLASS="HEADER" ALIGN="RIGHT">' + RightNav + '</TD>'#13#10;
		Result := Result + '</TR></TABLE>'#13#10;
	end;
end;

function GetGenericFileEnd: string;
begin
	Result := '</BODY>'#13#10
		+ '</HTML>'#13#10;
end;

function GetWebFileStart(const Title, Heading: string; NarrowTables: Boolean = False): string;
begin
	Result := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">'#13#10
		+ '<HTML>'#13#10
		+ '<HEAD>'#13#10;
	if Length (Title) > 0 then
		Result := Result + '<TITLE>' + Title + '</TITLE>'#13#10;
	Result := Result + '<STYLE TYPE="TEXT/CSS">'#13#10
		+ '<!--'#13#10
		+ '.IE3-DUMMY { CONT-SIZE: 100%; }'#13#10
		+ 'BODY { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; BACKGROUND-COLOR: #E0E0E0; }'#13#10
		+ 'P { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
		+ 'H1 { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
		+ 'H2 { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
		+ 'H3 { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
		+ 'H4 { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
		+ 'H5 { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
		+ 'H6 { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10
		+ 'UL { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; }'#13#10;
	if NarrowTables then
		Result := Result + 'TD { FONT-FAMILY: Arial Narrow; BACKGROUND-COLOR: #E0E0E0; FONT-SIZE: 10pt; }'#13#10
	else
		Result := Result + 'TD { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; BACKGROUND-COLOR: #FFFFFF; }'#13#10;
	Result := Result + '.NOBORDER { BACKGROUND-COLOR: #E0E0E0; PADDING: 0pt; }'#13#10
		+ '.NOBORDER TD { FONT-FAMILY: Verdana,Arial,Helvetica,Sans-Serif; BACKGROUND-COLOR: #E0E0E0; PADDING: 0pt; }'#13#10
		+ '.CODE { FONT-FAMILY: Courier New; }'#13#10
		+ '-->'#13#10
		+ '</STYLE>'#13#10
		+ '</HEAD>'#13#10
		+ '<BODY TEXT="#000000" BGCOLOR="#E0E0E0">'#13#10;
	if Length (Heading) > 0 then begin
		Result := Result + '<FONT SIZE="5"><B>' + Heading + '</B></FONT>'#13#10
			+ '<HR>'#13#10;
	end;
end;

function GetWebFileEnd: string;
begin
	Result := '<HR>'#13#10
		+ '<H3><A HREF="index.html">Return to the main index</A></H3>'#13#10
		+ '</BODY>'#13#10
		+ '</HTML>'#13#10;
end;

var
	Ln: Integer;
	Sec,
	Line: string;

initialization
	SingleTags := TStringList.Create;
	ClosingTags := TStringList.Create;
	DependentTags := TStringList.Create;
	SingleTags.Sorted := True;
	ClosingTags.Sorted := True;
	DependentTags.Sorted := True;
	if FileExists (ExtractFilePath (ParamStr (0)) + '..\HelpSystem.ini') then begin
		with TStringList.Create do try
			LoadFromFile (ExtractFilePath (ParamStr (0)) + '..\HelpSystem.ini');
			Sec := '';
			for Ln := 0 to Count - 1 do begin
				Line := Strings [Ln];
				if Length (Line) > 0 then begin
					IniReadSec (Line, Sec);
					if Sec = 'folders' then begin
						if IniReadString (Line, 'SystemFolder') then
							SystemFolder := Line
						else if IniReadString (Line, 'SourceFolder') then
							SourceFolder := Line
						else if IniReadString (Line, 'DestFolder') then
							DestFolder := Line
						else if IniReadString (Line, 'WebFolder') then
							WebFolder := Line
						else if IniReadString (Line, 'IncludeFolder') then
							IncludeFolder := Line
						else if IniReadString (Line, 'ExamplesFolder') then
							ExamplesFolder := Line;
					end;
				end;
			end;
		finally
			Free;
		end;
		if FileExists (SystemFolder + '\SingleTags.lst') then
			SingleTags.LoadFromFile (SystemFolder + '\SingleTags.lst');
		if FileExists (SystemFolder + '\ClosingTags.lst') then
			ClosingTags.LoadFromFile (SystemFolder + '\ClosingTags.lst');
		if FileExists (SystemFolder + '\DependentTags.lst') then
			DependentTags.LoadFromFile (SystemFolder + '\DependentTags.lst');
	end;
finalization
	DependentTags.Free;
	ClosingTags.Free;
	SingleTags.Free;
end.
