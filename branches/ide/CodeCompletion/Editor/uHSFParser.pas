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

unit uHSFParser;

interface

uses Classes, uEditor;

type
  TOnGetLine = function(const Symbol: string): Integer of object;

procedure ImportFile(const FileName: string; F: TCEditorForm; GetLine: TOnGetLine);
procedure ImportDir(Dir: string; F: TCEditorForm; GetLine: TOnGetLine);

function SimplifyType(T: string): string;
function SimplifyLeft(T: string; out Right: string): string;
function FilterComment(const Comment: string): string;
function FilterDesc(const Def: string): string;

var
  DoEnum: Boolean;

implementation

uses SysUtils, StrUtils, UtilsDos;

function SimplifyType(T: string): string;
begin
  T := Trim(T);
  if T = 'Type' then
    Result := '<c:Olive>type'
  else if T = 'Function' then
    Result := '<c:Teal>func'
  else if T = 'Constant' then
    Result := '<c:Green>const'
  else if T = 'Variable' then
    Result := '<c:Maroon>var'
  else if T = 'Enum' then
    Result := '<c:Olive>enum'
  else
    Result := T;
end;

function SimplifyLeft(T: string; out Right: string): string;
begin
  T := Trim(T);
  if AnsiStartsText('typedef', T) then
  begin
    Right := Copy(T, 9, MaxInt);
    Result := '';
  end
  else if AnsiStartsText('CALLBACK', T) then
  begin
    Result := '<i>callback';
    Right := Trim(Copy(T, 10, MaxInt)) + ' | ';
  end
  else if T = 'unknown_retval' then
    Result := '?'
  else if T = '#define' then
    Result := ''
  else
    Result := T;
end;

function FilterComment(const Comment: string): string;
var
  i, j: Integer;
begin
  Result := '';
  i := Pos('/*', Comment);
  j := -1;
  while i <> 0 do
  begin
    Result := Result + Copy(Comment, j + 2, i - j - 1);
    j := PosEx('*/', Comment, i);
    if j = 0 then
    begin
      j := MaxInt;
      Break;
    end;
    i := PosEx('/*', Comment, j);
  end;
  Result := Result + Copy(Comment, j + 2, MaxInt);
end;

function FilterDesc(const Def: string): string;
var
  i, j: Integer;
begin
  Result := '';
  i := Pos('<', Def);
  j := 0;
  while i <> 0 do
  begin
    Result := Result + Copy(Def, j + 1, i - j - 1);
    j := PosEx('>', Def, i);
    if j = 0 then
    begin
      j := MaxInt;
      Break;
    end;
    i := PosEx('<', Def, j);
  end;
  Result := Result + Copy(Def, j + 1, MaxInt);
end;

procedure FilterEnum(const Definition, Description: string; F: TCEditorForm; Line: Integer);
var
  Items: TStringList;
  i, p: Integer;
  L, R, S: string;
begin
  Items := TStringList.Create;
  p := Pos('{', Definition);
  if p <> 0 then
  begin
    Inc(p);
    Items.CommaText := Copy(Definition, p, PosEx('}', Definition, p) - p);
    L := '<c:Olive>' + Trim(Copy(Definition, 1, p - 2));
    for i := 0 to Items.Count - 1 do
    begin
      S := Trim(Items[i]);
      p := Pos('=', S);
      if P <> 0 then
      begin
        R := Trim(Copy(S, p + 1, MaxInt));
        S := Trim(Copy(S, 1, p - 1));
      end
      else
        R := '';
      F.SetItem(S, L, R, Description, Line);
    end;
  end;
  Items.Free;

end;

procedure ImportFile(const FileName: string; F: TCEditorForm; GetLine: TOnGetLine);
var
  Lst: TStringList;
  Name, Left, Right, Description, Definition, Tmp: string;
  i, Line: Integer;
begin
  Lst := TStringList.Create;
  Lst.LoadFromFile(FileName);

  Name := Lst.Values['Name'];
  Definition := Lst.Values['Definition'];
  Left := FilterComment(SimplifyType(Lst.Values['Type']) + ' <c:Blue>' + SimplifyLeft(Copy(Definition, 1, Pos(Name, Definition) - 1), Tmp));
  Right := Tmp + Trim(Copy(Definition, Pos(Name, Definition) + Length(Name), MaxInt));

  i := Lst.IndexOf('[Description]');
  if i <> -1 then
    Description := FilterDesc(Lst[i + 1])
  else
    Description := '';

  if Assigned(GetLine) then
    Line := GetLine(Name)
  else
    Line := 0;

  F.SetItem(Name, Left, Right, Description, Line);

  if DoEnum and (Lst.Values['SubType'] = 'Enumeration') then
    FilterEnum(Definition, Description, F, Line);

  Lst.Free;
end;

procedure ImportDir(Dir: string; F: TCEditorForm; GetLine: TOnGetLine);
var
  SR: TSearchRec;
begin
  Dir := IncludeTrailingPathDelimiter(Dir);
  if FindFirst(Dir + '*.*', faAnyFile, SR) = 0 then
  begin
    repeat
      if SR.Name[1] <> '.' then
      begin
        if SR.Attr and faDirectory = faDirectory then
          ImportDir(Dir + SR.Name, F, GetLine)
        else if UpperCase(ExtractFileExt(SR.Name)) = '.HSF' then
          ImportFile(Dir + SR.Name, F, GetLine);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

end.
