unit CodeCompletion;

interface

uses SysUtils, SourceFileUnit, Dialogs, Classes, MainUnit;

const
  DefautDesc = '';
  CCFVersion: Byte = 2;
  //  Zero: Cardinal = 0;

type
  TOnNeedFile = procedure(const FileName: string; out CCFData: string) of object;

  TCompletionItem = class(TObject)
  public
    Left, Right: string;
    Description: string;

    Line: Cardinal;

    // Not Saved
    SourceFileName: string;

    procedure ReadFromStream(F: TStream);
    procedure WriteToStream(F: TStream);
  end;

  TCompletionList = class(TStringList)
  protected
    FIncluded: TStringList;
    FOnNeed: TOnNeedFile;

    function aGetObject(Index: Integer): TCompletionItem;
    procedure aSetObject(Index: Integer; const Value: TCompletionItem);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function AddObject(const Symbol: string; Info: TCompletionItem): Integer; reintroduce;
    procedure Clear; override;
    property Completion[Index: Integer]: TCompletionItem read aGetObject write aSetObject;

    procedure AddCompletion(CCFData: TStream); overload;
    procedure AddCompletion(const CCFData: string); overload;

    // Completion state
    property IncludedFiles: TStringList read FIncluded;

    // When a file is needed
    property OnNeedFile: TOnNeedFile read FOnNeed write FOnNeed;
  end;

function ReadString(F: TStream; var S: string): Boolean;
function WriteString(F: TStream; const S: string): Boolean;

function FileToString(const FileName: string): string;

// Remove comments from a C Source
function GetIdent(const S: string; var aPos: Integer): string;
procedure FilterComment(Src, Dst: TStrings);

// Parse a C Source
function MakeCCF_C(const UnitName: string; Src: TSourceTextSourceFile): string;

// Parse an Header Source
function MakeCCF_H(const UnitName: string; Lines: TStrings; FilterTo: TStrings = nil): string;

implementation

uses StrUtils, UtilsDos;

// Routines

function GetIdent(const S: string; var aPos: Integer): string;
var
  i, l: Integer;
begin
  i := aPos;
  l := Length(S);
  while (i <= l) and (S[i] = ' ') do
    Inc(i);
  while (i <= l) and (S[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '#']) do
    Inc(i);
  Result := Copy(S, aPos, i - aPos);
  aPos := i;
end;

function ReadString(F: TStream; var S: string): Boolean;
var
  B: Byte;
begin
  if F.Read(B, 1) = 1 then
  begin
    Setlength(S, B);
    Result := F.Read(S[1], B) = B;
  end
  else
  begin
    Result := False;
    S := '';
  end;
end;

function WriteString(F: TStream; const S: string): Boolean;
var
  B: Byte;
begin
  B := Length(S);
  Result := False;
  if F.Write(B, 1) = 1 then
    Result := F.Write(S[1], B) = B;
end;

function FileToString(const FileName: string): string;
var
  F: TFileStream;
  c1, c2: Integer;
begin
  Result := '';
  try
    F := TFileStream.Create(FileName, fmOpenRead);
    try
      c1 := F.Size;
      SetLength(Result, c1);
      c2 := F.Read(Result[1], c1);
      if c1 <> c2 then
        SetLength(Result, c2);
    finally
      F.Free;
    end;
  except
  end;
end;

procedure FilterComment(Src, Dst: TStrings);
var
  S, S2: string;
  i, p: Integer;
  InComment: Boolean;
begin
  InComment := False;
  for i := 0 to Src.Count - 1 do
  begin
    S := Trim(Src[i]);

    // Check end of comment
    if InComment then
    begin
      p := Pos('*/', S);
      if p = 0 then
      begin
        Dst.Add('');
        Continue;
      end
      else
      begin
        InComment := False;
        S := Copy(S, p + 2, MaxInt);
      end;
    end;

    // Check line comment
    p := Pos('//', S);
    if p = 1 then
    begin
      Dst.Add('');
      Continue;
    end
    else if p <> 0 then
      S := Copy(S, 1, p - 1);

    // Check start of comment
    p := Pos('/*', S);
    if p <> 0 then
    begin
      S2 := S;
      S := Copy(S, 1, p - 1);
      p := PosEx('*/', S2, p + 2);
      InComment := p = 0;
      if not InComment then
      begin
        InComment := False;
        S := S + Copy(S2, p + 2, MaxInt);
      end;
    end;

    // Add result
    Dst.Add(S);
  end;
end;

procedure ParseIncludes(Src, Dst: TStrings);
var
  i, p: Integer;
  Line: string;
begin
  // find line which starts with '#include'
  for i := 0 to Src.Count - 1 do
  begin
    Line := Trim(Src[i]);
    if AnsiStartsText('#include', Line) then
    begin
      p := Pos('"', Line);
      if P <> 0 then
      begin
        Line := Copy(Line, p + 1, MaxInt);
        Line := Copy(Line, 1, Pos('"', Line) - 1);
        Dst.Add(Trim(Line));
      end
      else
      begin
        p := Pos('<', Line);
        if P <> 0 then
        begin
          Line := Copy(Line, p + 1, MaxInt);
          Line := Copy(Line, 1, Pos('>', Line) - 1);
          Dst.Add(Trim(Line));
        end;
      end;
    end;
  end;
end;

function MakeCCF_C(const UnitName: string; Src: TSourceTextSourceFile): string;
var
  Dst: TMemoryStream;

  // Parse files included in the Source unit
  procedure ParseInc;
  var
    Included: TStringList;
    w: Word;
    i: Integer;
    Source: TStrings;
  begin
    Source := TStringList.Create;
    FilterComment(Src.SourceEditor.Lines, Source);
    Included := TStringList.Create;
    Included.Sorted := True;
    Included.Duplicates := dupIgnore;
    ParseIncludes(Source, Included);
    w := Included.Count;
    // write result to stream
    Dst.Write(w, 2);
    for i := 0 to Included.Count - 1 do
      WriteString(Dst, Included[i]);
    Included.Free;
    Source.Free;
  end;
  // Parse Functions
  procedure ParseFuncs;
  var
    Funcs: TSourceFileFunctions;
    Func: TSourceFileFunction;
    i, p, c, l: Integer;
    S: string;
    Source: TStrings;
  begin
    Source := Src.SourceEditor.Lines;
    Funcs := Src.GetFunctions;
    c := Source.Count;
    for i := 0 to Length(Funcs) - 1 do
    begin
      Func := Funcs[i];
      if Func.Name <> '' then
      begin
        if (Func.ImplementationLine > 0) and (Func.ImplementationLine <= c) then
          l := Func.ImplementationLine - 1
        else if (Func.PrototypeLine > 0) and (Func.PrototypeLine <= c) then
          l := Func.PrototypeLine - 1
        else
          l := -1;
        if l > -1 then
          S := Source[l];
        if S <> '' then
        begin
          p := Pos(Func.Name, S);
          WriteString(Dst, Func.Name);
          Dst.Write(l, 4);
          WriteString(Dst, '<c:Teal>func <c:Blue>' + Trim(Copy(S, 1, p - 1)));
          WriteString(Dst, Trim(Copy(S, p + Length(Func.Name), MaxInt)));
          WriteString(Dst, DefautDesc);
        end;
      end;
    end;
  end;
begin
  Dst := TMemoryStream.Create;
  Dst.Write(CCFVersion, SizeOf(CCFVersion));
  WriteString(Dst, UnitName);
  ParseInc;
  ParseFuncs;
  SetString(Result, PChar(Dst.Memory), Dst.Size);
  Dst.Free;
end;

function MakeCCF_H(const UnitName: string; Lines: TStrings; FilterTo: TStrings = nil): string;
var
  Dst: TMemoryStream;
  Source: TStrings;

  // Parse files included in the Source unit
  procedure ParseInc;
  var
    Included: TStringList;
    w: Word;
    i: Integer;
  begin
    Included := TStringList.Create;
    Included.Sorted := True;
    Included.Duplicates := dupIgnore;
    ParseIncludes(Source, Included);
    w := Included.Count;
    // write result to stream
    Dst.Write(w, 2);
    for i := 0 to Included.Count - 1 do
      WriteString(Dst, Included[i]);
    Included.Free;
  end;

  procedure ParseHeader;
  var
    i, p, c: Integer;
    S, S2, Ident: string;
  begin
    c := Source.Count;
    i := 0;
    while i < c do
    begin
      S := Source[i];
      if AnsiStartsText('#define', S) then
      begin
        p := 9;
        Ident := GetIdent(S, p);
        S2 := Trim(Copy(S, p, MaxInt));
        if S2 <> '' then
        begin
          WriteString(Dst, Ident);
          Dst.Write(i, 4);
          WriteString(Dst, '<c:Green>macro');
          WriteString(Dst, S2);
          WriteString(Dst, '');
        end
      end
      else
      begin
        // terminer cette partie du code :)
      end;
      Inc(i);
    end;
  end;

begin
  Dst := TMemoryStream.Create;
  Dst.Write(CCFVersion, SizeOf(CCFVersion));

  if Assigned(FilterTo) then
    Source := FilterTo
  else
    Source := TStringList.Create;
  Source.Clear;

  FilterComment(Lines, Source);
  WriteString(Dst, UnitName);
  ParseInc;
  ParseHeader;
  SetString(Result, PChar(Dst.Memory), Dst.Size);

  if FilterTo = nil then
    Source.Free;
  Dst.Free;
end;

{ TCompletionList }

constructor TCompletionList.Create;
begin
	inherited;
	CaseSensitive := True;
	Sorted := True;
  Duplicates := dupAccept;
  FIncluded := TStringList.Create;
  FIncluded.Sorted := True;
  FIncluded.Duplicates := dupIgnore;
end;

destructor TCompletionList.Destroy;
begin
  Clear;
  inherited;
end;

function TCompletionList.AddObject(const Symbol: string;
  Info: TCompletionItem): Integer;
begin
  Result := inherited AddObject(Symbol, Info);
end;

procedure TCompletionList.Clear;
var
  i: Integer;
begin
  IncludedFiles.Clear;
  for i := 0 to Count - 1 do
    Objects[i].Free;
  inherited;
end;

function TCompletionList.aGetObject(Index: Integer): TCompletionItem;
begin
  Result := TCompletionItem(Objects[Index]);
end;

procedure TCompletionList.aSetObject(Index: Integer;
  const Value: TCompletionItem);
begin
  Objects[Index] := Value;
end;

procedure TCompletionList.AddCompletion(CCFData: TStream);
var
  aStream: TStream;
  UnitName, S1, S2: string;
  i, w: Word;
  C: TCompletionItem;
  B: Byte;
begin
  if (CCFData.Read(B, 1) <> 1) or (B <> CCFVersion) then
    Exit; // Wrong Version
  if ReadString(CCFData, UnitName) and (IncludedFiles.IndexOf(UnitName) = -1) and (CCFData.Read(w, 2) = 2) then
  begin
    IncludedFiles.Add(UnitName);
    // Check for included files
    for i := 1 to w do
    begin
      if ReadString(CCFData, S1) then
      begin
        if (IncludedFiles.IndexOf(S1) = -1) and Assigned(OnNeedFile) then
        begin
          OnNeedFile(S1, S2);
          if S2 <> '' then
          begin
            aStream := TStringStream.Create(S2);
            AddCompletion(aStream);
            aStream.Free;
          end;
        end;
      end
      else
        Exit;
    end;
    // Add completion info
    while ReadString(CCFData, S1) do
    begin
      C := TCompletionItem.Create;
      C.ReadFromStream(CCFData);
      C.SourceFileName := UnitName;
      if S1 <> '' then
        AddObject(S1, C)
      else
        C.Free;
    end;
  end;
end;

procedure TCompletionList.AddCompletion(const CCFData: string);
var
  S: TStringStream;
begin
  S := TStringStream.Create(CCFData);
  AddCompletion(S);
  S.Free;
end;

{ TCompletionItem }

procedure TCompletionItem.ReadFromStream(F: TStream);
begin
  F.Read(Line, SizeOf(Line));
  ReadString(F, Left);
  ReadString(F, Right);
  ReadString(F, Description);
end;

procedure TCompletionItem.WriteToStream(F: TStream);
begin
  F.Write(Line, SizeOf(Line));
  WriteString(F, Left);
  WriteString(F, Right);
  WriteString(F, Description);
end;

end.

