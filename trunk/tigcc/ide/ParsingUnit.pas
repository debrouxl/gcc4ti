unit ParsingUnit;

interface

uses
	CalcUnit,
	SysUtils, Classes;

const
	LineIndicator = '#L';
	OffsetIndicator = '#$';
	PatchToken = '$$PATCH';
	ErrorToken = '$$ERROR';
	WarningToken = '$$WARNING';
	ALineIdentifier = '_A_LINE';
	FLineIdentifier = '_F_LINE';
	SParseError = 'Line %d: Parse error before ''%s''';

type
	EFileParseError = class(Exception);

	PBoolean = ^Boolean;

	TLineChangeEvent = procedure(StartLine, Change: Integer) of object;
	TLineContentsEvent = function(StartLine, EndLine: Integer): string of object;

type
	TCalcInt1 = packed record
		Int: Byte;
	end;
	PCalcInt1 = ^TCalcInt1;

	TCalcInt2 = packed record
		Hi,
		Lo: TCalcInt1;
	end;
	PCalcInt2 = ^TCalcInt2;

	TCalcInt4 = packed record
		Hi,
		Lo: TCalcInt2;
	end;
	PCalcInt4 = ^TCalcInt4;

	TPCInt1 = packed record
		Int: Byte;
	end;
	PPCInt1 = ^TPCInt1;

	TPCInt2 = packed record
		Lo,
		Hi: TPCInt1;
	end;
	PPCInt2 = ^TPCInt2;

	TPCInt4 = packed record
		Lo,
		Hi: TPCInt2;
	end;
	PPCInt4 = ^TPCInt4;

function ReadCalcInt(Int: TCalcInt1): Byte; overload;
function ReadCalcInt(Int: TCalcInt2): Word; overload;
function ReadCalcInt(Int: TCalcInt4): LongWord; overload;

procedure WriteCalcInt(Input: Byte; out Int: TCalcInt1); overload;
procedure WriteCalcInt(Input: Word; out Int: TCalcInt2); overload;
procedure WriteCalcInt(Input: LongWord; out Int: TCalcInt4); overload;

function ReadPCInt(Int: TPCInt1): Byte; overload;
function ReadPCInt(Int: TPCInt2): Word; overload;
function ReadPCInt(Int: TPCInt4): LongWord; overload;

procedure WritePCInt(Input: Byte; out Int: TPCInt1); overload;
procedure WritePCInt(Input: Word; out Int: TPCInt2); overload;
procedure WritePCInt(Input: LongWord; out Int: TPCInt4); overload;

type
	TTFRes2 = array [0..1] of Byte;
	TTFRes3 = array [0..2] of Byte;
	TTFRes6 = array [0..5] of Byte;

	TTransferFileHeader = packed record
		Signature: array [0..7] of Char;  // "**TI92P*" or "**TI89**"
		Reserved1: TTFRes2;               // 01 00
		Folder: array [0..7] of Char;     // folder name
		Desc: array [0..39] of Char;      // not used
		Reserved2: TTFRes6;               // 01 00 52 00 00 00
		VarName: array [0..7] of Char;    // variable name
		LinkType: TPCInt1;                // variable link type (0C = string, 1C = other, 21 = program, ...)
		Reserved3: TTFRes3;               // 00 00 00
		FileSize: TPCInt4;                // file size from Signature to CheckSum
		Reserved4: TTFRes6;               // A5 5A 00 00 00 00
	end;
	PTransferFileHeader = ^TTransferFileHeader;

	TCalcVarHeader = packed record
		DataSize: TCalcInt2;              // data size (including Tag)
	end;
	PCalcVarHeader = ^TCalcVarHeader;

	TCalcVarFooter = packed record
		Tag: TCalcInt1;                   // variable tag
	end;
	PCalcVarFooter = ^TCalcVarFooter;

	TTransferFileFooter = packed record
		CheckSum: TPCInt2;                // checksum from DataSize to Tag
	end;
	PTransferFileFooter = ^TTransferFileFooter;

	TCalcOSFooter = packed record
		CheckSum: TCalcInt4;              // encrypted checksum
		SignatureHeader: TCalcInt2;       // 02 0D
		SignatureType: TCalcInt1;         // 40
		Signature: array [0..63] of Byte; // signature, encrypted using TI's private key
	end;
	PCalcOSFooter = ^TCalcOSFooter;

	PBinData = PChar;

const
	MaxCalcAllocBlock = $FFF0;

procedure ParseSFile(Contents: TStringList);
procedure ParseDebugSFile(Contents: TStringList; OnLineChange: TLineChangeEvent; LineContents: TStringList; OnGetLineContents: TLineContentsEvent);
procedure ParsePStarter(const InputFile: string; const OutputFile: string; const PackVar: string);

procedure ParseDebugSFileDisk(Contents: TStringList; const SourceFileName: string);

function GetCalcVarSize(ContentLength: LongWord; const Extension: string = ''): LongWord;
function GetTransferFileSize(ContentLength: LongWord; const Extension: string = ''; OutputBin: Boolean = False): LongWord;
procedure ProduceTransferFile(Output: Pointer; Contents: Pointer; ContentLength: LongWord; CalcDest: TCalcDest; const DestFolder, DestVarName: string; VarTag: Byte; const Extension: string = ''; OutputBin: Boolean = False);
function GetTransferFileExt(CalcDest: TCalcDest; VarTag: Byte; OutputBin: Boolean = False): string;
function GetOSUpgradeFileSize(ContentLength: LongWord; OutputBin: Boolean = True): LongWord;
procedure ProduceOSUpgradeFile(Output: Pointer; Contents: Pointer; ContentLength: LongWord; OutputBin: Boolean = True);
function GetOSUpgradeFileExt(CalcDest: TCalcDest; OutputBin: Boolean = False): string;

var
	ErrorMessageProc: procedure(const Msg: string) = nil;

implementation

uses
	Windows, UtilsDos;

{ General parsing procedures }

procedure ParseSFile(Contents: TStringList);
var
	I,
	P1,
	P2: Integer;
	RegRelative,
	NeedFLine,
	Changed: Boolean;
	L: string;
begin
	RegRelative := False;
	NeedFLine := False;
	with Contents do begin
		for I := 0 to Count - 1 do begin
			L := Strings [I];
			if L = #9'.set __relation,__ld_entry_point_plus_0x8000' then
				RegRelative := True;
			if Pos (FLineIdentifier, L) > 0 then
				NeedFLine := True;
			if RegRelative then begin
				P1 := Pos ('-__relation', L);
				while P1 > 0 do begin
					P2 := PosEx ('_CALL_', L, P1, True);
					if (P2 > 0) and (P2 < P1) and (P1 - P2 <= Length ('_CALL_') + 4) then begin
						System.Delete (L, P1, Length ('-__relation') + 1);
						while (Length (L) > 0) and (L [P1] <> ')') do
							System.Delete (L, P1, 1);
						System.Delete (L, P1, 1);
						Strings [I] := L;
					end;
					P2 := PosEx ('_ER_CODE_', L, P1, True);
					if (P2 > 0) and (P2 < P1) and (P1 - P2 <= Length ('_ER_CODE_') + 5) then begin
						System.Delete (L, P1, Length ('-__relation') + 1);
						while (Length (L) > 0) and (L [P1] <> ')') do
							System.Delete (L, P1, 1);
						System.Delete (L, P1, 1);
						Strings [I] := L;
					end;
					P1 := PosEx ('-__relation', L, P1 + 1);
				end;
			end;
			if (Copy (L, 1, Length (#9'jra _ER_CODE_')) = #9'jra _ER_CODE_') or (Copy (L, 1, Length (#9'jmp _ER_CODE_')) = #9'jmp _ER_CODE_') then begin
				System.Delete (L, 1, Length (#9'jxx _ER_CODE_'));
				if Length (L) <= 4 then
					Strings [I] := #9'.word _A_LINE+' + L;
			end else begin
				Changed := True;
				if (Copy (L, 1, Length (#9'jbsr')) = #9'jbsr') and (Pos ('_CALL_', L) > 0) then
					System.Delete (L, 1 + Length (#9'j'), Length ('b'))
				else if (Copy (L, 1, Length (#9'jra')) = #9'jra') and (Pos ('_CALL_', L) > 0) then begin
					System.Delete (L, 1, Length (#9'jra'));
					L := #9'jmp' + L;
				end else if Copy (L, 1, Length (#9'move.l #__ld_calc_const_')) = #9'move.l #__ld_calc_const_' then
					L [1 + Length (#9'move.')] := 'w'
				else
					Changed := False;
				if NeedFLine and (Copy (L, 1, Length (#9'jsr _ROM_CALL_')) = #9'jsr _ROM_CALL_') then begin
					System.Delete (L, 1, Length (#9'jsr _ROM_CALL_'));
					if Length (L) <= 3 then
						Strings [I] := #9'.word _F_LINE+0x' + L;
				end else begin
					P1 := Pos ('_ROM_CALL_', L);
					while P1 > 0 do begin
						P2 := P1;
						Inc (P1, Length ('_ROM_CALL_'));
						while (P1 <= Length (L)) and (L [P1] in ['0'..'9', 'a'..'z', 'A'..'Z', '_', '+', '-', '*', '/']) do
							Inc (P1);
						while (P1 <= Length (L)) and (L [P1] in [':', 'a'..'z', 'A'..'Z']) do
							System.Delete (L, P1, 1);
						System.Insert (':l', L, P1);
						Inc (P1, Length (':l'));
						if LowerCase (Copy (L, P1, Length ('(%pc)'))) = '(%pc)' then
							System.Delete (L, P1, Length ('(%pc)'))
						else if (LowerCase (Copy (L, P1, Length (',%pc)'))) = ',%pc)') and (P2 - 1 > 0) and (L [P2 - 1] = '(') then begin
							System.Delete (L, P1, Length (',%pc)'));
							System.Delete (L, P2 - 1, 1);
						end;
						Changed := True;
						P1 := PosEx ('_ROM_CALL_', L, P1);
					end;
					P1 := Pos ('__ld_calc_const_', L);
					while P1 > 0 do begin
						P2 := P1;
						Inc (P1, Length ('__ld_calc_const_'));
						while (P1 <= Length (L)) and (L [P1] in ['0'..'9', 'a'..'z', 'A'..'Z', '_', ':']) do
							Inc (P1);
						if LowerCase (Copy (L, P1, Length ('(%pc)'))) = '(%pc)' then begin
							System.Delete (L, P1, Length ('(%pc)'));
							Changed := True;
						end else if (LowerCase (Copy (L, P1, Length (',%pc)'))) = ',%pc)') and (P2 - 1 > 0) and (L [P2 - 1] = '(') then begin
							System.Delete (L, P1, Length (',%pc)'));
							System.Delete (L, P2 - 1, 1);
							Changed := True;
						end;
						P1 := PosEx ('__ld_calc_const_', L, P1);
					end;
					if Changed then
						Strings [I] := L;
				end;
			end;
		end;
	end;
end;

procedure ParseDebugSFile(Contents: TStringList; OnLineChange: TLineChangeEvent; LineContents: TStringList; OnGetLineContents: TLineContentsEvent);
function GetLineContents(StartLine, EndLine: Integer): string;
var
	I: Integer;
begin
	if Assigned (OnGetLineContents) then
		Result := OnGetLineContents (StartLine, EndLine)
	else if Assigned (LineContents) then begin
		Result := '';
		for I := StartLine - 1 to EndLine - 1 do
			if (I >= 0) and (I < LineContents.Count) then
				Insert (LineContents.Strings [I] + #13#10, Result, Length (Result) + 1);
		Delete (Result, Length (Result) - 1, 2);
	end else
		Result := '';
end;
var
	I,
	P,
	LastLine,
	RecontinueLine,
	LastDebugLine,
	CurDebugLine: Integer;
	S: string;
begin
	LastLine := -1;
	LastDebugLine := -1;
	RecontinueLine := -1;
	I := 0;
	with Contents do
		while I <= Count do begin
			if (I < Count) and (Copy (Strings [I], 1, Length (#9'.def'#9)) = #9'.def'#9) then begin
				Delete (I);
				if Assigned (OnLineChange) then
					OnLineChange (I, -1);
				Dec (I);
			end else if (I >= Count) or (Copy (Strings [I], 1, Length (#9'.ln'#9)) = #9'.ln'#9) then begin
				if I < Count then begin
					S := Strings [I];
					System.Delete (S, 1, Length (#9'.ln'#9));
					try
						CurDebugLine := StrToInt (S);
					except
						CurDebugLine := LastDebugLine;
					end;
					System.Insert (LineIndicator, S, 1);
					System.Insert (#9, S, Length (S) + 1);
					Strings [I] := S;
				end else
					CurDebugLine := LastDebugLine + 1;
				if (LastLine >= 0) and (CurDebugLine >= 0) then begin
					S := Strings [LastLine];
					if CurDebugLine < LastDebugLine then begin
						System.Insert (StringReplace (Trim (GetLineContents (LastDebugLine, LastDebugLine)), #9, '', [rfReplaceAll]) + ' ...', S, Length (S) + 1);
						RecontinueLine := LastDebugLine;
					end else begin
						if (RecontinueLine >= 0) and (CurDebugLine >= RecontinueLine) then begin
							System.Insert (StringReplace (Trim (GetLineContents (LastDebugLine, LastDebugLine)) + ' ...', #9, '', [rfReplaceAll]), S, Length (S) + 1);
							RecontinueLine := -1;
						end else
							System.Insert (StringReplace (Trim (GetLineContents (LastDebugLine, CurDebugLine - 1)), #9, '', [rfReplaceAll]), S, Length (S) + 1);
					end;
					repeat
						P := Pos (#10, S);
						if P <= 0 then
							P := Pos (#13, S);
						if P > 0 then
							S [P] := ' ';
					until P <= 0;
					Strings [LastLine] := S;
				end;
				LastLine := I;
				LastDebugLine := CurDebugLine;
			end;
			Inc (I);
		end;
end;

procedure ParsePStarter(const InputFile: string; const OutputFile: string; const PackVar: string);
const
	TempProg = 'TEMPPROG';
var
	I: Integer;
	ObjectFile: TMemoryStream;
	Buf: PChar;
	Len: Integer;
	TempBuf: array [0..Length(TempProg)] of Char;
begin
	TempBuf [Length (TempProg)] := #0;
	ObjectFile := TMemoryStream.Create;
	try
		ObjectFile.LoadFromFile (InputFile);
		Buf := ObjectFile.Memory;
		Len := ObjectFile.Size;
		for I := 0 to Len - Length (TempProg) do
			if UpCase (Buf [I]) = TempProg [1] then begin
				Move (Buf [I], TempBuf, Length (TempProg));
				if UpperCase (AnsiString (TempBuf)) = TempProg then begin
					FillChar (Buf [I], Length (TempProg), 0);
					StrPLCopy (PChar (@(Buf [I])), LowerCase (PackVar), Length (TempProg));
					Break;
				end;
			end;
		ObjectFile.SaveToFile (OutputFile);
	finally
		ObjectFile.Free;
	end;
end;

procedure ParseDebugSFileDisk(Contents: TStringList; const SourceFileName: string);
var
	L: TStringList;
begin
	L := TStringList.Create;
	with L do try
		LoadFromFile (SourceFileName);
		ParseDebugSFile (Contents, nil, L, nil);
	finally
		Free;
	end;
end;

{ Functions to read integers machine-independently }

function ReadCalcInt(Int: TCalcInt1): Byte;
begin
	Result := Int.Int;
end;

function ReadCalcInt(Int: TCalcInt2): Word;
begin
	Result := ReadCalcInt (Int.Hi) shl (SizeOf (Int.Lo) * 8) or ReadCalcInt (Int.Lo);
end;

function ReadCalcInt(Int: TCalcInt4): LongWord;
begin
	Result := ReadCalcInt (Int.Hi) shl (SizeOf (Int.Lo) * 8) or ReadCalcInt (Int.Lo);
end;

function ReadPCInt(Int: TPCInt1): Byte;
begin
	Result := Int.Int;
end;

function ReadPCInt(Int: TPCInt2): Word;
begin
	Result := ReadPCInt (Int.Hi) shl (SizeOf (Int.Lo) * 8) or ReadPCInt (Int.Lo);
end;

function ReadPCInt(Int: TPCInt4): LongWord;
begin
	Result := ReadPCInt (Int.Hi) shl (SizeOf (Int.Lo) * 8) or ReadPCInt (Int.Lo);
end;

{ Functions to write integers machine-independently }

procedure WriteCalcInt(Input: Byte; out Int: TCalcInt1);
begin
	Int.Int := Input;
end;

procedure WriteCalcInt(Input: Word; out Int: TCalcInt2);
begin
	WriteCalcInt (Byte (Input shr (SizeOf (Int.Lo) * 8)), Int.Hi);
	WriteCalcInt (Byte (Input), Int.Lo);
end;

procedure WriteCalcInt(Input: LongWord; out Int: TCalcInt4);
begin
	WriteCalcInt (Word (Input shr (SizeOf (Int.Lo) * 8)), Int.Hi);
	WriteCalcInt (Word (Input), Int.Lo);
end;

procedure WritePCInt(Input: Byte; out Int: TPCInt1);
begin
	Int.Int := Input;
end;

procedure WritePCInt(Input: Word; out Int: TPCInt2);
begin
	WritePCInt (Byte (Input shr (SizeOf (Int.Lo) * 8)), Int.Hi);
	WritePCInt (Byte (Input), Int.Lo);
end;

procedure WritePCInt(Input: LongWord; out Int: TPCInt4);
begin
	WritePCInt (Word (Input shr (SizeOf (Int.Lo) * 8)), Int.Hi);
	WritePCInt (Word (Input), Int.Lo);
end;

{ Calculator file creation functions }

function GetCalcVarSize(ContentLength: LongWord; const Extension: string = ''): LongWord;
begin
	Result := SizeOf (TCalcVarHeader) + ContentLength + SizeOf (TCalcVarFooter);
	if Length (Extension) > 0 then
		Inc (Result, Length (Extension) + 2);
	if Result > MaxCalcAllocBlock then
		Result := 0;
end;

function GetTransferFileSize(ContentLength: LongWord; const Extension: string = ''; OutputBin: Boolean = False): LongWord;
begin
	Result := GetCalcVarSize (ContentLength, Extension);
	if (Result > 0) and (not OutputBin) then
		Inc (Result, SizeOf (TTransferFileHeader) + SizeOf (TTransferFileFooter));
end;

procedure ProduceTransferFile(Output: Pointer; Contents: Pointer; ContentLength: LongWord; CalcDest: TCalcDest; const DestFolder, DestVarName: string; VarTag: Byte; const Extension: string = ''; OutputBin: Boolean = False);
const
	Res1: TTFRes2 = ($01, $00);
	Res2: TTFRes6 = ($01, $00, $52, $00, $00, $00);
	Res3: TTFRes3 = ($00, $00, $00);
	Res4: TTFRes6 = ($A5, $5A, $00, $00, $00, $00);
var
	Size,
	CalcSize: LongWord;
	Header: PTransferFileHeader;
	CalcHeader: PCalcVarHeader;
	CalcFooter: PCalcVarFooter;
	Footer: PTransferFileFooter;
	Data: Pointer;
	VarLinkType: Byte;
	I,
	Sum: Word;
begin
	CalcSize := GetCalcVarSize (ContentLength, Extension);
	Size := GetTransferFileSize (ContentLength, Extension, OutputBin);
	if Size > 0 then begin
		FillChar (Output^, Size, 0);
		if OutputBin then begin
			CalcHeader := Output;
			CalcFooter := Pointer (@(PBinData(Output) [Size - SizeOf (TCalcVarFooter)]));
			Data := @(PBinData(Output) [SizeOf (TCalcVarHeader)]);
		end else begin
			CalcHeader := Pointer (@(PBinData(Output) [SizeOf (TTransferFileHeader)]));
			CalcFooter := Pointer (@(PBinData(Output) [Size - SizeOf (TTransferFileFooter) - SizeOf (TCalcVarFooter)]));
			Data := @(PBinData(Output) [SizeOf (TTransferFileHeader) + SizeOf (TCalcVarHeader)]);
		end;
		WriteCalcInt (CalcSize - SizeOf (TCalcInt2), CalcHeader.DataSize);
		Move (Contents^, Data^, ContentLength);
		if Length (Extension) > 0 then
			StrPCopy (Pointer (@(PBinData(Data) [ContentLength + 1])), Extension);
		WriteCalcInt (VarTag, CalcFooter.Tag);
		if not OutputBin then begin
			Header := Output;
			Footer := Pointer (@(PBinData(Output) [Size - SizeOf (TTransferFileFooter)]));
			with Header^ do begin
				case CalcDest of
					cdTI92:
						Signature := '**TI92**';
					cdTI89:
						Signature := '**TI89**'
					else
						Signature := '**TI92P*';
				end;
				Reserved1 := Res1;
				Reserved2 := Res2;
				Reserved3 := Res3;
				Reserved4 := Res4;
				StrPLCopy (Folder, LowerCase (DestFolder), SizeOf (Folder));
				StrPLCopy (VarName, LowerCase (DestVarName), SizeOf (VarName));
				case VarTag of
					$2D: VarLinkType := $0C;
					$DC: VarLinkType := $12;
					$F3: VarLinkType := $21;
					$F8: VarLinkType := $1C;
					else VarLinkType := 0;
				end;
				WritePCInt (VarLinkType, LinkType);
				WritePCInt (Size, FileSize);
			end;
			Sum := 0;
			for I := 0 to CalcSize - 1 do
				Inc (Sum, Byte (PBinData(CalcHeader) [I]));
			WritePCInt (Sum, Footer.CheckSum);
		end;
	end;
end;

function GetTransferFileExt(CalcDest: TCalcDest; VarTag: Byte; OutputBin: Boolean = False): string;
var
	TypeExt: Char;
begin
	case CalcDest of
		cdTI92:
			Result := '92';
		cdTI89:
			Result := '89';
		cdTI92Plus:
			Result := '9x';
		cdV200:
			Result := 'v2';
		else
			Result := 'xx';
	end;
	case VarTag of
		$2D: TypeExt := 's';
		$DC: TypeExt := 'p';
		$F3: TypeExt := 'z';
		else TypeExt := 'y';
	end;
	if OutputBin then
		Result := '.' + TypeExt + Result
	else
		Result := '.' + Result + TypeExt;
end;

function GetOSUpgradeFileSize(ContentLength: LongWord; OutputBin: Boolean = True): LongWord;
begin
	Result := ContentLength + SizeOf (TCalcOSFooter);
end;

procedure ProduceOSUpgradeFile(Output: Pointer; Contents: Pointer; ContentLength: LongWord; OutputBin: Boolean = True);
var
	CalcFooter: PCalcOSFooter;
begin
	Move (Contents^, Output^, ContentLength);
	CalcFooter := Pointer (@(PBinData(Output) [ContentLength]));
	FillChar (CalcFooter^, SizeOf (TCalcOSFooter), 0);
	with CalcFooter^ do begin
		WriteCalcInt ($020D, SignatureHeader);
		WriteCalcInt ($40,   SignatureType);
	end;
end;

function GetOSUpgradeFileExt(CalcDest: TCalcDest; OutputBin: Boolean = False): string;
begin
	case CalcDest of
		cdTI92:
			Result := '92';
		cdTI89:
			Result := '89';
		cdTI89Titanium:
			Result := '89ti';
		cdTI92Plus:
			Result := '9x';
		cdV200:
			Result := 'v2';
		else
			Result := 'xx';
	end;
	Result := '-' + Result + '.tib';
end;

end.
