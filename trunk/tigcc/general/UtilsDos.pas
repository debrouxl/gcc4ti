unit UtilsDos;

interface

uses
	WinProcs, WinTypes, SysUtils, Classes;

type
	TByteFile = file of Byte;
	TDrive    = 'A'..'Z';
	TDrives   = set of TDrive;

function UCase (NormalChar: Char): Char;
function LCase (NormalChar: Char): Char;
function IsNormalChar (Ch: Char): Boolean;
function ChangeText (var ChText: string; ToChange, ToInsert: string): Byte;
function LastPos (const Substr, S: string): Integer;
function PosEx (const Substr, S: string; StartPos: Integer; Backwards: Boolean = False): Integer;
function FirstNonWhiteSpace (const S: string): Integer;

function GetDriveDescription (Drive: TDrive): string;
function DiskInDrive (Drive: TDrive): Boolean;
function FileExistsWithCase (const FileName: string): Boolean;
function DirExists (const Directory: string): Boolean;
function DirExistsWithCase (const Directory: string): Boolean;
function GetFileSize (const FileName: string): Integer;
function GetSubDir (const Directory: string): string;
function GetHigherDir (const Directory: string): string;
function GetLowerName (const N: string): string;
function SizeToStr (Size: Int64): string;
function AttrToStr (Attr: Integer): string;
procedure CreatePath (const Directory: string);
procedure CreatePathFor (const FileName: string);
procedure RemovePath (const Directory: string; const StopAt: string = '');
procedure RemovePathFor (const FileName: string; const StopAt: string = '');
procedure DelTree (const Directory: string);
procedure DelAllFiles (const Directory: string);

procedure PCSpeakerPlay (Frequency: Word);
procedure PCSpeakerStop;

procedure SetDate (a: Word; m, d: Byte);
procedure SetTime (h, m, s, hs: Byte);

procedure WrCStr (var F: TByteFile; Str: string);
procedure WrPStr (var F: TByteFile; Str: string; Len: Byte);
procedure WrVStr (var F: TByteFile; Str: string);
procedure WrSInt (var F: TByteFile; Value: ShortInt);
procedure WrByte (var F: TByteFile; Value: Byte);
procedure WrNInt (var F: TByteFile; Value: SmallInt);
procedure WrWord (var F: TByteFile; Value: Word);
procedure RdCStr (var F: TByteFile; var Str: string; Len: Byte);
procedure RdPStr (var F: TByteFile; var Str: string; Len: Byte);
procedure RdVStr (var F: TByteFile; var Str: string);
procedure RdSInt (var F: TByteFile; var Value: ShortInt);
procedure RdByte (var F: TByteFile; var Value: Byte);
procedure RdNInt (var F: TByteFile; var Value: SmallInt);
procedure RdWord (var F: TByteFile; var Value: Word);

type
	TFileAttribute = (atDirectory, atArchive, atReadOnly, atHidden, atSystem);
	// atDirectory only for internal use and property request
	TFileAttributes = set of TFileAttribute;

const
	atStd: TFileAttributes = [atArchive, atReadOnly];
	atAll: TFileAttributes = [atArchive, atReadOnly, atHidden, atSystem];

type
	TFileReferences = class;

	TFileReference = class(TPersistent)
	private
		FOwner: TFileReferences;
		FSearchRec: TSearchRec;
		function GetCreationTime: TDateTime;
		function GetFileAttr: TFileAttributes;
		function GetFileName: string;
		function GetFileSize: Integer;
		function GetFullName: string;
		function GetFullShortName: string;
		function GetLastAccessTime: TDateTime;
		function GetModificationTime: TDateTime;
		function GetShortFileName: string;
	public
	published
		property Owner: TFileReferences read FOwner;
		property RecData: TSearchRec read FSearchRec;
		property FullName: string read GetFullName;
		property FileName: string read GetFileName;
		property FullShortName: string read GetFullShortName;
		property ShortFileName: string read GetShortFileName;
		property CreationTime: TDateTime read GetCreationTime;
		property ModificationTime: TDateTime read GetModificationTime;
		property LastAccessTime: TDateTime read GetLastAccessTime;
		property FileSize: Integer read GetFileSize;
		property FileAttr: TFileAttributes read GetFileAttr;
	end;

	TFileReferences = class(TPersistent)
	private
		FList: TList;
		FDirectory: string;
		FWildCard: string;
		FSearchAttr: TFileAttributes;
		FSearchTime: TDateTime;
		function GetItem(ItemIndex: Integer): TFileReference;
		function GetCount: Integer;
	public
		constructor Create;
		destructor Destroy; override;
		function Add(const F: TSearchRec): Integer; virtual;
		procedure Delete(Index: Integer); virtual;
		procedure Clear;
		procedure SearchForFiles(FileName: string; FileAttr: TFileAttributes);
		procedure SearchForDirs(const ParentDir: string; FileAttr: TFileAttributes);
		procedure CopyToStrings(const S: TStrings);
		procedure CopyFullNamesToStrings(const S: TStrings);
		property Items[ItemIndex: Integer]: TFileReference read GetItem;
		property Count: Integer read GetCount;
	published
		property Directory: string read FDirectory;
		property WildCard: string read FWildCard;
		property SearchAttr: TFileAttributes read FSearchAttr;
		property SearchTime: TDateTime read FSearchTime;
	end;

function WinFileTimeToDateTime(WinFileTime: TFileTime): TDateTime;
function WinFileAttrToFileAttr(WinFileAttr: Integer): TFileAttributes;
function FileAttrToWinFileAttr(FileAttr: TFileAttributes): Integer;

procedure AddBackslash(var Dir: string);
function WithBackslash(const Dir: string): string;
function WithoutBackslash(const Dir: string): string;
function WithoutExt(const FileName: string): string;

implementation

function UCase;
begin
	Result := UpCase (NormalChar);
	case Result of
		'ä': Result := 'Ä';
		'ö': Result := 'Ö';
		'ü': Result := 'Ü';
	end;
end;

function LCase;
begin
	Result := LowerCase (NormalChar) [1];
	case Result of
		'Ä': Result := 'ä';
		'Ö': Result := 'ö';
		'Ü': Result := 'ü';
	end;
end;

function IsNormalChar;
begin
	Result := UCase (Ch) <> LCase (Ch);
end;

function LastPos;
var
	I: Integer;
begin
	Result := 0;
	for I := Length (S) - Length (Substr) + 1 downto 1 do
		if Copy (S, I, Length (Substr)) = Substr then begin
			Result := I;
			Break;
		end;
end;

function PosEx;
var
	I: Integer;
begin
	Result := 0;
	if Backwards then begin
		for I := StartPos downto 1 do
			if Copy (S, I, Length (Substr)) = Substr then begin
				Result := I;
				Break;
			end;
	end else begin
		for I := StartPos to Length (S) - Length (Substr) + 1 do
			if Copy (S, I, Length (Substr)) = Substr then begin
				Result := I;
				Break;
			end;
	end;
end;

function FirstNonWhiteSpace;
var
	I: Integer;
begin
	Result := Length (S) + 1;
	for I := 1 to Length (S) do
		if not (S [I] in [' ', #9]) then begin
			Result := I;
			Break;
		end;
end;

function GetDriveDescription;
var
	F: Text;
	R,
	N: array [0..255] of Char;
	V: DWord;
begin
	Result := '';
	if FileExists (Drive + ':\DiskID.clk') then begin
		AssignFile (F, Drive + ':\DiskID.clk');
		Reset (F);
		ReadLn (F, Result);
		CloseFile (F);
	end else begin
		StrPCopy (R, Drive + ':\');
		if GetVolumeInformation (R, N, 255, nil, V, V, nil,	0) then
			Result := StrPas (N);
	end;
	if Result = '' then
		Result := Drive + ':\';
	Result := GetLowerName (Result);
end;

function DiskInDrive;
var
	V: DWord;
begin
	Result := GetDiskFreeSpace (PChar (Drive + ':\'), V, V, V, V);
end;

procedure CreatePath;
var
	SeedPath: string;
	CurPath: string;
begin
	SeedPath := WithBackslash (Directory);
	CurPath := '';
	while Pos ('\', SeedPath) > 0 do begin
		CurPath := CurPath + Copy (SeedPath, 1, Pos ('\', SeedPath));
		Delete (SeedPath, 1, Pos ('\', SeedPath));
		if Length (CurPath) > 3 then
			if not DirExists (CurPath) then try
				MkDir (CurPath);
			except end;
	end;
end;

procedure CreatePathFor;
begin
	CreatePath (ExtractFilePath (FileName));
end;

procedure RemovePath;
var
	Dir: string;
	S: TSearchRec;
	Found: Boolean;
begin
	Dir := WithBackslash (Directory);
	if (Length (Dir) > 3) and (Dir <> WithBackslash (StopAt)) and DirExists (Dir) then begin
		Found := False;
		if FindFirst (Dir + '*', faAnyFile, S) = 0 then begin
			repeat
				if (S.Name <> '.') and (S.Name <> '..') then
					Found := True;
			until (FindNext (S) <> 0) or Found;
		end;
		FindClose (S);
		if not Found then try
			RmDir (Dir);
			RemovePath (GetHigherDir (Directory), StopAt);
		except end;
	end;
end;

procedure RemovePathFor;
begin
	RemovePath (ExtractFilePath (FileName), StopAt);
end;

procedure DelTree;
var
	CurDelDir: string;
begin
	CurDelDir := WithBackslash (Directory);
	repeat
		while GetSubDir (CurDelDir) <> '' do begin
			CurDelDir := CurDelDir + GetSubDir
				(CurDelDir) + '\';
		end;
		DelAllFiles (CurDelDir);
		if Length (CurDelDir) < 4 then
			RmDir (CurDelDir)
		else
			RmDir (Copy (CurDelDir, 1, Length (CurDelDir) - 1));
		CurDelDir := GetHigherDir (CurDelDir);
	until CurDelDir = GetHigherDir (Directory);
end;

function FileExistsWithCase;
var
	S: TSearchRec;
begin
	Result := (FindFirst (FileName, faAnyFile, S) = 0);
	if Result and (S.Name <> ExtractFileName (FileName)) then
		Result := False;
	FindClose (S);
end;

function DirExists;
var
	S: TSearchRec;
begin
	Result := (FindFirst (WithoutBackslash (Directory), faDirectory or faHidden or faSysFile, S) = 0);
	FindClose (S);
end;

function DirExistsWithCase;
var
	S: TSearchRec;
begin
	Result := (FindFirst (WithoutBackslash (Directory), faDirectory, S) = 0);
	if Result and (S.Name <> ExtractFileName (WithoutBackslash (Directory))) then
		Result := False;
	FindClose (S);
end;

function GetFileSize;
var
	S: TSearchRec;
begin
	if FindFirst (FileName, faAnyFile, S) = 0 then
		Result := S.Size
	else
		Result := 0;
	FindClose (S);
end;

function GetSubDir;
var
	SR1:  TSearchRec;
	GoOn: Boolean;
begin
	GoOn := True;
	if FindFirst (Directory + '*.*', $10, SR1) = 0 then begin
		while GoOn and (((SR1.Attr and $10) = 0) or (SR1.Name = '.') or (SR1.Name = '..')) do begin
			if FindNext (SR1) <> 0 then begin
				SR1.Name := '';
				GoOn := False;
			end;
		end;
		GetSubDir := SR1.Name
	end else
		GetSubDir := '';
	FindClose (SR1);
end;

function GetHigherDir;
begin
	Result := ExtractFilePath (WithoutBackslash (Directory));
end;

procedure DelAllFiles;
var
	SR1: TSearchRec;
begin
	if FindFirst (Directory + '*.*', $20, SR1) = 0 then begin
		SysUtils.DeleteFile (Directory + SR1.Name);
		while FindNext (SR1) = 0 do
			SysUtils.DeleteFile (Directory + SR1.Name);
	end;
	FindClose (SR1);
end;

function GetLowerName;
var
	I: Integer;
begin
	Result := N;
	if (Length (Result) > 0) and ((Result = UpperCase (Result)) or (Result = LowerCase (Result))) then begin
		Result := UpperCase (Result);
		for I := 2 to Length (Result) do begin
			if IsNormalChar (Result [I-1]) then
				Result [I] := LCase (Result [I]);
		end;
	end;
end;

function SizeToStr;
var
	Sz: Real;
	M:  Byte;
begin
	M := 0;
	Sz := Size;
	while Sz >= 1000 do begin
		Sz := Sz / 1000;
		Inc (M);
	end;
	Result := FloatToStr (Sz);
	if Length (Result) > 4 then
		Result := Copy (Result, 1, 4);
	if Result [Length (Result)] = DecimalSeparator then
		Delete (Result, Length (Result), 1);
	case M of
		0: Result := Result + ' B';
		1: Result := Result + ' KB';
		2: Result := Result + ' MB';
		3: Result := Result + ' GB';
		4: Result := Result + ' TB';
		else Result := Result + '...';
	end;
end;

function AttrToStr;
begin
	Result := '';
	if (Attr and faArchive)  <> 0 then Result := Result + 'A';
	if (Attr and faReadOnly) <> 0 then Result := Result + 'R';
	if (Attr and faHidden)   <> 0 then Result := Result + 'H';
	if (Attr and faSysFile)  <> 0 then Result := Result + 'S';
end;

function ChangeText;
var
	TxtPs: Byte;
begin
	TxtPs := 0;
	while Pos (ToChange, ChText) <> 0 do begin
		TxtPs := Pos (ToChange, ChText);
		Delete (Chtext, TxtPs, Length (ToChange));
		Insert (ToInsert, ChText, TxtPs);
	end;
	ChangeText := TxtPs;
end;

procedure PCSpeakerPlay (Frequency: Word); assembler;
asm
	Mov  BX,  Frequency
	Mov  AX,  $34DD
	Mov  DX,  $0012
	Cmp  DX,  BX
	Jnb  @EndOfProc
	Div  BX
	Mov  BX,  AX
	In   AL,  $61
	Test AL,  $03
	Jne  @NearlyEndOfProc
	Or   AL,  $03
	Out  $61, AL
	Mov  AL,  $B6
	Out  $43, AL
@NearlyEndOfProc:
	Mov  AL,  BL
	Out  $42, AL
	Mov  AL,  BH
	Out  $42, AL
@EndOfProc:
end;

procedure PCSpeakerStop; assembler;
asm
	In   AL,  $61
	And  AL,  $FC
	Out  $61, AL
end;

procedure SetDate; assembler;
asm
	Mov  CX,  a
	Mov  DH,  m
	Mov  DL,  d
	Mov  AH,  $2B
	Int  $21
end;

procedure SetTime; assembler;
asm
	Mov  CH,  h
	Mov  CL,  m
	Mov  DH,  s
	Mov  DL,  hs
	Mov  AH,  $2D
	Int  $21
end;

procedure WrCStr;
var
	VLp1,
	VHlp: Byte;
begin
	for VLp1 := 1 to Length (Str) do begin
		VHlp := Ord (Str [VLp1]);
		Write (F, VHlp);
	end;
end;

procedure WrPStr;
var
	VLp1,
	VHlp,
	VHp2: Byte;
begin
	VHlp := Length (Str);
	if VHlp > Len then VHlp := Len;
	Write (F, VHlp);
	for VLp1 := 1 to VHlp do begin
		VHp2 := Ord (Str [VLp1]);
		Write (F, VHp2);
	end;
	VHp2 := 0;
	for VLp1 := VHlp + 1 to Len do Write (F, VHp2);
end;

procedure WrVStr;
var
	VLp1,
	VHlp: Byte;
begin
	VHlp := Length (Str);
	Write (F, VHlp);
	for VLp1 := 1 to VHlp do begin
		VHlp := Ord (Str [VLp1]);
		Write (F, VHlp);
	end;
end;

procedure WrSInt;
var
	VHlp: Byte;
begin
	VHlp := Byte (Value);
	Write (F, VHlp);
end;

procedure WrByte;
var
	VHlp: Byte;
begin
	VHlp := Value;
	Write (F, VHlp);
end;

procedure WrNInt;
var
	VHlp: Byte;
begin
	VHlp := Hi (Word (Value));
	Write (F, VHlp);
	VHlp := Lo (Word (Value));
	Write (F, VHlp);
end;

procedure WrWord;
var
	VHlp: Byte;
begin
	VHlp := Hi (Value);
	Write (F, VHlp);
	VHlp := Lo (Value);
	Write (F, VHlp);
end;

procedure RdCStr;
var
	VLp1,
	VHlp: Byte;
begin
	Str := '';
	for VLp1 := 1 to Len do begin
		Read (F, VHlp);
		Str := Str + Chr (VHlp);
	end;
end;

procedure RdPStr;
var
	FPos,
	VLp1,
	VHlp: Byte;
begin
	FPos := FilePos (F);
	Str := '';
	Read (F, VHlp);
	for VLp1 := 1 to VHlp do begin
		Read (F, VHlp);
		Str := Str + Chr (VHlp);
	end;
	Seek (F, FPos + Len + 1);
end;

procedure RdVStr;
var
	VLp1,
	VHlp: Byte;
begin
	Str := '';
	Read (F, VHlp);
	for VLp1 := 1 to VHlp do begin
		Read (F, VHlp);
		Str := Str + Chr (VHlp);
	end;
end;

procedure RdSInt;
var
	VHlp: Byte;
begin
	Read (F, VHlp);
	Value := ShortInt (VHlp);
end;

procedure RdByte;
begin
	Read (F, Value);
end;

procedure RdNInt;
var
	VHlp1: Byte;
	VHlp2: Word;
begin
	Read (F, VHlp1);
	VHlp2 := VHlp1 * 256;
	Read (F, VHlp1);
	Inc (VHlp2, VHlp1);
	Value := SmallInt (VHlp2);
end;

procedure RdWord;
var
	VHlp: Byte;
begin
	Read (F, VHlp);
	Value := VHlp * 256;
	Read (F, VHlp);
	Inc (Value, VHlp);
end;

function WinFileTimeToDateTime(WinFileTime: TFileTime): TDateTime;
var
	Time: Integer;
	LocalFileTime: TFileTime;
begin
	FileTimeToLocalFileTime(WinFileTime, LocalFileTime);
	FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
	Result := FileDateToDateTime (Time);
end;

function WinFileAttrToFileAttr(WinFileAttr: Integer): TFileAttributes;
begin
	Result := [];
	if (WinFileAttr and faDirectory) <> 0 then
		Include (Result, atDirectory);
	if (WinFileAttr and faArchive) <> 0 then
		Include (Result, atArchive);
	if (WinFileAttr and faReadOnly) <> 0 then
		Include (Result, atReadOnly);
	if (WinFileAttr and faHidden) <> 0 then
		Include (Result, atHidden);
	if (WinFileAttr and faSysFile) <> 0 then
		Include (Result, atSystem);
end;

function FileAttrToWinFileAttr(FileAttr: TFileAttributes): Integer;
begin
	Result := 0;
	if atDirectory in FileAttr then
		Result := Result or faDirectory;
	if atArchive in FileAttr then
		Result := Result or faArchive;
	if atReadOnly in FileAttr then
		Result := Result or faReadOnly;
	if atHidden in FileAttr then
		Result := Result or faHidden;
	if atSystem in FileAttr then
		Result := Result or faSysFile;
end;

procedure AddBackslash(var Dir: string);
begin
	if (Length (Dir) > 0) and (Dir [Length (Dir)] <> '\') then
		Dir := Dir + '\';
end;

function WithBackslash(const Dir: string): string;
begin
	if (Length (Dir) > 0) and (Dir [Length (Dir)] <> '\') then
		Result := Dir + '\'
	else
		Result := Dir;
end;

function WithoutBackslash(const Dir: string): string;
begin
	if (Length (Dir) > 0) and (Length (Dir) > 3) and (Dir [Length (Dir)] = '\') then
		Result := Copy (Dir, 1, Length (Dir) - 1)
	else
		Result := Dir;
end;

function WithoutExt(const FileName: string): string;
begin
	Result := ChangeFileExt (FileName, '');
end;

{ TFileReference }

function TFileReference.GetCreationTime: TDateTime;
begin
	Result := WinFileTimeToDateTime (FSearchRec.FindData.ftCreationTime);
end;

function TFileReference.GetFileAttr: TFileAttributes;
begin
	Result := WinFileAttrToFileAttr (FSearchRec.Attr);
end;

function TFileReference.GetFileName: string;
begin
	Result := FSearchRec.Name;
end;

function TFileReference.GetFileSize: Integer;
begin
	Result := FSearchRec.Size;
end;

function TFileReference.GetFullName: string;
begin
	Result := FOwner.Directory + FSearchRec.Name;
end;

function TFileReference.GetFullShortName: string;
var
	TmpDir: string;
	F: TSearchRec;
begin
	Result := ShortFileName;
	TmpDir := FOwner.Directory;
	Delete (TmpDir, Length (TmpDir), 1);
	while Length (TmpDir) > 2 do begin
		if FindFirst (TmpDir, faDirectory or faReadOnly or faHidden or faSysFile or faArchive, F) = 0 then begin
			if F.FindData.cAlternateFileName = '' then
				Result := F.Name + '\' + Result
			else
				Result := F.FindData.cAlternateFileName + '\' + Result
		end else
			Result := ExtractFileName (TmpDir) + '\' + Result;
		FindClose (F);
		Delete (TmpDir, LastPos ('\', TmpDir), Length (TmpDir));
	end;
	Result := TmpDir + '\' + Result;
end;

function TFileReference.GetLastAccessTime: TDateTime;
begin
	Result := WinFileTimeToDateTime (FSearchRec.FindData.ftLastAccessTime);
end;

function TFileReference.GetModificationTime: TDateTime;
begin
	Result := WinFileTimeToDateTime (FSearchRec.FindData.ftLastWriteTime);
end;

function TFileReference.GetShortFileName: string;
begin
	Result := FSearchRec.FindData.cAlternateFileName;
	if Result = '' then
		Result := FSearchRec.Name;
end;

{ TFileReferences }

function TFileReferences.Add(const F: TSearchRec): Integer;
var
	R: TFileReference;
begin
	R := TFileReference.Create;
	with R do begin
		FOwner := Self;
		FSearchRec := F;
	end;
	Result := FList.Add (Pointer (R));
end;

procedure TFileReferences.Clear;
begin
	while Count > 0 do
		Delete (Count - 1);
end;

procedure TFileReferences.CopyFullNamesToStrings(const S: TStrings);
var
	I: Integer;
begin
	S.BeginUpdate;
	for I := 0 to Count - 1 do
		S.AddObject (Items[I].FullName, Items [I]);
	S.EndUpdate;
end;

procedure TFileReferences.CopyToStrings(const S: TStrings);
var
	I,
	P: Integer;
	N: string;
begin
	S.BeginUpdate;
	for I := 0 to Count - 1 do begin
		N := Items[I].FileName;
		P := LastPos ('.', N);
		if P < 1 then
			P := Length (N)
		else
			Dec (P);
		S.AddObject (GetLowerName (Copy (N, 1, P)), Items [I]);
	end;
	S.EndUpdate;
end;

constructor TFileReferences.Create;
begin
	inherited;
	FList := TList.Create;
end;

procedure TFileReferences.Delete(Index: Integer);
begin
	with FList do begin
		TFileReference(Items[Index]).Free;
		Delete (Index);
	end;
end;

destructor TFileReferences.Destroy;
begin
	inherited;
	Clear;
	FList.Free;
end;

function TFileReferences.GetCount: Integer;
begin
	Result := FList.Count;
end;

function TFileReferences.GetItem(ItemIndex: Integer): TFileReference;
begin
	Result := TFileReference (FList.Items [ItemIndex]);
end;

procedure TFileReferences.SearchForDirs(const ParentDir: string;
	FileAttr: TFileAttributes);
var
	Mask: string;
begin
	if Length (ParentDir) > 0 then
		Mask := WithBackslash (ParentDir)
	else
		Mask := '*.*';
	SearchForFiles (Mask, FileAttr + [atDirectory]);
end;

procedure TFileReferences.SearchForFiles(FileName: string;
	FileAttr: TFileAttributes);
var
	F: TSearchRec;
	R: Integer;
begin
	Clear;
	if (FileName = '') or (FileName [Length (FileName)] = '\') then
		FileName := FileName + '*.*';
	FileName := ExpandFileName (FileName);
	FDirectory := ExtractFilePath (FileName);
	FWildCard := ExtractFileName (FileName);
	FSearchAttr := FileAttr;
	R := FindFirst (FileName, FileAttrToWinFileAttr (FileAttr), F);
	while R = 0 do begin
		if (not ((atDirectory in FileAttr) and ((faDirectory and F.Attr) = 0))) and (F.Name [1] <> '.') and ((WinFileAttrToFileAttr (F.Attr) - FileAttr) = []) then
			Add (F);
		R := FindNext (F);
	end;
	FindClose (F);
	FSearchTime := Now;
end;

end.
