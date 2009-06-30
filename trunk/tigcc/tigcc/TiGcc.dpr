{
  TIGCC.EXE

  Copyright (C) 2000-2004 Sebastian Reichelt
  Copyright (C) 2005 Kevin Kofler

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

program TiGcc;

uses
  Windows,
  Classes,
  SysUtils,
  ParsingUnit in '..\ide\ParsingUnit.pas',
  CalcUnit in '..\ide\CalcUnit.pas',
  LinkDLLUnit in '..\ide\LinkDLLUnit.pas',
  VersionUnit in '..\ide\VersionUnit.pas',
  UtilsDos in '..\general\UtilsDos.pas';

const
	SErrorPrefix = 'TIGCC.EXE: ';
	SFileNotFound = '%s: No such file or directory';
	SNoFiles = 'No input files';
	SRunFailed = 'Program execution failed';
	SProgNotFound = 'Installation error: Cannot find %s';
	SQuillNotFound = 'File ''Quill.drv'' not found in Bin or Include directory';
	SVariableTooLarge = 'Variable size of %ld bytes is too large, unexpectedly';
	SUnexpectedMismatch = 'Unexpected destination calculator mismatch';
	SLinkDLLNotLoaded = 'Link DLL not loaded';
	SNoNewOSUpgradeFiles = 'Support for ''.??u'' files is not implemented; use ''--outputbin''';

procedure ErrorOutput(const Msg: PChar);
var
	WriteResult: Cardinal;
begin
	WriteFile (GetStdHandle (Std_Error_Handle), Msg^, StrLen (Msg), WriteResult, nil);
end;

procedure Error(const Msg: string = '');
begin
	if Length (Msg) > 0 then begin
		ErrorOutput (SErrorPrefix);
		ErrorOutput (PChar (Msg));
		ErrorOutput (#13#10);
	end;
end;

procedure Fatal(const Msg: string = '');
begin
	raise Exception.Create (Msg);
end;

function LastPos(const Substr, S: string): Integer;
var
	I: Integer;
begin
	Result := 0;
	for I := Length (S) - Length (Substr) + 1 downto 1 do
		if Copy (S, I, Length (SubStr)) = Substr then begin
			Result := I;
			Break;
		end;
end;

function GetShortFileName(FileName: string): string;
var
	F: TSearchRec;
	Found: Boolean;
begin
	Result := '';
	while Length (FileName) > 2 do begin
		FileName := StringReplace (FileName, '/', '\', [rfReplaceAll]);
		Found := FindFirst (FileName, faAnyFile, F) = 0;
		while Found and ((F.Name = '') or (F.Name = '.') or (F.Name = '..')) do
			Found := FindNext (F) = 0;
		if Found then begin
			if F.FindData.cAlternateFileName <> '' then
				Result := F.FindData.cAlternateFileName + '\' + Result
			else if Length (F.Name) > 0 then
				Result := F.Name + '\' + Result
			else
				Result := ExtractFileName (FileName) + '\' + Result;
		end else
			Result := ExtractFileName (FileName) + '\' + Result;
		FindClose (F);
		Delete (FileName, LastPos ('\', FileName), Length (FileName));
	end;
	Result := FileName + '\' + Copy (Result, 1, Length (Result) - 1);
end;

function Enquote(const S: string): string;
begin
	if Pos (' ', S) > 0 then
		Result := '"' + S + '"'
	else
		Result := S;
end;

procedure InsertOptionString(var Dest: string; Options: string);
var
	P: Integer;
begin
	Options := Options + ',';
	while Length (Options) > 0 do begin
		P := Pos (',', Options);
		Dest := Dest + ' ' + Enquote (Copy (Options, 1, P - 1));
		Delete (Options, 1, P);
	end;
end;

procedure OutputText(const S: string);
var
	C: Cardinal;
begin
	WriteFile (GetStdHandle (Std_Output_Handle), PChar(S)^, Length (S), C, nil);
end;

procedure OutputTextLine(const S: string);
begin
	OutputText (S + #13#10);
end;

procedure InsertNewLine;
begin
	OutputTextLine ('');
end;

function CreatePPChar(List: TStrings): PPChar;
var
	I: Integer;
begin
	Result := AllocMem (SizeOf (PPChar) * (List.Count + 1));
	for I := 0 to List.Count - 1 do
		Result [I] := PChar (List.Strings [I]);
end;

procedure FreePPChar(Param: PPChar);
begin
	FreeMem (Param);
end;

procedure TryDeleteFile(const FileName: string);
begin
	if FileExists (FileName) then try
		DeleteFile (FileName);
	except end;
end;

procedure TryDeleteFiles(const FileName, Extension: string);
begin
	TryDeleteFile (FileName + '89' + Extension);
	TryDeleteFile (FileName + '9x' + Extension);
	TryDeleteFile (FileName + 'v2' + Extension);
	TryDeleteFile (FileName + Extension + '89');
	TryDeleteFile (FileName + Extension + '9x');
	TryDeleteFile (FileName + Extension + 'v2');
end;

procedure ParseVarName(const Param: string; var FolderName, VarName: string);
var
	P: Integer;
begin
	P := Pos ('\', Param);
	if P <= 0 then
		P := Pos ('/', Param);
	if P > 0 then begin
		FolderName := LowerCase (Copy (Param, 1, P - 1));
		VarName := LowerCase (Copy (Param, P + 1, Length (Param)));
	end else
		VarName := LowerCase (Param);
end;

var
	PrintCommands: Boolean = False;
	Verbose: Boolean = False;
	Quiet: Boolean = False;

procedure Execute(const CommandLine: string);
var
	TempStr: array [0..1] of Char;
	WriteResult: Cardinal;
	StartupInfo: TStartupInfo;
	ProcessInfo: TProcessInformation;
begin
	if FileExists (Copy (CommandLine, 1, Pos (' ', CommandLine))) then begin
		if PrintCommands then begin
			WriteFile (GetStdHandle (Std_Output_Handle), PChar(CommandLine)^, Length (CommandLine), WriteResult, nil);
			TempStr[0] := #13;
			TempStr[1] := #10;
			WriteFile (GetStdHandle (Std_Output_Handle), TempStr, 2, WriteResult, nil);
		end;
		FillChar (StartupInfo, SizeOf (StartupInfo), 0);
		with StartupInfo do begin
			cb := SizeOf (StartupInfo);
			lpTitle := PChar ('TIGCC');
		end;
		if CreateProcess (nil, PChar (CommandLine), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then begin
			WaitForSingleObject (ProcessInfo.hProcess, Infinite);
			GetExitCodeProcess (ProcessInfo.hProcess, WriteResult);
			CloseHandle (ProcessInfo.hProcess);
			CloseHandle (ProcessInfo.hThread);
			if WriteResult <> 0 then
				Fatal;
		end else
			Fatal (SRunFailed);
	end else
		Fatal (Format (SProgNotFound, [Copy (CommandLine, 1, Pos (' ', CommandLine))]));
end;

procedure LinkLibErrorMessage(FileName, Text: PChar; MessageType: LongInt); cdecl;
begin
	if Assigned (FileName) then begin
		ErrorOutput (FileName);
		ErrorOutput (': ');
	end;
	case MessageType of
		llmtError:
			ErrorOutput ('Error: ');
		llmtWarning:
			ErrorOutput ('Warning: ');
	end;
	ErrorOutput (Text);
	ErrorOutput (#13#10);
end;

type
	TFileRole = (frMain, frData);
	TLinkOutputFile = record
		Data: TMemoryStream;
		Tag: Byte;
		VarExt: string;
		ExeFile,
		OSUpgrade: Boolean;
	end;

var
	LinkOutputFiles: array [TCalcDest, TFileRole] of TLinkOutputFile;
	LinkDebugFile: TLinkOutputFile;

function LinkLibGetOutputFile(var DestFile: TLinkLibDestFile; FileSize, DestCalc, FileRole, FileFormat, FileType: LongInt; Extension: PChar; Executable: WordBool; var EffectiveSize: LongInt): WordBool; cdecl;
var
	CalcDest: TCalcDest;
	CalcFileRole: TFileRole;
begin
	Result := False;
	case FileFormat of
		llffTIOS:
			EffectiveSize := 2 + FileSize + 1;
		llffTIOSUpgrade:
			EffectiveSize := FileSize + SizeOF (TCalcOSFooter);
		llffGDBCOFF:
      begin
			  EffectiveSize := FileSize;
		  	with LinkDebugFile do begin
	  			if not Assigned (Data) then
  					Data := TMemoryStream.Create;
				  Data.Size := FileSize;
		  		DestFile.Data := Data.Memory;
			  end;
	  		Result := True;
  			Exit;
      end;
		else
			Exit;
	end;
	case DestCalc of
		llcdTI89:                     CalcDest := cdTI89;
		llcdTI89 or llcdFlagTitanium: CalcDest := cdTI89Titanium;
		llcdTI92Plus:                 CalcDest := cdTI92Plus;
		llcdV200:                     CalcDest := cdV200;
		llcdTI92:                     CalcDest := cdTI92;
		else
			Exit;
	end;
	case FileRole of
		llfrMain: CalcFileRole := frMain;
		llfrData: CalcFileRole := frData;
		else
			Exit;
	end;
	with LinkOutputFiles [CalcDest, CalcFileRole] do begin
		if not Assigned (Data) then
			Data := TMemoryStream.Create;
		Data.Size := FileSize;
		DestFile.Data := Data.Memory;
		Tag := FileType;
		if Assigned (Extension) then begin
			VarExt := Extension;
			Inc (EffectiveSize, Length (VarExt) + 2);
		end else
			VarExt := '';
		ExeFile := Executable;
		OSUpgrade := (FileFormat = llffTIOSUpgrade);
	end;
	Result := True;
end;

var
	ProgPath: string;
	LinkLibHandle: HModule;
	LinkLibGetInterfaceVersion: TLinkLibGetInterfaceVersion;
	LinkLibLinkFiles: TLinkLibLinkFiles;
	LinkLibCreateArchive: TLinkLibCreateArchive;
	OutputBin: Boolean = False;
	DelFiles: TStringList;

procedure HandleContents(const DestFile, FolderName, VarName, DataFolderName, DataVarName: string; Pack: Boolean; const PackVar: string; CalcDest: TCalcDest);
var
	InputStream,
	OutputStream: TMemoryStream;
	OutputSize: LongWord;
	FileSize: Integer;
	F: file;
	B: Byte;
	PackSwitches: string;
begin
	with LinkOutputFiles [CalcDest, frMain] do
		if Assigned (Data) then begin
			OutputStream := TMemoryStream.Create;
			try
				if OSUpgrade then begin
					OutputStream.Size := GetOSUpgradeFileSize (Data.Size, OutputBin);
					ProduceOSUpgradeFile (OutputStream.Memory, Data.Memory, Data.Size, OutputBin);
					OutputStream.SaveToFile (DestFile + GetOSUpgradeFileExt (CalcDest, OutputBin));
				end else begin
					FileSize := 2 + Data.Size + 1;
					if Pack then begin
						TryDeleteFile ('tempprog.pck');
						AssignFile (F, 'tempprog.bin');
						Rewrite (F, 1);
						B := (FileSize - 2) shr 8;
						BlockWrite (F, B, 1);
						B := (FileSize - 2);
						BlockWrite (F, B, 1);
						BlockWrite (F, Data.Memory^, Data.Size);
						B := Tag;
						BlockWrite (F, B, 1);
						CloseFile (F);
						if Verbose then
							PackSwitches := '-v'
						else if Quiet then
							PackSwitches := '-quiet'
						else
							PackSwitches := '';
						Execute (ProgPath + 'BIN\PACK.EXE ' + PackSwitches + ' tempprog.bin tempprog.pck');
						InputStream := TMemoryStream.Create;
						with InputStream do try
							LoadFromFile ('tempprog.pck');
							OutputSize := GetTransferFileSize (Size, 'ppg', OutputBin);
							if OutputSize > 0 then begin
								OutputStream.Size := OutputSize;
								ProduceTransferFile (OutputStream.Memory, Memory, Size, CalcDest, FolderName, PackVar, $F8, 'ppg', OutputBin);
								OutputStream.SaveToFile (DestFile + GetTransferFileExt (CalcDest, $F8, OutputBin));
							end else
								Fatal (Format (SVariableTooLarge, [IntToStr (Size)]));
						finally
							Free;
						end;
						DelFiles.Add ('tempprog.bin');
						DelFiles.Add ('tempprog.pck');
					end else begin
						OutputSize := GetTransferFileSize (Data.Size, VarExt, OutputBin);
						if OutputSize > 0 then begin
							OutputStream.Size := OutputSize;
							ProduceTransferFile (OutputStream.Memory, Data.Memory, Data.Size, CalcDest, FolderName, VarName, Tag, VarExt, OutputBin);
							OutputStream.SaveToFile (DestFile + GetTransferFileExt (CalcDest, Tag, OutputBin));
						end else
							Fatal (Format (SVariableTooLarge, [IntToStr (FileSize)]));
					end;
				end;
			finally
				OutputStream.Free;
			end;
		end else
			Fatal (SUnexpectedMismatch);
	with LinkOutputFiles [CalcDest, frData] do
		if Assigned (Data) then begin
			FileSize := 2 + Data.Size + 1;
			OutputStream := TMemoryStream.Create;
			try
				OutputSize := GetTransferFileSize (Data.Size, VarExt, OutputBin);
				if OutputSize > 0 then begin
					OutputStream.Size := OutputSize;
					ProduceTransferFile (OutputStream.Memory, Data.Memory, Data.Size, CalcDest, DataFolderName, DataVarName, Tag, VarExt, OutputBin);
					OutputStream.SaveToFile (DestFile + '-data' + GetTransferFileExt (CalcDest, Tag, OutputBin));
				end else
					Fatal (Format (SVariableTooLarge, [IntToStr (FileSize)]));
			finally
				OutputStream.Free;
			end;
		end;
end;

procedure HandleDebugContents(const DestFile: string);
begin
	with LinkDebugFile do
		if Assigned (Data) then
			Data.SaveToFile (DestFile + '.dbg');
end;

procedure CreatePackStarter(const DestFile, StarterFileName, FolderName, VarName, PackVar: string; CalcDests: TCalcDests);
var
	CurCalcDest: TCalcDest;
	ObjectFileNames: array [0..1] of PChar;
	DataVarInfo: TLinkLibDataVarInfo;
	OptimizeInfo: TLinkLibOptimizeInfo;
begin
	ParsePStarter (ProgPath + 'LIB\' + StarterFileName, StarterFileName, PackVar);
	ObjectFileNames [0] := PChar (StarterFileName);
	ObjectFileNames [1] := nil;
	FillChar (DataVarInfo, SizeOf (DataVarInfo), 0);
	FillChar (OptimizeInfo, SizeOf (OptimizeInfo), 0);
	try
		if Assigned (LinkLibLinkFiles) then begin
			LinkLibLinkFiles (@ObjectFileNames, nil, LinkLibErrorMessage, LinkLibGetOutputFile, nil, False, False, False, DataVarInfo, OptimizeInfo, False);
			for CurCalcDest := cdTI89 to cdV200 do
				if CurCalcDest in CalcDests then
					HandleContents (DestFile, FolderName, VarName, '', '', False, '', CurCalcDest);
		end else
			Fatal (SLinkDLLNotLoaded);
		DelFiles.Add (StarterFileName);
	finally
		for CurCalcDest := FirstCalcDest to LastCalcDest do begin
			if Assigned (LinkOutputFiles[CurCalcDest,frMain].Data) then
				LinkOutputFiles[CurCalcDest,frMain].Data.Free;
			if Assigned (LinkOutputFiles[CurCalcDest,frData].Data) then
				LinkOutputFiles[CurCalcDest,frData].Data.Free;
			LinkOutputFiles[CurCalcDest,frMain].Data := nil;
			LinkOutputFiles[CurCalcDest,frData].Data := nil;
		end;
	end;
end;

var
	Assemble: Boolean = True;
	Link: Boolean = True;
	Archive: Boolean = False;
	NativeMode: Boolean = False;
	FlashOSMode: Boolean = False;
	FargoMode: Boolean = False;
	OmitBSSInit: Boolean = False;
	DebugInfo: Boolean = False;
	Pack: Boolean = False;
	StdLib: Boolean = True;
	KeepObjectFiles: Boolean = False;
	SaveTemps: Boolean = False;
	FirstFile: string = '';
	DestFile: string = '';
	VarName: string = '';
	FolderName: string = 'main';
	DataVarName: string = '';
	DataFolderName: string = '';
	PackVarName: string = '';
	GCCFiles,
	A68kFiles,
	ObjectFiles,
	ArchiveFiles: TStringList;
	A68kDest: Boolean = False;
	GCCLine,
	AsLine,
	A68kLine: string;
	DataVar: string = '';
	DataVarInfo: TLinkLibDataVarInfo;
	OptimizeInfo: TLinkLibOptimizeInfo;
	CalcDests: TCalcDests;
	CurCalcDest: TCalcDest;
	PassOn,
	GCCFile: Boolean;
	I: Integer;
	S,
	T: string;
	L: TStringList;
	ObjectFileArray,
	ArchiveFileArray: PPChar;

begin
	ParsingUnit.ErrorMessageProc := Error;

	FillChar (DataVarInfo, SizeOf (DataVarInfo), 0);
	FillChar (OptimizeInfo, SizeOf (OptimizeInfo), 0);
	DataVarInfo.CreateCopy := True;
	DataVarInfo.CopyOnlyIfArchived := True;

	DelFiles := TStringList.Create;
	GCCFiles := TStringList.Create;
	A68kFiles := TStringList.Create;
	ObjectFiles := TStringList.Create;
	ArchiveFiles := TStringList.Create;

	try
		try

			if ParamCount <= 0 then
				Fatal (SNoFiles);

			// Basic command lines
			ProgPath := ExtractFilePath (GetShortFileName (ParamStr (0)));
			GCCLine := ProgPath + 'BIN\GCC.EXE -B' + ProgPath + 'BIN\ -I ' + ProgPath + 'INCLUDE\C\';
			AsLine := ProgPath + 'BIN\AS.EXE -mc68000 -I ' + ProgPath + 'INCLUDE\S\';
			A68kLine := ProgPath + 'BIN\A68K.EXE -i' + ProgPath + 'INCLUDE\ASM\ -g -t';

			// Parsing of command line arguments
			I := 1;
			while I <= ParamCount do begin
				S := ParamStr (I);
				if (Length (S) >= 2) and (S [1] = '-') then begin
					PassOn := False;
					if Length (S) = 2 then begin
						case S [2] of
							'E', 'S': begin
								Link := False;
								Assemble := False;
								if S [2] = 'E' then
									PassOn := True;
							end;
							'c':
								Link := False;
							'q':
								Quiet := True;
							'g':
								DebugInfo := True;
							'x':
								if I < ParamCount then begin
									Inc (I);
									Insert (' ' + S + ' ' + Enquote (ParamStr (I)), GCCLine, Length (GCCLine) + 1);
								end;
							'o':
								if I < ParamCount then begin
									Inc (I);
									DestFile := ParamStr (I);
								end;
							'n':
								if I < ParamCount then begin
									Inc (I);
									ParseVarName (ParamStr (I), FolderName, VarName);
								end;
							'd':
								if I < ParamCount then begin
									Inc (I);
									ParseVarName (ParamStr (I), DataFolderName, DataVarName);
								end;
							else
								PassOn := True;
						end;
					end else begin
						if S = '-bsr' then
							// ignore for compatibility
						else if S = '--output' then begin
							if I < ParamCount then begin
								Inc (I);
								DestFile := ParamStr (I);
							end;
						end else if S = '--varname' then begin
							if I < ParamCount then begin
								Inc (I);
								ParseVarName (ParamStr (I), FolderName, VarName);
							end;
						end else if S = '--data-var' then begin
							if I < ParamCount then begin
								Inc (I);
								ParseVarName (ParamStr (I), DataFolderName, DataVarName);
							end;
						end else if (S = '-outputbin') or (S = '--outputbin') then
							OutputBin := True
						else if (S = '-standalone') or (S = '--standalone') then
							StdLib := False
						else if S = '-ar' then begin
							Archive := True;
							StdLib := False;
						end else if (S = '-keep') or (S = '--keep') then
							KeepObjectFiles := True
						else if (S = '-save-temps') or (S = '--save-temps') then begin
							KeepObjectFiles := True;
							SaveTemps := True;
							Insert (' ' + Enquote (S), GCCLine, Length (GCCLine) + 1);
						end else if (S = '-include') or (S = '--param') or (S = '-isystem') then begin
							if I < ParamCount then begin
								Inc (I);
								Insert (' ' + Enquote (S) + ' ' + Enquote (ParamStr (I)), GCCLine, Length (GCCLine) + 1);
							end;
						end else if (S = '-pack') or (S = '--pack') then begin
							if I < ParamCount then begin
								Pack := True;
								Inc (I);
								PackVarName := LowerCase (Copy (ParamStr (I), 1, 8));
							end;
						end else if S = '-quill' then begin
							if FileExists (ProgPath + 'BIN\Quill.drv') then
								Insert (' -Os -include ' + Enquote (ProgPath + 'BIN\Quill.drv') + ' -x c', GCCLine, Length (GCCLine) + 1)
							else if FileExists (ProgPath + 'INCLUDE\C\Quill.drv') then
								Insert (' -Os -include ' + Enquote (ProgPath + 'BIN\Quill.drv') + ' -x c', GCCLine, Length (GCCLine) + 1)
							else if FileExists (ProgPath + 'INCLUDE\QUILL\Quill.drv') then
								Insert (' -Os -include ' + Enquote (ProgPath + 'BIN\Quill.drv') + ' -x c', GCCLine, Length (GCCLine) + 1)
							else
								Fatal (SQuillNotFound);
						end else if S = '--native' then
							NativeMode := True
						else if S = '--flash-os' then
							FlashOSMode := True
						else if S = '--fargo' then
							FargoMode := True
						else if S = '--remove-unused' then
							OptimizeInfo.RemoveUnused := True
						else if S = '--optimize-relocs' then
							OptimizeInfo.OptimizeRelocs := True
						else if S = '--optimize-code' then begin
							OptimizeInfo.OptimizeNOPs := True;
							OptimizeInfo.OptimizeReturns := True;
							OptimizeInfo.OptimizeBranches := True;
							OptimizeInfo.OptimizeMoves := True;
							OptimizeInfo.OptimizeTests := True;
							OptimizeInfo.OptimizeCalcs := True;
						end else if S = '--optimize-nops' then
							OptimizeInfo.OptimizeNOPs := True
						else if S = '--optimize-returns' then
							OptimizeInfo.OptimizeReturns := True
						else if S = '--optimize-branches' then
							OptimizeInfo.OptimizeBranches := True
						else if S = '--optimize-moves' then
							OptimizeInfo.OptimizeMoves := True
						else if S = '--optimize-tests' then
							OptimizeInfo.OptimizeTests := True
						else if S = '--optimize-calcs' then
							OptimizeInfo.OptimizeCalcs := True
						else if S = '--cut-ranges' then
							OptimizeInfo.CutRanges := True
						else if S = '--reorder-sections' then
							OptimizeInfo.ReorderSections := True
						else if S = '--merge-constants' then
							OptimizeInfo.MergeConstants := True
						else if S = '--omit-bss-init' then
							OmitBSSInit := True
						else if Copy (S, 1, 4) = '-Wa,' then
							InsertOptionString (AsLine, Copy (S, 5, Length (S)))
						else if Copy (S, 1, 4) = '-WA,' then
							InsertOptionString (A68kLine, Copy (S, 5, Length (S)))
						else if Copy (S, 1, Length ('--data-var-copy=')) = '--data-var-copy=' then begin
							Delete (S, 1, Length ('--data-var-copy='));
							if S = 'never' then begin
								DataVarInfo.CreateCopy := False;
								DataVarInfo.CopyOnlyIfArchived := False;
							end else if S = 'always' then begin
								DataVarInfo.CreateCopy := True;
								DataVarInfo.CopyOnlyIfArchived := False;
							end else if S = 'archived' then begin
								DataVarInfo.CreateCopy := True;
								DataVarInfo.CopyOnlyIfArchived := True;
							end;
						end else
							PassOn := True;
					end;
					if PassOn then begin
						if Copy (S, 1, 2) = '-v' then begin
							PrintCommands := True;
							if S <> '-v0' then begin
								Verbose := True;
								Quiet := False;
								Insert (' ' + Enquote (S), GCCLine, Length (GCCLine) + 1);
							end;
						end else begin
							if (S = '--help') or (S = '--version') then begin
								Assemble := False;
								Link := False;
								if S = '--version' then
									OutputTextLine ('tigcc.exe built for GCC4TI Version ' + TIGCCLongVersion);
							end;
							Insert (' ' + Enquote (S), GCCLine, Length (GCCLine) + 1);
						end;
					end;
				end else begin
					if LowerCase (ExtractFileExt (S)) = '.a' then begin
						if (not FileExists (S)) and FileExists (ProgPath + 'LIB\' + S) then
							S := ProgPath + 'LIB\' + S;
						ArchiveFiles.Add (S);
					end else begin
						if FileExists (S) then begin
							ObjectFiles.Add (ChangeFileExt (S, '.o'));
							if Length (FirstFile) <= 0 then
								FirstFile := Copy (S, 1, Pos (ExtractFileExt (S), S) - 1);
							if LowerCase (ExtractFileExt (S)) <> '.o' then begin
								if LowerCase (ExtractFileExt (S)) = '.asm' then
									A68kFiles.Add (S)
								else begin
									Insert (' ' + Enquote (S), GCCLine, Length (GCCLine) + 1);
									GCCFiles.Add (S);
								end;
								if Link then
									DelFiles.Add (ChangeFileExt (S, '.o'));
							end;
						end else
							Error (Format (SFileNotFound, [S]));
					end;
				end;
				Inc (I);
			end;
			if FlashOSMode and (not OutputBin) then
				Fatal (SNoNewOSUpgradeFiles);
			if Length (DataVarName) > 0 then begin
				if Length (DataFolderName) > 0 then
					DataVar := DataFolderName + '\' + DataVarName
				else begin
					DataVar := DataVarName;
					DataFolderName := FolderName;
				end;
				DataVarInfo.VarName := PChar (DataVar);
			end;
			if OptimizeInfo.CutRanges or Archive then begin
				Insert (' --all-relocs', AsLine, Length (AsLine) + 1);
				Insert (' -a', A68kLine, Length (A68kLine) + 1);
			end;
			if OptimizeInfo.OptimizeReturns or Archive then begin
				Insert (' --keep-locals', AsLine, Length (AsLine) + 1);
				Insert (' -d', A68kLine, Length (A68kLine) + 1);
			end;
			if StdLib then begin
				if FlashOSMode then
					ArchiveFiles.Add (ProgPath + 'LIB\flashos.a')
				else if FargoMode then
					ArchiveFiles.Add (ProgPath + 'LIB\fargo.a')
				else
					ArchiveFiles.Add (ProgPath + 'LIB\tigcc.a');
			end;
			if (not Link) or KeepObjectFiles then
				with DelFiles do
					for I := Count - 1 downto 0 do
						if LowerCase (ExtractFileExt (Strings [I])) = '.o' then
							Delete (I);

			// Execution of GCC.exe (compiling and assembling)
			if (GCCFiles.Count > 0) or (Pos ('--', GCCLine) > 0) then begin
				if (Pos (' -S', GCCLine) <= 0) and (Pos (' -E', GCCLine) <= 0) then
					Insert (' -S', GCCLine, Length (GCCLine) + 1);
				if DebugInfo then begin
					Insert (' -gdwarf-2 -g3 -fasynchronous-unwind-tables', GCCLine, Length (GCCLine) + 1);
					Insert (' --gdwarf2', AsLine, Length (AsLine) + 1);
				end;
				if Length (DataVar) > 0 then
					Insert (' -mno-merge-sections', GCCLine, Length (GCCLine) + 1);
				if FlashOSMode then
					Insert (' -DFLASH_OS', GCCLine, Length (GCCLine) + 1)
				else if FargoMode then
					Insert (' -DFARGO', GCCLine, Length (GCCLine) + 1);
				if (GCCFiles.Count = 1) and (A68kFiles.Count <= 0) and (Length (DestFile) > 0) and (not Assemble) then
					Insert (' -o ' + Enquote (DestFile), GCCLine, Length (GCCLine) + 1);
				Execute (GCCLine);
				for I := 0 to GCCFiles.Count - 1 do begin
					S := GCCFiles.Strings [I];
					GCCFile := LowerCase (ExtractFileExt (S)) <> '.s';
					T := ChangeFileExt (StringReplace (S, '/', '\', [rfReplaceAll]), '.s');
					if GCCFile then
						T := ExtractFileName (T);
					if FileExists (T) then begin
						if GCCFile then begin
							if Assemble then begin
								DelFiles.Add (T);
							end;
							L := TStringList.Create;
							with L do try
								LoadFromFile (T);
								ParseSFile (L);
								SaveToFile (T);
							finally
								Free;
							end;
						end;
						if Assemble then begin
							if (not Link) and (GCCFiles.Count = 1) and (Length (DestFile) > 0) then
								Execute (AsLine + ' ' + Enquote (T) + ' -o ' + Enquote (DestFile))
							else
								Execute (AsLine + ' ' + Enquote (T) + ' -o ' + Enquote (ChangeFileExt (S, '.o')));
						end;
					end;
				end;
				if Link and Verbose then
					InsertNewLine;
			end;

			// Execution of A68k.exe (assembling)
			if A68kFiles.Count > 0 then begin
				if DebugInfo then
					Insert (' -d', A68kLine, Length (A68kLine) + 1);
				if Quiet then
					Insert (' -q', A68kLine, Length (A68kLine) + 1);
				if (A68kFiles.Count = 1) and (GCCFiles.Count <= 0) and (Length (DestFile) > 0) and (not Link) then begin
					Insert (' ' + Enquote ('-o' + DestFile), A68kLine, Length (A68kLine) + 1);
					A68kDest := True;
				end;
				while A68kFiles.Count > 0 do begin
					Execute (A68kLine + ' ' + Enquote (A68kFiles.Strings [0]));
					A68kFiles.Delete (0);
					if Link and not Quiet then begin
						InsertNewLine;
						InsertNewLine;
					end;
				end;
			end;

			// Execution of Link.dll (linking and archiving)
			if Link and (ObjectFiles.Count > 0) then begin
				LinkLibHandle := LoadLibrary (PChar (ProgPath + 'BIN\LINK.DLL'));
				if LinkLibHandle <> 0 then begin
					try
						LinkLibGetInterfaceVersion := GetProcAddress (LinkLibHandle, 'GetInterfaceVersion');
						if Assigned (LinkLibGetInterfaceVersion) and (LinkLibGetInterfaceVersion = LinkLibCurInterfaceVersion) then begin
							LinkLibLinkFiles := GetProcAddress (LinkLibHandle, 'LinkFiles');
							LinkLibCreateArchive := GetProcAddress (LinkLibHandle, 'CreateArchive');
							if Length (DestFile) <= 0 then
								DestFile := ChangeFileExt (FirstFile, '');
							if Archive then begin
								if Pos ('.', DestFile) <= 0 then
									DestFile := DestFile + '.a';
								ObjectFileArray := CreatePPChar (ObjectFiles);
								try
									if LinkLibCreateArchive (PChar (DestFile), ObjectFileArray, LinkLibErrorMessage, True) <> 0 then
										Fatal;
								finally
									FreePPChar (ObjectFileArray);
								end;
							end else begin
								TryDeleteFiles (DestFile + '.', 'z');
								TryDeleteFiles (DestFile + '-titanium.', 'z');
								TryDeleteFiles (DestFile + '.', 'y');
								TryDeleteFiles (DestFile + '-data.', 'y');
								TryDeleteFiles (DestFile + '-', '.tib');
								if Length (VarName) <= 0 then
									VarName := DestFile;
								ObjectFileArray := CreatePPChar (ObjectFiles);
								try
									ArchiveFileArray := CreatePPChar (ArchiveFiles);
									try
										for CurCalcDest := FirstCalcDest to LastCalcDest do begin
											LinkOutputFiles[CurCalcDest,frMain].Data := nil;
											LinkOutputFiles[CurCalcDest,frData].Data := nil;
										end;
										LinkDebugFile.Data := nil;
										try
											if LinkLibLinkFiles (ObjectFileArray, ArchiveFileArray, LinkLibErrorMessage, LinkLibGetOutputFile, nil, NativeMode, FlashOSMode, FargoMode, DataVarInfo, OptimizeInfo, OmitBSSInit) <> 0 then
												Fatal;
											for CurCalcDest := FirstCalcDest to LastCalcDest do
												if Assigned (LinkOutputFiles[CurCalcDest,frMain].Data) then begin
													Include (CalcDests, CurCalcDest);
													HandleContents (DestFile, FolderName, VarName, DataFolderName, DataVarName, Pack and (CurCalcDest <> cdTI92), PackVarName, CurCalcDest);
												end;
											if Assigned (LinkDebugFile.Data) then begin
												HandleDebugContents (DestFile);
											end;
										finally
											for CurCalcDest := FirstCalcDest to LastCalcDest do begin
												if Assigned (LinkOutputFiles[CurCalcDest,frMain].Data) then
													LinkOutputFiles[CurCalcDest,frMain].Data.Free;
												if Assigned (LinkOutputFiles[CurCalcDest,frData].Data) then
													LinkOutputFiles[CurCalcDest,frData].Data.Free;
												LinkOutputFiles[CurCalcDest,frMain].Data := nil;
												LinkOutputFiles[CurCalcDest,frData].Data := nil;
											end;
											if Assigned (LinkDebugFile.Data) then
												LinkDebugFile.Data.Free;
											LinkDebugFile.Data := nil;
										end;
									finally
										FreePPChar (ArchiveFileArray);
									end;
								finally
									FreePPChar (ObjectFileArray);
								end;
								if Pack then
									CreatePackStarter (DestFile, 'pstarter.o', FolderName, VarName, PackVarName, CalcDests - [cdTI92]);
								if Verbose then
									with OptimizeInfo do begin
										OutputTextLine ('Program Variable Size:                      ' + IntToStr (ProgramSize) + ' Bytes');
										if DataSize > 0 then
											OutputTextLine ('Data Variable Size:                         ' + IntToStr (DataSize) + ' Bytes');
										if BSSSize > 0 then
											OutputTextLine ('BSS Size:                                   ' + IntToStr (BSSSize) + ' Bytes');
										OutputTextLine ('Absolute Relocs:                            ' + IntToStr (RelocCount));
										OutputTextLine ('Natively Emitted Relocs:                    ' + IntToStr (NativeRelocCount));
										if OptimizeBranchesResult > 0 then begin
											if OptimizeBranches then
												S := 'Relocs Saved by Branch Optimization:        '
											else
												S := 'Relocs Savable by Branch Optimization:      ';
											OutputTextLine (S + IntToStr (OptimizeBranchesResult));
										end;
										if OptimizeMovesResult > 0 then begin
											if OptimizeMoves then
												S := 'Relocs Saved by Move Optimization:          '
											else
												S := 'Relocs Savable by Move Optimization:        ';
											OutputTextLine (S + IntToStr (OptimizeMovesResult));
										end;
										if OptimizeTestsResult > 0 then begin
											if OptimizeTests then
												S := 'Relocs Saved by Test Optimization:          '
											else
												S := 'Relocs Savable by Test Optimization:        ';
											OutputTextLine (S + IntToStr (OptimizeTestsResult));
										end;
										if OptimizeCalcsResult > 0 then begin
											if OptimizeCalcs then
												S := 'Relocs Saved by Calculation Optimization:   '
											else
												S := 'Relocs Savable by Calculation Optimization: ';
											OutputTextLine (S + IntToStr (OptimizeCalcsResult));
										end;
										if UseFLineJumpsResult > 0 then begin
											if UseFLineJumps or Use4ByteFLineJumps then
												S := 'Relocs Saved by F-Line Jumps:               '
											else
												S := 'Relocs Savable by F-Line Jumps:             ';
											OutputTextLine (S + IntToStr (UseFLineJumpsResult));
										end;
										if CutRangesResult > 0 then begin
											if CutRanges then
												S := 'Space Saved by Range-Cutting:               '
											else
												S := 'Space Savable by Range-Cutting:             ';
											OutputTextLine (S + IntToStr (CutRangesResult) + ' Bytes');
										end;
										if NearAssemblyResult > 0 then
											OutputTextLine ('Space Savable by Using GNU Assembler ''-l'' Switch: ' + IntToStr (NearAssemblyResult) + ' Bytes');
									end;
							end;
						end else
							Fatal (SLinkDLLNotLoaded);
					finally
						FreeLibrary (LinkLibHandle);
					end;
				end else
					Fatal (Format (SProgNotFound, ['LINK.DLL']));
			end;

			if not SaveTemps then
				with DelFiles do
					for I := 0 to Count - 1 do
						TryDeleteFile (Strings [I]);

		finally
			ArchiveFiles.Free;
			ObjectFiles.Free;
			A68kFiles.Free;
			GCCFiles.Free;
			DelFiles.Free;
		end;

	except
		on E: Exception do begin
			if not (E is EAbort) then
				Error (E.Message);
			Halt (1);
		end;
	end;
end.
