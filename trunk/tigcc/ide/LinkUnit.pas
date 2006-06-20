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

unit LinkUnit;

{	Uncomment to enable logging of all bytes sent and received to
	C:\Linklog.txt. }
(* {$DEFINE LOGLINK} *)

interface

uses
	CalcUnit,
	Windows, Classes;

{ Calculator-specific Types }
type
	{ Enumeration Types }
	TLinkPortType = (lpCOM);
	TLinkCableType = (lcBlack, lcGray);
	TCalcVarTypeID = Byte;

	TLinkPort = record
		PortType: TLinkPortType;
		PortNumber: Cardinal;
	end;

	TLinkConnection = record
		Port: TLinkPort;
		CableType: TLinkCableType;
		CalcType: TCalcDest;
		Open: Boolean;
		PortHandle: THandle;
	end;

	{	Variable or Folder Name }
	TCalcVarName = array [0..9] of Char;

	{	Variable Structure }
	TCalcVar = record
		CalcType: TCalcDest;
		Folder,
		Name: TCalcVarName;
		TypeID: TCalcVarTypeID;
		Size: LongWord;
	end;

	{	Progress callback function
		Should return False if cancelled, True otherwise }
	TProgressCallBack = function(ID: Pointer; Progress: PDWord): Boolean; 

	{	Variable list callback function
		This type is called by GetVarList on every item found }
	TVarListCallBack = function(ID: Pointer; const NewVar: TCalcVar): Boolean; 

{ Exported Functions }
function CreateConnection(var Connection: TLinkConnection): Boolean;
function OpenConnection(var Connection: TLinkConnection): Boolean;
procedure CloseConnection(var Connection: TLinkConnection);

function CalcReady(var Connection: TLinkConnection): Boolean; 

function SendAck(var Connection: TLinkConnection): Boolean; 
function SendWait(var Connection: TLinkConnection): Boolean; 
function SendByte(var Connection: TLinkConnection; Value: Byte; CheckSum: PWord = nil): Boolean; 
function SendWord(var Connection: TLinkConnection; Value: Word; CheckSum: PWord = nil): Boolean; 
function SendLongWord(var Connection: TLinkConnection; Value: LongWord; CheckSum: PWord = nil): Boolean;
function SendString(var Connection: TLinkConnection; Value: PChar; CheckSum: PWord = nil): Boolean;
function SendKey(var Connection: TLinkConnection; Key: Char): Boolean;  overload;
function SendKey(var Connection: TLinkConnection; Key: Word): Boolean;  overload;
function SendKeys(var Connection: TLinkConnection; const Keys: string): Boolean;
function SendVar(var Connection: TLinkConnection; Folder, Name: PChar; Buffer: Pointer; BufSize: Cardinal; Progress: PDWord = nil; ProgressCallBack: TProgressCallBack = nil; ID: Pointer = nil): Boolean;
function SendFile(var Connection: TLinkConnection; Folder, Name: PChar; const FileName: string; Progress: PDWord = nil; ProgressCallBack: TProgressCallBack = nil; ID: Pointer = nil): Boolean;
function ExecuteHomeLine(var Connection: TLinkConnection; const Line: string): Boolean;

function CheckFileFormat(var Connection: TLinkConnection; const FileName: string; Folder: PChar = nil; Name: PChar = nil; Size: PWord = nil): Boolean;

function WaitForAck(var Connection: TLinkConnection; LastResult: PByte = nil): Boolean;
function CalcWaiting(var Connection: TLinkConnection; LastResult: PByte = nil): Boolean;
function ReceiveByte(var Connection: TLinkConnection; out Value: Byte; CheckSum: PWord = nil): Boolean;
function ReceiveWord(var Connection: TLinkConnection; out Value: Word; CheckSum: PWord = nil): Boolean;
function ReceiveLongWord(var Connection: TLinkConnection; out Value: LongWord; CheckSum: PWord = nil): Boolean;

function GetCalcType(var Connection: TLinkConnection): Boolean;

implementation

uses
	SysUtils, Registry, ComServ;

const
	SendRetry = 1;
	SendTimeout = 500;
	ReceiveTimeout = 2000;
	SleepBetweenReady = 500;
	FirstReadyTimeout = 5000;
	NormalReadyTimeout = 5000;
	SendVarTimeout = 2000;
	ProgressInterval = 100;
	NonBlockingRead = False;
	NonBlockingWrite = True;

{$IFDEF LOGLINK}
var
	LogFile: Text;
	LogSend: Boolean;
{$ENDIF}

var
	OpenedOnce: Boolean = False;

function GetPortState(Handle: THandle): Cardinal; forward;
procedure SetPortState(Handle: THandle; State: Cardinal); forward;
function GetPortName(const Port: TLinkPort): string; forward;
function IsValidAck(Value: Byte): Boolean; forward;

{	Creates a connection, opening the link port, but does not transfer any
	data. }
function CreateConnection(var Connection: TLinkConnection): Boolean; 
var
	NewHandle: THandle;
	Settings: TDCB;
	Timeouts: TCommTimeouts;
begin
	Result := False;
	NewHandle := CreateFile (PChar (GetPortName (Connection.Port) + ':'), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if NewHandle = INVALID_HANDLE_VALUE then
		Exit;
	if Connection.CableType = lcBlack then begin
		if not SetCommBreak (NewHandle) then begin
			CloseHandle (NewHandle);
			Exit;
		end;
	end else begin
		if not SetupComm (NewHandle, 1024, 1024) then begin
			CloseHandle (NewHandle);
			Exit;
		end;
		FillChar (Settings, SizeOf (Settings), 0);
		with Settings do begin
			DCBLength := SizeOf (Settings);
			BaudRate := CBR_9600;
			ByteSize := 8;
			StopBits := ONESTOPBIT;
			Flags := $00001001;
		end;
		if not SetCommState (NewHandle, Settings) then begin
			CloseHandle (NewHandle);
			Exit;
		end;
		FillChar (Timeouts, SizeOf (Timeouts), 0);
		with Timeouts do begin
			ReadIntervalTimeout := MaxDWord;
			if not NonBlockingRead then
				ReadTotalTimeoutConstant := ReceiveTimeout;
			if not NonBlockingWrite then
				WriteTotalTimeoutConstant := SendTimeOut;
		end;
		if not SetCommTimeouts (NewHandle, Timeouts) then begin
			CloseHandle (NewHandle);
			Exit;
		end;
	end;
	Connection.PortHandle := NewHandle;
	Result := True;
end;

{	Opens a connection if it is not already open. }
function OpenConnection(var Connection: TLinkConnection): Boolean; 
var
	Timeout,
	StartTime: Cardinal;
begin
	if Connection.Open then
		Result := True
	else begin
		{$IFDEF LOGLINK}
			AssignFile (LogFile, 'C:\Linklog.txt');
			ReWrite (LogFile);
			Write (LogFile, ' - Calculator Link Log - ');
			LogSend := False;
		{$ENDIF}
		Connection.Open := True;
		if OpenedOnce then
			Timeout := NormalReadyTimeout
		else
			Timeout := FirstReadyTimeout;
		StartTime := GetTickCount;
		repeat
			Result := CalcReady (Connection);
			if not Result then
				Sleep (SleepBetweenReady);
		until Result or (GetTickCount - StartTime > Timeout);
		OpenedOnce := True;
		if not Result then
			Connection.Open := False;
	end;
end;

{	Closes a connection if it is open, and frees the handle. }
procedure CloseConnection(var Connection: TLinkConnection);
begin
	if Connection.CableType = lcBlack then
		SetPortState (Connection.PortHandle, 0);
	CloseHandle (Connection.PortHandle);
	{$IFDEF LOGLINK}
		CloseFile (LogFile);
	{$ENDIF}
end;

procedure ResetConnection(var Connection: TLinkConnection);
begin
	if Connection.CableType = lcBlack then
		SetPortState (Connection.PortHandle, 0);
end;

function WaitForAck(var Connection: TLinkConnection; LastResult: PByte = nil): Boolean;
var
	TempResult: Byte;
begin
	Result :=
		ReceiveByte (Connection, TempResult) and IsValidAck (TempResult) and
		ReceiveByte (Connection, TempResult) and (TempResult = $56) and
		ReceiveByte (Connection, TempResult) and
		ReceiveByte (Connection, TempResult);
	if Assigned (LastResult) then
		LastResult^ := TempResult;
end;

function CalcReady(var Connection: TLinkConnection): Boolean;
var
	TempResult: Byte;
begin
	Result := False;
	if not
		(SendByte (Connection, $09) and
		SendByte (Connection, $68) and
		SendByte (Connection, $00) and
		SendByte (Connection, $00) and
		WaitForAck (Connection) and
		SendByte (Connection, $08) and
		SendByte (Connection, $68) and
		SendByte (Connection, $00) and
		SendByte (Connection, $00) and
		ReceiveByte (Connection, TempResult) and IsValidAck (TempResult)) then
		Exit;
	case TempResult of
		$98, $89: Connection.CalcType := cdTI89;
		$88:      Connection.CalcType := cdTI92Plus;
		else Exit;
	end;
	Result :=
		ReceiveByte (Connection, TempResult) and (TempResult = $56) and
		ReceiveByte (Connection, TempResult) and
		ReceiveByte (Connection, TempResult);
end;

function CalcWaiting(var Connection: TLinkConnection; LastResult: PByte = nil): Boolean;
var
	TempResult: Byte;
begin
	Result :=
		ReceiveByte (Connection, TempResult) and IsValidAck (TempResult) and
		ReceiveByte (Connection, TempResult) and (TempResult = $09) and
		ReceiveByte (Connection, TempResult) and
		ReceiveByte (Connection, TempResult);
	if Assigned (LastResult) then
		LastResult^ := TempResult;
end;

function SendAck(var Connection: TLinkConnection): Boolean;
begin
	Result :=
		SendByte (Connection, $08) and
		SendByte (Connection, $56) and
		SendByte (Connection, $00) and
		SendByte (Connection, $00);
end;

function SendWait(var Connection: TLinkConnection): Boolean; 
begin
	Result :=
		SendByte (Connection, $08) and
		SendByte (Connection, $09) and
		SendByte (Connection, $00) and
		SendByte (Connection, $00);
end;

function SendByte(var Connection: TLinkConnection; Value: Byte; CheckSum: PWord = nil): Boolean;
var
	I: Cardinal;
	StartTime: Cardinal;
	RetryCount: Integer;
	State: Cardinal;
begin
	Result := OpenConnection (Connection);
	if Result then begin
		{$IFDEF LOGLINK}
			if not LogSend then begin
				WriteLn (LogFile);
				WriteLn (LogFile, 'Sending:');
				LogSend := True;
			end;
			Write (LogFile, ' ' + IntToHex (Value, 2));
		{$ENDIF}
		if Connection.CableType = lcGray then
			Result := WriteFile (Connection.PortHandle, Value, 1, I, nil)
		else begin
			StartTime := GetTickCount;
			for I := 0 to 7 do begin
				for RetryCount := 0 to SendRetry do begin
					if ((Value and (1 shl I)) <> 0) then
						SetPortState (Connection.PortHandle, 2)
					else
						SetPortState (Connection.PortHandle, 1);
					repeat
						State := GetPortState (Connection.PortHandle);
						if GetTickCount - StartTime > SendTimeout then
							Break;
					until State = 0;
					if State = 0 then
						break;
				end;
				if State <> 0 then begin
					Result := False;
					Exit;
				end;
				for RetryCount := 0 to SendRetry do begin
					SetPortState (Connection.PortHandle, 3);
					repeat
						State := GetPortState (Connection.PortHandle);
						if GetTickCount - StartTime > SendTimeout then
							Break;
					until State = 3;
					if State = 3 then
						break;
				end;
				if State <> 3 then begin
					Result := False;
					Exit;
				end;
			end;
		end;
		if Assigned (CheckSum) then
			Inc (CheckSum^, Value);
	end;
end;

function SendWord(var Connection: TLinkConnection; Value: Word; CheckSum: PWord = nil): Boolean; 
begin
	Result :=
		SendByte (Connection, Value, CheckSum) and
		SendByte (Connection, (Value shr (SizeOf (Value) * 4)), CheckSum);
end;

function SendLongWord(var Connection: TLinkConnection; Value: LongWord; CheckSum: PWord = nil): Boolean; 
begin
	Result :=
		SendWord (Connection, Value, CheckSum) and
		SendWord (Connection, (Value shr (SizeOf (Value) * 4)), CheckSum);
end;

function SendString(var Connection: TLinkConnection; Value: PChar; CheckSum: PWord = nil): Boolean;
begin
	Result := True;
	while Value [0] <> #0 do begin
		Result := Result and SendByte (Connection, Byte (Value [0]), CheckSum);
		Value := @(Value[1]);
	end;
end;

function SendKey(var Connection: TLinkConnection; Key: Char): Boolean;  overload;
begin
	Result :=
		SendByte (Connection, $09) and
		SendByte (Connection, $87) and
		SendWord (Connection, Byte (Key)) and
		WaitForAck (Connection);
end;

function SendKey(var Connection: TLinkConnection; Key: Word): Boolean;  overload;
begin
	Result :=
		SendByte (Connection, $09) and
		SendByte (Connection, $87) and
		SendWord (Connection, Key) and
		WaitForAck (Connection);
end;

function SendKeys(var Connection: TLinkConnection; const Keys: string): Boolean;
var
	I: Integer;
begin
	Result := OpenConnection (Connection);
	for I := 1 to Length (Keys) do begin
		Result := SendKey (Connection, Keys [I]);
		if not Result then
			Break;
	end;
end;

function SendVar(var Connection: TLinkConnection; Folder, Name: PChar; Buffer: Pointer; BufSize: Cardinal; Progress: PDWord = nil; ProgressCallBack: TProgressCallBack = nil; ID: Pointer = nil): Boolean;
var
	I: Cardinal;
	Buf: PByteArray;
	FileType: Byte;
	FileSize,
	BlockSize: Word;
	Sum: Word;
	TempProgress: DWord;
	StartTime: Cardinal;
begin
	Result := BufSize >= 88;
	if not Result then
		Exit;
	if not Assigned (Progress) then
		Progress := @TempProgress;
	Buf := Buffer;
	FileType := Buf [72];
	Buf [72] := 0;
	FileSize := Buf [86] shl 8 + Buf [87] + 2;
	if not Assigned (Folder) then
		Folder := @(Buf[10]);
	Folder [8] := #0;
	if not Assigned (Name) then
		Name := @(Buf[64]);
	Name [8] := #0;
	{$IFDEF LOGLINK}
		WriteLn (LogFile);
		WriteLn (LogFile);
		WriteLn (LogFile, AnsiString (Folder), '\', AnsiString (Name), ': ', FileSize, ' (', IntToHex (FileSize, 4), ') Bytes');
	{$ENDIF}
	Sum := 0;
	StartTime := GetTickCount;
	repeat until CalcReady (Connection) or (GetTickCount - StartTime > SendVarTimeout);
	Result :=
		SendKey (Connection, 264) and
		SendKey (Connection, 264) and
		SendKey (Connection, 277) and
		SendByte (Connection, $08) and
		SendByte (Connection, $06) and
		SendWord (Connection, StrLen (Folder) + StrLen (Name) + 8) and
		SendLongWord (Connection, FileSize, @Sum) and
		SendByte (Connection, FileType, @Sum) and
		SendByte (Connection, StrLen (Folder) + StrLen (Name) + 1, @Sum) and
		SendString (Connection, Folder, @Sum) and
		SendString (Connection, '\', @Sum) and
		SendString (Connection, Name, @Sum) and
		SendByte (Connection, $00) and
		SendWord (Connection, Sum) and
		WaitForAck (Connection) and
		CalcWaiting (Connection) and
		SendAck (Connection) and
		SendByte (Connection, $08) and
		SendByte (Connection, $15) and
		SendWord (Connection, FileSize + 4) and
		SendLongWord (Connection, 0);
	if not Result then
		Exit;
	BlockSize := FileSize - 2;
	Sum := 0;
	Result :=
		SendByte (Connection, BlockSize and $FF00 shr 8, @Sum) and
		SendByte (Connection, BlockSize and $00FF, @Sum);
	Inc (Progress^, 2);
	if not Result then
		Exit;
	for I := 0 to BlockSize - 1 do begin
		if (I < BufSize) and SendByte (Connection, Buf [I + 88], @Sum) then
			Inc (Progress^)
		else begin
			Result := False;
			Break;
		end;
		if ((I + 2) mod ProgressInterval = 0) and Assigned (ProgressCallBack) and (not ProgressCallBack (ID, Progress)) then begin
			Result := False;
			Break;
		end;
	end;
	if not Result then
		Exit;
	Result :=
		SendWord (Connection, Sum) and
		WaitForAck (Connection) and
		SendByte (Connection, $08) and
		SendByte (Connection, $92) and
		SendByte (Connection, $00) and
		SendByte (Connection, $00) and
		WaitForAck (Connection);
end;

function SendFile(var Connection: TLinkConnection; Folder, Name: PChar; const FileName: string; Progress: PDWord = nil; ProgressCallBack: TProgressCallBack = nil; ID: Pointer = nil): Boolean;
var
	FHandle: HFile;
	Buffer: Pointer;
	Size,
	Read: Cardinal;
begin
	FHandle := CreateFile (PChar (FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
	Result := FHandle <> 0;
	if not Result then
		Exit;
	Size := GetFileSize (FHandle, nil);
	Buffer := AllocMem (Size);
	Result := Assigned (Buffer);
	if not Result then begin
		CloseHandle (FHandle);
		Exit;
	end;
	Result := ReadFile (FHandle, Buffer^, Size, Read, nil);
	CloseHandle (FHandle);
	if Result then
		Result := SendVar (Connection, Folder, Name, Buffer, Size, Progress, ProgressCallBack, ID);
	FreeMem (Buffer);
end;

function CheckFileFormat(var Connection: TLinkConnection; const FileName: string; Folder: PChar = nil; Name: PChar = nil; Size: PWord = nil): Boolean;
var
	FHandle: HFile;
	Buffer: array [0..4] of Char;
	Read: Cardinal;
begin
	FHandle := CreateFile (PChar (FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
	Result :=
		(FHandle <> 0) and
		ReadFile (FHandle, Buffer, 4, Read, nil);
	if not Result then
		Exit;
	Buffer [4] := #0;
	Result := Buffer = '**TI';
	if not Result then
		Exit;
	if Assigned (Folder) then begin
		SetFilePointer (FHandle, 10, nil, FILE_BEGIN);
		Result := ReadFile (FHandle, Folder^, 8, Read, nil) and Result;
		Folder [8] := #0;
	end;
	if Assigned (Name) then begin
		SetFilePointer (FHandle, 64, nil, FILE_BEGIN);
		Result := ReadFile (FHandle, Name^, 8, Read, nil) and Result;
		Name [8] := #0;
	end;
	if Assigned (Size) then begin
		SetFilePointer (FHandle, 86, nil, FILE_BEGIN);
		Result := ReadFile (FHandle, Buffer, 2, Read, nil) and Result;
		Size^ := Byte (Buffer [0]) shl 8 + Byte (Buffer [1]) + 2;
	end;
	if FHandle <> 0 then
		CloseHandle (FHandle);
end;

function ExecuteHomeLine(var Connection: TLinkConnection; const Line: string): Boolean;
begin
	Result :=
		SendKey (Connection, 264) and
		SendKey (Connection, 264) and
		SendKey (Connection, 277) and
		SendKey (Connection, 263) and
		SendKey (Connection, 263) and
		SendKeys (Connection, Line) and
		SendKey (Connection, 13);
end;

function ReceiveByte(var Connection: TLinkConnection; out Value: Byte; CheckSum: PWord = nil): Boolean;
var
	I,
	State: Cardinal;
	StartTime: Cardinal;
begin
	Value := 0;
	Result := OpenConnection (Connection);
	if Result then begin
		if Connection.CableType = lcGray then begin
			StartTime := GetTickCount;
			repeat
				Result := ReadFile (Connection.PortHandle, Value, 1, I, nil);
			until Result or (GetTickCount - StartTime > ReceiveTimeout);
		end else begin
			StartTime := GetTickCount;
			for I := 0 to 7 do begin
				repeat
					State := GetPortState (Connection.PortHandle);
					if (State = 3) and (GetTickCount - StartTime > ReceiveTimeout) then begin
						Result := False;
						Exit;
					end;
				until State <> 3;
				if State = 1 then begin
					Value := Value or (1 shl I);
					SetPortState (Connection.PortHandle, 1);
					State := 2;
				end else begin
					SetPortState (Connection.PortHandle, 2);
					State := 1;
				end;
				while (GetPortState (Connection.PortHandle) and State) = 0 do
					if GetTickCount - StartTime > ReceiveTimeout then begin
						Result := False;
						Exit;
					end;
				SetPortState (Connection.PortHandle, 3);
			end;
		end;
		if Assigned (CheckSum) then
			Inc (CheckSum^, Value);
		{$IFDEF LOGLINK}
			if LogSend then begin
				WriteLn (LogFile);
				WriteLn (LogFile, 'Receiving:');
				LogSend := False;
			end;
			Write (LogFile, ' ' + IntToHex (Value, 2));
		{$ENDIF}
	end;
end;

function ReceiveWord(var Connection: TLinkConnection; out Value: Word; CheckSum: PWord = nil): Boolean;
var
	Temp1,
	Temp2: Byte;
begin
	Result :=
		ReceiveByte (Connection, Temp1, CheckSum) and
		ReceiveByte (Connection, Temp2, CheckSum);
	Value := Temp1 or (Temp2 shl (SizeOf (Temp1) * 8));
end;

function ReceiveLongWord(var Connection: TLinkConnection; out Value: LongWord; CheckSum: PWord = nil): Boolean;
var
	Temp1,
	Temp2: Word;
begin
	Result :=
		ReceiveWord (Connection, Temp1, CheckSum) and
		ReceiveWord (Connection, Temp2, CheckSum);
	Value := Temp1 or (Temp2 shl (SizeOf (Temp1) * 8));
end;

function GetCalcType(var Connection: TLinkConnection): Boolean;
begin
	Result := OpenConnection (Connection);
end;

function GetPortState(Handle: THandle): Cardinal;
var
	InternalState: Cardinal;
begin
	GetCommModemStatus (Handle, InternalState);
	Result := 0;
	if (InternalState and MS_CTS_ON) <> 0 then
		Inc (Result, 1);
	if (InternalState and MS_DSR_ON) <> 0 then
		Inc (Result, 2);
end;

procedure SetPortState(Handle: THandle; State: Cardinal);
procedure Send(Func: Cardinal);
begin
	EscapeCommFunction (Handle, Func);
end;
begin
	if (State and 2) <> 0 then
		Send (SETRTS)
	else
		Send (CLRRTS);
	if (State and 1) <> 0 then
		Send (SETDTR)
	else
		Send (CLRDTR);
end;

function GetPortName(const Port: TLinkPort): string;
begin
	case Port.PortType of
		lpCOM: Result := 'COM' + IntToStr (Port.PortNumber);
		else   Result := '';
	end;
end;

function IsValidAck(Value: Byte): Boolean;
begin
	Result := Value in [$88, $89, $98];
end;

end.
