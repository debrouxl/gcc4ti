{
  TIGCC IDE

  Copyright (C) 2000-2004 Sebastian Reichelt
  Copyright (C) 2005 Fréderic Bour

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

unit ProcessUnit;

interface

uses
	HandleWaitThreadUnit, FileReadToBufferThreadUnit,
	Windows, Classes;

type
	TRedirect = class;

	TProcessConsole = class(TObject)
	private
		FTitle: string;
		FProcessInfo: TProcessInformation;
		FStdOut: TRedirect;
		FStdErr: TRedirect;
		FWaitThread: THandleWaitThread;
		FPriorityClass: DWord;
		function GetLastOutText: string;
		function GetLastErrText: string;
		function GetRunning: Boolean;
    function GetLastErrSize: Integer;
    function GetLastOutSize: Integer;
	protected
		property StdOut: TRedirect read FStdOut;
		property StdErr: TRedirect read FStdErr;
		property ProcessInfo: TProcessInformation read FProcessInfo;
		property WaitThread: THandleWaitThread read FWaitThread;
	public
		constructor Create;
		destructor Destroy; override;
		procedure StartProcess(const ProgramFile, Parameters, HomeDir: string);
		procedure KillProcess;
		procedure KillProcessAndWait;
		procedure WaitForTermination;
		property Running: Boolean read GetRunning;
		property LastOutText: string read GetLastOutText;
		property LastErrText: string read GetLastErrText;
		property LastOutSize: Integer read GetLastOutSize;
		property LastErrSize: Integer read GetLastErrSize;
	published
		property Title: string read FTitle write FTitle;
		property PriorityClass: DWord read FPriorityClass write FPriorityClass;
	end;

	TRedirect = class(TObject)
	private
		FProgramOutputHandle: HFile;
		FPipe: HFile;
		FThread: TFileReadToBufferThread;
		FStream: TMemoryStream;
		FFinish: Boolean;
		function GetText: string;
		function GetOutputSize: Integer;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Clear;
		procedure Finish;
		property Stream: TMemoryStream read FStream;
		property Thread: TFileReadToBufferThread read FThread;
		property Pipe: HFile read FPipe;
		property ProgramOutputHandle: HFile read FProgramOutputHandle;
		property Text: string read GetText;
		property OutputSize: Integer read GetOutputSize;
	end;

implementation

uses
	SysUtils, UtilsWin;

{ TProcessConsole }

constructor TProcessConsole.Create;
begin
	inherited;
	FStdOut := TRedirect.Create;
	FStdErr := TRedirect.Create;
	Title := 'Console';
	PriorityClass := HIGH_PRIORITY_CLASS;
end;

destructor TProcessConsole.Destroy;
begin
	KillProcessAndWait;
	FStdErr.Free;
	FStdOut.Free;
	inherited;
end;

function TProcessConsole.GetLastErrSize: Integer;
begin
	Result := StdErr.OutputSize;
end;

function TProcessConsole.GetLastErrText: string;
begin
	Result := StdErr.Text;
end;

function TProcessConsole.GetLastOutSize: Integer;
begin
	Result := StdOut.OutputSize;
end;

function TProcessConsole.GetLastOutText: string;
begin
	Result := StdOut.Text;
end;

function TProcessConsole.GetRunning: Boolean;
var
	T: TThread;
begin
	if Assigned (WaitThread) then begin
		Result := not WaitThread.HasTerminated;
		if not Result then begin
			T := FWaitThread;
			FWaitThread := nil;
			T.Free;
			with FProcessInfo do begin
				CloseHandle (hProcess);
				CloseHandle (hThread);
				hProcess := 0;
				hThread := 0;
			end;
			StdOut.Finish;
			StdErr.Finish;
		end;
	end else
		Result := False;
end;

procedure TProcessConsole.KillProcess;
begin
	if Running then begin
		TerminateProcess (ProcessInfo.hProcess, 1);
		if Assigned (WaitThread) then
			WaitThread.Terminate;
	end;
end;

procedure TProcessConsole.KillProcessAndWait;
begin
	KillProcess;
	WaitForTermination;
end;

procedure TProcessConsole.StartProcess(const ProgramFile, Parameters, HomeDir: string);
var
	StartupInfo: TStartupInfo;
	CommandLine: string;
	HomeDirPC: PChar;
begin
	if Running then
		raise EFOpenError.CreateFmt ('Another process is still running', [])
	else begin
		StdOut.Clear;
		StdErr.Clear;
		FillChar (StartupInfo, SizeOf (StartupInfo), 0);
		with StartupInfo do begin
			cb := SizeOf (StartupInfo);
			lpTitle := PChar (Title);
			wShowWindow := sw_Hide;
			hStdOutput := StdOut.ProgramOutputHandle;
			hStdError := StdErr.ProgramOutputHandle;
			dwFlags := StartF_UseShowWindow or StartF_UseStdHandles;
		end;
		if Length (ProgramFile) > 0 then
			CommandLine := '"' + ProgramFile + '" ' + Parameters
		else
			CommandLine := Parameters;
		if Length (HomeDir) > 0 then
			HomeDirPC := PChar (HomeDir)
		else
			HomeDirPC := nil;
		if CreateProcess (nil, PChar (CommandLine), nil, nil, True, PriorityClass, nil, HomeDirPC, StartupInfo, FProcessInfo) then begin
			FWaitThread := THandleWaitThread.Create (ProcessInfo.hProcess);
		end else
			raise EFOpenError.CreateFmt ('Could not start process', []);
	end;
end;

procedure TProcessConsole.WaitForTermination;
begin
	if Assigned (WaitThread) then
		WaitThread.WaitFor;
end;

{ TRedirect }

procedure TRedirect.Clear;
begin
	FFinish := False;
	Thread.Lock.BeginWrite;
	Stream.Clear;
	Thread.Lock.EndWrite;
end;

constructor TRedirect.Create;
var
	Attr: TSecurityAttributes;
	TempPipe: HFile;
begin
	inherited;
	FillChar (Attr, SizeOf (Attr), 0);
	with Attr do begin
		nLength := SizeOf (Attr);
		lpSecurityDescriptor := nil;
		bInheritHandle := True;
	end;
	CreatePipe (TempPipe, FProgramOutputHandle, @Attr, 0);
	DuplicateHandle (GetCurrentProcess, TempPipe, GetCurrentProcess, @FPipe, 0, False, Duplicate_Close_Source or Duplicate_Same_Access);
	FStream := TMemoryStream.Create;
	FThread := TFileReadToBufferThread.Create (Pipe, Stream);
end;

destructor TRedirect.Destroy;
begin
	if Assigned (Thread) then
		Thread.Terminate;
	Finish;
	CloseHandle (FProgramOutputHandle);
	if Assigned (Thread) then begin
		Thread.WaitFor;
		FThread.Free;
	end;
	if Assigned (Stream) then
		FStream.Free;
	CloseHandle (FPipe);
	inherited;
end;

procedure TRedirect.Finish;
const
	B: Byte = 0;
var
	BW: Cardinal;
begin
	if not FFinish then begin
		FFinish := True;
		WriteFile (ProgramOutputHandle, B, 1, BW, nil);
	end;
end;

function TRedirect.GetOutputSize: Integer;
begin
	Thread.Lock.BeginRead;
	Result := Stream.Size;
	Thread.Lock.EndRead;
end;

function TRedirect.GetText: string;
var
	Finished: Boolean;
begin
	if FFinish then begin
		repeat
			Thread.Lock.BeginRead;
			with Stream do
				Finished := (Size > 0) and (PChar(Memory) [Size - 1] = #0);
			Thread.Lock.EndRead;
			if not Finished then
				Sleep (100);
		until Finished;
		Result := AnsiString (PChar (Stream.Memory));
	end else begin
		Thread.Lock.BeginWrite;
		with Stream do begin
			Size := Size + 1;
			PChar(Memory) [Size - 1] := #0;
			Result := AnsiString (PChar (Memory));
			Size := Size - 1;
		end;
		Thread.Lock.EndWrite;
	end;
end;

end.
