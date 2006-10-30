{
  TIGCC IDE

  Copyright (C) 2000-2004 Sebastian Reichelt
  Copyright (C) 2006 Kevin Kofler

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

unit VTIStartUnit;

interface

uses
	MasterUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, ComObj, ActiveX, TiEmuOLELib_TLB;

type
	TVTIStartForm = class(TForm)
		Label1: TLabel;
		CancelButton: TButton;
    FindTimer: TTimer;
		procedure FormShow(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FindTimerTimer(Sender: TObject);
	private
		ProcessHandle: THandle;
	public
		TiEmuInterface: ITiEmuOLE;
	end;

implementation

{$R *.DFM}

uses
	UtilsWin;

procedure TVTIStartForm.FormShow(Sender: TObject);
var
	StartupInfo: TStartupInfo;
	ProcessInfo: TProcessInformation;
begin
	FillChar (StartupInfo, SizeOf (StartupInfo), 0);
	StartupInfo.cb := SizeOf (StartupInfo);
	if CreateProcess (nil, PChar (VTIPath), nil, nil, False, CREATE_NEW_PROCESS_GROUP or DETACHED_PROCESS, nil, PChar (ExtractFilePath (VTIPath)), StartupInfo, ProcessInfo) then begin
		ProcessHandle := ProcessInfo.hProcess;
		CloseHandle (ProcessInfo.hThread);
	end else begin
		ShowDefaultMessageBox ('An error occurred while trying to start TiEmu.', 'Error', mtProgramError);
		ModalResult := mrAbort;
	end;
end;

procedure TVTIStartForm.FormClose(Sender: TObject;
	var Action: TCloseAction);
begin
	if ProcessHandle <> 0 then
		CloseHandle (ProcessHandle);
end;

procedure TVTIStartForm.FindTimerTimer(Sender: TObject);
var
	ExitCode: Cardinal;
	Unknown: IUnknown;
	OLEResult: HResult;
begin
	if GetExitCodeProcess (ProcessHandle, ExitCode) then begin
		if ExitCode <> STILL_ACTIVE then begin
			ModalResult := mrAbort;
			Exit;
		end;
	end;
	OLEResult := GetActiveObject(CLASS_TiEmuOLE, nil, Unknown);
	if OLEResult = S_OK then begin
		OleCheck(Unknown.QueryInterface(ITiEmuOLE, TiEmuInterface));
		ModalResult := mrOK;
	end;
end;

end.
