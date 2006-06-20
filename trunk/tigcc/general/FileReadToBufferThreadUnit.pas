{
  This Delphi unit is part of TIGCC.

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

unit FileReadToBufferThreadUnit;

interface

uses
	SysUtils, Classes, Windows, Forms, Controls, SyncObjs;

type
	TFileReadToBufferThread = class(TThread)
	private
		FHasTerminated: Boolean;
		FFileHandle: THandle;
		FBufferStream: TStream;
		FLock: TMultiReadExclusiveWriteSynchronizer;
	protected
		procedure Execute; override;
	public
		constructor Create(FileHandle: THandle; BufferStream: TStream);
		destructor Destroy; override;
		property HasTerminated: Boolean read FHasTerminated;
		property Lock: TMultiReadExclusiveWriteSynchronizer read FLock;
	end;

implementation

const
	BytesToRead = 256;

{ THandleWaitThread }

constructor TFileReadToBufferThread.Create(FileHandle: HFile; BufferStream: TStream);
begin
	inherited Create (True);
	FLock := TMultiReadExclusiveWriteSynchronizer.Create;
	FHasTerminated := False;
	FreeOnTerminate := False;
	FFileHandle := FileHandle;
	FBufferStream := BufferStream;
	Resume;
end;

destructor TFileReadToBufferThread.Destroy;
begin
	FLock.Free;
	inherited;
end;

procedure TFileReadToBufferThread.Execute;
var
	Buffer: array [1..BytesToRead] of Byte;
	Count: Cardinal;
begin
	while (not Terminated) and ReadFile (FFileHandle, Buffer, BytesToRead, Count, nil) do begin
		Lock.BeginWrite;
		FBufferStream.Write (Buffer, Count);
		Lock.EndWrite;
	end;
	FHasTerminated := True;
end;

end.
