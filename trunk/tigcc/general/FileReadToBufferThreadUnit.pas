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
