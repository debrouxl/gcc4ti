unit HandleWaitThreadUnit;

interface

uses
	SysUtils, Classes, Windows, Forms, Controls;

type
	THandleWaitThread = class(TThread)
	private
		FHasTerminated: Boolean;
		FWaitHandle: THandle;
	protected
		procedure Execute; override;
	public
		property WaitHandle: THandle read FWaitHandle;
		property HasTerminated: Boolean read FHasTerminated;
		constructor Create(WaitHandle: THandle);
	end;

implementation

{ THandleWaitThread }

constructor THandleWaitThread.Create(WaitHandle: THandle);
begin
	inherited Create (True);
	FHasTerminated := False;
	FreeOnTerminate := False;
	FWaitHandle := WaitHandle;
	Resume;
end;

procedure THandleWaitThread.Execute;
begin
	while (not Terminated) and (WaitForSingleObject (FWaitHandle, 1000) = Wait_TimeOut) do;
	FHasTerminated := True;
end;

end.
