unit SendProgressUnit;

interface

uses
	LinkUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls;

type
	TSendProgressForm = class(TForm)
		ProgressBar: TProgressBar;
		FileNameLabel: TLabel;
		CancelButton: TButton;
		procedure CancelButtonClick(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	private
	public
		Cancelled: Boolean;
		CloseNow: Boolean;
	end;

function ProgressProg(ID: Pointer; Progress: PDWord): Boolean;

implementation

{$R *.DFM}

function ProgressProg(ID: Pointer; Progress: PDWord): Boolean;
begin
	with TSendProgressForm (ID) do begin
		ProgressBar.Position := Progress^;
		Result := not Cancelled;
	end;
	Application.ProcessMessages;
end;

procedure TSendProgressForm.CancelButtonClick(Sender: TObject);
begin
	Cancelled := True;
end;

procedure TSendProgressForm.FormCloseQuery(Sender: TObject;
	var CanClose: Boolean);
begin
	CanClose := CloseNow;
	if not CanClose then
		Cancelled := True;
end;

end.
