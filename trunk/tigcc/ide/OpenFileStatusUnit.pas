unit OpenFileStatusUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type
	TOpenFileStatusForm = class(TForm)
    FileAnimate: TAnimate;
    FileNameLabel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
	public
	end;

implementation

{$R *.DFM}

procedure TOpenFileStatusForm.FormCreate(Sender: TObject);
begin
	Screen.Cursor := crHourglass;
end;

procedure TOpenFileStatusForm.FormDestroy(Sender: TObject);
begin
	Screen.Cursor := crDefault;
end;

end.
