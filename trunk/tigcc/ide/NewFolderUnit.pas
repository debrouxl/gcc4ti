unit NewFolderUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TNewFolderForm = class(TForm)
    Label1: TLabel;
    InputBox: TEdit;
    Bevel1: TBevel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure InputBoxChange(Sender: TObject);
  private
	public
	end;

var
  NewFolderForm: TNewFolderForm;

implementation

{$R *.DFM}

procedure TNewFolderForm.InputBoxChange(Sender: TObject);
begin
	ButtonOK.Enabled := InputBox.Text <> '';
end;

end.
