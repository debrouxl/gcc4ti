unit FunctionsWinUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SourceFileUnit;

type
  TFunctionsForm = class(TForm)
    FuncList: TListBox;
    PrototypeButton: TButton;
    ImplementationButton: TButton;
    CancelButton: TButton;
    procedure FuncListDblClick(Sender: TObject);
    procedure FuncListClick(Sender: TObject);
		procedure FuncListMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
	private
	public
		Funcs: PSourceFileFunctions;
	end;

implementation

{$R *.DFM}

procedure TFunctionsForm.FuncListDblClick(Sender: TObject);
begin
	if ImplementationButton.Enabled then
		ModalResult := mrNo
	else if PrototypeButton.Enabled then
		ModalResult := mrYes;
end;

procedure TFunctionsForm.FuncListClick(Sender: TObject);
begin
	PrototypeButton.Enabled := (FuncList.ItemIndex >= 0) and (Funcs^[Integer(FuncList.Items.Objects[FuncList.ItemIndex])].PrototypeLine > 0);
	ImplementationButton.Enabled := (FuncList.ItemIndex >= 0) and (Funcs^[Integer(FuncList.Items.Objects[FuncList.ItemIndex])].ImplementationLine > 0);
end;

procedure TFunctionsForm.FuncListMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	FuncListClick (Sender);
end;

end.
