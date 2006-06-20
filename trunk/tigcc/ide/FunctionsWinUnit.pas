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
