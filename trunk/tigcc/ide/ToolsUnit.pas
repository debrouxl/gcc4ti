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

unit ToolsUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls;

type
	TToolsForm = class(TForm)
		ToolsList: TListView;
		AddButton: TButton;
		EditButton: TButton;
		RemoveButton: TButton;
		OKButton: TButton;
		CancelButton: TButton;
		procedure AddButtonClick(Sender: TObject);
		procedure EditButtonClick(Sender: TObject);
		procedure RemoveButtonClick(Sender: TObject);
		procedure ToolsListSelectItem(Sender: TObject; Item: TListItem;
			Selected: Boolean);
		procedure ToolsListKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure ToolsListDragDrop(Sender, Source: TObject; X, Y: Integer);
		procedure ToolsListDragOver(Sender, Source: TObject; X, Y: Integer;
			State: TDragState; var Accept: Boolean);
	private
	public
		function WindowStateToString(WindowState: TWindowState): string;
		function StringToWindowState(const S: string): TWindowState;
	end;

const
	NormalString = 'Normal';
	MaximizedString = 'Maximized';
	MinimizedString = 'Minimized';

implementation

{$R *.DFM}

uses
	ToolPropertiesUnit;

procedure TToolsForm.AddButtonClick(Sender: TObject);
begin
	with TToolPropertiesForm.Create (Self) do try
		WindowStateEdit.ItemIndex := 0;
		if ShowModal = mrOK then begin
			with ToolsList.Items.Add do begin
				Caption := TitleEdit.Text;
				SubItems.Add (CommandLineEdit.Text);
				SubItems.Add (WorkingDirEdit.Text);
				SubItems.Add (WindowStateEdit.Text);
			end;
		end;
	finally
		Free;
	end;
end;

procedure TToolsForm.EditButtonClick(Sender: TObject);
begin
	if Assigned (ToolsList.Selected) then
		with TToolPropertiesForm.Create (Self) do try
			with ToolsList.Selected do begin
				TitleEdit.Text := Caption;
				CommandLineEdit.Text := SubItems [0];
				WorkingDirEdit.Text := SubItems [1];
				case StringToWindowState (SubItems [2]) of
					wsMaximized:
						WindowStateEdit.ItemIndex := 1;
					wsMinimized:
						WindowStateEdit.ItemIndex := 2;
					else
						WindowStateEdit.ItemIndex := 0;
				end;
			end;
			if ShowModal = mrOK then begin
				with ToolsList.Selected do begin
					Caption := TitleEdit.Text;
					SubItems [0] := CommandLineEdit.Text;
					SubItems [1] := WorkingDirEdit.Text;
					case WindowStateEdit.ItemIndex of
						1:
							SubItems [2] := MaximizedString;
						2:
							SubItems [2] := MinimizedString;
						else
							SubItems [2] := NormalString;
					end;
				end;
			end;
		finally
			Free;
		end;
end;

procedure TToolsForm.RemoveButtonClick(Sender: TObject);
begin
	if Assigned (ToolsList.Selected) then
		ToolsList.Selected.Free;
end;

procedure TToolsForm.ToolsListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
	EditButton.Enabled := Assigned (ToolsList.Selected);
	RemoveButton.Enabled := Assigned (ToolsList.Selected);
end;

procedure TToolsForm.ToolsListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = vk_Delete then
		RemoveButtonClick (Sender);
end;

procedure TToolsForm.ToolsListDragDrop(Sender, Source: TObject; X, Y: Integer);
var
	S,
	D: TListItem;
	DIndex: Integer;
begin
	with ToolsList do begin
		S := Selected;
		if Assigned (S) then begin
			D := GetItemAt (X, Y);
			if Assigned (D) and (S <> D) then begin
				DIndex := D.Index;
				if DIndex > S.Index then
					Inc (DIndex);
				with Items.Insert (DIndex) do begin
					Caption := S.Caption;
					SubItems.Assign (S.SubItems);
				end;
				S.Free;
			end;
		end;
	end;
end;

procedure TToolsForm.ToolsListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
	S,
	D: TListItem;
begin
	Accept := False;
	with ToolsList do begin
		S := Selected;
		if Assigned (S) then begin
			D := GetItemAt (X, Y);
			if Assigned (D) and (S <> D) then
				Accept := True;
		end;
	end;
end;

function TToolsForm.WindowStateToString(WindowState: TWindowState): string;
begin
	case WindowState of
		wsMaximized:
			Result := MaximizedString;
		wsMinimized:
			Result := MinimizedString;
		else
			Result := NormalString;
	end;
end;

function TToolsForm.StringToWindowState(const S: string): TWindowState;
begin
	if S = MaximizedString then
		Result := wsMaximized
	else if S = MinimizedString then
		Result := wsMinimized
	else
		Result := wsNormal;
end;

end.
