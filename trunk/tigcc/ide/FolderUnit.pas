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

unit FolderUnit;

interface

uses
	Classes, ComCtrls;

type
	TFolder = class(TObject)
	private
		FTreeItem: TTreeNode;
		function GetFolderName: string;
		procedure SetFolderName(const Value: string);
		function GetParent: TFolder;
    function GetPath: string;
	public
		property Parent: TFolder read GetParent;
	published
		property FolderName: string read GetFolderName write SetFolderName;
		property Path: string read GetPath;
		property TreeItem: TTreeNode read FTreeItem write FTreeItem;
	end;

implementation

uses
	UtilsDos;

{ TFolder }

function TFolder.GetFolderName: string;
begin
	if Assigned (TreeItem) then
		Result := TreeItem.Text
	else
		Result := '';
end;

function TFolder.GetParent: TFolder;
var
	ParentItem: TTreeNode;
begin
	Result := nil;
	if Assigned (TreeItem) then begin
		ParentItem := TreeItem.Parent;
		if Assigned (ParentItem) and Assigned (ParentItem.Data) and (TObject (ParentItem.Data) is TFolder) then
			Result := ParentItem.Data;
	end;
end;

function TFolder.GetPath: string;
var
	P: TFolder;
begin
	P := Parent;
	if Assigned (P) and (Length (P.FolderName) > 0) then begin
		Result := P.Path;
		if Length (Result) > 0 then
			Result := WithBackslash (Result) + FolderName
		else
			Result := FolderName;
	end else
		Result := FolderName;
end;

procedure TFolder.SetFolderName(const Value: string);
begin
	if Assigned (TreeItem) then
		TreeItem.Text := Value;
end;

end.
