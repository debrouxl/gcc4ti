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
