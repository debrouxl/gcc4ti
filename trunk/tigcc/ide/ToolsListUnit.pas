unit ToolsListUnit;

interface

uses
	ObjList,
	Classes, Controls, Forms, Menus;

type
	TToolsList = class(TFastObjectContainer);

	TToolsListItem = class(TFastContainerItem)
	private
		FWorkingDir: string;
		FCommandLine: string;
		FTitle: TCaption;
		FWindowState: TWindowState;
    FMenuItem: TMenuItem;
	public
		destructor Destroy; override;
		property MenuItem: TMenuItem read FMenuItem write FMenuItem;
	published
		property Title: TCaption read FTitle write FTitle;
		property CommandLine: string read FCommandLine write FCommandLine;
		property WorkingDir: string read FWorkingDir write FWorkingDir;
		property WindowState: TWindowState read FWindowState write FWindowState;
	end;

implementation

{ TToolsListItem }

destructor TToolsListItem.Destroy;
begin
	if Assigned (MenuItem) then
		MenuItem.Free;
	inherited;
end;

end.
