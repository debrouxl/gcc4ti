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
