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

unit ProgramOutputUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, MemoComponentUnit;

type
  TProgramOutputForm = class(TForm)
    OutputMemo: TMemoComponent;
    ErrorMemo: TMemoComponent;
		Splitter: TSplitter;
		Panel1: TPanel;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
  private
	public
	end;

implementation

{$R *.DFM}

procedure TProgramOutputForm.FormShow(Sender: TObject);
begin
	OutputMemo.Selection.RStart := OutputMemo.TextLength + 1;
	OutputMemo.Selection.ScrollInView (0);
	ErrorMemo.Selection.RStart := ErrorMemo.TextLength + 1;
	ErrorMemo.Selection.ScrollInView (0);
end;

end.
