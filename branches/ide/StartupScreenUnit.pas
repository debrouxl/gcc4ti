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

unit StartupScreenUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls;

type
	TStartupScreenForm = class(TForm)
		FramePanel: TPanel;
		BackgroundPanel: TPanel;
		BackgroundImage: TImage;
		VersionLabel: TLabel;
		NameLabel1: TLabel;
		NameLabel3: TLabel;
		NameLabel4: TLabel;
		NameLabel5: TLabel;
		NameLabel6: TLabel;
		NameLabel7: TLabel;
		NameLabel8: TLabel;
    StartupStatusDisplay: TLabel;
		procedure FormCreate(Sender: TObject);
	private
		function GetDisplayText: string;
		procedure SetDisplayText(const Value: string);
	public
		property DisplayText: string read GetDisplayText write SetDisplayText;
	end;

var
	StartupScreenForm: TStartupScreenForm;

implementation

{$R *.DFM}

uses
	VersionUnit;

procedure TStartupScreenForm.FormCreate(Sender: TObject);
begin
	PixelsPerInch := 96;
	VersionLabel.Caption := TIGCCShortVersion;
end;

function TStartupScreenForm.GetDisplayText: string;
begin
	Result := StartupStatusDisplay.Caption;
end;

procedure TStartupScreenForm.SetDisplayText(const Value: string);
begin
	StartupStatusDisplay.Caption := Value;
	Update;
end;

end.
