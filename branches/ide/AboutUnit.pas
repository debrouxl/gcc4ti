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

unit AboutUnit;

interface

uses
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, URLLabelUnit;

type
  TAboutForm = class(TForm)
    FramePanel: TPanel;
    ProgramIcon: TImage;
    ProductNameLabel: TLabel;
    VersionLabel: TLabel;
    OKButton: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    URLLabel1: TURLLabel;
    URLLabel2: TURLLabel;
    URLLabel3: TURLLabel;
    URLLabel4: TURLLabel;
    URLLabel5: TURLLabel;
    Label1: TLabel;
    URLLabel6: TURLLabel;
    CopyrightLabel: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    URLLabel7: TURLLabel;
    Label10: TLabel;
    URLLabel8: TURLLabel;
    Label7: TLabel;
    URLLabel9: TURLLabel;
    procedure FormCreate(Sender: TObject);
  private
	public
	end;

implementation

{$R *.DFM}

uses
	VersionUnit;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
	PixelsPerInch := 96;
	VersionLabel.Caption := 'Version ' + TIGCCLongVersion;
end;

end.
 
