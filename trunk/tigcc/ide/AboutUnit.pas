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
 
