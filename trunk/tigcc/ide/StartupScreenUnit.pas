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
