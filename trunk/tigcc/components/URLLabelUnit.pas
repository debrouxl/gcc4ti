unit URLLabelUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls;

type
	TShowWindowStyle = (swHide, swShowNormal, swShowMinimized, swShowMaximized, swMaximize, swShowNoActivate, swShow, swMinimize, swShowMinNoActivate, swShowNA, swRestore, swShowDefault);

	TURLLabel = class(TStaticText)
	private
		FURL: string;
    FShowWindow: TShowWindowStyle;
	protected
		procedure CreateWnd; override;
	public
		constructor Create(AOwner: TComponent); override;
		procedure Click; override;
	published
		property URL: string read FURL write FURL;
		property ShowWindow: TShowWindowStyle read FShowWindow write FShowWindow;
	end;

procedure Register;

implementation

uses
	ShellAPI;

procedure Register;
begin
	RegisterComponents('User', [TURLLabel]);
end;

{ TURLLabel }

procedure TURLLabel.Click;
begin
	inherited;
	if Length (URL) > 0 then
		ShellExecute (0, nil, PChar (URL), nil, nil, Integer (ShowWindow));
end;

constructor TURLLabel.Create(AOwner: TComponent);
begin
	inherited;
	Font.Color := clHighlight;
	Font.Style := Font.Style + [fsUnderline];
	FURL := '';
	FShowWindow := swShow;
end;

procedure TURLLabel.CreateWnd;
var
	NewCursor: HCursor;
begin
	inherited;
	if HandleAllocated and not (csDesigning in ComponentState) then begin
		NewCursor := LoadCursor (0, idc_Hand);
		if NewCursor = 0 then
			NewCursor := Screen.Cursors [crHandPoint];
		SetClassLong (Handle, gcl_HCursor, NewCursor);
	end;
end;

end.
