unit NewsUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ScktComp, StdCtrls;

const
	NewsID = 'TIGCC News Format'#13#10#13#10;

type
	TNewsForm = class(TForm)
		NewsClient: TClientSocket;
		NewsBox: TScrollBox;
		VisitButton: TButton;
		CloseButton: TButton;
		RetreiveLabel: TLabel;
    ProxyCheckBox: TCheckBox;
    ProxyNameEdit: TEdit;
    Label1: TLabel;
    ProxyPortEdit: TEdit;
    RefreshButton: TButton;
		procedure NewsClientError(Sender: TObject; Socket: TCustomWinSocket;
			ErrorEvent: TErrorEvent; var ErrorCode: Integer);
		procedure FormActivate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure NewsClientConnect(Sender: TObject; Socket: TCustomWinSocket);
		procedure NewsClientRead(Sender: TObject; Socket: TCustomWinSocket);
		procedure ProxyCheckBoxClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
	private
		FSilentConnect: Boolean;
	public
		Labels: array of TLabel;
		procedure Connect;
		function Execute: Boolean;
		property SilentConnect: Boolean read FSilentConnect write FSilentConnect;
	end;

implementation

{$R *.DFM}

uses
	MasterUnit, MainUnit,
	UtilsWin,
	ShellAPI, Registry;

procedure TNewsForm.Connect;
var
	I: Integer;
begin
	if not NewsClient.Active then begin
		for I := High (Labels) downto Low (Labels) do
			Labels[I].Free;
		SetLength (Labels, 0);
		RetreiveLabel.Show;
		Update;
		try
			with NewsClient do begin
				if ProxyCheckBox.Checked then begin
					Host := '';
					Address := '';
					if (Pos ('.', ProxyNameEdit.Text) > 0) and (ProxyNameEdit.Text [1] in ['0'..'9']) then
						Address := ProxyNameEdit.Text
					else
						Host := ProxyNameEdit.Text;
					try
						Port := StrToInt (ProxyPortEdit.Text);
					except
						Port := 8080;
					end;
				end else begin
					Address := '';
					Host := 'tigcc.ticalc.org';
					Port := 80;
				end;
				NewsClient.Active := True;
			end;
		except
			RetreiveLabel.Hide;
			ShowDefaultMessageBox ('Error connecting to tigcc.ticalc.org. Please check your connection.', 'Error', mtProgramError);
			if not Visible then
				Free;
		end;
	end;
end;

function TNewsForm.Execute: Boolean;
begin
	Result := ShowModal = mrOK;
	if Result then
		ShellExecute (0, nil, 'http://tigcc.ticalc.org/', nil, nil, sw_ShowMaximized);
end;

procedure TNewsForm.NewsClientError(Sender: TObject;
	Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
	var ErrorCode: Integer);
begin
	NewsClient.Active := False;
	RetreiveLabel.Hide;
	ShowDefaultMessageBox ('Error downloading news from tigcc.ticalc.org. Please check your connection.', 'Error', mtProgramError);
	if not Visible then
		Free;
end;

procedure TNewsForm.FormActivate(Sender: TObject);
begin
	if not SilentConnect then
		Connect;
end;

procedure TNewsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	NewsClient.Active := False;
	ProxyName := ProxyNameEdit.Text;
	try
		if Length (ProxyPortEdit.Text) > 0 then
			ProxyPort := StrToInt (ProxyPortEdit.Text)
		else
			ProxyPort := 0;
	except end;
end;

procedure TNewsForm.NewsClientConnect(Sender: TObject;
	Socket: TCustomWinSocket);
var
	Line: string;
begin
	Line := 'GET http://tigcc.ticalc.org/newsheadlines.txt';
	if ProxyCheckBox.Checked then
		Line := Line + ' HTTP/1.1'#13#10'Accept: */*'#13#10'Accept-Language: en'#13#10'User-Agent: TIGCC'#13#10'Host: tigcc.ticalc.org'#13#10'Proxy-Connection: Keep-Alive'#13#10;
	Line := Line + #13#10;
	Socket.SendText (Line);
end;

procedure TNewsForm.NewsClientRead(Sender: TObject;
	Socket: TCustomWinSocket);
var
	S: string;
	Date,
	LastDate: Integer;
	CurLabel: TLabel;
begin
	RetreiveLabel.Hide;
	S := Socket.ReceiveText;
	NewsClient.Active := False;
	if Pos (NewsID, S) > 0 then begin
		LastDate := 0;
		Delete (S, 1, Pos (NewsID, S) - 1 + Length (NewsID));
		while Pos (#13#10, S) > 0 do begin
			if S [1] = #13 then
				Break;
			CurLabel := TLabel.Create (Self);
			CurLabel.Left := 2;
			if Length (Labels) > 0 then
				with Labels [High (Labels)] do
					CurLabel.Top := Top + Height + 2
			else
				CurLabel.Top := 0;
			try
				Date := StrToInt (Copy (S, 1, Pos (#13#10, S) - 1));
				if LastDate = 0 then
					LastDate := Date;
			except
				Date := 0;
			end;
			Delete (S, 1, Pos (#13#10, S) + 1);
			with CurLabel.Font do begin
				Name := 'Arial';
				Size := 9;
				Style := [fsBold];
				if Date > LastNewsDate then
					Color := $0000C0
				else
					Color := $808080;
			end;
			CurLabel.Caption := Copy (S, 1, Pos (#13#10, S) - 1);
			Delete (S, 1, Pos (#13#10, S) + 1);
			Delete (S, 1, Pos (#13#10, S) + 1);
			CurLabel.Parent := NewsBox;
			CurLabel.Show;
			SetLength (Labels, Length (Labels) + 1);
			Labels [High (Labels)] := CurLabel;
		end;
		if (LastDate > LastNewsDate) and (not Visible) then
			Execute;
		if LastNewsDate <> LastDate then begin
			LastNewsDate := LastDate;
			MainForm.SavePreferences;
		end;
	end else
		ShowDefaultMessageBox ('Error in news format from tigcc.ticalc.org. Please contact the site administrator.', 'Error', mtProgramError);
	if not Visible then
		Free;
end;

procedure TNewsForm.ProxyCheckBoxClick(Sender: TObject);
var
	NewProxy: string;
begin
	ProxyNameEdit.Enabled := ProxyCheckBox.Checked;
	ProxyPortEdit.Enabled := ProxyCheckBox.Checked;
	if ProxyCheckBox.Checked then begin
		with TRegistry.Create do try
			RootKey := HKEY_CURRENT_USER;
			if OpenKeyReadOnly ('\Software\Microsoft\Windows\CurrentVersion\Internet Settings') then begin
				if ValueExists ('ProxyServer') then begin
					NewProxy := ReadString ('ProxyServer');
					if Pos ('http=', LowerCase (NewProxy)) > 0 then begin
						Delete (NewProxy, 1, Pos ('http=', LowerCase (NewProxy)) - 1 + Length ('http='));
						if Pos (';', NewProxy) > 0 then
							Delete (NewProxy, Pos (';', NewProxy), Length (NewProxy));
					end else
						if Pos (';', NewProxy) > 0 then
							NewProxy := '';
					if Pos (':', NewProxy) > 0 then begin
						ProxyNameEdit.Text := Copy (NewProxy, 1, Pos (':', NewProxy) - 1);
						ProxyPortEdit.Text := Copy (NewProxy, Pos (':', NewProxy) + 1, Length (NewProxy));
					end;
				end;
			end;
		finally
			Free;
		end;
	end else begin
		ProxyNameEdit.Text := '';
		ProxyPortEdit.Text := '';
	end;
end;

procedure TNewsForm.RefreshButtonClick(Sender: TObject);
begin
	NewsClient.Active := False;
	Connect;
end;

procedure TNewsForm.FormCreate(Sender: TObject);
begin
	ProxyCheckBox.Checked := Length (ProxyName) > 0;
	ProxyNameEdit.Text := ProxyName;
	if ProxyCheckBox.Checked then
		ProxyPortEdit.Text := IntToStr (ProxyPort)
	else
		ProxyPortEdit.Text := '';
end;

end.
