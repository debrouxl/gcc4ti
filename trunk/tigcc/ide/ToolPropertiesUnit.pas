unit ToolPropertiesUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TToolPropertiesForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TitleEdit: TEdit;
    CommandLineEdit: TEdit;
    WorkingDirEdit: TEdit;
    Bevel1: TBevel;
    OKButton: TButton;
    CancelButton: TButton;
    WindowStateEdit: TComboBox;
    CommandLineBrowseButton: TButton;
    WorkingDirBrowseButton: TButton;
    BrowseDialog: TOpenDialog;
    procedure EditChange(Sender: TObject);
    procedure CommandLineBrowseButtonClick(Sender: TObject);
    procedure WorkingDirBrowseButtonClick(Sender: TObject);
  private
	public
	end;

implementation

{$R *.DFM}

uses
	ShlObj;

procedure TToolPropertiesForm.EditChange(Sender: TObject);
begin
	OKButton.Enabled := (Length (TitleEdit.Text) > 0) and (Length (CommandLineEdit.Text) > 0);
end;

procedure TToolPropertiesForm.CommandLineBrowseButtonClick(Sender: TObject);
var
	S: string;
begin
	with BrowseDialog do begin
		S := CommandLineEdit.Text;
		if (Length (S) > 0) and ((S [1] = '"') or (Pos (' ', S) <= 0)) then begin
			if S [1] = '"' then begin
				Delete (S, 1, 1);
				if Pos ('"', S) > 0 then
					Delete (S, Pos ('"', S), Length (S));
			end;
			FileName := S;
		end;
		if Execute then
			CommandLineEdit.Text := '"' + FileName + '"';
	end;
end;

procedure TToolPropertiesForm.WorkingDirBrowseButtonClick(Sender: TObject);
var
	Info: TBrowseInfo;
	Path: PItemIDList;
	FolderName: array [0..MAX_PATH] of Char;
begin
	FillChar (Info, SizeOf (Info), 0);
	with Info do begin
		hwndOwner := Handle;
		lpszTitle := 'Browse';
		ulFlags := BIF_RETURNONLYFSDIRS;
	end;
	Path := SHBrowseForFolder (Info);
	if Assigned (Path) then begin
		if SHGetPathFromIDList (Path, FolderName) then
			WorkingDirEdit.Text := FolderName;
	end;
end;

end.
