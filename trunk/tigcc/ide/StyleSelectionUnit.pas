unit StyleSelectionUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TStyleSelectionForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
	public
		Style: TFontStyles;
		CustomStyle: Boolean;
	end;

implementation

{$R *.DFM}

procedure TStyleSelectionForm.CheckBox1Click(Sender: TObject);
begin
	CheckBox2.Enabled := CheckBox1.Checked;
	CheckBox3.Enabled := CheckBox1.Checked;
	CheckBox4.Enabled := CheckBox1.Checked;
	CheckBox5.Enabled := CheckBox1.Checked;
	if not CheckBox2.Enabled then
		CheckBox2.Checked := False;
	if not CheckBox3.Enabled then
		CheckBox3.Checked := False;
	if not CheckBox4.Enabled then
		CheckBox4.Checked := False;
	if not CheckBox5.Enabled then
		CheckBox5.Checked := False;
end;

procedure TStyleSelectionForm.FormShow(Sender: TObject);
begin
	CheckBox1.Checked := CustomStyle;
	CheckBox2.Checked := fsBold in Style;
	CheckBox3.Checked := fsItalic in Style;
	CheckBox4.Checked := fsUnderline in Style;
	CheckBox5.Checked := fsStrikeOut in Style;
	CheckBox1Click (Sender);
end;

procedure TStyleSelectionForm.FormClose(Sender: TObject;
	var Action: TCloseAction);
begin
	CustomStyle := CheckBox1.Checked;
	if CustomStyle then begin
		Style := [];
		if CheckBox2.Checked then
			Include (Style, fsBold);
		if CheckBox3.Checked then
			Include (Style, fsItalic);
		if CheckBox4.Checked then
			Include (Style, fsUnderline);
		if CheckBox5.Checked then
			Include (Style, fsStrikeOut);
	end;
end;

end.
