unit ProgramOptionsUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, ComCtrls;

type
	TProgramOptionsForm = class(TForm)
    CloseButton: TButton;
		PageController: TPageControl;
		CalculatorSheet: TTabSheet;
    Label2: TLabel;
		TI89CheckBox: TCheckBox;
		TI92PlusCheckBox: TCheckBox;
    V200CheckBox: TCheckBox;
    Label5: TLabel;
    OSSheet: TTabSheet;
		Label3: TLabel;
    NoStubRadioButton: TRadioButton;
    DoorsRadioButton: TRadioButton;
    PreOSRadioButton: TRadioButton;
    MinAMSCheckBox: TCheckBox;
    MinAMSEdit: TEdit;
		RelocFormatSheet: TTabSheet;
		Panel2: TPanel;
    Label4: TLabel;
    RelocAMSRadioButton: TRadioButton;
		RelocKernelRadioButton: TRadioButton;
		RelocCompressedRadioButton: TRadioButton;
    Panel3: TPanel;
    Label15: TLabel;
    ROMCallDirectRadioButton: TRadioButton;
		ROMCallKernelRadioButton: TRadioButton;
		ROMCallCompressedRadioButton: TRadioButton;
    ROMCallFLineRadioButton: TRadioButton;
    InternalFLineEmulatorCheckBox: TCheckBox;
		BSSFormatSheet: TTabSheet;
		HomeScreenSheet: TTabSheet;
		LCDSaveCheckBox: TCheckBox;
		EnableErrorReturnCheckBox: TCheckBox;
		Label11: TLabel;
		ReturnValueRadioButton: TRadioButton;
		ReturnDoneRadioButton: TRadioButton;
		Label10: TLabel;
		Label7: TLabel;
    Panel1: TPanel;
		Label6: TLabel;
    BSSKernelRadioButton: TRadioButton;
    BSSCompressedRadioButton: TRadioButton;
    BSSMergeRadioButton: TRadioButton;
    Panel4: TPanel;
    Label8: TLabel;
		DataVarKernelRadioButton: TRadioButton;
		DataVarCompressedRadioButton: TRadioButton;
		WelcomeSheet: TTabSheet;
		Label1: TLabel;
		Label9: TLabel;
		OptimizeCalcConstsCheckBox: TCheckBox;
		Label12: TLabel;
		RelocFLineJumpsCheckBox: TCheckBox;
		UnofficialOSSupportCheckBox: TCheckBox;
		RelocFLineJumps4ByteCheckBox: TCheckBox;
    ROMCallOptimizedCheckBox: TCheckBox;
    RelocMlinkRadioButton: TRadioButton;
    ROMCallMlinkRadioButton: TRadioButton;
    BSSMlinkRadioButton: TRadioButton;
    DataVarMlinkRadioButton: TRadioButton;
		procedure FormShow(Sender: TObject);
		procedure ShellRadioButtonClick(Sender: TObject);
		procedure MinAMSCheckBoxClick(Sender: TObject);
		procedure RelocRadioButtonClick(Sender: TObject);
		procedure ItemMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure CalcCheckBoxClick(Sender: TObject);
		procedure RelocFLineJumpsCheckBoxClick(Sender: TObject);
	private
	public
	end;

implementation

uses
	MainUnit;

{$R *.DFM}

procedure TProgramOptionsForm.FormShow(Sender: TObject);
begin
	ActiveControl := PageController;
	CalcCheckBoxClick            (Sender);
	ShellRadioButtonClick        (Sender);
	MinAMSCheckBoxClick          (Sender);
	RelocRadioButtonClick        (Sender);
	RelocFLineJumpsCheckBoxClick (Sender);
end;

procedure TProgramOptionsForm.ShellRadioButtonClick(Sender: TObject);
begin
	with LCDSaveCheckBox do begin
		Enabled := NostubRadioButton.Checked;
		if not Enabled then
			Checked := True;
	end;
	with RelocAMSRadioButton do begin
		Enabled := NostubRadioButton.Checked;
		if Checked and not Enabled then
			RelocKernelRadioButton.Checked := True;
	end;
	with RelocMLinkRadioButton do begin
		Enabled := NostubRadioButton.Checked;
		if Checked and not Enabled then
			RelocKernelRadioButton.Checked := True;
	end;
	with RelocKernelRadioButton do begin
		Enabled := not PreOSRadioButton.Checked;
		if Checked and not Enabled then
			RelocCompressedRadioButton.Checked := True;
	end;
	with RelocCompressedRadioButton do begin
		Enabled := not DoorsRadioButton.Checked;
		if Checked and not Enabled then
			RelocKernelRadioButton.Checked := True;
	end;
	with ROMCallDirectRadioButton do begin
		Enabled := NostubRadioButton.Checked;
		if Checked and not Enabled then
			ROMCallKernelRadioButton.Checked := True;
	end;
	with ROMCallMLinkRadioButton do begin
		Enabled := NostubRadioButton.Checked;
		if Checked and not Enabled then
			ROMCallKernelRadioButton.Checked := True;
	end;
	with ROMCallKernelRadioButton do begin
		Enabled := not PreOSRadioButton.Checked;
		if Checked and not Enabled then
			ROMCallCompressedRadioButton.Checked := True;
	end;
	with ROMCallCompressedRadioButton do begin
		Enabled := not DoorsRadioButton.Checked;
		if Checked and not Enabled then
			ROMCallKernelRadioButton.Checked := True;
	end;
	with BSSMLinkRadioButton do begin
		Enabled := NostubRadioButton.Checked;
		if Checked and not Enabled then
			BSSKernelRadioButton.Checked := True;
	end;
	with BSSKernelRadioButton do begin
		Enabled := not PreOSRadioButton.Checked;
		if Checked and not Enabled then
			BSSCompressedRadioButton.Checked := True;
	end;
	with BSSCompressedRadioButton do begin
		Enabled := not DoorsRadioButton.Checked;
		if Checked and not Enabled then
			BSSKernelRadioButton.Checked := True;
	end;
	RelocRadioButtonClick (Sender);
end;

procedure TProgramOptionsForm.MinAMSCheckBoxClick(Sender: TObject);
begin
	MinAMSEdit.Enabled := MinAMSCheckBox.Checked;
end;

procedure TProgramOptionsForm.RelocRadioButtonClick(Sender: TObject);
begin
	with InternalFLineEmulatorCheckBox do begin
		Enabled := (RelocFLineJumpsCheckBox.Checked or ROMCallFLineRadioButton.Checked) and (not RelocFLineJumps4ByteCheckBox.Checked);
		if not Enabled then
			Checked := RelocFLineJumps4ByteCheckBox.Checked;
	end;
	with ROMCallOptimizedCheckBox do begin
		Enabled := (ROMCallDirectRadioButton.Checked or ROMCallFLineRadioButton.Checked) and NostubRadioButton.Checked;
		if not Enabled then
			Checked := False;
	end;
end;

procedure TProgramOptionsForm.ItemMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if Button = mbRight then
		if Sender is TControl then
			with TControl (Sender) do
				MainForm.DocFile.KeywordLookup (Hint);
end;

procedure TProgramOptionsForm.CalcCheckBoxClick(Sender: TObject);
var
	CalcCount: Integer;
begin
	CalcCount := 0;
	if TI89CheckBox.Checked then
		Inc (CalcCount);
	if TI92PlusCheckBox.Checked then
		Inc (CalcCount);
	if V200CheckBox.Checked then
		Inc (CalcCount);
	with OptimizeCalcConstsCheckBox do begin
		Enabled := CalcCount > 1;
		if not Enabled then
			Checked := CalcCount = 1;
	end;
end;

procedure TProgramOptionsForm.RelocFLineJumpsCheckBoxClick(
	Sender: TObject);
begin
	with RelocFLineJumps4ByteCheckBox do begin
		Enabled := RelocFLineJumpsCheckBox.Checked;
		if not Enabled then
			Checked := False;
	end;
	RelocRadioButtonClick (Sender);
end;

end.
