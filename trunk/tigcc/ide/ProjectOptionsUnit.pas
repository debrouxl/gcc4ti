unit ProjectOptionsUnit;

interface

uses
	MasterUnit, ParsingUnit, ProgramOptionsUnit,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, ComCtrls;

type
	TProjectOptionsForm = class(TForm)
		OKButton: TButton;
		CancelButton: TButton;
		PageController: TPageControl;
		GeneralSheet: TTabSheet;
		CompilationSheet: TTabSheet;
		LinkingSheet: TTabSheet;
		PostBuildSheet: TTabSheet;
		TargetGroupBox: TGroupBox;
		PackVarEditLabel: TLabel;
		ExecutableRadioButton: TRadioButton;
    FlashOSRadioButton: TRadioButton;
		FargoRadioButton: TRadioButton;
		ArchiveRadioButton: TRadioButton;
		PackCheckBox: TCheckBox;
		PackVarEdit: TEdit;
		SwitchesGroupBox: TGroupBox;
		GCCSwitchesEditLabel: TLabel;
		GCCSwitchesEdit: TEdit;
		AsSwitchesEditLabel: TLabel;
		AsSwitchesEdit: TEdit;
		AsmSwitchesEditLabel: TLabel;
		AsmSwitchesEdit: TEdit;
		DebugInfoCheckBox: TCheckBox;
		OptimizationGroupBox: TGroupBox;
		OptimizeNOPsCheckBox: TCheckBox;
		OptimizeReturnsCheckBox: TCheckBox;
		OptimizeBranchesCheckBox: TCheckBox;
		OptimizeMovesCheckBox: TCheckBox;
		OptimizeTestsCheckBox: TCheckBox;
		OptimizeCalculationsCheckBox: TCheckBox;
		RemoveUnusedSectionsCheckBox: TCheckBox;
    ReorderSectionsCheckBox: TCheckBox;
		CutUnusedRangesCheckBox: TCheckBox;
		StdLibCheckBox: TCheckBox;
		OutputBinCheckBox: TCheckBox;
		BrowseDialog: TOpenDialog;
		ProcessFileGroupBox: TGroupBox;
		ProcessFileEditLabel: TLabel;
		ProcessFileEdit: TEdit;
		BrowseButton: TButton;
		ExecutionGroupBox: TGroupBox;
		CommandLineEditLabel: TLabel;
		CommandLineEdit: TEdit;
		DataVarCheckBox: TCheckBox;
		DataVarEditLabel: TLabel;
		DataVarEdit: TEdit;
		DataVarCopyLabel: TLabel;
		DataVarCopyPanel: TPanel;
		DataVarCopyNeverRadioButton: TRadioButton;
		DataVarCopyIfArchivedRadioButton: TRadioButton;
		DataVarCopyAlwaysRadioButton: TRadioButton;
		ProgramOptionsButton: TButton;
    MergeConstantsCheckBox: TCheckBox;
    InitBSSCheckBox: TCheckBox;
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure DataVarCheckBoxClick(Sender: TObject);
		procedure PackCheckBoxClick(Sender: TObject);
		procedure VarEditChange(Sender: TObject);
		procedure TargetRadioButtonClick(Sender: TObject);
		procedure BrowseButtonClick(Sender: TObject);
		procedure ProgramOptionsButtonClick(Sender: TObject);
	private
	public
		InitialLibOptions: TPredefinedLibOptions;
		ProgramOptionsForm: TProgramOptionsForm;
	end;

implementation

{$R *.DFM}

uses
	CalcUnit,
	MainUnit;

procedure TProjectOptionsForm.FormDestroy(Sender: TObject);
begin
	if Assigned (ProgramOptionsForm) then
		ProgramOptionsForm.Free;
end;

procedure TProjectOptionsForm.FormShow(Sender: TObject);
begin
	TargetRadioButtonClick (Sender);
end;

procedure TProjectOptionsForm.DataVarCheckBoxClick(Sender: TObject);
begin
	DataVarEdit.Enabled := DataVarCheckBox.Checked;
	DataVarEditLabel.Enabled := DataVarEdit.Enabled;
	DataVarCopyPanel.Enabled := DataVarEdit.Enabled;
	DataVarCopyNeverRadioButton.Enabled := DataVarEdit.Enabled;
	DataVarCopyIfArchivedRadioButton.Enabled := DataVarEdit.Enabled;
	DataVarCopyAlwaysRadioButton.Enabled := DataVarEdit.Enabled;
	DataVarCopyLabel.Enabled := DataVarEdit.Enabled;
	VarEditChange (Sender);
end;

procedure TProjectOptionsForm.PackCheckBoxClick(Sender: TObject);
begin
	PackVarEdit.Enabled := PackCheckBox.Checked;
	PackVarEditLabel.Enabled := PackVarEdit.Enabled;
	VarEditChange (Sender);
end;

procedure TProjectOptionsForm.VarEditChange(Sender: TObject);
var
	S: string;
	I: Integer;
	OK,
	HasFolder: Boolean;
begin
	if PackCheckBox.Checked then begin
		S := PackVarEdit.Text;
		OK := (Length (S) > 0) and (Length (S) <= 8) and (IsCharAlpha (S [1]) or (S [1] in ['A'..'Z', 'a'..'z'])) and (LowerCase (S) <> LowerCase (MainForm.TopNode.Text));
		if OK then
			for I := Length (S) downto 1 do
				if (not (IsCharAlphaNumeric (S [I]) or (S [I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']))) or ((I = 1) and (S [I] = '_')) then begin
					OK := False;
					Break;
				end;
	end else
		OK := True;
	if OK then begin
		if DataVarCheckBox.Checked then begin
			S := DataVarEdit.Text;
			if PackCheckBox.Checked and (LowerCase (DataVarEdit.Text) = LowerCase (PackVarEdit.Text)) then
				OK := False
			else begin
				OK := (Length (S) > 0) and (IsCharAlpha (S [1]) or (S [1] in ['A'..'Z', 'a'..'z'])) and (LowerCase (S) <> LowerCase (MainForm.TopNode.Text));
				if OK then begin
					HasFolder := False;
					for I := Length (S) downto 1 do begin
						if S [I] = '\' then begin
							if HasFolder then begin
								OK := False;
								Break;
							end else
								HasFolder := True;
						end else if (not (IsCharAlphaNumeric (S [I]) or (S [I] in ['A'..'Z', 'a'..'z', '0'..'9', '_']))) or ((I = 1) and (S [I] = '_')) then begin
							OK := False;
							Break;
						end;
					end;
					I := Pos ('\', S);
					if I > 0 then begin
						if (I - 1 > MaxNameLength) or (Length (S) - I > MaxNameLength) or (I + 1 > Length (S)) or (not (IsCharAlpha (S [I + 1]) or (S [I + 1] in ['A'..'Z', 'a'..'z']))) then
							OK := False;
					end else begin
						if Length (S) > MaxNameLength then
							OK := False;
					end;
				end;
			end;
		end;
	end;
	OKButton.Enabled := OK;
end;

procedure TProjectOptionsForm.TargetRadioButtonClick(Sender: TObject);
begin
	LinkingSheet.TabVisible := ExecutableRadioButton.Checked or FlashOSRadioButton.Checked or FargoRadioButton.Checked;
	PostBuildSheet.TabVisible := LinkingSheet.TabVisible;
	DataVarCheckBox.Enabled := ExecutableRadioButton.Checked and (ssPack in SpecialSupport);
	if not DataVarCheckBox.Enabled then
		DataVarCheckBox.Checked := False;
	PackCheckBox.Enabled := ExecutableRadioButton.Checked and (ssPack in SpecialSupport);
	if not PackCheckBox.Enabled then
		PackCheckBox.Checked := False;
	OutputBinCheckBox.Enabled := not FlashOSRadioButton.Checked;
	if not OutputBinCheckBox.Enabled then
		OutputBinCheckBox.Checked := True;
	DataVarCheckBoxClick (Sender);
	PackCheckBoxClick (Sender);
	ProgramOptionsButton.Visible := ExecutableRadioButton.Checked;
end;

procedure TProjectOptionsForm.BrowseButtonClick(Sender: TObject);
var
	S: string;
begin
	with BrowseDialog do begin
		S := ProcessFileEdit.Text;
		if (Length (S) > 0) and ((S [1] = '"') or (Pos (' ', S) <= 0)) then begin
			if S [1] = '"' then begin
				Delete (S, 1, 1);
				if Pos ('"', S) > 0 then
					Delete (S, Pos ('"', S), Length (S));
			end;
			FileName := S;
		end;
		if Execute then
			ProcessFileEdit.Text := '"' + FileName + '" "($TI89File)" "($TI92PlusFile)" "($V200File)"';
	end;
end;

procedure TProjectOptionsForm.ProgramOptionsButtonClick(Sender: TObject);
begin
	if not Assigned (ProgramOptionsForm) then begin
		ProgramOptionsForm := TProgramOptionsForm.Create(Self);
		if Assigned (InitialLibOptions) then
			with ProgramOptionsForm, InitialLibOptions do begin
				TI89CheckBox.Checked     := cdTI89     in CalcDests;
				TI92PlusCheckBox.Checked := cdTI92Plus in CalcDests;
				V200CheckBox.Checked     := cdV200     in CalcDests;
				OptimizeCalcConstsCheckBox.Checked := OptimizeCalcConsts;
				case KernelFormat of
					kfNone:             NoStubRadioButton.Checked := True;
					kfStandard:         DoorsRadioButton.Checked  := True;
					kfCompressedTables: PreOsRadioButton.Checked  := True;
				end;
				MinAMSCheckBox.Checked := UseMinAMS;
				MinAMSEdit.Text        := MinAMS;
				UnofficialOSSupportCheckBox.Checked := UnofficialOSSupport;
				case RelocFormat of
					rfAMS:         RelocAMSRadioButton.Checked          := True;
					rfKernel:      RelocKernelRadioButton.Checked       := True;
					rfCompressed:  RelocCompressedRadioButton.Checked   := True;
          rfMlink:       RelocMlinkRadioButton.Checked        := True;
				end;
				case ROMCallFormat of
					rfDirect:      ROMCallDirectRadioButton.Checked     := True;
					rfKernel:      ROMCallKernelRadioButton.Checked     := True;
					rfCompressed:  ROMCallCompressedRadioButton.Checked := True;
          rfMlink:       ROMCallMlinkRadioButton.Checked      := True;
					rfFLine:       ROMCallFLineRadioButton.Checked      := True;
				end;
				case BSSRefFormat of
					rfNone:        BSSMergeRadioButton.Checked          := True;
					rfKernel:      BSSKernelRadioButton.Checked         := True;
					rfCompressed:  BSSCompressedRadioButton.Checked     := True;
          rfMlink:       BSSMlinkRadioButton.Checked          := True;
				end;
				case DataRefFormat of
					rfKernel:      DataVarKernelRadioButton.Checked     := True;
					rfCompressed:  DataVarCompressedRadioButton.Checked := True;
          rfMlink:       DataVarMlinkRadioButton.Checked      := True;
				end;
				RelocFLineJumpsCheckBox.Checked      := UseFLineJumps;
				RelocFLineJumps4ByteCheckBox.Checked := Use4ByteFLineJumps;
				ROMCallOptimizedCheckBox.Checked := OptimizeROMCalls;
				InternalFLineEmulatorCheckBox.Checked := UseInternalFLineEmulator;
				if UseReturnValue then
					ReturnValueRadioButton.Checked := True
				else
					ReturnDoneRadioButton.Checked  := True;
				EnableErrorReturnCheckBox.Checked := EnableErrorReturn;
				LCDSaveCheckBox.Checked           := SaveScreen;
			end;
	end;
	ProgramOptionsForm.ShowModal;
end;

end.
