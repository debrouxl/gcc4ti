{
  This Delphi unit is part of TIGCC.

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

unit UtilsWin;

interface

uses
	Windows;

type
	TMessageButtons = (mbOK, mbOKCancel, mbYesNo, mbYesNoCancel, mbRetryCancel, mbAbortRetryIgnore);
	TMessageIcon = (miNone, miAsterisk, miInformation, miQuestion, miExclamation, miWarning, miHand, miStop, miError);
	TMessageModalState = (msDefault, msApplModal, msSystemModal, msTaskModal);
	TDefaultMessageType = (mtDefault, mtInformation, mtQuestion, mtWarning, mtProgramError, mtDeviceError);

function ShowMessageBox(const Content, Title: string; Buttons: TMessageButtons = mbOK; Icon: TMessageIcon = miNone; ModalState: TMessageModalState = msDefault; DisplayHelp: Boolean = False; DefaultButton: Integer = 0; Flags: Integer = 0): Integer;
function ShowDefaultMessageBox(const Content, Title: string; MessageType: TDefaultMessageType = mtDefault; Critical: Boolean = False; DisplayHelp: Boolean = False; DefaultButton: Integer = 0): Integer;

function CompressMessageFlags(Buttons: TMessageButtons; Icon: TMessageIcon; ModalState: TMessageModalState; DisplayHelp: Boolean; DefaultButton: Integer): Integer;

implementation

function ShowMessageBox;
begin
	Result := MessageBox (GetActiveWindow, PChar (Content), PChar (Title), Flags or CompressMessageFlags (Buttons, Icon, ModalState, DisplayHelp, DefaultButton));
end;

function ShowDefaultMessageBox;
var
	Buttons: TMessageButtons;
	Icon:    TMessageIcon;
begin
	case MessageType of
		mtInformation: begin
			if Critical then
				Buttons := mbOKCancel
			else
				Buttons := mbOK;
			Icon := miInformation;
		end;
		mtQuestion: begin
			if Critical then
				Buttons := mbYesNoCancel
			else
				Buttons := mbYesNo;
			Icon := miQuestion;
		end;
		mtWarning: begin
			if Critical then
				Buttons := mbOKCancel
			else
				Buttons := mbOK;
			Icon := miWarning;
		end;
		mtProgramError: begin
			if Critical then
				Buttons := mbOKCancel
			else
				Buttons := mbOK;
			Icon := miError;
		end;
		mtDeviceError: begin
			if Critical then
				Buttons := mbRetryCancel
			else
				Buttons := mbAbortRetryIgnore;
			Icon := miError;
		end;
		else begin
			if Critical then
				Buttons := mbOKCancel
			else
				Buttons := mbOK;
			Icon := miNone;
		end;
	end;
	Result := ShowMessageBox (Content, Title, Buttons, Icon, msDefault, DisplayHelp, DefaultButton, 0);
end;

function CompressMessageFlags;
begin
	case Buttons of
		mbOK:               Result := mb_OK;
		mbOKCancel:         Result := mb_OKCancel;
		mbYesNo:            Result := mb_YesNo;
		mbYesNoCancel:      Result := mb_YesNoCancel;
		mbRetryCancel:      Result := mb_RetryCancel;
		mbAbortRetryIgnore: Result := mb_AbortRetryIgnore;
		else                Result := 0;
	end;
	case Icon of
		miAsterisk:    Result := Result or mb_IconAsterisk;
		miInformation: Result := Result or mb_IconInformation;
		miQuestion:    Result := Result or mb_IconQuestion;
		miExclamation: Result := Result or mb_IconExclamation;
		miWarning:     Result := Result or mb_IconWarning;
		miHand:        Result := Result or mb_IconHand;
		miStop:        Result := Result or mb_IconStop;
		miError:       Result := Result or mb_IconError;
	end;
	case ModalState of
		msApplModal:   Result := Result or mb_ApplModal;
		msSystemModal: Result := Result or mb_SystemModal;
		msTaskModal:   Result := Result or mb_TaskModal;
	end;
	if DisplayHelp then
		Result := Result or mb_Help;
	case DefaultButton of
		1: Result := Result or mb_DefButton1;
		2: Result := Result or mb_DefButton2;
		3: Result := Result or mb_DefButton3;
		4: Result := Result or mb_DefButton4;
	end;
end;

end.
