program UpdateInclude;

uses
	Windows,
	SysUtils,
	Classes,
	UtilsDos,
	UtilsWin,
	HelpSystemMasterUnit in '..\HelpSystemMasterUnit.pas';

const
	HTMLStartBlock = '<TABLE CLASS="DEFSUBTABLE"><TR>%s<TD CLASS="DEFINITION">';
	HTMLEndBlock = '</TD></TR></TABLE>';
	HTMLStartCommentBlock = '<FONT CLASS="COMMENT">';
	HTMLEndCommentBlock = '</FONT>';
	IndentWidth = 16;

var
	S1,
	S2,
	HeaderFile,
	Contents,
	WebContents,
	IniContents,
	Description,
	PrepDescription,
	CHMUnknownLink,
	WebUnknownLink,
	CHMMinAMSLink,
	WebMinAMSLink,
	CHMDefineLink,
	WebDefineLink,
	CHMAttributeLink,
	WebAttributeLink,
	CHMAMSDepAddrLink,
	CHMAMSDepFuncTechLink,
	CHMAMSDepMacroTechLink,
	CHMAMSDepFuncAMSLink,
	CHMAMSDepMacroAMSLink,
	CHMExplicitRegParmLink: string;
	HeaderFiles,
	LanguageExts,
	Functions,
	Variables,
	Constants,
	Types,
	Others,
	ROMCallCount,
	DefsByAMS: TStringList;
	I,
	J,
	L,
	CloseCount: Integer;

type
	TDefInfo = class(TObject)
	public
		Constants,
		RefTypes,
		DefTypes,
		Variables,
		Functions,
		Others: string;
	end;

procedure InitializeList(out List: TStringList);
begin
	List := TStringList.Create;
	List.Sorted := True;
	List.Duplicates := dupAccept;
end;

procedure FinalizeList(var List: TStringList);
begin
	List.Free;
	List := nil;
end;

function IsAlNum(C: Char): Boolean;
begin
	Result := C in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'];
end;

function EncodeIdentifier(const Identifier: string): string;
begin
	Result := Identifier;
	if (Length (Result) >= 2) and (Result [Length (Result)] in ['0'..'9']) and (not (Result [Length (Result) - 1] in ['0'..'9'])) then
		Insert (' ', Result, Length (Result));
end;

function DecodeIdentifier(const Identifier: string): string;
var
	P: Integer;
begin
	Result := Identifier;
	P := Pos (' ', Result);
	if P > 0 then begin
		if P = Length (Result) - 1 then
			Delete (Result, P, 1)
		else
			raise ESubStrNotFound.CreateFmt ('Invalid Identifier: %s', [Result]);
	end;
end;

function GetDefine(const Value: string): string;
begin
	Result := Trim (Value);
	if Pos ('-', Result) > 0 then
		Result := '(' + Result + ')'
	else begin
		try
			StrToInt (Result);
		except
			Result := '(' + Result + ')';
		end;
	end;
end;

function DefinedTypeWord(const TypeName: string): Boolean;
var
	I: Integer;
begin
	Result := (TypeName = LowerCase (TypeName)) and (FileExists (SystemFolder + '\Keywords\' + TypeName + '.hsk') or FileExists (SystemFolder + '\Keywords\' + TypeName + '.ref'));
	if not Result then
		with Types do
			for I := 0 to Count - 1 do
				if Copy (Strings [I], 1, Pos (',', Strings [I]) - 1) = EncodeIdentifier (TypeName) then begin
					Result := True;
					Break;
				end;
end;

function DefinedType(const TypeName: string): Boolean;
var
	I: Integer;
	CurWord: string;
begin
	Result := True;
	CurWord := '';
	for I := 1 to Length (TypeName) do
		if IsAlNum (TypeName [I]) then
			CurWord := CurWord + TypeName [I]
		else begin
			if Length (CurWord) > 0 then begin
				if not DefinedTypeWord (CurWord) then begin
					Result := False;
					Break;
				end;
				CurWord := '';
			end;
		end;
	if (Length (CurWord) > 0) and (not DefinedTypeWord (CurWord)) then
		Result := False;
end;

function ParseDefinition(const Definition, IdentifierName: string; Keep: Boolean = False; Builtin: Boolean = False; LibCall: Boolean = False; AsmLibCall: Boolean = False; NoReturn: Boolean = False; TIOSCallback: Boolean = False; TIOSStyle: Boolean = False; Attributes: string = ''; ROMCall: Boolean = False; ROMIdx: Integer = -1; Reference: Boolean = False; AddressHack: string = ''; ValueHack: string = ''; RealMinAMSValue: string = ''; MinAMSROMCallCount: Integer = 0; Registers: TStringList = nil; NeedInUseBit: Boolean = False): string;
function HandleInUseBit(const Def: string): string;
begin
	if NeedInUseBit then
		Result := '({__need_in_use_bit;' + Def + ';})'
	else
		Result := Def;
end;
var
	I,
	P1,
	P2: Integer;
	DefType: (dtUnknown, dtFunction, dtVariable, dtConstant, dtMacro);
	S,
	Identifier,
	ReturnType,
	ArgStr,
	ArgType,
	ArgName,
	ArgReg: string;
	Args: array of string;
	ArgNames: array of string;
	HasAddressHack,
	HasValueHack: Boolean;
begin
	if (Pos (IdentifierName, Definition) <= 0) and (Copy (Definition, 1, 2) <> '/*') then
		raise ESubStrNotFound.CreateFmt ('%s not found in %s', [IdentifierName, Definition]);
	if not BalancedParentheses (Definition) then
		raise ESubStrNotFound.CreateFmt ('Unbalanced parentheses in %s', [Definition]);
	if not BalancedParentheses (AddressHack) then
		raise ESubStrNotFound.CreateFmt ('Unbalanced parentheses in address hack in %s', [Definition]);
	if not BalancedParentheses (ValueHack) then
		raise ESubStrNotFound.CreateFmt ('Unbalanced parentheses in value hack in %s', [Definition]);
	if Length (RealMinAMSValue) > 0 then begin
		RealMinAMSValue := IntToStr (Round (StrToFloat (RealMinAMSValue) * 100));
		HasAddressHack := Length (AddressHack) > 0;
		HasValueHack := Length (ValueHack) > 0;
	end else begin
		HasAddressHack := False;
		HasValueHack := False;
	end;
	DefType := dtUnknown;
	Result := Definition;
	if (Pos ('asm(', Definition) > 0) or (Pos ('asm (', Definition) > 0) or (Pos ('asm volatile', Definition) > 0) or (Pos ('typeof', Definition) > 0) then
		Exit;
	Identifier := IdentifierName;
	repeat
		P1 := Pos ('/*', Result);
		P2 := Pos ('*/', Result);
		if P1 > 0 then begin
			if P2 < P1 then
				raise ESubStrNotFound.CreateFmt ('Invalid comment in %s', [Definition]);
			Delete (Result, P1, P2 - P1 + 2);
		end;
	until P1 <= 0;
	for I := Length (Attributes) downto 1 do
		if (Attributes [I] = ' ') and ((I - 1 <= 0) or (not IsAlNum (Attributes [I - 1])) or (I + 1 > Length (Attributes)) or (not IsAlNum (Attributes [I + 1]))) then
			Delete (Attributes, I, 1);
	if HasAddressHack then
		for I := Length (AddressHack) downto 1 do
			if (AddressHack [I] = ' ') and ((I - 1 <= 0) or (not IsAlNum (AddressHack [I - 1])) or (I + 1 > Length (AddressHack)) or (not IsAlNum (AddressHack [I + 1]))) then
				Delete (AddressHack, I, 1);
	if HasValueHack then
		for I := Length (ValueHack) downto 1 do
			if (ValueHack [I] = ' ') and ((I - 1 <= 0) or (not IsAlNum (ValueHack [I - 1])) or (I + 1 > Length (ValueHack)) or (not IsAlNum (ValueHack [I + 1]))) then
				Delete (ValueHack, I, 1);
	if (not Keep) and (Pos ('(', Result) > 0) and (not (StartsWith ('#define', Result) or StartsWith ('typedef', Result) or StartsWith ('enum', Result) or StartsWith ('struct', Result) or StartsWith ('union', Result))) then
		DefType := dtFunction;
	if StartsWith ('#define ', Result) then begin
		S := Copy (Result, Length ('#define ') + 1, Length (Result));
		I := Pos (' ', S);
		if I > 0 then begin
			if Pos ('(', Copy (S, 1, I - 1)) > 0 then
				DefType := dtMacro
			else
				DefType := dtConstant;
		end;
	end;
	if (DefType = dtUnknown) and ROMCall and (Pos ('(', Result) <= 0) then
		DefType := dtVariable;
	case DefType of
		dtFunction: begin
			Identifier := Trim (Copy (Definition, 1, Pos ('(', Definition) - 1));
			ReturnType := Trim (Copy (Identifier, 1, LastPos (' ', Identifier) - 1));
			Delete (Identifier, 1, Length (ReturnType) + 1);
			if (not DefinedType (ReturnType)) and (ShowDefaultMessageBox (Format ('Undefined return type "%s" in %s', [ReturnType, Definition]), 'Error', mtProgramError, True) = idCancel) then
				Abort;
			if (Length (Identifier) > 0) and (Identifier [1] = '*') then begin
				Delete (Identifier, 1, 1);
				ReturnType := ReturnType + '*';
			end;
			SetLength (Args, 0);
			SetLength (ArgNames, 0);
			ArgStr := Copy (Definition, Pos ('(', Definition) + 1, Length (Definition));
			Delete (ArgStr, LastPos (')', ArgStr), Length (ArgStr));
			ArgStr := ArgStr + ',';
			while Pos (',', ArgStr) > 0 do begin
				P1 := Pos (',', ArgStr);
				while (P1 <= Length (ArgStr)) and (not BalancedParentheses (Copy (ArgStr, 1, P1 - 1))) do
					Inc (P1);
				ArgType := Trim (Copy (ArgStr, 1, P1 - 1));
				Delete (ArgStr, 1, P1);
				ArgName := '';
				if Pos ('(', ArgType) <= 0 then begin
					if Pos ('*', ArgType) > 0 then
						Insert (' ', ArgType, LastPos ('*', ArgType) + 1);
					P1 := LastPos (' ', ArgType);
					if (P1 > 0) and (not DefinedType (ArgType)) then begin
						ArgName := Trim (Copy (ArgType, P1 + 1, Length (ArgType)));
						Delete (ArgType, P1, Length (ArgType));
					end;
				end;
				ArgType := Trim (ArgType);
				if Length (ArgType) > 0 then begin
					if (not DefinedType (ArgType)) and (ShowDefaultMessageBox (Format ('Undefined argument type "%s" in %s', [ArgType, Definition]), 'Error', mtProgramError, True) = idCancel) then
						Abort;
					P1 := Pos ('*', ArgType);
					if P1 > 0 then begin
						if ArgType [P1 - 1] = ' ' then
							Delete (ArgType, P1 - 1, 1);
						if StartsWith ('unsigned short*', ArgType, True, 1, True) then
							ArgType := '__pushort' + ArgType;
						if StartsWith ('short*', ArgType, True, 1, True) then
							ArgType := '__pshort' + ArgType;
						if StartsWith ('unsigned long*', ArgType, True, 1, True) then
							ArgType := '__pulong' + ArgType;
						if StartsWith ('long*', ArgType, True, 1, True) then
							ArgType := '__plong' + ArgType;
					end else begin
						StartsWith ('signed ', ArgType, True, 1, True);
						StartsWith ('unsigned ', ArgType, True, 1, True);
					end;
					if Assigned (Registers) and (Length (ArgName) > 0) then begin
						ArgReg := Registers.Values [ArgName];
						if Length (ArgReg) > 0 then
							ArgType := ArgType + ' asm("' + ArgReg + '")';
					end;
					SetLength (Args, Length (Args) + 1);
					Args [High (Args)] := ArgType;
					if ArgType <> 'void' then begin
						SetLength (ArgNames, Length (ArgNames) + 1);
						ArgNames [High (ArgNames)] := ArgName;
					end;
				end;
			end;
			if RomCall and (ROMIdx >= 0) then begin
				if (ReturnType = 'float') or (ReturnType = 'double') then begin
					if Length (Args) = 1 then
						Result := '#define ' + Identifier + '(x) ' + HandleInUseBit ('_tios_float_1(' + IntToHex (ROMIdx, 1) + ',x,' + Args [Low (Args)] + ')')
					else if Length (Args) = 2 then
						Result := '#define ' + Identifier + '(x,y) ' + HandleInUseBit ('_tios_float_2(' + IntToHex (ROMIdx, 1) + ',x,y,' + Args [Low (Args)] + ',' + Args [Low (Args) + 1] + ')')
					else
						raise ESubStrNotFound.CreateFmt ('Invalid float return in %s', [Definition]);
				end else begin
					Result := '_rom_call';
					if HasAddressHack then
						Result := Result + '_hack';
					if (Length (Attributes) > 0) or (MinAMSROMCallCount > 0) then
						Result := Result + '_attr';
					if MinAMSROMCallCount > 0 then
						Result := Result + '_concat';
					Result := Result + '(' + ReturnType + ',(';
					for I := Low (Args) to High (Args) do
						Result := Result + Args [I] + ',';
					if Result [Length (Result)] = ',' then
						Delete (Result, Length (Result), 1);
					Result := Result + '),';
					if (Length (Attributes) > 0) or (MinAMSROMCallCount > 0) then
						Result := Result + Attributes + ',';
					if MinAMSROMCallCount > 0 then
						Result := Result + '0x' + IntToHex (ROMIdx, 1) + ',_ROM_CALL_' + IntToHex (ROMIdx, 1)
					else
						Result := Result + IntToHex (ROMIdx, 1);
					if HasAddressHack then
						Result := Result + ',(' + AddressHack + '),' + RealMinAMSValue;
					if MinAMSROMCallCount > 0 then
						Result := Result + ',0x' + IntToHex (MinAMSROMCallCount, 1);
					Result := Result + ')';
					Result := '#define ' + Identifier + ' ' + HandleInUseBit (Result);
				end;
				if HasValueHack then begin
					Result := StringReplace (Result, '#define ' + Identifier, '#define _' + Identifier, []) + #13#10
						+ '#if MIN_AMS>=' + RealMinAMSValue + #13#10
						+ '#define ' + Identifier + ' _' + Identifier + #13#10
						+ '#elif !defined(UNOFFICIAL_OS_SUPPORT)'#13#10
						+ '#define ' + Identifier + '(';
					for I := Low (ArgNames) to High (ArgNames) do begin
						if I > Low (ArgNames) then
							Result := Result + ',';
						Result := Result + ArgNames [I];
					end;
					Result := Result + ') (TIOS_entries>0x' + IntToHex (ROMIdx, 1) + '?_' + Identifier + '(';
					for I := Low (ArgNames) to High (ArgNames) do begin
						if I > Low (ArgNames) then
							Result := Result + ',';
						Result := Result + '(' + ArgNames [I] + ')';
					end;
					Result := Result + '):(' + ValueHack + '))'#13#10
						+ '#endif';
				end;
			end else begin
				if (Length (ReturnType) > 0) and (ReturnType [Length (ReturnType)] = '*') then
					Result := Copy (ReturnType, 1, Length (ReturnType) - 1) + ' *' + Identifier
				else
					Result := ReturnType + ' ' + Identifier;
				Result := Result + '(';
				for I := Low (Args) to High (Args) do
					Result := Result + Args [I] + ',';
				if Result [Length (Result)] = ',' then
					Delete (Result, Length (Result), 1);
				Result := Result + ')';
				if Builtin then
					Result := Result + '__ATTR_GCC__'
				else if LibCall then begin
					if TIOSCallback then
						Result := 'extern ' + Result + '__ATTR_TIOS_CALLBACK__'
					else if TIOSStyle then
						Result := 'extern ' + Result + '__ATTR_TIOS__'
					else begin
						Result := 'extern ' + Result + '__ATTR_LIB';
						if AsmLibCall then
							Result := Result + '_ASM'
						else
							Result := Result + '_C';
						if NoReturn then
							Result := Result + '_NORETURN';
						Result := Result + '__';
					end;
				end;
				if Length (Attributes) > 0 then
					Result := Result + ' ' + Attributes;
				Result := Result + ';';
			end;
		end;
		dtVariable: begin
			Identifier := Copy (Result, LastPos (' ', Result) + 1, Length (Result));
			if Length (Identifier) > 0 then begin
				while Identifier [1] in ['*', ' '] do
					Delete (Identifier, 1, 1);
				ReturnType := Trim (Copy (Result, 1, Length (Result) - Length (Identifier)));
				while Identifier [Length (Identifier)] = ';' do
					Delete (Identifier, Length (Identifier), 1);
				if (not DefinedType (ReturnType)) and (ShowDefaultMessageBox (Format ('Undefined variable type "%s" in %s', [ReturnType, Definition]), 'Error', mtProgramError, True) = idCancel) then
					Abort;
				for I := Length (ReturnType) downto 1 do
					if (ReturnType [I] = ' ') and ((I - 1 <= 0) or (not IsAlNum (ReturnType [I - 1])) or (I + 1 > Length (ReturnType)) or (not IsAlNum (ReturnType [I + 1]))) then
						Delete (ReturnType, I, 1);
				if ROMCall then begin
					Result := '#define ' + Identifier + ' ';
					if not Reference then
						Result := Result + '(*';
					Result := Result + '((' + ReturnType;
					if not Reference then
						Result := Result + '*';
					Result := Result + ')(_rom_call_addr';
					if HasAddressHack then begin
						Result := Result + '_hack';
					end;
					if MinAMSROMCallCount > 0 then
						Result := Result + '_concat(0x' + IntToHex (ROMIdx, 1) + ',_ROM_CALL_' + IntToHex (ROMIdx, 1)
					else
						Result := Result + '(' + IntToHex (ROMIdx, 1);
					if HasAddressHack then
						Result := Result + ',(' + AddressHack + '),' + RealMinAMSValue;
					if MinAMSROMCallCount > 0 then
						Result := Result + ',0x' + IntToHex (MinAMSROMCallCount, 1);
					Result := Result + ')))';
					if not Reference then
						Result := Result + ')';
					if HasValueHack then begin
						Result := StringReplace (Result, '#define ' + Identifier, '#define _' + Identifier, []) + #13#10
							+ '#if MIN_AMS>=' + RealMinAMSValue + #13#10
							+ '#define ' + Identifier + ' _' + Identifier + #13#10
							+ '#elif !defined(UNOFFICIAL_OS_SUPPORT)'#13#10
							+ '#define ' + Identifier + ' ((' + ReturnType + ')(' + ValueHack + '))'#13#10
							+ '#endif';
					end;
				end else
					Result := 'extern ' + ReturnType + ' ' + Identifier + ';';
			end else
				Result := '';
		end;
		dtConstant: begin
			if not StartsWith ('#define ', Result) then
				Abort;
			S := Copy (Result, Length ('#define ') + 1, Length (Result));
			for I := Length (S) downto Pos (' ', S) + 1 do
				if (S [I] = ' ') and ((I - 1 <= 0) or (not IsAlNum (S [I - 1])) or (I + 1 > Length (S)) or (not IsAlNum (S [I + 1]))) then
					Delete (S, I, 1);
			Result := '#define ' + S;
		end;
		dtMacro: begin
			if not StartsWith ('#define ', Result) then
				Abort;
			S := Copy (Result, Length ('#define ') + 1, Length (Result));
			for I := Length (S) downto Pos (')', S) + 2 do
				if (S [I] = ' ') and ((I - 1 <= 0) or (not IsAlNum (S [I - 1])) or (I + 1 > Length (S)) or (not IsAlNum (S [I + 1]))) and (Copy (S, I + 2, 2) <> '##') then
					Delete (S, I, 1);
			Result := '#define ' + S;
		end;
		else begin
			for I := Length (Result) downto 1 do
				if (Result [I] = ' ') and ((I - 1 <= 0) or (not IsAlNum (Result [I - 1])) or (I + 1 > Length (Result)) or (not IsAlNum (Result [I + 1]))) then
					Delete (Result, I, 1);
		end;
	end;
	if Identifier <> IdentifierName then
		raise ESubStrNotFound.CreateFmt ('%s is not the identifier of %s', [IdentifierName, Definition]);
end;

procedure InsertFile(const HeaderFile, Section: string);
var
	Sec,
	Line,
	Identifier: string;
	CurCategory: TStrings;
	Ln,
	I: Integer;
begin
	Identifier := '';
	CurCategory := Others;
	with TStringList.Create do try
		LoadFromFile (SystemFolder + '\Include\' + HeaderFile + '\' + Section + '.hsf');
		Sec := '';
		for Ln := 0 to Count - 1 do begin
			Line := Strings [Ln];
			if Length (Line) > 0 then begin
				IniReadSec (Line, Sec);
				if Sec = 'main' then begin
					if IniReadString (Line, 'Name') then
						Identifier := Line
					else if IniReadString (Line, 'Type') then begin
						Line := LowerCase (Line);
						if Line = 'language extension' then
							CurCategory := LanguageExts
						else if Line = 'function' then
							CurCategory := Functions
						else if Line = 'variable' then
							CurCategory := Variables
						else if Line = 'constant' then
							CurCategory := Constants
						else if Line = 'type' then
							CurCategory := Types;
					end else if IniReadString (Line, 'Header Files') then begin
						if HeaderFile = UpdateInclude.HeaderFile then
							with TStringList.Create do try
								CommaText := Line;
								if Pos (HeaderFile, Text) <= 0 then
									raise ESubStrNotFound.CreateFmt ('Invalid header file info in %s/%s', [HeaderFile, Section]);
								for I := 0 to Count - 1 do
									if Strings [I] <> HeaderFile then begin
										if (not FileExists (SystemFolder + '\Include\' + Strings [I] + '\' + Section + '.ref')) or (LoadFile (SystemFolder + '\Include\' + Strings [I] + '\' + Section + '.ref') <> HeaderFile) then
											WriteFile (SystemFolder + '\Include\' + Strings [I] + '\' + Section + '.ref', HeaderFile);
									end;
							finally
								Free;
							end;
					end;
				end;
			end;
		end;
	finally
		Free;
	end;
	CurCategory.Add (EncodeIdentifier (Identifier) + ', ' + HeaderFile + '/' + Section);
	if (HeaderFile <> 'unknown.h') and (FileExists (DestFolder + '\unknown_' + Section + '.html') or FileExists (SystemFolder + '\Include\unknown.h\' + Section + '.hsf')) then begin
		if ShowDefaultMessageBox ('Remove ' + Section + ' from unknown.h (also defined in ' + HeaderFile + ')?', 'Question', mtQuestion) = idYes then begin
			if FileExists (DestFolder + '\unknown_' + Section + '.html') then
				DeleteFile (DestFolder + '\unknown_' + Section + '.html');
			if FileExists (SystemFolder + '\Include\unknown.h\' + Section + '.hsf') then
				DeleteFile (SystemFolder + '\Include\unknown.h\' + Section + '.hsf');
		end;
	end;
end;

procedure WriteContents(const Heading: string; List: TStrings; const IconFile: string = '');
var
	I: Integer;
	S,
	Identifier,
	HeaderFile,
	Section,
	Description,
	PrepDescription: string;
begin
	if List.Count > 0 then begin
		if Length (IconFile) > 0 then
			Contents := Contents + '<SPAN CLASS="SMALLICON"><IMG SRC="' + IconFile + '.gif" WIDTH="20" HEIGHT="20" BORDER="0"></SPAN>&nbsp;';
		Contents := Contents + '<SPAN CLASS="HEADING">' + Heading + '</SPAN>'#13#10
			+ '<DL INDENT="20">';
		WebContents := WebContents + '<H3><U>' + Heading + '</U></H3>'#13#10
			+ '<DL INDENT="20">';
		with List do
			for I := 0 to Count - 1 do begin
				S := Strings [I];
				Identifier := DecodeIdentifier (DeleteToAfterFirst (', ', S));
				if Length (Identifier) > 0 then begin
					HeaderFile := DeleteToAfterFirst ('/', S);
					Section := S;
					S := LoadFile (SystemFolder + '\Include\' + HeaderFile + '\' + Section + '.hsf');
					DeleteToAfterFirst ('[Description]', S);
					Description := Trim (DeleteToFirst (#13#10'[', S));
					if (Length (Description) > 0) and (Description [Length (Description)] <> '.') then
						raise ESubStrNotFound.CreateFmt ('Missing "." at end of description in %s/%s', [HeaderFile, Section]);
					PrepDescription := Description;
					PrepareBody (PrepDescription, True, UpdateInclude.HeaderFile);
					Contents := Contents + '<DT><B><A HREF="' + EncodeLink (HeaderFile + '/' + Section, True, UpdateInclude.HeaderFile) + '">' + Identifier + '</A></B>';
					if Length (PrepDescription) > 0 then begin
						Contents := Contents + '<DD>' + PrepDescription;
						if I < Count - 1 then
							Contents := Contents + '<IMG WIDTH="1" HEIGHT="19" ALIGN="TOP">';
					end else
						Contents := Contents + '<BR><BR>';
					PrepDescription := Description;
					PrepareBody (PrepDescription, False, UpdateInclude.HeaderFile);
					WebContents := WebContents + '<DT><B><A HREF="' + EncodeLink (HeaderFile + '/' + Section, False, UpdateInclude.HeaderFile) + '">' + Identifier + '</A></B>';
					if Length (PrepDescription) > 0 then begin
						WebContents := WebContents + '<DD>' + PrepDescription;
						if I < Count - 1 then
							WebContents := WebContents + '<IMG WIDTH="1" HEIGHT="20" ALIGN="TOP">';
					end else
						WebContents := WebContents + '<BR><BR>';
				end;
			end;
		Contents := Contents + '</DL>'#13#10;
		WebContents := WebContents + '</DL>'#13#10;
	end;
end;

function HTMLGetIndentText(Indent: Integer): string;
begin
	if Indent > 0 then
		Result := Format ('<TD CLASS="DEFBG" WIDTH="%d"></TD>', [Indent * IndentWidth])
	else
		Result := '';
end;

procedure HTMLStartDefinition(out Definition: string; out Indent: Integer);
begin
	Indent := 0;
	Definition := Format (HTMLStartBlock, ['']);
end;

procedure HTMLStartLine(var Definition: string; Indent: Integer);
begin
	Definition := Definition + HTMLEndBlock + #13#10 + Format (HTMLStartBlock, [HTMLGetIndentText (Indent)]);
end;

procedure HTMLStartComment(var Definition: string; Indent: Integer);
begin
	Definition := Definition + HTMLStartCommentBlock;
end;

procedure HTMLEndComment(var Definition: string; Indent: Integer);
begin
	Definition := Definition + HTMLEndCommentBlock;
end;

procedure HTMLIndent(var Definition: string; var Indent: Integer);
begin
	Inc (Indent);
	HTMLStartLine (Definition, Indent);
end;

procedure HTMLUnindent(var Definition: string; var Indent: Integer);
var
	S: string;
begin
	S := HTMLEndBlock + #13#10 + Format (HTMLStartBlock, [HTMLGetIndentText (Indent)]);
	if Copy (Definition, Length (Definition) - Length (S) + 1, Length (S)) = S then
		Delete (Definition, Length (Definition) - Length (S) + 1, Length (S));
	Dec (Indent);
	HTMLStartLine (Definition, Indent);
end;

procedure HTMLEndDefinition(var Definition: string; var Indent: Integer);
begin
	while Indent > 0 do
		HTMLUnindent (Definition, Indent);
	Definition := Definition + HTMLEndBlock;
end;

procedure InsertLinks(var Definition: string; CHMSystem: Boolean; const HeaderFile, Section, Identifier: string; Identifiers: TStrings);
var
	I: Integer;
	S,
	TypeName,
	LinkValue: string;
begin
	with Identifiers do
		for I := 0 to Count - 1 do begin
			S := Strings [I];
			TypeName := DecodeIdentifier (DeleteToAfterFirst (', ', S));
			if (TypeName <> Identifier) and (Pos (TypeName, Definition) > 0) then begin
				LinkValue := S;
				S := Definition;
				Definition := '';
				while Length (S) > 0 do begin
					Definition := Definition + DeleteToFirst (TypeName, S);
					if Length (S) > 0 then begin
						DeleteToAfterFirst (TypeName, S, 0);
						if not (((Length (Definition) > 0) and (Definition [Length (Definition)] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'])) or ((Length (S) > 0) and (S [1] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']))) then begin
							Definition := Definition + '<A HREF="' + EncodeLink (LinkValue, CHMSystem, HeaderFile, Section) + '">' + TypeName + '</A>';
						end else
							Definition := Definition + TypeName;
					end;
				end;
			end;
		end;
end;

procedure ClearWithObjects(L: TStrings);
var
	I: Integer;
begin
	with L do begin
		for I := Count - 1 downto 0 do begin
			Objects[I].Free;
			Objects [I] := nil;
		end;
		Clear;
	end;
end;

procedure WriteSections(List: TStrings; const IconFile: string = '');
var
	Ln,
	I,
	J,
	P,
	PStart,
	PEnd,
	CurPos: Integer;
	Sec,
	Line,
	S,
	TempS,
	SectionContents,
	Identifier,
	HeaderFile,
	Section,
	Category,
	SubType,
	TypeStr,
	MinAMS,
	SubMinAMS,
	PrevSubMinAMS,
	Definition,
	TempDefinition,
	Attributes,
	AddressHack,
	ValueHack,
	RealMinAMSValue,
	Body,
	PrepBody,
	MinAMSStr,
	HeaderStr,
	SeeAlso,
	Alias,
	RefOut,
	RefIn: string;
	IniCont,
	HeaderFiles,
	EnumConstants,
	NeededBy,
	RegisterList: TStringList;
	ROMIdx,
	Indent,
	MinAMSROMCallCount: Integer;
	HasOtherSections,
	HasRealDefinition,
	AsmLibCall,
	NoReturn,
	TIOSCallback,
	TIOSStyle,
	Reference,
	VirtualEnum,
	InComment,
	LineBreakAfterComment,
	NeedDoc,
	NeedInUseBit: Boolean;
procedure InsertDefinition(var S: string);
var
	I,
	P,
	CurPos: Integer;
begin
	CurPos := Length (S) + 1;
	if Assigned (NeededBy) then
		with NeededBy do
			for I := 0 to Count - 1 do begin
				P := Pos (Strings [I], S);
				if P > 0 then begin
					while (P - 1 > 0) and (S [P - 1] <> #10) do
						Dec (P);
					if CurPos > P then
						CurPos := P;
				end;
			end;
	System.Insert (TempDefinition + #13#10, S, CurPos);
end;
begin
	with List do
		for I := 0 to Count - 1 do begin
			S := Strings [I];
			Identifier := DecodeIdentifier (DeleteToAfterFirst (', ', S));
			HeaderFile := DeleteToAfterFirst ('/', S);
			Section := S;
			if True then begin
				HeaderFiles := TStringList.Create;
				EnumConstants := nil;
				NeededBy := nil;
				RegisterList := nil;
				try
					Category := '';
					SubType := '';
					MinAMS := '1.00';
					Definition := '';
					TempDefinition := '';
					HasRealDefinition := False;
					Attributes := '';
					Alias := '';
					SeeAlso := '';
					ROMIdx := -1;
					AddressHack := '';
					ValueHack := '';
					RealMinAMSValue := '';
					MinAMSROMCallCount := 0;
					AsmLibCall := False;
					NoReturn := False;
					TIOSCallback := False;
					TIOSStyle := False;
					Reference := False;
					VirtualEnum := False;
					RefOut := '';
					RefIn := '';
					NeedInUseBit := False;
					IniCont := TStringList.Create;
					with IniCont do try
						LoadFromFile (SystemFolder + '\Include\' + HeaderFile + '\' + Section + '.hsf');
						Sec := '';
						HasOtherSections := False;
						for Ln := 0 to Count - 1 do begin
							Line := Strings [Ln];
							if Length (Line) > 0 then begin
								IniReadSec (Line, Sec);
								if Sec = 'main' then begin
									if IniReadString (Line, 'Type') then
										Category := Line
									else if IniReadString (Line, 'Subtype') then
										SubType := Line
									else if IniReadString (Line, 'Header Files') then
										HeaderFiles.CommaText := Line
									else if IniReadString (Line, 'MinAMS') then
										MinAMS := Line
									else if IniReadString (Line, 'Definition') then
										Definition := Line
									else if IniReadString (Line, 'Real Definition') then begin
										TempDefinition := Line;
										HasRealDefinition := True;
									end else if IniReadString (Line, 'Attributes') then
										Attributes := Line
									else if IniReadString (Line, 'Alias') then
										Alias := Line
									else if IniReadString (Line, 'Needed by') then begin
										if not Assigned (NeededBy) then
											NeededBy := TStringList.Create;
										NeededBy.CommaText := Line;
									end else if IniReadString (Line, 'See Also') then
										SeeAlso := Line;
								end else if Sec = 'rom call' then begin
									if IniReadString (Line, 'Index') then
										ROMIdx := StrToInt (Line)
									else if IniReadString (Line, 'Reference') then
										Reference := StrToBool (Line)
									else if IniReadString (Line, 'Address Hack') then
										AddressHack := Line
									else if IniReadString (Line, 'Value Hack') then
										ValueHack := Line
									else if IniReadString (Line, 'MinAMS') then
										RealMinAMSValue := Line;
								end else if Sec = 'library call' then begin
									if IniReadString (Line, 'Asm') then
										AsmLibCall := StrToBool (Line)
									else if IniReadString (Line, 'NoReturn') then
										NoReturn := StrToBool (Line)
									else if IniReadString (Line, 'TIOS Callback') then
										TIOSCallback := StrToBool (Line)
									else if IniReadString (Line, 'TIOS') then
										TIOSStyle := StrToBool (Line);
								end else if Sec = 'enum' then begin
									if IniReadString (Line, 'Virtual') then
										VirtualEnum := StrToBool (Line);
								end else if Sec = 'registers' then begin
									if not Assigned (RegisterList) then
										RegisterList := TStringList.Create;
									RegisterList.Add (Line);
								end else if Sec = 'references' then begin
									if IniReadString (Line, 'Out') then
										RefOut := Line
									else if IniReadString (Line, 'In') then
										RefIn := Line
									else if IniReadString (Line, 'Out_EV_eventLoop') then
										NeedInUseBit := StrToBool (Line);
								end else if (Length (Sec) > 0) and (Sec <> 'description') and (Sec <> 'explanation') then
									HasOtherSections := True;
							end;
						end;
						if ((Length (Definition) > 0) and (Definition [1] <> '#') and (Definition [Length (Definition)] <> ';')) or ((Length (TempDefinition) > 0) and (TempDefinition [1] <> '#') and (TempDefinition [Length (TempDefinition)] <> ';') and (Copy (TempDefinition, 1, 2) <> '/*')) then
							raise ESubStrNotFound.CreateFmt ('Missing ";" at end of definition in %s/%s', [HeaderFile, Section]);
						if Length (ROMCallCount.Values [MinAMS]) <= 0 then
							raise ESubStrNotFound.CreateFmt ('MinAMS value of "%s" in %s/%s unsupported', [MinAMS, HeaderFile, Section]);
						if not HasRealDefinition then
							TempDefinition := Definition;
						TypeStr := Category;
						if Assigned (RegisterList) then
							TypeStr := TypeStr + '<A HREF="' + CHMExplicitRegParmLink + '">*</A>';
						if Length (SubType) > 0 then begin
							TypeStr := TypeStr + ' (' + SubType;
							if UpperCase (SubType) = 'ROM CALL' then begin
								if ROMIdx >= 0 then begin
									TypeStr := TypeStr + ' 0x' + IntToHex (ROMIdx, 1);
									if (Length (AddressHack) > 0) or (Length (ValueHack) > 0) then begin
										if Length (RealMinAMSValue) > 0 then begin
											S := ROMCallCount.Values [RealMinAMSValue];
											if Length (S) > 0 then
												MinAMSROMCallCount := StrToInt (S)
											else
												raise ESubStrNotFound.CreateFmt ('MinAMS value of "%s" in %s/%s unsupported', [RealMinAMSValue, HeaderFile, Section]);
										end;
									end;
									if (StrToInt (ROMCallCount.Values [MinAMS]) <= ROMIdx) and (not HasRealDefinition) then begin
										if (Length (AddressHack) > 0) or (Length (ValueHack) > 0) then begin
											if Length (RealMinAMSValue) <= 0 then
												with ROMCallCount do
													for J := 0 to Count - 1 do
														if StrToInt (Values [Names [J]]) > ROMIdx then begin
															RealMinAMSValue := Names [J];
															Break;
														end;
										end else begin
											with ROMCallCount do
												for J := 0 to Count - 1 do
													if StrToInt (Values [Names [J]]) > ROMIdx then begin
														MinAMS := Names [J];
														Break;
													end;
											for J := 0 to Count - 1 do begin
												S := Strings [J];
												if StartsWith ('MinAMS=', S) then begin
													Strings [J] := 'MinAMS=' + MinAMS;
													Break;
												end else if Length (S) <= 0 then begin
													Insert (J, 'MinAMS=' + MinAMS);
													Break;
												end;
											end;
											SaveToFile (SystemFolder + '\Include\' + HeaderFile + '\' + Section + '.hsf');
										end;
									end;
								end;
								if (UpperCase (Category) = 'FUNCTION') and (UpperCase (SubType) = 'ROM CALL') and (HasRealDefinition or (Length (AddressHack) > 0) or (Length (ValueHack) > 0) or StartsWith ('float ', Definition) or StartsWith ('double ', Definition)) then begin
									if StartsWith ('#define', TempDefinition) or (Length (AddressHack) > 0) or (Length (ValueHack) > 0) then begin
										if StartsWith ('#define ' + Identifier + ' (*', TempDefinition) or (Length (AddressHack) > 0) then
											TypeStr := TypeStr + '<A HREF="' + CHMAMSDepAddrLink + '">*</A>'
										else if StrToInt (ROMCallCount.Values [MinAMS]) <= ROMIdx then
											TypeStr := TypeStr + '<A HREF="' + CHMAMSDepMacroAMSLink + '">*</A>'
										else
											TypeStr := TypeStr + '<A HREF="' + CHMAMSDepMacroTechLink + '">*</A>';
									end else begin
										if StrToInt (ROMCallCount.Values [MinAMS]) <= ROMIdx then
											TypeStr := TypeStr + '<A HREF="' + CHMAMSDepFuncAMSLink + '">*</A>'
										else
											TypeStr := TypeStr + '<A HREF="' + CHMAMSDepFuncTechLink + '">*</A>';
									end;
								end else if (UpperCase (Category) = 'VARIABLE') and (UpperCase (SubType) = 'ROM CALL') and HasRealDefinition then
									TypeStr := TypeStr + '*';
							end;
							TypeStr := TypeStr + ')';
						end;
						if Length (TempDefinition) > 0 then begin
							// These don't work yet in GCC
							if (Attributes = '__attribute__((__pure__))') or
								(Attributes = '__attribute__((__const__))') or
								(Attributes = '__attribute__((__malloc__))') then
								Attributes := '';
							TempDefinition := ParseDefinition (TempDefinition, Identifier, HasRealDefinition, UpperCase (SubType) = 'BUILTIN', UpperCase (SubType) = 'TIGCC.A', AsmLibCall, NoReturn, TIOSCallback, TIOSStyle, Attributes, UpperCase (SubType) = 'ROM CALL', ROMIdx, Reference, AddressHack, ValueHack, RealMinAMSValue, MinAMSROMCallCount, RegisterList, NeedInUseBit);
							if UpperCase (SubType) = 'ENUMERATION' then begin
								EnumConstants := TStringList.Create;
								with EnumConstants do begin
									S := TempDefinition;
									DeleteToAfterFirst ('{', S);
									CommaText := StringReplace (DeleteToFirst ('}', S), ' ', '', [rfReplaceAll]);
									if VirtualEnum then
										TempDefinition := '';
									if Count > 0 then begin
										PrevSubMinAMS := '';
										CurPos := 0;
										for J := 0 to Count - 1 do begin
											SubMinAMS := '';
											if HasOtherSections then begin
												Sec := '';
												for Ln := 0 to IniCont.Count - 1 do begin
													Line := IniCont.Strings [Ln];
													if Length (Line) > 0 then begin
														IniReadSec (Line, Sec);
														if Sec = LowerCase (Names [J]) then begin
															if IniReadString (Line, 'MinAMS') then begin
																SubMinAMS := Line;
																Break;
															end;
														end;
													end;
												end;
											end;
											if VirtualEnum then begin
												if PrevSubMinAMS <> SubMinAMS then begin
													if CurPos > 0 then begin
														TempDefinition := TempDefinition + '#endif'#13#10;
														CurPos := 0;
													end;
													if Length (SubMinAMS) > 0 then begin
														TempDefinition := TempDefinition + '#if MIN_AMS>=' + IntToStr (Round (StrToFloat (SubMinAMS) * 100)) + #13#10;
														CurPos := 1;
													end;
													PrevSubMinAMS := SubMinAMS;
												end;
												S := Strings [J];
												P := Pos ('=', S);
												if P > 0 then
													TempDefinition := TempDefinition + '#define ' + Names [J] + ' ' + GetDefine (Copy (S, P + 1, Length (S))) + #13#10;
											end else begin
												if PrevSubMinAMS <> SubMinAMS then begin
													if CurPos > 0 then begin
														System.Insert (#13#10'#endif'#13#10, TempDefinition, CurPos);
														CurPos := 0;
													end;
													if Length (SubMinAMS) > 0 then begin
														CurPos := Pos (',' + Names [J], TempDefinition);
														if CurPos > 0 then
															System.Insert (#13#10'#if MIN_AMS>=' + IntToStr (Round (StrToFloat (SubMinAMS) * 100)) + #13#10, TempDefinition, CurPos);
													end;
													PrevSubMinAMS := SubMinAMS;
												end;
												if CurPos > 0 then begin
													CurPos := Pos (',' + Names [J], TempDefinition);
													if CurPos > 0 then begin
														Inc (CurPos, Length (',' + Names [J]));
														while (CurPos <= Length (TempDefinition)) and (not (TempDefinition [CurPos] in [',', '}'])) do
															Inc (CurPos);
													end else
														raise ESubStrNotFound.CreateFmt ('Unknown format for %s in %s/%s', [Names [J], HeaderFile, Section]);
												end;
											end;
										end;
										if CurPos > 0 then begin
											if VirtualEnum then
												TempDefinition := TempDefinition + '#endif'
											else
												System.Insert (#13#10'#endif'#13#10, TempDefinition, CurPos);
										end;
										TempDefinition := StringReplace (TempDefinition, #13#10#13#10, #13#10, [rfReplaceAll]);
										if Copy (TempDefinition, Length (TempDefinition) - 1, 2) = #13#10 then
											System.Delete (TempDefinition, Length (TempDefinition) - 1, 2);
									end;
								end;
							end;
						end;
						if Length (TempDefinition) > 0 then begin
							if Length (Alias) > 0 then
								TempDefinition := TempDefinition + #13#10'#define ' + Alias + ' ' + Identifier;
							if (HeaderFiles.Count > 1) and (not StartsWith ('#define', TempDefinition)) then
								TempDefinition := '#ifndef __HAVE_' + Identifier + #13#10'#define __HAVE_' + Identifier + #13#10 + TempDefinition + #13#10'#endif';
							with DefsByAMS do begin
								if IndexOf (MinAMS) < 0 then
									AddObject (MinAMS, TDefInfo.Create);
								with Objects [IndexOf (MinAMS)] as TDefInfo do begin
									if List = UpdateInclude.Constants then
										InsertDefinition (Constants)
									else if List = UpdateInclude.Types then begin
										if HeaderFile = UpdateInclude.HeaderFile then
											InsertDefinition (DefTypes)
										else
											InsertDefinition (RefTypes);
									end else if List = UpdateInclude.Variables then
										InsertDefinition (Variables)
									else if List = UpdateInclude.Functions then
										InsertDefinition (Functions)
									else
										InsertDefinition (Others);
								end;
							end;
						end;
						if HeaderFile = UpdateInclude.HeaderFile then begin
							if MinAMS <> '1.00' then
								MinAMSStr := '<A HREF="' + CHMMinAMSLink + '">AMS ' + MinAMS + ' or higher</A>'
							else
								MinAMSStr := '';
							HeaderStr := '';
							with HeaderFiles do
								for J := 0 to Count - 1 do begin
									HeaderStr := HeaderStr + '<A HREF="' + EncodeLink (Strings [J] + '/', True, HeaderFile, Section) + '">' + Strings [J] + '</A>';
									if J < Count - 1 then
										HeaderStr := HeaderStr + ', '
									else
										HeaderStr := HeaderStr + #13#10;
								end;
							SectionContents := GetGenericFileStart (Identifier, Identifier, TypeStr, IconFile, MinAMSStr, '', HeaderStr);
							if Length (Identifier) > 0 then begin
								WebContents := WebContents + '<HR>'#13#10
									+ '<H3><A NAME="' + Section + '"><U>' + Identifier + '</U></A></H3>'#13#10;
								if MinAMS <> '1.00' then
									WebContents := WebContents + '<P><A HREF="' + WebMinAMSLink + '">AMS ' + MinAMS + ' or higher</A></P>'#13#10;
								if Length (Definition) > 0 then begin
									Definition := HTMLize (Definition);
									TempDefinition := Definition;
									S := '';
									InComment := False;
									for J := Length (Definition) downto 0 do begin
										if InComment then begin
											if Copy (Definition, J, 2) = '/*' then
												InComment := False;
										end else begin
											if (J > 0) and (Definition [J] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) then
												S := Definition [J] + S
											else if Copy (Definition, J, 2) = '*/' then
												InComment := True
											else begin
												if FileExistsWithCase (SystemFolder + '\Keywords\' + S + '.hsk') or FileExistsWithCase (SystemFolder + '\Keywords\' + S + '.ref') then begin
													System.Insert ('</A></B>', Definition, J + Length (S) + 1);
													System.Insert ('<B><A HREF="' + EncodeInfoLink ('keywords/' + S, True, HeaderFile, Section) + '">', Definition, J + 1);
												end;
												S := '';
											end;
										end;
									end;
									InsertLinks (Definition, True, HeaderFile, Section, Identifier, Types);
									InsertLinks (Definition, True, HeaderFile, Section, Identifier, Constants);
									if StartsWith ('#define', Definition) then begin
										System.Insert ('</A></B>', Definition, Length ('#define') + 1);
										System.Insert ('<B><A HREF="' + CHMDefineLink + '">', Definition, 1);
									end;
									PStart := Pos ('__ATTR_', Definition);
									if PStart > 0 then begin
										PEnd := PStart + Length ('__ATTR_');
										while (PEnd <= Length (Definition)) and (IsAlNum (Definition [PEnd])) do
											Inc (PEnd);
										System.Insert ('</A>', Definition, PEnd);
										System.Insert ('<A HREF="' + CHMAttributeLink + '">', Definition, PStart);
									end;
									PStart := Pos ('CALLBACK', Definition);
									if PStart > 0 then begin
										PEnd := PStart + Length ('CALLBACK');
										if ((PStart - 1 <= 0) or (not IsAlNum (Definition [PStart - 1]))) and ((PEnd > Length (Definition)) or (not IsAlNum (Definition [PEnd]))) then begin
											System.Insert ('</A>', Definition, PEnd);
											System.Insert ('<A HREF="' + CHMAttributeLink + '">', Definition, PStart);
										end;
									end;
									if Pos ('{ ', Definition) > 0 then begin
										S := Definition;
										HTMLStartDefinition (Definition, Indent);
										LineBreakAfterComment := False;
										while Length (S) > 0 do begin
											if Copy (S, 1, 2) = '{ ' then begin
												System.Delete (S, 1, 2);
												Definition := Definition + '{';
												HTMLIndent (Definition, Indent);
											end else if Copy (S, 1, 2) = ' }' then begin
												System.Delete (S, 1, 2);
												HTMLUnindent (Definition, Indent);
												Definition := Definition + '}';
											end else if Copy (S, 1, 2) = '; ' then begin
												System.Delete (S, 1, 1);
												if Copy (S, 1, 2) <> ' }' then
													System.Delete (S, 1, 1);
												if Copy (S, 1, 2) = '/*' then begin
													Definition := Definition + '; ';
													LineBreakAfterComment := True;
												end else begin
													Definition := Definition + ';';
													HTMLStartLine (Definition, Indent);
												end;
											end else if Copy (S, 1, 2) = '/*' then begin
												System.Delete (S, 1, 2);
												HTMLStartComment (Definition, Indent);
												Definition := Definition + '/*';
											end else if Copy (S, 1, 2) = '*/' then begin
												System.Delete (S, 1, 2);
												Definition := Definition + '*/';
												HTMLEndComment (Definition, Indent);
												if LineBreakAfterComment then begin
													if (Copy (S, 1, 1) = ' ') and (Copy (S, 1, 2) <> ' }') then
														System.Delete (S, 1, 1);
													HTMLStartLine (Definition, Indent);
													LineBreakAfterComment := False;
												end;
											end else begin
												Definition := Definition + S [1];
												System.Delete (S, 1, 1);
											end;
										end;
										HTMLEndDefinition (Definition, Indent);
										Definition := '<TABLE CLASS="DEFTABLE"><TR><TD CLASS="DEFBG">'#13#10 + Definition + #13#10'</TD></TR></TABLE>';
									end else
										Definition := '<TABLE CLASS="DEFTABLE"><TR><TD CLASS="DEFINITION">' + Definition + '</TD></TR></TABLE>';
									SectionContents := SectionContents + '<P>' + Definition + #13#10;
									Definition := TempDefinition;
									S := '';
									InComment := False;
									for J := Length (Definition) downto 0 do begin
										if InComment then begin
											if Copy (Definition, J, 2) = '/*' then
												InComment := False;
										end else begin
											if (J > 0) and (Definition [J] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']) then
												S := Definition [J] + S
											else if Copy (Definition, J, 2) = '*/' then
												InComment := True
											else begin
												if FileExistsWithCase (SystemFolder + '\Keywords\' + S + '.hsk') or FileExistsWithCase (SystemFolder + '\Keywords\' + S + '.ref') then begin
													System.Insert ('</A></B>', Definition, J + Length (S) + 1);
													System.Insert ('<B><A HREF="' + EncodeInfoLink ('keywords/' + S, False, HeaderFile, Section) + '">', Definition, J + 1);
												end;
												S := '';
											end;
										end;
									end;
									InsertLinks (Definition, False, HeaderFile, Section, Identifier, Types);
									InsertLinks (Definition, False, HeaderFile, Section, Identifier, Constants);
									if StartsWith ('#define', Definition) then begin
										System.Insert ('</A></B>', Definition, Length ('#define') + 1);
										System.Insert ('<B><A HREF="' + WebDefineLink + '">', Definition, 1);
									end;
									PStart := Pos ('__ATTR_', Definition);
									if PStart > 0 then begin
										PEnd := PStart + Length ('__ATTR_');
										while (PEnd <= Length (Definition)) and (IsAlNum (Definition [PEnd])) do
											Inc (PEnd);
										System.Insert ('</A>', Definition, PEnd);
										System.Insert ('<A HREF="' + WebAttributeLink + '">', Definition, PStart);
									end;
									PStart := Pos ('CALLBACK', Definition);
									if PStart > 0 then begin
										PEnd := PStart + Length ('CALLBACK');
										if ((PStart - 1 <= 0) or (not IsAlNum (Definition [PStart - 1]))) and ((PEnd > Length (Definition)) or (not IsAlNum (Definition [PEnd]))) then begin
											System.Insert ('</A>', Definition, PEnd);
											System.Insert ('<A HREF="' + WebAttributeLink + '">', Definition, PStart);
										end;
									end;
									if Pos ('{ ', Definition) > 0 then begin
										S := Definition;
										Indent := 0;
										Definition := '';
										LineBreakAfterComment := False;
										while Length (S) > 0 do begin
											if Copy (S, 1, 2) = '{ ' then begin
												System.Delete (S, 1, 2);
												Inc (Indent);
												Definition := Definition + '{'#13#10
													+	'<TABLE><TR><TD WIDTH="12"></TD><TD CLASS="CODE">'#13#10;
											end else if Copy (S, 1, 2) = ' }' then begin
												System.Delete (S, 1, 2);
												Dec (Indent);
												Definition := Definition + '</TD></TR></TABLE>'#13#10'}';
											end else if Copy (S, 1, 2) = '; ' then begin
												System.Delete (S, 1, 1);
												if Copy (S, 1, 2) <> ' }' then
													System.Delete (S, 1, 1);
												if Copy (S, 1, 2) = '/*' then begin
													Definition := Definition + '; ';
													LineBreakAfterComment := True;
												end else begin
													Definition := Definition + ';<BR>'#13#10;
												end;
											end else if Copy (S, 1, 2) = '/*' then begin
												System.Delete (S, 1, 2);
												Definition := Definition + '<I>/*';
											end else if Copy (S, 1, 2) = '*/' then begin
												System.Delete (S, 1, 2);
												Definition := Definition + '*/</I>';
												if LineBreakAfterComment then begin
													if (Copy (S, 1, 1) = ' ') and (Copy (S, 1, 2) <> ' }') then
														System.Delete (S, 1, 1);
													Definition := Definition + '<BR>'#13#10;
													LineBreakAfterComment := False;
												end;
											end else begin
												Definition := Definition + S [1];
												System.Delete (S, 1, 1);
											end;
										end;
									end;
									WebContents := WebContents + '<P><TABLE BORDER="1" CELLPADDING="2"><TR><TD CLASS="CODE">' + Definition + '</TD></TR></TABLE></P>'#13#10;
								end;
							end;
							S := Text;
							DeleteToAfterFirst ('[Description]', S);
							Body := Trim (DeleteToFirst (#13#10'[', S));
							NeedDoc := (Length (Body) <= 0) or (Pos ('TI-Basic', Body) > 0);
							if Length (Body) > 0 then begin
								PrepBody := Body;
								PrepareBody (PrepBody, True, HeaderFile, Section, False, True);
								SectionContents := SectionContents + '<P CLASS="ITEMDESC">' + PrepBody + #13#10;
								if Length (Identifier) > 0 then begin
									PrepBody := Body;
									PrepareBody (PrepBody, False, HeaderFile, Section, True, True);
									if Length (Body) > 0 then
										WebContents := WebContents + PrepBody + #13#10;
								end;
							end;
							S := Text;
							DeleteToAfterFirst ('[Explanation]', S);
							Body := Trim (DeleteToFirst (#13#10'[', S));
							if Length (Body) > 0 then begin
								if Assigned (EnumConstants) then
									with EnumConstants do
										for J := 0 to Count - 1 do begin
											Body := StringReplace (Body, '>' + Names [J] + '</TD>', '><A NAME="' + Names [J] + '">' + Names [J] + '</A></TD>', []);
											Body := StringReplace (Body, '>' + Names [J] + '&nbsp;&nbsp;', '><A NAME="' + Names [J] + '">' + Names [J] + '</A>&nbsp;&nbsp;', []);
										end;
								PrepBody := Body;
								PrepareBody (PrepBody, True, HeaderFile, Section, False);
								SectionContents := SectionContents + '<P>' + PrepBody + #13#10;
								if Length (Identifier) > 0 then begin
									PrepBody := Body;
									PrepareBody (PrepBody, False, HeaderFile, Section, True);
									WebContents := WebContents + PrepBody + #13#10
								end;
							end else if NeedDoc then
								SectionContents := SectionContents + '<P>This identifier has not been documented yet. Please <A HREF="' + CHMUnknownLink + '">help us out</A>.'#13#10;
							if (Length (RefOut) > 0) or (Length (RefIn) > 0) then begin
								SectionContents := SectionContents + '<P><HR>';
								if Length (RefOut) > 0 then
									SectionContents := SectionContents + 'Uses: ' + GetSeeAlsoText (RefOut, True, HeaderFile, Section) + #13#10;
								if Length (RefIn) > 0 then begin
									if Length (RefOut) > 0 then
										SectionContents := SectionContents + '<BR>';
									SectionContents := SectionContents + 'Used by: ' + GetSeeAlsoText (RefIn, True, HeaderFile, Section) + #13#10;
								end;
							end;
							if Length (Alias) > 0 then begin
								SectionContents := SectionContents + '<P><HR>Deprecated alias: ' + Alias + #13#10;
								if Length (Identifier) > 0 then
									WebContents := WebContents + '<P>Deprecated alias: ' + Alias + '</P>'#13#10;
							end;
							if Length (SeeAlso) > 0 then begin
								SectionContents := SectionContents + '<P><HR>See also: ' + GetSeeAlsoText (SeeAlso, True, HeaderFile, Section) + #13#10;
								if Length (Identifier) > 0 then
									WebContents := WebContents + '<P>See also: ' + GetSeeAlsoText (SeeAlso, False, HeaderFile, Section) + '</P>'#13#10;
							end;
							SectionContents := SectionContents + GetGenericFileEnd;
							WriteFile (DestFolder + '\' + WithoutExt (HeaderFile) + '_' + Section + '.html', SectionContents);
						end;
					finally
						Free;
					end;
				finally
					if Assigned (RegisterList) then
						RegisterList.Free;
					if Assigned (NeededBy) then
						NeededBy.Free;
					if Assigned (EnumConstants) then
						EnumConstants.Free;
					HeaderFiles.Free;
				end;
			end;
		end;
end;

procedure WriteContentTree(List: TStrings);
var
	I: Integer;
	S,
	Identifier,
	HeaderFile,
	Section: string;
begin
	if List.Count > 0 then begin
		with List do
			for I := 0 to Count - 1 do begin
				S := Strings [I];
				Identifier := DecodeIdentifier (DeleteToAfterFirst (', ', S));
				if Length (Identifier) > 0 then begin
					HeaderFile := DeleteToAfterFirst ('/', S);
					Section := S;
					Contents := Contents + '<LI> <OBJECT TYPE="TEXT/SITEMAP">'#13#10
						+ '<PARAM NAME="Name" VALUE="' + Identifier + '">'#13#10
						+ '<PARAM NAME="Local" VALUE="' + EncodeLink (HeaderFile + '/' + Section, True) + '">'#13#10
						+ '</OBJECT>'#13#10;
				end;
			end;
	end;
end;

begin
	try
		HeaderFiles := TStringList.Create;
		ROMCallCount := TStringList.Create;
		DefsByAMS := TStringList.Create;
		try
			HeaderFiles.Sorted := True;
			DefsByAMS.Sorted := True;
			DecimalSeparator := '.';
			ROMCallCount.LoadFromFile (SystemFolder + '\Include\MinAMS.chk');
			CHMUnknownLink := EncodeInfoLink (LoadFile (SystemFolder + '\Include\Unknown.irf'), True);
			WebUnknownLink := EncodeInfoLink (LoadFile (SystemFolder + '\Include\Unknown.irf'), False);
			CHMMinAMSLink := EncodeInfoLink (LoadFile (SystemFolder + '\Include\MinAMS.irf'), True);
			WebMinAMSLink := EncodeInfoLink (LoadFile (SystemFolder + '\Include\MinAMS.irf'), False);
			CHMDefineLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Keywords\define.irf')), True);
			WebDefineLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Keywords\define.irf')), False);
			CHMAttributeLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\attribute.irf')), True);
			WebAttributeLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\attribute.irf')), False);
			CHMAMSDepAddrLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\amsdep_addr.irf')), True);
			CHMAMSDepFuncTechLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\amsdep_func_tech.irf')), True);
			CHMAMSDepMacroTechLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\amsdep_macro_tech.irf')), True);
			CHMAMSDepFuncAMSLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\amsdep_func_ams.irf')), True);
			CHMAMSDepMacroAMSLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\amsdep_macro_ams.irf')), True);
			CHMExplicitRegParmLink := EncodeInfoLink (Trim (LoadFile (SystemFolder + '\Include\regparm_explicit.irf')), True);
			if (ParamCount < 1) or (UpperCase (ParamStr (1)) = '/ALL') then begin
				with TFileReferences.Create do try
					SearchForDirs (SystemFolder + '\Include', atStd);
					CopyFullNamesToStrings (HeaderFiles);
				finally
					Free;
				end;
				Contents := GetGenericFileStart ('Header File Index', 'TIGCC Library Header Files', '', 'info');
				WebContents := GetWebFileStart ('Header File Index', 'TIGCC Library Header Files');
				S1 := '';
				if FileExists (SystemFolder + '\Include\Include.hsi') then begin
					S1 := LoadFile (SystemFolder + '\Include\Include.hsi');
					IniContents := S1;
					DeleteToAfterFirst ('[Top]', IniContents);
					Description := Trim (DeleteToFirst (#13#10'[', IniContents));
					if Length (Description) > 0 then begin
						PrepDescription := Description;
						PrepareBody (PrepDescription, True, 'info/hdrindex', '', True);
						Contents := Contents + PrepDescription + #13#10;
						PrepDescription := Description;
						PrepareBody (PrepDescription, False, 'info/hdrindex', '', True);
						WebContents := WebContents + PrepDescription + #13#10;
					end;
				end;
				Contents := Contents + '<TABLE CLASS="NOBORDER" CELLPADDING="1">'#13#10;
				WebContents := WebContents + '<TABLE CLASS="NOBORDER" CELLPADDING="1">'#13#10;
				for J := 0 to HeaderFiles.Count - 1 do begin
					HeaderFile := ExtractFileName (HeaderFiles.Strings [J]);
					S2 := LoadFile (SystemFolder + '\Include\' + HeaderFile + '\' + ChangeFileExt (HeaderFile, '.hsh'));
					DeleteToAfterFirst ('[Description]', S2);
					Description := Trim (DeleteToFirst (#13#10'[', S2));
					PrepDescription := Description;
					PrepareBody (PrepDescription, True, 'info/hdrindex');
					Contents := Contents + '<TR><TD CLASS="NOBORDER" ALIGN="LEFT" VALIGN="TOP">'
						+ '<B><A HREF="' + EncodeLink (HeaderFile + '/', True) + '">' + HeaderFile + '</A></B>'
						+ '</TD><TD CLASS="NOBORDER" WIDTH="10"></TD><TD CLASS="NOBORDER" ALIGN="LEFT">'
						+ PrepDescription
						+ '</TD></TR>'#13#10;
					PrepDescription := Description;
					PrepareBody (PrepDescription, False, 'info/hdrindex');
					WebContents := WebContents + '<TR><TD CLASS="NOBORDER" ALIGN="LEFT" VALIGN="TOP">'
						+ '<B><A HREF="' + EncodeLink (HeaderFile + '/', False) + '">' + HeaderFile + '</A></B>'
						+ '</TD><TD CLASS="NOBORDER" WIDTH="10"></TD><TD CLASS="NOBORDER" ALIGN="LEFT">'
						+ PrepDescription
						+ '</TD></TR>'#13#10;
				end;
				Contents := Contents + '</TABLE>'#13#10;
				WebContents := WebContents + '</TABLE>'#13#10;
				IniContents := S1;
				DeleteToAfterFirst ('[Bottom]', IniContents);
				Description := Trim (DeleteToFirst (#13#10'[', IniContents));
				if Length (Description) > 0 then begin
					PrepDescription := Description;
					PrepareBody (PrepDescription, True, 'info/hdrindex');
					Contents := Contents + '<P>' + PrepDescription + #13#10;
					PrepDescription := Description;
					PrepareBody (PrepDescription, False, 'info/hdrindex', '', True);
					WebContents := WebContents + PrepDescription + #13#10;
				end;
				Contents := Contents + GetGenericFileEnd;
				WebContents := WebContents + GetWebFileEnd;
				WriteFile (DestFolder + '\hdrindex.html', Contents);
				WriteFile (WebFolder + '\hdrindex.html', WebContents);
			end else begin
				if Pos ('\', ParamStr (1)) > 0 then
					HeaderFile := ExtractFilePath (ParamStr (1))
				else
					HeaderFile := ParamStr (1) + '\';
				HeaderFiles.Add (Copy (HeaderFile, 1, Length (HeaderFile) - 1));
			end;
			for J := 0 to HeaderFiles.Count - 1 do begin
				HeaderFile := ExtractFileName (HeaderFiles.Strings [J]);
				if not DirExists (SystemFolder + '\Include\' + HeaderFile) then
					Continue;
				ClearWithObjects (DefsByAMS);
				InitializeList (LanguageExts);
				InitializeList (Functions);
				InitializeList (Variables);
				InitializeList (Constants);
				InitializeList (Types);
				InitializeList (Others);
				try
					with TFileReferences.Create do try
						SearchForFiles (SystemFolder + '\Include\' + HeaderFile + '\*.hsf', atStd);
						for I := 0 to Count - 1 do
							with Items [I] do
								InsertFile (HeaderFile, WithoutExt (FileName));
						SearchForFiles (SystemFolder + '\Include\' + HeaderFile + '\*.ref', atStd);
						for I := 0 to Count - 1 do
							with Items [I] do begin
								S1 := Trim (LoadFile (FullName));
								S2 := WithoutExt (FileName);
								if Pos (HeaderFile, LoadFile (SystemFolder + '\Include\' + S1 + '\' + S2 + '.hsf')) <= 0 then
									raise ESubStrNotFound.CreateFmt ('Invalid reference file %s in %s', [FileName, HeaderFile]);
								InsertFile (S1, S2);
							end;
					finally
						Free;
					end;
					Contents := GetGenericFileStart (HeaderFile, HeaderFile, 'Header File', 'header', '', '', '<A HREF="' + EncodeInfoLink ('hdrindex', True, HeaderFile) + '">Header File Index</A>');
					WebContents := GetWebFileStart (HeaderFile, 'The &lt;' + HeaderFile + '&gt; Header File');
					S1 := '';
					if FileExists (SystemFolder + '\Include\' + HeaderFile + '\' + ChangeFileExt (HeaderFile, '.hsh')) then begin
						S1 := LoadFile (SystemFolder + '\Include\' + HeaderFile + '\' + ChangeFileExt (HeaderFile, '.hsh'));
						IniContents := S1;
						DeleteToAfterFirst ('[Description]', IniContents);
						Description := Trim (DeleteToFirst (#13#10'[', IniContents));
						if Length (Description) > 0 then begin
							PrepDescription := Description;
							PrepareBody (PrepDescription, True, HeaderFile);
							Contents := Contents + '<P CLASS="ITEMDESC">' + PrepDescription + '</P>'#13#10;
							PrepDescription := Description;
							PrepareBody (PrepDescription, False, HeaderFile, '', True, True);
							WebContents := WebContents + PrepDescription + #13#10;
						end;
						IniContents := S1;
						DeleteToAfterFirst ('[Top]', IniContents);
						Description := Trim (DeleteToFirst (#13#10'[', IniContents));
						if Length (Description) > 0 then begin
							PrepDescription := Description;
							PrepareBody (PrepDescription, True, HeaderFile, '', True);
							Contents := Contents + PrepDescription + #13#10;
							PrepDescription := Description;
							PrepareBody (PrepDescription, False, HeaderFile, '', True);
							WebContents := WebContents + PrepDescription + #13#10;
						end;
					end;
					WriteContents ('Language Extensions', LanguageExts, 'langext-s');
					WriteContents ('Functions', Functions, 'function-s');
					WriteContents ('Global Variables', Variables, 'variable-s');
					WriteContents ('Constants', Constants, 'constant-s');
					WriteContents ('Predefined Types', Types, 'type-s');
					WriteContents ('Other Identifiers', Others);
					IniContents := S1;
					DeleteToAfterFirst ('[Bottom]', IniContents);
					Description := Trim (DeleteToFirst (#13#10'[', IniContents));
					if Length (Description) > 0 then begin
						PrepDescription := Description;
						PrepareBody (PrepDescription, True, HeaderFile);
						Contents := Contents + '<P>' + PrepDescription + #13#10;
						PrepDescription := Description;
						PrepareBody (PrepDescription, False, HeaderFile, '', True);
						WebContents := WebContents + PrepDescription + #13#10;
					end;
					IniContents := S1;
					DeleteToAfterFirst ('[Main]', IniContents);
					S2 := DeleteToFirst (#13#10'[', IniContents);
					I := Pos ('SEE ALSO=', UpperCase (S2));
					if I > 0 then begin
						Delete (S2, 1, I + Length ('SEE ALSO=') - 1);
						I := Pos (#13, S2);
						if I > 0 then
							Delete (S2, I, Length (S2));
						Contents := Contents + '<P><HR>See also: ' + GetSeeAlsoText (S2, True, HeaderFile) + #13#10;
						WebContents := WebContents + '<P>See also: ' + GetSeeAlsoText (S2, False, HeaderFile) + '</P>'#13#10;
					end;
					Contents := Contents + GetGenericFileEnd;
					WriteFile (DestFolder + '\' + ChangeFileExt (HeaderFile, '.html'), Contents);
					WriteSections (LanguageExts, 'langext');
					WriteSections (Functions, 'function');
					WriteSections (Variables, 'variable');
					WriteSections (Constants, 'constant');
					WriteSections (Types, 'type');
					WriteSections (Others);
					WebContents := WebContents + GetWebFileEnd;
					WriteFile (WebFolder + '\' + ChangeFileExt (HeaderFile, '.html'), WebContents);
					Contents := LoadFile (DestFolder + '\Contents.hhc');
					S1 := DeleteToFirst ('"' + HeaderFile + '"', Contents);
					S2 := '';
					S1 := S1 + DeleteToAfterFirst ('</OBJECT>', Contents) + '</OBJECT>';
					if Length (Contents) > 0 then begin
						try
							S1 := S1 + DeleteToAfterFirst ('<UL>', Contents, 20);
							DeleteToAfterFirst ('</UL>', Contents);
						except
							S1 := S1 + #13#10;
						end;
						S1 := S1 + '<UL>'#13#10;
						S2 := '</UL>' + Contents;
						Contents := '';
						WriteContentTree (LanguageExts);
						WriteContentTree (Functions);
						WriteContentTree (Variables);
						WriteContentTree (Constants);
						WriteContentTree (Types);
						WriteContentTree (Others);
						Contents := S1 + Contents + S2;
						WriteFile (DestFolder + '\Contents.hhc', Contents);
					end;
					with DefsByAMS do
						if Count > 0 then begin
							Contents := '';
							CloseCount := 0;
							for I := 0 to Count - 1 do begin
								if Strings [I] <> '1.00' then begin
									Contents := Contents + '#if MIN_AMS>=' + IntToStr (Round (StrToFloat (Strings [I]) * 100)) + #13#10;
									Inc (CloseCount);
								end;
								with Objects [I] as TDefInfo do
									Contents := Contents + Constants + RefTypes + DefTypes + Variables + Functions + Others;
							end;
							for I := 1 to CloseCount do
								Contents := Contents + '#endif'#13#10;
							if FileExists (IncludeFolder + '\' + HeaderFile) then begin
								S1 := LoadFile (IncludeFolder + '\' + HeaderFile);
								I := Pos ('/* Begin Auto-Generated Part */', S1);
								if I > 0 then begin
									Inc (I, Length ('/* Begin Auto-Generated Part */'));
									L := Pos ('/* End Auto-Generated Part */', Copy (S1, I, Length (S1))) - 1;
									if L > 0 then begin
										System.Delete (S1, I, L);
										System.Insert (#13#10 + Contents, S1, I);
									end else
										raise ESubStrNotFound.CreateFmt ('Unterminated auto-generated part in %s', [HeaderFile]);
								end else
									raise ESubStrNotFound.CreateFmt ('Missing auto-generated part in %s', [HeaderFile]);
								WriteFile (IncludeFolder + '\' + HeaderFile, S1);
							end;
						end;
				finally
					FinalizeList (Others);
					FinalizeList (Types);
					FinalizeList (Constants);
					FinalizeList (Variables);
					FinalizeList (Functions);
					FinalizeList (LanguageExts);
				end;
			end;
		finally
			try
				ClearWithObjects (DefsByAMS);
			except end;
			DefsByAMS.Free;
			ROMCallCount.Free;
			HeaderFiles.Free;
		end;
	except
		on E: Exception do
			if not (E is EAbort) then
				MessageBox (0, PChar (E.Message), 'Error', mb_OK or mb_IconError);
	end;
end.
