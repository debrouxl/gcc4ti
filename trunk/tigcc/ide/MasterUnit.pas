{
  TIGCC IDE

  Copyright (C) 2000-2004 Sebastian Reichelt
  Copyright (C) 2005 Fréderic Bour
  Copyright (C) 2006 Kevin Kofler

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

unit MasterUnit;

interface

uses
	CalcUnit, ProcessUnit, ParsingUnit,
	LinkUnit,
	Classes, Graphics, SourceEditUnit;

const
	IDELocation          = 'Bin\';
	LinkLibLocation      = 'Bin\';
	GCCLocation          = 'Bin\';
	AsLocation           = 'Bin\';
	A68kLocation         = 'Bin\';
	PackLocation         = 'Bin\';
	ASMIncludeLocation   = 'Include\ASM\';
	GASIncludeLocation   = 'Include\S\';
	CIncludeLocation     = 'Include\C\';
	QuillIncludeLocation = 'Include\Quill\';
	DocLocation          = 'Doc\';
	ProjectsLocation     = 'Projects\';
	StdLibLocation       = 'Lib\';
	TIPatchLocation      = 'Lib\';
	PStarterLocation     = 'Lib\';
	TemplatesLocation    = 'Bin\';
	CompletionLocation   = 'Include\C\Completion\';

	DefaultGCCSwitches      = '-B .\ -I- -I ..\Include\C\';
	DefaultAsSwitches       = '-mc68000 -I ..\Include\S\';
	DefaultA68kSwitches     = '-i..\Include\ASM';
	SpecialQuillGCCSwitches = '-I ..\Include\Quill\';

	ProjectFileExt = '.tpr';

	MaxNameLength = 8;

type
	TSpecialSupportOption = (ssA68k, ssQuill, ssPack, ssFlashOS, ssFargo);
	TSpecialSupport = set of TSpecialSupportOption;
	TProjectTarget = (ptRegular, ptFlashOS, ptFargo, ptArchive);
	TTransferTarget = (ttNone, ttVTI, ttCalc);
	TTiEmuCalcType = (cvNone = 0, cvTI92 = 1, cvTI89 = 2, cvTI92Plus = 4, cvV200 = 8, cvTI89Ti = 16);
	TKernelFormat = (kfUnknown, kfNone, kfStandard, kfCompressedTables);
	TRelocFormat = (rfUnknown, rfNone, rfDirect, rfAMS, rfKernel, rfCompressed, rfMlink, rfFLine);

	TPredefinedLibOptions = class(TObject)
		CalcDests: TCalcDests;
		OptimizeCalcConsts: Boolean;
		KernelFormat: TKernelFormat;
		UseMinAMS: Boolean;
		MinAMS: string;
		UnofficialOSSupport: Boolean;
		RelocFormat,
		ROMCallFormat,
		BSSRefFormat,
		DataRefFormat: TRelocFormat;
		UseFLineJumps,
		Use4ByteFLineJumps,
		OptimizeROMCalls,
		UseInternalFLineEmulator: Boolean;
		UseReturnValue,
		EnableErrorReturn: Boolean;
		SaveScreen: Boolean;
		function GetSwitches: string;
	end;

var
	MainConsole: TProcessConsole;
	StopOnErrors,
	DeleteAssemblyFiles,
	DeleteObjectFiles,
	AssumeUndefined,
	AutoBlocks,
	AutoNews,
	UseDataVar,
	DataVarCopy,
	DataVarCopyIfArchived,
	Pack,
	DebugInfo,
	StdLib,
	InitBSS,
	OptimizeNOPs,
	OptimizeReturns,
	OptimizeBranches,
	OptimizeMoves,
	OptimizeTests,
	OptimizeCalculations,
	RemoveUnusedSections,
	CutUnusedRanges,
	ReorderSections,
	MergeConstants,
	OutputBin: Boolean;
	ProjectTarget: TProjectTarget;
	GCCSwitches,
	AsSwitches,
	AsmSwitches,
	DataVar,
	PackVar,
	CommandLine,
	PostBuildProcessFile: string;
	PredefinedLibOptions: TPredefinedLibOptions;
	ProxyName: string;
	ProxyPort: Integer;
	TIGCCFolder: string;
	Temp: string;
	OperationCancelled,
	OperationSuccessful: Boolean;
	ProjectFileName: string;
	SyntaxC,
	SyntaxAsmGNU,
	SyntaxAsm,
	SyntaxQuill: TSyntaxColoring;
	TabSizeC,
	TabSizeAsm: Integer;
	EditorColor: TColor;
	EditorFont: TFont;
	EditorOnFly,
	EditorDragDrop,
	EditorRemoveTrSp: Boolean;
	LastNewsDate: Integer;
	TransferTarget: TTransferTarget;
	VTIPath: string;
	LinkPort: TLinkPort;
	LinkCable: TLinkCableType;
	SpecialSupport: TSpecialSupport;
	Compiling,
	CompFinishAndStop,
	CompStopNow: Boolean;
	CompStartTime,
	CompFileStartTime,
	CompLastTime: TDateTime;
	CompStartFile: procedure of object;
	CompStop: procedure of object;
	CompSetMessage: procedure(const Msg: string) of object;
	CompUpdate: procedure of object;
	CompUpdateProgramOutput: procedure of object;

{$IFDEF CanSplit}
var
	SplitFiles: Boolean;
{$ENDIF}

function StartsWith(const SubStr: string; var Str: string; CaseSensitive: Boolean = False; Remove: Boolean = False): Boolean;

procedure WaitForMainConsole(const ProcessDesc: string);

function Runnable: Boolean;

implementation

uses
	SysUtils, Forms;

procedure WaitForMainConsole(const ProcessDesc: string);
begin
	CompUpdate;
	while MainConsole.Running do begin
		CompUpdate;
		Application.ProcessMessages;
		if CompFinishAndStop then begin
			OperationCancelled := True;
			CompSetMessage ('Finishing ' + ProcessDesc + ' Process');
		end;
		if CompStopNow then begin
			OperationCancelled := True;
			MainConsole.KillProcess;
			Break;
		end;
	end;
	CompUpdate;
end;

function Runnable: Boolean;
begin
	Result := (ProjectTarget <> ptArchive) and (ProjectTarget <> ptFlashOS) and (TransferTarget <> ttNone);
end;

function StartsWith(const SubStr: string; var Str: string; CaseSensitive: Boolean; Remove: Boolean): Boolean;
begin
	if CaseSensitive then
		Result := Copy (Str, 1, Length (SubStr)) = SubStr
	else
		Result := UpperCase (Copy (Str, 1, Length (SubStr))) = UpperCase (SubStr);
	if Remove and Result then
		Delete (Str, 1, Length (SubStr));
end;

{ TPredefinedLibOptions }

function TPredefinedLibOptions.GetSwitches: string;
procedure AddSwitch(Define: string; const Value: string = '');
begin
	if Length (Result) > 0 then
		Result := Result + ' ';
	Define := '-D' + Define;
	if Length (Value) > 0 then
		Define := '"' + Define + '=' + Value + '"';
	Result := Result + Define;
end;
begin
	Result := '';
	if cdTI92 in CalcDests then
		AddSwitch ('USE_TI92');
	if cdTI89 in CalcDests then
		AddSwitch ('USE_TI89');
	if cdTI92Plus in CalcDests then
		AddSwitch ('USE_TI92PLUS');
	if cdV200 in CalcDests then
		AddSwitch ('USE_V200');
	if OptimizeCalcConsts then
		AddSwitch ('OPTIMIZE_CALC_CONSTS');
	if KernelFormat in [kfStandard, kfCompressedTables] then begin
		AddSwitch ('USE_KERNEL');
		if KernelFormat = kfCompressedTables then
			AddSwitch ('USE_PREOS_COMPRESSED_TABLES');
	end;
	if UseMinAMS then try
		AddSwitch ('MIN_AMS', IntToStr (Round (StrToFloat (MinAMS) * 100)));
	except end;
	if UnofficialOSSupport then
		AddSwitch ('UNOFFICIAL_OS_SUPPORT');
	if KernelFormat = kfCompressedTables then begin
		if BSSRefFormat = rfNone then
			AddSwitch ('MERGE_BSS');
	end else begin
		case RelocFormat of
			rfKernel:      AddSwitch ('KERNEL_FORMAT_RELOCS');
			rfCompressed:  AddSwitch ('COMPRESSED_FORMAT_RELOCS');
      rfMLink:       AddSwitch ('MLINK_FORMAT_RELOCS');
			rfFLine:       AddSwitch ('USE_FLINE_JUMPS');
		end;
		case ROMCallFormat of
			rfKernel:      AddSwitch ('KERNEL_FORMAT_ROM_CALLS');
			rfCompressed:  AddSwitch ('COMPRESSED_FORMAT_ROM_CALLS');  
			rfMlink:       AddSwitch ('MLINK_FORMAT_ROM_CALLS');
			rfFLine: begin
				AddSwitch ('USE_FLINE_ROM_CALLS');
				Result := Result + ' -fno-function-cse';
			end;
		end;
		case BSSRefFormat of
			rfNone:        AddSwitch ('MERGE_BSS');
			rfKernel:      AddSwitch ('KERNEL_FORMAT_BSS');
			rfCompressed:  AddSwitch ('COMPRESSED_FORMAT_BSS');
      rfMlink:       AddSwitch ('MLINK_FORMAT_BSS');
		end;
	end;
	case DataRefFormat of
		rfKernel:      AddSwitch ('KERNEL_FORMAT_DATA_VAR');
		rfCompressed:  AddSwitch ('COMPRESSED_FORMAT_DATA_VAR');
    rfMlink:       AddSwitch ('MLINK_FORMAT_DATA_VAR');
	end;
	if UseFLineJumps then begin
		AddSwitch ('USE_FLINE_JUMPS');
		if Use4ByteFLineJumps then
			AddSwitch ('USE_4_BYTE_FLINE_JUMPS');
	end;
	if OptimizeROMCalls then
		AddSwitch ('OPTIMIZE_ROM_CALLS');
	if UseInternalFLineEmulator then
		AddSwitch ('USE_INTERNAL_FLINE_EMULATOR');
	if UseReturnValue then
		AddSwitch ('RETURN_VALUE');
	if EnableErrorReturn then
		AddSwitch ('ENABLE_ERROR_RETURN');
	if SaveScreen then
		AddSwitch ('SAVE_SCREEN');
end;

initialization
	PredefinedLibOptions := TPredefinedLibOptions.Create;
	PredefinedLibOptions.MinAMS := '1.01';
	MainConsole := TProcessConsole.Create;
	SyntaxC := TSyntaxColoring.Create (nil);
	SyntaxAsm := TSyntaxColoring.Create (nil);
	SyntaxAsmGNU := TSyntaxColoring.Create (nil);
	SyntaxQuill := TSyntaxColoring.Create (nil);
	EditorFont := TFont.Create;
	with SyntaxC do begin
		Enabled := True;
		SymbolColor := clOlive;
		SymbolStyle := [fsBold];
		SymbolCustomStyle := True;
		NumberColor := clMaroon;
		NumberStyle := [];
		NumberCustomStyle := False;
		with WordLists.Add do begin
			Caption := 'C Keywords';
			Words.CommaText :=
				'__alignof__,__asm__,__attribute__,__complex__,__const__,__extension__,__imag__,__inline__,__label__,__real__,__typeof__,' +
				'asm,auto,break,case,char,const,continue,default,do,double,else,enum,extern,float,for,goto,if,inline,int,long,register,return,short,signed,sizeof,static,struct,switch,typedef,typeof,union,unsigned,void,volatile,while';
			CaseSensitive := True;
			CustomColor := True;
			Color := clBlue;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with CustomStyles.Add do begin
			Caption := 'Comment Area';
			CustomColor := True;
			Color := clGreen;
			CustomStyle := True;
			Style := [fsItalic];
			BeginText := '/*';
			EndText := '*/';
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'Comment Line';
			CustomColor := True;
			Color := clGreen;
			CustomStyle := True;
			Style := [fsItalic];
			BeginText := '//';
			EndText := #13;
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'String';
			CustomColor := True;
			Color := clMaroon;
			CustomStyle := False;
			Style := [];
			BeginText := '"';
			EndText := '"';
			IgnoreChar := '\';
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'Character';
			CustomColor := True;
			Color := clMaroon;
			CustomStyle := False;
			Style := [];
			BeginText := '''';
			EndText := '''';
			IgnoreChar := '\';
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'Preprocessor Directive';
			CustomColor := True;
			Color := clTeal;
			CustomStyle := True;
			Style := [fsBold];
			BeginText := '#';
			EndText := ' ';
			Switchable := False;
		end;
		ParenthesisColors.CommaText := '$800080,$C08000,$8080FF,$008000';
		ParenthesisStyle := [fsBold];
		ParenthesisCustomStyle := True;
	end;
	with SyntaxAsmGNU do begin
		NumberColor := SyntaxC.NumberColor;
		NumberStyle := SyntaxC.NumberStyle;
		NumberCustomStyle := SyntaxC.NumberCustomStyle;
		SymbolColor := SyntaxC.SymbolColor;
		SymbolStyle := SyntaxC.SymbolStyle;
		SymbolCustomStyle := SyntaxC.SymbolCustomStyle;
		ParenthesisColors.Assign (SyntaxC.ParenthesisColors);
		ParenthesisStyle := SyntaxC.ParenthesisStyle;
		ParenthesisCustomStyle := SyntaxC.ParenthesisCustomStyle;
		with CustomStyles.Add do begin
			Caption := 'Comment Area';
			CustomColor := True;
			Color := clGreen;
			CustomStyle := True;
			Style := [fsItalic];
			BeginText := '/*';
			EndText := '*/';
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'Comment Line (|)';
			CustomColor := True;
			Color := clGreen;
			CustomStyle := True;
			Style := [fsItalic];
			BeginText := '|';
			EndText := #13;
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'Comment Line (#)';
			CustomColor := True;
			Color := clGreen;
			CustomStyle := True;
			Style := [fsItalic];
			BeginText := '#';
			EndText := #13;
			Switchable := False;
			LineStartOnly := True;
		end;
		with CustomStyles.Add do begin
			Caption := 'String';
			CustomColor := True;
			Color := clMaroon;
			CustomStyle := False;
			Style := [];
			BeginText := '"';
			EndText := '"';
			IgnoreChar := '\';
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'Character';
			CustomColor := True;
			Color := clMaroon;
			CustomStyle := False;
			Style := [];
			BeginText := '''';
			EndText := '''';
			IgnoreChar := '\';
			Switchable := False;
		end;
		with WordLists.Add do begin
			Caption := 'Data Movement';
			Words.CommaText := 'EXG,LEA,LINK,MOV,MOVE,MOVEA,MOVEM,MOVEP,MOVEQ,MOVM,MOVP,MOVQ,PEA,UNLK';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Integer Arithmetic';
			Words.CommaText := 'ADD,ADDA,ADDI,ADDQ,ADDX,CLR,CMP,CMPA,CMPI,CMPM,DIVS,DIVU,EXT,MULS,MULU,NEG,NEGX,SUB,SUBA,SUBI,SUBQ,SUBX,TAS';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Logical Instructions';
			Words.CommaText := 'AND,ANDI,EOR,EORI,NOT,OR,ORI';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Shift/Rotation Instructions';
			Words.CommaText := 'ASL,ASR,LSL,LSR,ROL,ROR,ROXL,ROXR,SWAP';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Bit Manipulation';
			Words.CommaText := 'BCHG,BCLR,BSET,BTST';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Program Control';
			Words.CommaText := 'BCC,BCS,BEQ,BGE,BGT,BHI,BLE,BLS,BLT,BMI,BNE,BPL,BRA,BSR,BVC,BVS,JBCC,JBCS,JBEQ,JBGE,JBGT,JBHI,JBLE,JBLS,JBLT,JBMI,JBNE,JBPL,JBRA,JBSR,JBVC,JBVS,JSR,JRA,JMP,'
				+ 'NOP,RTR,RTS,SCC,SCS,SEQ,SF,SGE,SGT,SHI,SLE,SLS,SLT,SMI,SNE,SPL,ST,SVC,SVS,TST,'
				+ 'JHI,JLS,JCC,JCS,JNE,JEQ,JVC,JVS,JPL,JMI,JGE,JLT,JGT,JLE,DBHI,DBLS,DBCC,DBCS,DBNE,DBEQ,DBVC,DBVS,DBPL,DBMI,DBGE,DBLT,DBGT,DBLE,DBF,DBRA,DBT,'
				+ 'FJNE,FJEQ,FJGE,FJLT,FJGT,FJLE,FJF,FJT,FJGL,FJGLE,FJNGE,FJNGL,FJNGLE,FJNGT,FJNLE,FJNLT,FJOGE,FJOGL,FJOGT,FJOLE,FJOLT,FJOR,FJSEQ,FJSF,FJSNE,FJST,FJUEQ,FJUGE,FJUGT,FJULE,FJULT,FJUN';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'System Control';
			Words.CommaText := 'ILLEGAL,RTE,TRAP';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Extensions';
			Words.CommaText := 'B,L,S,W';
			Color := $408000;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Assembler Directives';
			Words.CommaText := 'abort,align,altmacro,ascii,asciz,balign,balignw,balignl,byte,comm,data,'
				+ 'def,dim,double,eject,else,end,elseif,endef,endfunc,endif,endm,endr,equ,equiv,'
				+ 'err,even,exitm,extern,fail,file,fill,float,func,global,globl,hword,ident,if,include,'
				+ 'incbin,int,irp,irpc,lcomm,lflags,line,ln,list,long,macro,mri,noaltmacro,nolist,octa,org,'
				+ 'p2align,p2alignw,p2alignl,print,psize,purgem,quad,rept,sbttl,scl,section,set,'
				+ 'short,single,size,sleb128,skip,space,stabd,stabn,stabs,string,struct,tag,text,'
				+ 'title,type,uleb128,val,vtable_entry,word,xdef';
			Color := clBlue;
			CustomColor := True;
			Style := [fsBold];
			CustomStyle := True;
			CaseSensitive := True;
		end;
		with WordLists.Add do begin
			Caption := 'Registers';
			Words.CommaText := 'a0,a1,a2,a3,a4,a5,a6,a7,d0,d1,d2,d3,d4,d5,d6,d7,fp,pc,sp,sr';
			Color := clRed;
			CustomColor := True;
			Style := [fsUnderline];
			CustomStyle := True;
			CaseSensitive := False;
		end;
	end;
	with SyntaxAsm do begin
		NumberColor := SyntaxC.NumberColor;
		NumberStyle := SyntaxC.NumberStyle;
		NumberCustomStyle := SyntaxC.NumberCustomStyle;
		SymbolColor := SyntaxC.SymbolColor;
		SymbolStyle := SyntaxC.SymbolStyle;
		SymbolCustomStyle := SyntaxC.SymbolCustomStyle;
		ParenthesisColors.Assign (SyntaxC.ParenthesisColors);
		ParenthesisStyle := SyntaxC.ParenthesisStyle;
		ParenthesisCustomStyle := SyntaxC.ParenthesisCustomStyle;
		with CustomStyles.Add do begin
			Caption := 'Comment';
			CustomColor := True;
			Color := clGreen;
			CustomStyle := True;
			Style := [fsItalic];
			BeginText := ';';
			EndText := #13;
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'String (double-quoted)';
			CustomColor := True;
			Color := clMaroon;
			CustomStyle := False;
			Style := [];
			BeginText := '"';
			EndText := '"';
			IgnoreChar := '';
			Switchable := False;
		end;
		with CustomStyles.Add do begin
			Caption := 'String (single-quoted)';
			CustomColor := True;
			Color := clMaroon;
			CustomStyle := False;
			Style := [];
			BeginText := '''';
			EndText := '''';
			IgnoreChar := '';
			Switchable := False;
		end;
		with WordLists.Add do begin
			Caption := 'Data Movement';
			Words.CommaText := 'EXG,LEA,LINK,MOVE,MOVEA,MOVEM,MOVEP,MOVEQ,PEA,UNLK';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Integer Arithmetic';
			Words.CommaText := 'ADD,ADDA,ADDI,ADDQ,ADDX,CLR,CMP,CMPA,CMPI,CMPM,DIVS,DIVU,EXT,MULS,MULU,NEG,NEGX,SUB,SUBA,SUBI,SUBQ,SUBX,TAS';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Logical Instructions';
			Words.CommaText := 'AND,ANDI,EOR,EORI,NOT,OR,ORI';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Shift/Rotation Instructions';
			Words.CommaText := 'ASL,ASR,LSL,LSR,ROL,ROLX,ROR,RORX,ROXL,ROXR,SWAP';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Bit Manipulation';
			Words.CommaText := 'BCHG,BCLR,BSET,BTST';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Program Control';
			Words.CommaText := 'BCC,BCS,BEQ,BGE,BGT,BHI,BHS,BLE,BLO,BLS,BLT,BMI,BNE,BPL,BRA,BSR,BVC,BVS,DBCC,DBCS,DBEQ,DBF,DBGE,DBGT,DBHI,DBHS,DBLE,DBLO,DBLS,DBLT,DBMI,DBNE,DBPL,DBRA,DBT,DBVC,DBVS,JMP,JSR,NOP,RTR,RTS,'
				+ 'SCC,SCS,SEQ,SF,SGE,SGT,SHI,SHS,SLE,SLO,SLS,SLT,SMI,SNE,SPL,ST,SVC,SVS,TST';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'System Control';
			Words.CommaText := 'ILLEGAL,RTE,TRAP';
			Color := clBlue;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Extensions';
			Words.CommaText := 'B,L,S,W';
			Color := $408000;
			CustomColor := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Assembler Directives';
			Words.CommaText := 'BSS,CNOP,CSEG,DSEG,DC,DCB,DS,END,ENDC,ENDIF,ENDM,EQU,EQUR,EVEN,FAR,IDNT,IFC,IFD,IFEQ,IFGE,IFGT,IFLE,IFLT,IFNC,IFND,IFNE,INCBIN,INCLUDE,LIST,MACRO,NEAR,NOLIST,ORG,PAGE,PUBLIC,REG,RORG,SECTION,SET,SPC,TITLE,TTL,XDEF,XREF';
			Color := clBlue;
			CustomColor := True;
			Style := [fsBold];
			CustomStyle := True;
			CaseSensitive := False;
		end;
		with WordLists.Add do begin
			Caption := 'Registers';
			Words.CommaText := 'a0,a1,a2,a3,a4,a5,a6,a7,d0,d1,d2,d3,d4,d5,d6,d7,fp,pc,sp,sr';
			Color := clRed;
			CustomColor := True;
			Style := [fsUnderline];
			CustomStyle := True;
			CaseSensitive := False;
		end;
	end;
	SyntaxQuill.Assign (SyntaxC);
	with SyntaxQuill do begin
		with WordLists.Add do begin
			Caption := 'Sections';
			Words.CommaText := '$$ACTIONS,$$CONNECTIONS,$$END,$$END_TEST,$$EVENTS,$$EXTERN,$$LOCATIONS,$$MESSAGES,$$OBJECTS,$$OLDSTYLE_SYSTEM_MESSAGES,$$PICTURES,$$PICTURES_TEST,$$SYSTEM_MESSAGES,$$TITLE,$$VOCABULARY';
			CaseSensitive := True;
			CustomColor := True;
			Color := $0000FF;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Section-specific Keywords';
			Words.CommaText := 'ACTION,BITMAP,CBLOCK,CONN,DEFINE,DRAWING,END_BITMAP,END_CBLOCK,END_DRAWING,END_PACKED_BITMAP,EVENT,FROM,LOC,MSG,OBJ,PACKED_BITMAP,WORD';
			CaseSensitive := True;
			CustomColor := True;
			Color := $808040;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Additional Keywords';
			Words.CommaText := 'CONTINUE,ELSE';
			CaseSensitive := True;
			CustomColor := True;
			Color := $808040;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Non-functional Keywords';
			Words.CommaText := 'BEEP,BLOCK,BORDER,BRIGHT,FLASH,INK,PAPER';
			CaseSensitive := True;
			CustomColor := True;
			Color := $C0C0C0;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Predefined Aliases';
			Words.CommaText := '$ALSOSEE,$ARG,$CARRIED,$CENTER,$CNT1,$CNT2,$CNT3,$CNT4,$CONT,$CURLOC,$DARK,$DARKCNT,$DESC,$DESC_CNT,$DESC_DARKCNT,$DESC_NOLIGHTCNT,$DONE,$ENDGAME,$EXIT,$FAIL,$FONT,$FULLSCR,$GCONTROL,$LSOURCE,'
				+ '$MAXCAR,$NOLIGHTCNT,$NOUN,$NOWHERE,$NULL,$NUMCAR,$PROMPT,$RESTART,$SCORE,$SPECIAL,$SUBROUTINE,$TURNHI,$TURNLO,$VERB,$WORN';
			CaseSensitive := True;
			CustomColor := True;
			Color := $800080;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Conditions';
			Words.CommaText := 'ABSENT,AT,ATGT,ATLT,CARRIED,CHANCE,CREATED,EQ,EQWORD,EXTWORD,GT,HERE,ISAT,ISDESC,ISNOTAT,ISNOTNULL,ISNULL,LT,NEQWORD,NOTAT,NOTCARR,NOTCREATED,NOTEQ,NOTHERE,NOTSAME,NOTWORN,NOTZERO,PRESENT,SAME,TRYMOVE,WORN,ZERO';
			CaseSensitive := True;
			CustomColor := True;
			Color := $FF0000;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Actions';
			Words.CommaText := 'ADD,ALSOSEE,ANYKEY,AUTOD,AUTOG,AUTOR,AUTOW,BIGFONT,CANCEL,CLEAR,CLS,COPYFF,COPYFO,COPYOF,COPYOO,CREATE,DECCAR,DESC,DESTROY,DONE,DROP,DROPALL,END,ENDDESC,EXIT,EXTERN,GET,GETWORD,GOTO,INCCAR,INVEN,LET,'
				+ 'LISTAT,LISTOBJ,LOAD,MAXCAR,MES,MESFLAG,MESSAGE,MINUS,NEWLINE,NOTDONE,OK,PAUSE,PICNORM,PICOFF,PICON,PLACE,PLUS,PRINT,PROMPT,PUTO,QUIT,QVERSION,RAMLOAD,RAMSAVE,RANDOM,REDRAW,REMOVE,RESTART,SAVE,SCORE,'
				+ 'SET,SETNOUN,SETVERB,SHOWLOC,SMLFONT,SUB,SWAP,SYSMESS,TURNS,WEAR,WHATO,WHEREO,ZAPSCR';
			CaseSensitive := True;
			CustomColor := True;
			Color := $A00000;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Drawing Primitives';
			Words.CommaText := 'AMOVE,CALL,ELLIPSE,FILL,INV_ELLIPSE,INV_LINE,INV_PLOT,INV_RPLOT,LINE,MOVE,PLOT,RPLOT,SHADE,XOR_ELLIPSE,XOR_LINE,XOR_PLOT,XOR_RPLOT';
			CaseSensitive := True;
			CustomColor := True;
			Color := $FF8000;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Drawing Directions';
			Words.CommaText := 'DOWN,DOWN_LEFT,DOWN_RIGHT,LEFT,LEFT_DOWN,LEFT_UP,RIGHT,RIGHT_DOWN,RIGHT_UP,UP,UP_LEFT,UP_RIGHT';
			CaseSensitive := True;
			CustomColor := True;
			Color := $008000;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'Shading Patterns';
			Words.CommaText := '$BKSLASHFILL,$BRICKFILL,$CHAINFILL,$CIRCLEFILL,$CLOSEDASHFILL,$CLOSEDOTFILL,$CLOSEWAVEFILL,$CROSSFILL,$DASHFILL,$DOTFILL,$HATCHFILL,$INTERLEAVEFILL,$LIGHTDOTFILL,$LIGHTLINEFILL,$LINEFILL,$SLASHFILL,'
				+ '$SOLIDFILL,$SQDOTFILL,$SQUAREFILL,$THICKBKSLASHFILL,$THICKHATCHFILL,$THICKLINEFILL,$THICKSLASHFILL,$VDASHFILL,$VINTERLEAVEFILL,$VLDOTFILL,$VLIGHTLINEFILL,$VLINEFILL,$VTHICKLINEFILL,$WAVEFILL,$WIDEDOTFILL,$XMARKFILL,$ZIGZAGFILL';
			CaseSensitive := True;
			CustomColor := True;
			Color := $800080;
			CustomStyle := True;
			Style := [fsBold];
		end;
		with WordLists.Add do begin
			Caption := 'External Symbols';
			Words.CommaText := '$ACTIONS$,$ARG$,$BMPUT$,$BPCKPUT$,$BUFFER$,$CONNECTIONS$,$EVENTS$,$EXTERN$,$FLAGS$,$FLAGS_BACKUP$,$GDF$,$GETLINE$,$LOCATIONS$,$LQL$,$MAXCAR$,$MESSAGES$,$NFLAG$,$NLOC$,$NMSG$,$NOBJ$,$NSYSMSG$,$NWORD$,'
				+ '$OBJECTS$,$PDRAW$,$PICTURE$,$PRINT$,$RAM_SAVED$,$SCALEX$,$SCALEY$,$SSCR$,$SYSTEM_MESSAGES$,$WORDS$';
			CaseSensitive := True;
			CustomColor := True;
			Color := $408000;
			CustomStyle := True;
			Style := [fsBold];
		end;
	end;
	EditorFont.Name := 'Courier New';
	EditorFont.Size := 10;
	EditorColor := clWindow;
	DecimalSeparator := '.';
finalization
	EditorFont.Free;
	SyntaxQuill.Free;
	SyntaxAsm.Free;
	SyntaxAsmGNU.Free;
	SyntaxC.Free;
	MainConsole.Free;
	PredefinedLibOptions.Free;
end.
