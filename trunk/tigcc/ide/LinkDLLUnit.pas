unit LinkDLLUnit;

interface

uses
	Classes;

const
	LinkLibCurInterfaceVersion = 17;

	llcdTI92         =  $01;
	llcdTI89         =  $02;
	llcdTI92Plus     =  $04;
	llcdV200         =  $08;
	llcdFlagTitanium = $100;

	llffTIOS        = 0;
	llffTIOSUpgrade = 1;

	llfrMain = 0;
	llfrData = 1;

	llmtError   = 0;
	llmtWarning = 1;

type
	TLinkLibDestFile = packed record
		Data: Pointer;
	end;

	TLinkLibDataVarInfo = packed record
		VarName: PChar;
		CreateCopy,
		CopyOnlyIfArchived: WordBool;
	end;

	TLinkLibOptimizeInfo = packed record
		RemoveUnused,
		OptimizeRelocs,
		OptimizeNOPs,
		OptimizeReturns,
		OptimizeBranches,
		OptimizeMoves,
		OptimizeTests,
		OptimizeCalcs,
		UseFLineJumps,
		Use4ByteFLineJumps,
		CutRanges,
		ReorderSections,
		MergeConstants: WordBool;
		ProgramSize,
		DataSize,
		BSSSize,
		RelocCount,
		NativeRelocCount,
		OptimizeBranchesResult,
		OptimizeMovesResult,
		OptimizeTestsResult,
		OptimizeCalcsResult,
		UseFLineJumpsResult,
		CutRangesResult,
		NearAssemblyResult: LongInt;
	end;

	PCharArray = array [0..MaxListSize-1] of PChar;
	PPChar = ^PCharArray;

	TLinkLibGetInterfaceVersion = function: LongInt; cdecl;
	TLinkLibGetOutputFile = function (var DestFile: TLinkLibDestFile; FileSize, DestCalc, FileRole, FileFormat, FileType: LongInt; Extension: PChar; Executable: WordBool; var EffectiveSize: LongInt): WordBool; cdecl;
	TLinkLibFinalizeOutputFile = procedure (var DestFile: TLinkLibDestFile); cdecl;
	TLinkLibError = procedure (FileName, Text: PChar; MessageType: LongInt); cdecl;
	TLinkLibLinkFiles = function (ObjectFiles, ArchiveFiles: PPChar; ErrorMessage: TLinkLibError; GetOutputFile: TLinkLibGetOutputFile; FinalizeOutputFile: TLinkLibFinalizeOutputFile; NativeMode, FlashOS, Fargo: WordBool; var DataVarInfo: TLinkLibDataVarInfo; var OptimizeInfo: TLinkLibOptimizeInfo; OmitBSSInitialization: WordBool): ShortInt; cdecl;
	TLinkLibCreateArchive = function (DestFile: PChar; ObjectFiles: PPChar; ErrorMessage: TLinkLibError; NoNames: WordBool): ShortInt; cdecl;

implementation

end.
