unit HTMLHelpUnit;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ShellAPI, Menus;

const
	HH_DISPLAY_TOPIC        = $0000;
	HH_HELP_FINDER          = $0000;  // WinHelp equivalent
	HH_DISPLAY_TOC          = $0001;
	HH_DISPLAY_INDEX        = $0002;
	HH_DISPLAY_SEARCH       = $0003;
	HH_SET_WIN_TYPE         = $0004;
	HH_GET_WIN_TYPE         = $0005;
	HH_GET_WIN_HANDLE       = $0006;
	HH_ENUM_INFO_TYPE       = $0007;  // Get Info type name, call repeatedly to enumerate, -1 at end
	HH_SET_INFO_TYPE        = $0008;  // Add Info type to filter.
	HH_SYNC                 = $0009;
	HH_RESERVED1            = $000A;
	HH_RESERVED2            = $000B;
	HH_RESERVED3            = $000C;
	HH_KEYWORD_LOOKUP       = $000D;
	HH_DISPLAY_TEXT_POPUP   = $000E;  // display string resource id or text in a popup window
	HH_HELP_CONTEXT         = $000F;  // display mapped numeric value in dwData
	HH_TP_HELP_CONTEXTMENU  = $0010;  // text popup help, same as WinHelp HELP_CONTEXTMENU
	HH_TP_HELP_WM_HELP      = $0011;  // text popup help, same as WinHelp HELP_WM_HELP
	HH_CLOSE_ALL            = $0012;  // close all windows opened directly or indirectly by the caller
	HH_ALINK_LOOKUP         = $0013;  // ALink version of HH_KEYWORD_LOOKUP
	HH_GET_LAST_ERROR       = $0014;  // not currently implemented // See HHERROR.h
	HH_ENUM_CATEGORY        = $0015;  // Get category name, call repeatedly to enumerate, -1 at end
	HH_ENUM_CATEGORY_IT     = $0016;  // Get category info type members, call repeatedly to enumerate, -1 at end
	HH_RESET_IT_FILTER      = $0017;  // Clear the info type filter of all info types.
	HH_SET_INCLUSIVE_FILTER = $0018;  // set inclusive filtering method for untyped topics to be included in display
	HH_SET_EXCLUSIVE_FILTER = $0019;  // set exclusive filtering method for untyped topics to be excluded from display
	HH_INITIALIZE           = $001C;  // Initializes the help system.
	HH_UNINITIALIZE         = $001D;  // Uninitializes the help system.
	HH_PRETRANSLATEMESSAGE  = $00FD;  // Pumps messages. (NULL, NULL, MSG*).
	HH_SET_GLOBAL_PROPERTY  = $00FC;  // Set a global property. (NULL, NULL, HH_GPROP)

type
	DWordPtr = DWord;

	THHAKLink = packed record
		cbStruct: Integer;     // Size of this structure
		fReserved: Bool;       // Must be FALSE (really!)
		pszKeywords: LPCTStr;  // Semi-colon separated keywords
		pszUrl: LPCTStr;       // URL to jump to if no keywords found (may be NULL)
		pszMsgText: LPCTStr;   // Message text to display in MessageBox if pszUrl is NULL and no keyword match
		pszMsgTitle: LPCTStr;  // Message title to display in MessageBox if pszUrl is NULL and no keyword match
		pszWindow: LPCTStr;    // Window to display URL in
		fIndexOnFail: Bool;    // Displays index if keyword lookup fails.
	end;
	HH_AKLINK = THHAKLink;
	tagHH_AKLINK = HH_AKLINK;
	PHHAKLink = ^THHAKLink;

	THHFTSQuery = record
		cbStruct: Integer;        // Size of structure in bytes.
		fUniCodeStrings: Bool;    // TRUE if all strings are unicode.
		pszSearchQuery: LPCTStr;  // String containing the search query.
		iProximity: LongInt;      // Word proximity.
		fStemmedSearch: Bool;     // TRUE for StemmedSearch only.
		TitleOnly: Bool;          // TRUE for Title search only.
		fExecute: Bool;           // TRUE to initiate the search.
		pszWindow: LPCTStr;       // Window to display in
	end;
	HH_FTS_QUERY = THHFTSQuery;
	tagHH_FTS_QUERY = HH_FTS_QUERY;

	THtmlHelpAProc = function(hwndCaller: HWnd; pszFile: PAnsiChar; uCommand: UInt; dwData: DWordPtr): HWnd; stdcall;
	THtmlHelpWProc = function(hwndCaller: HWnd; pszFile: PWideChar; uCommand: UInt; dwData: DWordPtr): HWnd; stdcall;
	THtmlHelpProc = function(hwndCaller: HWnd; pszFile: PChar; uCommand: UInt; dwData: DWordPtr): HWnd; stdcall;

var
	HtmlHelpA: THtmlHelpAProc;
	HtmlHelpW: THtmlHelpWProc;
	HtmlHelp: THtmlHelpProc;

resourcestring
	SHTMLHelpNotInstalled = 'The HTML Help Viewer is not installed. Download it at www.microsoft.com.';
	SHTMLHelpFileNotFound = 'The help file could not be found.';

type
	EHTMLHelp = class(Exception);
	EHTMLHelpNotInstalled = class(EHTMLHelp);
	EHTMLHelpFileNotFound = class(EHTMLHelp);

	THTMLHelp = class(TComponent)
	private
		FWindowHandle: HWnd;
		FCookie: DWord;
		FWindowType: string;
		FFileName: string;
		FCurrentTopic: string;
		FMaximizeOnShow: Boolean;
		procedure WndProc(var Msg: TMessage);
		function GetFileString: string;
	protected
		property CurrentTopic: string read FCurrentTopic write FCurrentTopic;
		property FileString: string read GetFileString;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function SendMessage(pszFile: PChar; uCommand: UInt; dwData: DWordPtr): HWnd;
		function Display: Boolean;
		function DisplayTopic(const Topic: string): Boolean;
		function KeywordLookup(const Keyword: string): Boolean;
		function KeywordLookupEx(const Link: THHAKLink): Boolean;
		function DisplayContentsTab: Boolean;
		function DisplayIndexTab: Boolean;
		function DisplaySearchTab: Boolean;
		procedure CloseAllWindows;
	published
		property FileName: string read FFileName write FFileName;
		property WindowType: string read FWindowType write FWindowType;
		property MaximizeOnShow: Boolean read FMaximizeOnShow write FMaximizeOnShow;
	end;

procedure Register;

const
	hhctrl = 'hhctrl.ocx';

implementation

procedure Register;
begin
	RegisterComponents('Help', [THTMLHelp]);
end;

{ THTMLHelp }

procedure THTMLHelp.CloseAllWindows;
begin
	SendMessage (nil, HH_CLOSE_ALL, 0);
end;

constructor THTMLHelp.Create;
begin
	inherited;
	if Assigned (HtmlHelp) then begin
		FWindowHandle := AllocateHWnd (WndProc);
		HtmlHelp (0, nil, HH_INITIALIZE, DWordPtr (@FCookie));
	end;
end;

destructor THTMLHelp.Destroy;
begin
	if Assigned (HtmlHelp) then begin
		CloseAllWindows;
		HtmlHelp (0, nil, HH_UNINITIALIZE, DWordPtr (FCookie));
		DeallocateHWnd (FWindowHandle);
	end;
	inherited;
end;

function THTMLHelp.Display: Boolean;
begin
	Result := DisplayTopic ('');
end;

function THTMLHelp.DisplayContentsTab: Boolean;
begin
	Display;
	Result := SendMessage (PChar (FileString), HH_DISPLAY_TOC, 0) <> 0;
end;

function THTMLHelp.DisplayIndexTab: Boolean;
begin
	Display;
	Result := SendMessage (PChar (FileString), HH_DISPLAY_INDEX, 0) <> 0;
end;

function THTMLHelp.DisplaySearchTab: Boolean;
var
	Query: THHFTSQuery;
begin
	Display;
	with Query do begin
		cbStruct := SizeOf (Query);
		fUniCodeStrings := False;
		pszSearchQuery := '';
		iProximity := 0;
		fStemmedSearch := False;
		TitleOnly := False;
		fExecute := False;
		pszWindow := PChar (WindowType);
	end;
	Result := SendMessage (PChar (FileString), HH_DISPLAY_SEARCH, DWordPtr (@Query)) <> 0;
end;

function THTMLHelp.DisplayTopic(const Topic: string): Boolean;
begin
	CurrentTopic := Topic;
	Result := SendMessage (PChar (FileString), HH_DISPLAY_TOPIC, 0) <> 0;
end;

function THTMLHelp.GetFileString: string;
begin
	if FileName = '' then
		Result := ''
	else begin
		Result := FileName;
		if CurrentTopic <> '' then
			Result := Result + '::/' + CurrentTopic;
		if WindowType <> '' then
			Result := Result + '>' + WindowType;
	end;
end;

function THTMLHelp.KeywordLookup(const Keyword: string): Boolean;
var
	Link: THHAKLink;
begin
	if Length (Keyword) > 0 then begin
		FillChar (Link, SizeOf (Link), 0);
		with Link do begin
			cbStruct := SizeOf (Link);
			pszKeywords := PChar (Keyword);
			fIndexOnFail := True;
		end;
		Result := KeywordLookupEx (Link);
	end else
		Result := False;
end;

function THTMLHelp.KeywordLookupEx(const Link: THHAKLink): Boolean;
begin
	Result := SendMessage (PChar (FileName), HH_KEYWORD_LOOKUP, DWord (@Link)) <> 0;
end;

function THTMLHelp.SendMessage(pszFile: PChar; uCommand: UInt;
	dwData: DWordPtr): HWnd;
begin
	if Assigned (pszFile) and (pszFile <> '') and (not FileExists (FileName)) then
		raise EHTMLHelpFileNotFound.Create (SHTMLHelpFileNotFound);
	if Assigned (HtmlHelp) then
		Result := HtmlHelp (FWindowHandle, pszFile, uCommand, dwData)
	else
		raise EHTMLHelpNotInstalled.Create (SHTMLHelpNotInstalled);
	if (Result <> 0) and MaximizeOnShow then
		ShowWindow (Result, SW_MAXIMIZE);
end;

procedure THTMLHelp.WndProc(var Msg: TMessage);
begin
	if Assigned (HtmlHelp) then
		try
			if HtmlHelp (0, nil, HH_PRETRANSLATEMESSAGE, DWordPtr (@Msg)) = 0 then
				with Msg do
					Result := DefWindowProc (FWindowHandle, Msg, wParam, lParam);
		except
			Application.HandleException (Self);
		end
	else
		with Msg do
			Result := DefWindowProc (FWindowHandle, Msg, wParam, lParam);
end;

{ HTML Help Initialization }

var
	LibHandle: THandle;

initialization
	LibHandle := LoadLibrary (hhctrl);
	if LibHandle <> 0 then begin
		@HtmlHelpA := GetProcAddress (LibHandle, 'HtmlHelpA');
		@HtmlHelpW := GetProcAddress (LibHandle, 'HtmlHelpW');
		@HtmlHelp := GetProcAddress (LibHandle, 'HtmlHelpA');
	end;
finalization
	if LibHandle <> 0 then
		FreeLibrary (LibHandle);
end.
