program ConvertTexi;

uses
	Windows,
	SysUtils,
	Classes,
	UtilsDos,
	UtilsWin,
	HelpSystemMasterUnit in '..\HelpSystemMasterUnit.pas';

function GetSecID(Ref: string): string;
begin
	Ref := StringReplace (Ref, #13#10, ' ', [rfReplaceAll]);
	Ref := StringReplace (Ref, #13, ' ', [rfReplaceAll]);
	Ref := StringReplace (Ref, #10, ' ', [rfReplaceAll]);
	Ref := StringReplace (Ref, ',, ', ',,', [rfReplaceAll]);
	if Ref = 'Invoking GCC,,GCC Command Options' then
		Result := 'comopts/comopts'
	else if (Ref = 'Overall Options,,Options Controlling the Kind of Output') or (Ref = 'Overall Options,,Options Controlling the Kind of Output, gcc.info, Using GNU CC') then
		Result := 'comopts/SEC4'
	else if Ref = 'C Dialect Options,,Options Controlling C Dialect' then
		Result := 'comopts/SEC6'
	else if Ref = 'Warning Options,,Options to Request or Suppress Warnings' then
		Result := 'comopts/SEC8'
	else if (Ref = 'Debugging Options,,Options for Debugging Your Program or GCC') or (Ref = 'Debugging Options,,Options for Debugging Your Program or @command{gcc}') then
		Result := 'comopts/SEC9'
	else if Ref = 'Optimize Options,,Options That Control Optimization' then
		Result := 'comopts/SEC10'
	else if Ref = 'Preprocessor Options,,Options Controlling the Preprocessor' then
		Result := 'comopts/SEC11'
	else if Ref = 'Assembler Options,,Passing Options to the Assembler' then
		Result := 'comopts/SEC12'
	else if Ref = 'Directory Options,,Options for Directory Search' then
		Result := 'comopts/SEC14'
	else if Ref = 'Spec Files,,Specifying subprocesses and the switches to pass to them' then
		Result := 'comopts/SEC15'
	else if Ref = 'M680x0 Options,,M680x0 Options' then
		Result := 'comopts/SEC16'
	else if Ref = 'Code Gen Options,,Options for Code Generation Conventions' then
		Result := 'comopts/SEC44'
	else if Ref = 'Environment Variables,,Environment Variables Affecting GCC' then
		Result := 'comopts/SEC45'
	else if Ref = 'CPP Overview' then
		Result := 'cpp/SEC2'
	else if Ref = 'Initial processing' then
		Result := 'cpp/SEC3'
	else if Ref = 'Tokenization' then
		Result := 'cpp/SEC3a'
	else if Ref = 'The preprocessing language' then
		Result := 'cpp/SEC3b'
	else if Ref = 'Header Files' then
		Result := 'cpp/SEC4'
	else if Ref = 'Include Syntax' then
		Result := 'cpp/SEC6'
	else if Ref = 'Include Operation' then
		Result := 'cpp/SEC7'
	else if Ref = 'Once-Only Headers' then
		Result := 'cpp/SEC8'
	else if Ref = 'Computed Includes' then
		Result := 'cpp/SEC8a'
	else if Ref = 'Wrapper Headers' then
		Result := 'cpp/SEC9'
	else if Ref = 'System Headers' then
		Result := 'cpp/SEC9a'
	else if Ref = 'Macros' then
		Result := 'cpp/SEC10'
	else if Ref = 'Object-like Macros' then
		Result := 'cpp/SEC11'
	else if Ref = 'Function-like Macros' then
		Result := 'cpp/SEC12'
	else if Ref = 'Macro Arguments' then
		Result := 'cpp/SEC12a'
	else if Ref = 'Variadic Macros' then
		Result := 'cpp/SEC13'
	else if Ref = 'Predefined Macros' then
		Result := 'cpp/SEC14'
	else if Ref = 'Standard Predefined Macros' then
		Result := 'cpp/SEC15'
	else if Ref = 'Common Predefined Macros' then
		Result := 'cpp/SEC15a'
	else if Ref = 'System-specific Predefined Macros' then
		Result := 'cpp/SEC16'
	else if Ref = 'Stringification' then
		Result := 'cpp/SEC17'
	else if Ref = 'Concatenation' then
		Result := 'cpp/SEC18'
	else if Ref = 'Undefining and Redefining Macros' then
		Result := 'cpp/SEC19'
	else if Ref = 'Directives Within Macro Arguments' then
		Result := 'cpp/SEC20'
	else if Ref = 'Macro Pitfalls' then
		Result := 'cpp/SEC22'
	else if Ref = 'Misnesting' then
		Result := 'cpp/SEC23'
	else if Ref = 'Operator Precedence Problems' then
		Result := 'cpp/SEC24'
	else if Ref = 'Swallowing the Semicolon' then
		Result := 'cpp/SEC25'
	else if Ref = 'Duplication of Side Effects' then
		Result := 'cpp/SEC26'
	else if Ref = 'Self-Referential Macros' then
		Result := 'cpp/SEC27'
	else if Ref = 'Argument Prescan' then
		Result := 'cpp/SEC28'
	else if Ref = 'Newlines in Arguments' then
		Result := 'cpp/SEC30'
	else if Ref = 'Conditionals' then
		Result := 'cpp/SEC31'
	else if Ref = 'Conditional Uses' then
		Result := 'cpp/SEC32'
	else if Ref = 'Conditional Syntax' then
		Result := 'cpp/SEC33'
	else if Ref = 'If' then
		Result := 'cpp/SEC34'
	else if Ref = 'Else' then
		Result := 'cpp/SEC35'
	else if Ref = 'Elif' then
		Result := 'cpp/SEC36'
	else if Ref = 'Deleted Code' then
		Result := 'cpp/SEC37'
	else if Ref = 'Ifdef' then
		Result := 'cpp/SEC38'
	else if Ref = 'Defined' then
		Result := 'cpp/SEC38a'
	else if Ref = 'Assertions' then
		Result := 'cpp/SEC39'
	else if Ref = 'Diagnostics' then
		Result := 'cpp/SEC40'
	else if Ref = 'Line Control' then
		Result := 'cpp/SEC41'
	else if Ref = 'Pragmas' then
		Result := 'cpp/SEC46'
	else if Ref = 'Other Directives' then
		Result := 'cpp/SEC42'
	else if Ref = 'Preprocessor Output' then
		Result := 'cpp/SEC43'
	else if Ref = 'Traditional Mode' then
		Result := 'cpp/SEC70'
	else if Ref = 'Traditional lexical analysis' then
		Result := 'cpp/SEC71'
	else if Ref = 'Traditional macros' then
		Result := 'cpp/SEC72'
	else if Ref = 'Traditional miscellany' then
		Result := 'cpp/SEC73'
	else if Ref = 'Traditional warnings' then
		Result := 'cpp/SEC74'
	else if Ref = 'Implementation Details' then
		Result := 'cpp/SEC80'
	else if Ref = 'Implementation-defined behavior' then
		Result := 'cpp/SEC81'
	else if Ref = 'Implementation limits' then
		Result := 'cpp/SEC82'
	else if Ref = 'Obsolete Features' then
		Result := 'cpp/SEC83'
	else if Ref = 'Obsolete once-only headers' then
		Result := 'cpp/SEC84'
	else if Ref = 'Miscellaneous obsolete features' then
		Result := 'cpp/SEC85'
	else if Ref = 'Differences from previous versions' then
		Result := 'cpp/SEC86'
	else if Ref = 'Invocation' then
		Result := 'cpp/SEC44'
	else if Ref = 'Environment Variables' then
		Result := 'cpp/SEC45'
	else if Ref = 'C Extensions' then
		Result := 'gnuexts/gnuexts'
	else if Ref = 'Statement Exprs' then
		Result := 'gnuexts/SEC63'
	else if Ref = 'Local Labels' then
		Result := 'gnuexts/SEC64'
	else if Ref = 'Labels as Values' then
		Result := 'gnuexts/SEC65'
	else if Ref = 'Nested Functions' then
		Result := 'gnuexts/SEC66'
	else if Ref = 'Constructing Calls' then
		Result := 'gnuexts/SEC67'
	else if Ref = 'Naming Types' then
		Result := 'gnuexts/SEC68'
	else if Ref = 'Typeof' then
		Result := 'gnuexts/SEC69'
	else if Ref = 'Lvalues' then
		Result := 'gnuexts/SEC70'
	else if Ref = 'Conditional Extensions' then
		Result := 'gnuexts/SEC71'
	else if Ref = 'Long Long' then
		Result := 'gnuexts/SEC72'
	else if Ref = 'Complex' then
		Result := 'gnuexts/SEC73'
	else if Ref = 'Hex Floats' then
		Result := 'gnuexts/SEC74'
	else if Ref = 'Zero Length' then
		Result := 'gnuexts/SEC75'
	else if Ref = 'Variable Length' then
		Result := 'gnuexts/SEC76'
	else if Ref = 'Variadic Macro Extensions' then
		Result := 'gnuexts/SEC77'
	else if Ref = 'Subscripting' then
		Result := 'gnuexts/SEC78'
	else if Ref = 'Pointer Arith' then
		Result := 'gnuexts/SEC79'
	else if Ref = 'Initializers' then
		Result := 'gnuexts/SEC80'
	else if Ref = 'Compound Literals' then
		Result := 'gnuexts/SEC81'
	else if Ref = 'Designated Inits' then
		Result := 'gnuexts/SEC82'
	else if Ref = 'Case Ranges' then
		Result := 'gnuexts/SEC83'
	else if Ref = 'Cast to Union' then
		Result := 'gnuexts/SEC84'
	else if Ref = 'Function Attributes' then
		Result := 'gnuexts/SEC85'
	else if Ref = 'Attribute Syntax' then
		Result := 'gnuexts/SEC85a'
	else if Ref = 'Function Prototypes' then
		Result := 'gnuexts/SEC86'
	else if Ref = 'C++ Comments' then
		Result := 'gnuexts/SEC87'
	else if Ref = 'Dollar Signs' then
		Result := 'gnuexts/SEC88'
	else if Ref = 'Character Escapes' then
		Result := 'gnuexts/SEC89'
	else if Ref = 'Alignment' then
		Result := 'gnuexts/SEC90'
	else if Ref = 'Variable Attributes' then
		Result := 'gnuexts/SEC91'
	else if Ref = 'Type Attributes' then
		Result := 'gnuexts/SEC92'
	else if Ref = 'Inline' then
		Result := 'gnuexts/SEC93'
	else if Ref = 'Extended Asm,,Assembler Instructions with C Expression Operands' then
		Result := 'gnuexts/SEC94'
	else if Ref = 'Asm Labels' then
		Result := 'gnuexts/SEC96'
	else if Ref = 'Explicit Reg Vars' then
		Result := 'gnuexts/SEC97'
	else if Ref = 'Global Reg Vars' then
		Result := 'gnuexts/SEC98'
	else if Ref = 'Local Reg Vars' then
		Result := 'gnuexts/SEC99'
	else if Ref = 'Alternate Keywords' then
		Result := 'gnuexts/SEC100'
	else if Ref = 'Incomplete Enums' then
		Result := 'gnuexts/SEC101'
	else if Ref = 'Function Names' then
		Result := 'gnuexts/SEC102'
	else if Ref = 'Return Address' then
		Result := 'gnuexts/SEC103'
	else if Ref = 'Other Builtins' then
		Result := 'gnuexts/SEC104'
	else if Ref = 'Escaped Newlines' then
		Result := 'gnuexts/SEC105'
	else if Ref = 'Multi-line Strings' then
		Result := 'gnuexts/SEC106'
	else if Ref = 'Mixed Declarations' then
		Result := 'gnuexts/SEC107'
	else if Ref = 'Unnamed Fields' then
		Result := 'gnuexts/SEC108'
	else if Ref = 'Volatiles' then
		Result := 'gnuexts/SEC109'
	else if Ref = 'Empty Structures' then
		Result := 'gnuexts/SEC111'
	else if Ref = 'Acknowledgements,,GNU Assembler Acknowledgements' then
		Result := 'gnuasm/acknowledge'
	else if Ref = 'Command Line,,Command Line' then
		Result := 'gnuasm/SEC9'
	else if (Ref = 'Invoking,,Command-Line Options') or (Ref = 'Invoking,,Comand-Line Options') then
		Result := 'gnuasm/SEC10'
	else if Ref = 'Overview,,Overview' then
		Result := 'gnuasm/SEC11'
	else if Ref = 'M68K-Opts,,M680x0 Options' then
		Result := 'gnuasm/SEC12'
	else if Ref = 'a,,Enable Listings: @option{-a[cdhlns]}' then
		Result := 'gnuasm/SEC13'
	else if Ref = 'listing,,Configuring listing output: @option{--listing}' then
		Result := 'gnuasm/SEC14'
	else if Ref = 'o,,Name the Object File: @option{-o}' then
		Result := 'gnuasm/SEC15'
	else if Ref = 'W,,Control Warnings: @option{-W}, @option{--warn}, @option{--no-warn}, @option{--fatal-warnings}' then
		Result := 'gnuasm/SEC16'
	else if Ref = 'R,,Join Data and Text Sections: @option{-R}' then
		Result := 'gnuasm/SEC17'
	else if Ref = 'L,,Include Local Labels: @option{-L}' then
		Result := 'gnuasm/SEC18'
	else if Ref = 'traditional-format,,Compatible Output: @option{--traditional-format}' then
		Result := 'gnuasm/SEC19'
	else if Ref = 'M,,Assemble in MRI Compatibility Mode: @option{-M}' then
		Result := 'gnuasm/SEC19a'
	else if Ref = 'Input Files,,Input Files' then
		Result := 'gnuasm/SEC21'
	else if Ref = 'Input Files,,Filenames and Line-numbers' then
		Result := 'gnuasm/SEC22'
	else if Ref = 'Object,,Output (Object) File' then
		Result := 'gnuasm/SEC23'
	else if Ref = 'Errors,,Error and Warning Messages' then
		Result := 'gnuasm/SEC24'
	else if Ref = 'Syntax,,Syntax' then
		Result := 'gnuasm/SEC25'
	else if Ref = 'Preprocessing,,Preprocessing' then
		Result := 'gnuasm/SEC26'
	else if Ref = 'Whitespace,,Whitespace' then
		Result := 'gnuasm/SEC27'
	else if Ref = 'Comments,,Comments' then
		Result := 'gnuasm/SEC28'
	else if Ref = 'Symbol Intro,,Symbols' then
		Result := 'gnuasm/SEC29'
	else if Ref = 'Statements,,Statements' then
		Result := 'gnuasm/SEC30'
	else if Ref = 'Constants,,Constants' then
		Result := 'gnuasm/SEC31'
	else if Ref = 'Characters,,Character Constants' then
		Result := 'gnuasm/SEC32'
	else if (Ref = 'Strings,,Strings') or (Ref = 'Strings') then
		Result := 'gnuasm/SEC33'
	else if Ref = 'Chars,,Characters' then
		Result := 'gnuasm/SEC34'
	else if Ref = 'Numbers,,Number Constants' then
		Result := 'gnuasm/SEC35'
	else if Ref = 'Integers,,Integers' then
		Result := 'gnuasm/SEC36'
	else if Ref = 'Bignums,,Bignums' then
		Result := 'gnuasm/SEC37'
	else if Ref = 'Flonums,,Flonums' then
		Result := 'gnuasm/SEC38'
	else if Ref = 'M68K-Syntax,,Syntax' then
		Result := 'gnuasm/SEC216'
	else if Ref = 'M68K-Moto-Syntax,,Motorola Syntax' then
		Result := 'gnuasm/SEC217'
	else if Ref = 'M68K-Branch,,Branch Improvement' then
		Result := 'gnuasm/SEC221'
	else if Ref = 'M68K-Chars,,Special Characters' then
		Result := 'gnuasm/SEC222'
	else if (Ref = 'Sections,,Sections and Relocation') or (Ref = 'Secs Background,,Background') then
		Result := 'gnuasm/SEC39'
	else if Ref = 'Ld Sections,,Linker Sections' then
		Result := 'gnuasm/SEC41'
	else if Ref = 'As Sections,,Assembler Internal Sections' then
		Result := 'gnuasm/SEC42'
	else if Ref = 'Sub-Sections,,Sub-Sections' then
		Result := 'gnuasm/SEC43'
	else if Ref = 'bss,,bss Section' then
		Result := 'gnuasm/SEC44'
	else if (Ref = 'Symbols,,Symbols') or (Ref = 'Symbols') then
		Result := 'gnuasm/SEC45'
	else if Ref = 'Labels,,Labels' then
		Result := 'gnuasm/SEC46'
	else if Ref = 'Setting Symbols,,Giving Symbols Other Values' then
		Result := 'gnuasm/SEC47'
	else if (Ref = 'Symbol Names') or (Ref = 'Symbol Names,,Symbol Names') then
		Result := 'gnuasm/SEC48'
	else if Ref = 'Symbol Names,,Local Symbol Names' then
		Result := 'gnuasm/SEC48L'
	else if Ref = 'Symbol Names,,Dollar Local Labels' then
		Result := 'gnuasm/SEC48LD'
	else if Ref = 'Dot,,The Special Dot Symbol' then
		Result := 'gnuasm/SEC49'
	else if (Ref = 'Symbol Attributes') or (Ref = 'Symbol Attributes,,Symbol Attributes') then
		Result := 'gnuasm/SEC50'
	else if Ref = 'Symbol Value,,Value' then
		Result := 'gnuasm/SEC51'
	else if Ref = 'Symbol Type,,Type' then
		Result := 'gnuasm/SEC52'
	else if Ref = 'COFF Symbols,,Symbol Attributes for COFF' then
		Result := 'gnuasm/SEC56'
	else if Ref = 'COFF Symbols,,Primary Attributes' then
		Result := 'gnuasm/SEC57'
	else if Ref = 'COFF Symbols,,Auxiliary Attributes' then
		Result := 'gnuasm/SEC58'
	else if (Ref = 'Expressions') or (Ref = 'Expressions,,Expressions') then
		Result := 'gnuasm/SEC60'
	else if Ref = 'Empty Exprs,,Empty Expressions' then
		Result := 'gnuasm/SEC61'
	else if Ref = 'Integer Exprs,,Integer Expressions' then
		Result := 'gnuasm/SEC62'
	else if Ref = 'Arguments,,Arguments' then
		Result := 'gnuasm/SEC63'
	else if Ref = 'Operators,,Operators' then
		Result := 'gnuasm/SEC64'
	else if (Ref = 'Prefix Ops,,Prefix Operator') or (Ref = 'Prefix Ops,,Prefix Operators') then
		Result := 'gnuasm/SEC65'
	else if (Ref = 'Infix Ops,,Infix Operator') or (Ref = 'Infix Ops,,Infix Operators') then
		Result := 'gnuasm/SEC66'
	else if Ref = 'Pseudo Ops,,Assembler Directives' then
		Result := 'gnuasm/SEC67'
	else if Pos ('.abort', Ref) > 0 then
		Result := 'gnuasm/SEC68'
	else if Pos ('.ABORT', Ref) > 0 then
		Result := 'gnuasm/SEC69'
	else if Pos ('.align', Ref) > 0 then
		Result := 'gnuasm/SEC70'
	else if Pos ('.app-file', Ref) > 0 then
		Result := 'gnuasm/SEC71'
	else if Pos ('.ascii', Ref) > 0 then
		Result := 'gnuasm/SEC72'
	else if Pos ('.asciz', Ref) > 0 then
		Result := 'gnuasm/SEC73'
	else if Pos ('.balign', Ref) > 0 then
		Result := 'gnuasm/SEC74'
	else if Pos ('.byte', Ref) > 0 then
		Result := 'gnuasm/SEC75'
	else if Pos ('.comm', Ref) > 0 then
		Result := 'gnuasm/SEC76'
	else if Pos ('.data', Ref) > 0 then
		Result := 'gnuasm/SEC77'
	else if Pos ('.def', Ref) > 0 then
		Result := 'gnuasm/SEC78'
	else if Pos ('.desc', Ref) > 0 then
		Result := 'gnuasm/SEC79'
	else if Pos ('.dim', Ref) > 0 then
		Result := 'gnuasm/SEC80'
	else if Pos ('.double', Ref) > 0 then
		Result := 'gnuasm/SEC81'
	else if Pos ('.eject', Ref) > 0 then
		Result := 'gnuasm/SEC82'
	else if Pos ('.elseif', Ref) > 0 then
		Result := 'gnuasm/SEC83IF'
	else if Pos ('.else', Ref) > 0 then
		Result := 'gnuasm/SEC83'
	else if Pos ('.endef', Ref) > 0 then
		Result := 'gnuasm/SEC84'
	else if Pos ('.endif', Ref) > 0 then
		Result := 'gnuasm/SEC84IF'
	else if Pos ('.endfunc', Ref) > 0 then
		Result := 'gnuasm/SEC84FUNC'
	else if Pos ('.endm', Ref) > 0 then
		Result := 'gnuasm/SEC84M'
	else if Pos ('.endr', Ref) > 0 then
		Result := 'gnuasm/SEC84R'
	else if Pos ('.end', Ref) > 0 then
		Result := 'gnuasm/SEC83b'
	else if Pos ('.equiv', Ref) > 0 then
		Result := 'gnuasm/SEC87'
	else if Pos ('.equ', Ref) > 0 then
		Result := 'gnuasm/SEC86'
	else if Pos ('.err', Ref) > 0 then
		Result := 'gnuasm/SEC88'
	else if Pos ('.even', Ref) > 0 then
		Result := 'gnuasm/SEC70E'
	else if Pos ('.exitm', Ref) > 0 then
		Result := 'gnuasm/SEC85'
	else if Pos ('.extern', Ref) > 0 then
		Result := 'gnuasm/SEC89'
	else if Pos ('.fail', Ref) > 0 then
		Result := 'gnuasm/SEC89a'
	else if Pos ('.file', Ref) > 0 then
		Result := 'gnuasm/SEC90'
	else if Pos ('.fill', Ref) > 0 then
		Result := 'gnuasm/SEC91'
	else if Pos ('.float', Ref) > 0 then
		Result := 'gnuasm/SEC92'
	else if Pos ('.func', Ref) > 0 then
		Result := 'gnuasm/SEC92a'
	else if (Pos ('.globl', Ref) > 0) or (Pos ('.global', Ref) > 0) then
		Result := 'gnuasm/SEC93'
	else if Pos ('.hword', Ref) > 0 then
		Result := 'gnuasm/SEC94'
	else if Pos ('.ident', Ref) > 0 then
		Result := 'gnuasm/SEC95'
	else if Pos ('.ifdef', Ref) > 0 then
		Result := 'gnuasm/SEC96D'
	else if Pos ('.ifndef', Ref) > 0 then
		Result := 'gnuasm/SEC96N'
	else if Pos ('.ifnotdef', Ref) > 0 then
		Result := 'gnuasm/SEC96ND'
	else if (Pos ('.if ', Ref) > 0) or (Pos ('.if}', Ref) > 0) then
		Result := 'gnuasm/SEC96'
	else if Pos ('.incbin', Ref) > 0 then
		Result := 'gnuasm/SEC97B'
	else if Pos ('.include', Ref) > 0 then
		Result := 'gnuasm/SEC97'
	else if Pos ('.int', Ref) > 0 then
		Result := 'gnuasm/SEC98'
	else if Pos ('.irpc', Ref) > 0 then
		Result := 'gnuasm/SEC100'
	else if Pos ('.irp', Ref) > 0 then
		Result := 'gnuasm/SEC99'
	else if Pos ('.lcomm', Ref) > 0 then
		Result := 'gnuasm/SEC101'
	else if Pos ('.lflags', Ref) > 0 then
		Result := 'gnuasm/SEC102'
	else if Pos ('.line', Ref) > 0 then
		Result := 'gnuasm/SEC103'
	else if Pos ('.ln', Ref) > 0 then
		Result := 'gnuasm/SEC105'
	else if Pos ('.list', Ref) > 0 then
		Result := 'gnuasm/SEC107'
	else if Pos ('.long', Ref) > 0 then
		Result := 'gnuasm/SEC108'
	else if Pos ('.macro', Ref) > 0 then
		Result := 'gnuasm/SEC109'
	else if Pos ('.mri', Ref) > 0 then
		Result := 'gnuasm/SEC106'
	else if Pos ('.nolist', Ref) > 0 then
		Result := 'gnuasm/SEC110'
	else if Pos ('.octa', Ref) > 0 then
		Result := 'gnuasm/SEC111'
	else if Pos ('.org', Ref) > 0 then
		Result := 'gnuasm/SEC112'
	else if Pos ('.p2align[wl]', Ref) > 0 then
		Result := 'gnuasm/SEC113'
	else if Pos ('.print', Ref) > 0 then
		Result := 'gnuasm/SEC113a'
	else if Pos ('.psize', Ref) > 0 then
		Result := 'gnuasm/SEC114'
	else if Pos ('.purgem', Ref) > 0 then
		Result := 'gnuasm/SEC114a'
	else if Pos ('.quad', Ref) > 0 then
		Result := 'gnuasm/SEC115'
	else if Pos ('.rept', Ref) > 0 then
		Result := 'gnuasm/SEC116'
	else if Pos ('.sbttl', Ref) > 0 then
		Result := 'gnuasm/SEC117'
	else if Pos ('.scl', Ref) > 0 then
		Result := 'gnuasm/SEC118'
	else if Ref = 'Section,,COFF Version' then
		Result := 'gnuasm/SEC119'
	else if Pos ('.set', Ref) > 0 then
		Result := 'gnuasm/SEC120'
	else if Pos ('.short', Ref) > 0 then
		Result := 'gnuasm/SEC121'
	else if Pos ('.single', Ref) > 0 then
		Result := 'gnuasm/SEC122'
	else if Ref = 'Size,,COFF Version' then
		Result := 'gnuasm/SEC123'
	else if Pos ('.sleb128', Ref) > 0 then
		Result := 'gnuasm/SEC124'
	else if Pos ('.skip', Ref) > 0 then
		Result := 'gnuasm/SEC125'
	else if Pos ('.space', Ref) > 0 then
		Result := 'gnuasm/SEC126'
	else if Pos ('.stabd', Ref) > 0 then
		Result := 'gnuasm/SEC127'
	else if Pos ('.stabn', Ref) > 0 then
		Result := 'gnuasm/SEC127N'
	else if Pos ('.stabs', Ref) > 0 then
		Result := 'gnuasm/SEC127S'
	else if Pos ('.string', Ref) > 0 then
		Result := 'gnuasm/SEC128'
	else if Pos ('.struct', Ref) > 0 then
		Result := 'gnuasm/SEC128a'
	else if Pos ('.symver', Ref) > 0 then
		Result := 'gnuasm/SEC129'
	else if Pos ('.tag', Ref) > 0 then
		Result := 'gnuasm/SEC130'
	else if Pos ('.text', Ref) > 0 then
		Result := 'gnuasm/SEC131'
	else if Pos ('.title', Ref) > 0 then
		Result := 'gnuasm/SEC132'
	else if Ref = 'Type,,COFF Version' then
		Result := 'gnuasm/SEC133'
	else if Pos ('.uleb128', Ref) > 0 then
		Result := 'gnuasm/SEC134'
	else if Pos ('.val', Ref) > 0 then
		Result := 'gnuasm/SEC135'
	else if Pos ('.vtable_entry', Ref) > 0 then
		Result := 'gnuasm/SEC135a'
	else if Pos ('.word', Ref) > 0 then
		Result := 'gnuasm/SEC136'
	else if Ref = 'Bug Criteria,,Have You Found a Bug?' then
		Result := 'Bug Criteria,,Have You Found a Bug'
	else
		Result := StringReplace (Ref, '/', '_', [rfReplaceAll]);
	if Pos (':', Result) > 0 then
		Delete (Result, Pos (':', Result), Length (Result));
end;

procedure PreProcessSource(var Source: string);
begin
	Source := StringReplace (Source, '@value{AS}', 'as', [rfReplaceAll]);
	Source := StringReplace (Source, '@value{LD}', 'ld', [rfReplaceAll]);
	Source := StringReplace (Source, '@value{GCC}', 'gcc', [rfReplaceAll]);
	Source := StringReplace (Source, '@value{CPP}', 'cpp', [rfReplaceAll]);
	Source := StringReplace (Source, '''''', '"', [rfReplaceAll]);
	Source := StringReplace (Source, '``', '"', [rfReplaceAll]);
	Source := StringReplace (Source, '´´', '"', [rfReplaceAll]);
end;

procedure PostProcessDest(var Dest: string);
var
	P,
	I: Integer;
begin
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'<BR><BR>', '<BR><BR>', [rfReplaceAll]);
	Dest := StringReplace (Dest, #13#10#13#10'<BR><BR>', #13#10'<BR><BR>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10#13#10, '<BR><BR>'#13#10, [rfReplaceAll]);
	Dest := StringReplace (Dest, '<LI>'#13#10'</LI>'#13#10, '', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<LI>'#13#10'<BR><BR>', '<LI>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'</LI>'#13#10'</UL>', '</LI>'#13#10'</UL>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'</LI>'#13#10'</OL>', '</LI>'#13#10'</OL>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'<PRE>', '<PRE>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'</PRE>', '</PRE>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '</PRE>'#13#10'<BR><BR>', '</PRE>', [rfReplaceAll]);
	Dest := StringReplace (Dest, #13#10#13#10'</PRE>', #13#10'</PRE>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'<UL', '<UL', [rfReplaceAll]);
	Dest := StringReplace (Dest, '</UL>'#13#10'<BR><BR>', '</UL>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'<OL', '<OL', [rfReplaceAll]);
	Dest := StringReplace (Dest, '</OL>'#13#10'<BR><BR>', '</OL>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'<DL', '<DL', [rfReplaceAll]);
	Dest := StringReplace (Dest, '</DL>'#13#10'<BR><BR>', '</DL>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<BR><BR>'#13#10'</DL>', '</DL>', [rfReplaceAll]);
	Dest := StringReplace (Dest, '<DD><DT>', '<BR>', [rfReplaceAll]);
	P := Pos ('</CODE></A>', Dest);
	while P > 0 do begin
		Dest [P + 2] := 'A';
		Dest [P + 3] := '>';
		Dest [P + 4] := '<';
		Dest [P + 5] := '/';
		Dest [P + 6] := 'C';
		Dest [P + 7] := 'O';
		Dest [P + 8] := 'D';
		Dest [P + 9] := 'E';
		I := P - 1;
		while (I > 0) and (Copy (Dest, I, Length ('<CODE>')) <> '<CODE>') do
			Dec (I);
		if I > 0 then begin
			Delete (Dest, I, Length ('<CODE>'));
			Dec (I);
			while (I > 0) and (Dest [I] <> '<') do
				Dec (I);
			if I > 0 then
				Insert ('<CODE>', Dest, I);
		end;
		P := Pos ('</CODE></A>', Dest);
	end;
	Dest := StringReplace (Dest, '.   ', '.  ', [rfReplaceAll]);
end;

function CountChars(Ch: Char; S: string): Integer;
var
	I: Integer;
begin
	Result := 0;
	for I := 1 to Length (S) do
		if S [I] = Ch then
			Inc (Result);
end;

type
	TListType = (ltNone, ltNormal, ltColumns, ltFiles);

var
	FullContents,
	NewContents,
	CurFile,
	CurNode,
	CurSection,
	CurSectionTitle,
	CurSubFile: string;
	P,
	SecStart,
	PreFormatted,
	SCase,
	CodeFormat: Integer;
	InsertedSeeAlso: Boolean = False;

procedure ProcessSecID(var SecID: string);
begin
	if (SecID = 'Overview') and (CurFile = 'cpp') then
		SecID := 'CPP Overview'
	else if (SecID = 'Conditionals') and (CurFile = 'gnuexts') then
		SecID := 'Conditional Extensions'
	else if (SecID = 'Variadic Macros') and (CurFile = 'gnuexts') then
		SecID := 'Variadic Macro Extensions'
	else if (SecID = 'Pragmas') and (CurFile = 'gnuexts') then
		SecID := 'Pragma Extensions'
	else if (SecID = 'Acknowledgements,,Acknowledgements') and (CurFile = 'gnuasm') then
		SecID := 'Acknowledgements,,GNU Assembler Acknowledgements';
end;

function GetLinkTo(Ref: string): string;
begin
	ProcessSecID (Ref);
	Result := GetSecID (Ref);
	if StartsWith (CurFile + '/', Result, True, 1, True) then
		Result := '$$LINK(' + Result + ')'
	else
		Result := '$$INFOLINK(' + Result + ')';
end;

procedure FinishSection;
var
	I: Integer;
	SecID,
	TempContents,
	S: string;
begin
	SecID := CurNode;
	if (Length (CurSection) > 0) and (((CurFile <> 'cpp') and (CurFile <> 'gnuexts')) or (SecID = 'Extended Asm')) then
		SecID := SecID + ',,' + CurSection;
	ProcessSecID (SecID);
	SecID := GetSecID (SecID);
	if Length (CurSubFile) > 0 then
		SecID := SecID + '_' + StringReplace (CurSubFile, '/', ' and ', [rfReplaceAll]);
	TempContents := Copy (NewContents, SecStart, Length (NewContents));
	PostProcessDest (TempContents);
	TrimHTMLWhiteSpace (TempContents);
	if (Pos ('@code', CurSectionTitle) > 0) and (Pos ('.', CurSectionTitle) > 0) then begin
		Delete (CurSectionTitle, 1, Pos ('.', CurSectionTitle) - 1);
		for I := 3 to Length (CurSectionTitle) do
			if not (CurSectionTitle [I] in ['A'..'Z', 'a'..'z', '_', '.', '0'..'9', '[', ']']) then begin
				Delete (CurSectionTitle, I, Length (CurSectionTitle));
				Break;
			end;
	end;
	if Length (TempContents) > 0 then begin
		if Length (CurSubFile) > 0 then
			S := CurSubFile
		else
			S := CurSectionTitle;
		WriteFile (SecID + '.hss', '[Main]'#13#10'Title=' + S + #13#10#13#10'[Top]'#13#10 + TempContents + #13#10);
	end;
	SecStart := Length (NewContents) + 1;
end;

procedure StartSeeAlso;
var
	I: Integer;
begin
	I := Length (NewContents);
	while (I > 0) and (NewContents [I] in [#13, #10, ' ', #9]) do
		Dec (I);
	InsertedSeeAlso := True;
	if (I <= 0) or (NewContents [I] in ['.', '>']) then
		NewContents := NewContents + 'See '
	else if (NewContents [I] in [',', ';', ':', '(', '[', '{']) then
		NewContents := NewContents + 'see '
	else
		InsertedSeeAlso := False;
end;

procedure EndSeeAlso;
begin
	if InsertedSeeAlso and (P <= Length (FullContents)) and (FullContents [P] = ',') then
		Inc (P);
	InsertedSeeAlso := False;
end;

procedure ConvertFromP(ListType: TListType = ltNone);
var
	I,
	J: Integer;
	Tag,
	Parameters,
	Quotes,
	S: string;
begin
	while P <= Length (FullContents) do begin
		if FullContents [P] = '}' then begin
			Inc (P);
			Break;
		end else if FullContents [P] = '{' then begin
			Inc (P);
			ConvertFromP (ListType);
		end else if FullContents [P] = #13 then begin
			while (Length (NewContents) > 0) and (NewContents [Length (NewContents)] = ' ') do
				Delete (NewContents, Length (NewContents), 1);
			if PreFormatted > 0 then begin
				NewContents := NewContents + #13#10;
				Inc (P);
			end else begin
				if (Copy (FullContents, P, 2) = #13#13) or (Copy (FullContents, P, 4) = #13#10#13#10) or (((Copy (FullContents, P, 2) = #13'@') or (Copy (FullContents, P, 3) = #13#10'@')) and (Pos ('{', Copy (FullContents, P, 25)) <= 0) and (Pos ('if', Copy (FullContents, P, 25)) <= 0)) then begin
					if Copy (NewContents, Length (NewContents) - 9, 10) <> '<BR><BR>'#13#10 then
						NewContents := NewContents + #13#10'<BR><BR>'#13#10;
					while (P <= Length (FullContents)) and (FullContents [P] in [#13, #10]) do
						Inc (P);
				end else begin
					if Copy (NewContents, Length (NewContents) - 1, 2) <> #13#10 then
						NewContents := NewContents + #13#10;
					Inc (P);
				end;
			end;
		end else if FullContents [P] = '@' then begin
			Inc (P);
			if P <= Length (FullContents) then begin
				if FullContents [P] in ['A'..'Z', 'a'..'z', '0'..'9'] then begin
					Tag := '';
					Parameters := '';
					Quotes := '';
					while (P <= Length (FullContents)) and (not (FullContents [P] in [#13, #10, ' ', '{'])) do begin
						Tag := Tag + FullContents [P];
						Inc (P);
					end;
					if (P <= Length (FullContents)) and (FullContents [P] = '{') then begin
						Inc (P);
						if (Tag = 'w')
							or (Tag = 'r')
							or (Tag = 'gccoptlist')
							or (Tag = 'key')
							or (Tag = 'value')
							or (Tag = 'copyrightnotice')
							or (Tag = 'covertexts')
							or (Tag = 'acronym')
							then
							Tag := ''
						else if (Tag = 'sc') then
						else if (Tag = 'anchor') then begin
							Tag := '';
							while (P <= Length (FullContents)) and (not (FullContents [P] in ['@', '{', '}'])) do
								Inc (P);
						end else if Tag = 'copyright' then begin
							Tag := '';
							NewContents := NewContents + '(c)';
						end else if Tag = 'TeX' then begin
							Tag := '';
							NewContents := NewContents + 'TeX';
						end else if Tag = 'equiv' then begin
							Tag := '';
							NewContents := NewContents + '&lt;=&gt;';
						end else if Tag = 'expansion' then begin
							Tag := '';
							NewContents := NewContents + 'expands to';
						end else if Tag = 'error' then begin
							Tag := '';
							NewContents := NewContents + 'Error:';
						end else if Tag = 'minus' then begin
							Tag := '';
							NewContents := NewContents + '<FONT FACE="Courier New">-</FONT>';
						end else if (Tag = 'dots') then begin
							Tag := '';
							NewContents := NewContents + '...';
						end else if (Tag = 'code') or (Tag = 'samp') or (Tag = 'command') or (Tag = 'file') or (Tag = 'math') or (Tag = 'env') or (Tag = 'kbd') or (Tag = 't') then begin
							if (Length (FullContents) >= P + 2) and (FullContents [P] = '-') and ((FullContents [P + 1] in ['A'..'Z', 'a'..'z', '0'..'9']) or ((FullContents [P + 1] = '-') and (FullContents [P + 2] in ['A'..'Z', 'a'..'z', '0'..'9']))) then begin
								Tag := 'B';
								Quotes := '''';
							end else
								Tag := 'CODE';
						end else if (Tag = 'strong') or (Tag = 'b') then
							Tag := 'B'
						else if (Tag = 'emph') or (Tag = 'var') or (Tag = 'i') then
							Tag := 'I'
						else if (Tag = 'dfn') then
							Tag := 'U'
						else if (Tag = 'option') then begin
							Tag := 'B';
							Quotes := '''';
						end else if (Tag = 'cite') then begin
							Tag := '';
							Quotes := '"';
						end else if (Tag = 'ref') or (Tag = 'xref') or (Tag = 'pxref') then begin
							Tag := 'A';
							I := Pos ('}', Copy (FullContents, P, 255));
							if I > 0 then begin
								while CountChars ('{', Copy (FullContents, P, I - 1)) > CountChars ('}', Copy (FullContents, P, I - 1)) do
									Inc (I);
								Parameters := 'HREF="' + GetLinkTo (Copy (FullContents, P, I - 1)) + '"';
								if ((Pos (',,', Copy (FullContents, P, 255)) > 0) or (Pos (','#13',', Copy (FullContents, P, 255)) > 0) or (Pos (','#13#10',', Copy (FullContents, P, 255)) > 0)) and (Pos ('{', Copy (FullContents, P, Pos (',', Copy (FullContents, P, 255)))) <= 0) then begin
									Delete (FullContents, P, Pos (',', Copy (FullContents, P, 255)));
									Delete (FullContents, P, Pos (',', Copy (FullContents, P, 255)));
									while FullContents [P] in [' ', #9, #13, #10] do
										Delete (FullContents, P, 1);
								end;
								StartSeeAlso;
							end;
						end else if (Tag = 'uref') then begin
							Tag := 'A';
							I := Pos ('}', Copy (FullContents, P, 255));
							if I > 0 then begin
								Parameters := 'HREF="' + Copy (FullContents, P, I - 1) + '"';
								StartSeeAlso;
							end;
						end else if (Tag = 'email') then begin
							Tag := 'A';
							I := Pos ('}', Copy (FullContents, P, 255));
							if I > 0 then
								Parameters := 'HREF="mailto:' + StringReplace (Copy (FullContents, P, I - 1), '@@', '@', []) + '"';
						end else if (Tag = 'footnote') then begin
							Tag := '';
							Quotes := '(';
							NewContents := NewContents + ' ';
						end else
							raise ESubStrNotFound.CreateFmt ('Unsupported style "%s"', [Tag]);
						if (Length (Tag) > 0) and (Tag <> 'sc') then begin
							NewContents := NewContents + '<' + Tag;
							if Length (Parameters) > 0 then
								NewContents := NewContents + ' ' + Parameters;
							NewContents := NewContents + '>' + Quotes;
						end;
						if Tag = 'sc' then
							Inc (SCase)
						else if Tag = 'CODE' then
							Inc (CodeFormat);
						ConvertFromP (ListType);
						if Tag = 'sc' then
							Dec (SCase)
						else if Tag = 'CODE' then
							Dec (CodeFormat);
						if Quotes = '(' then
							Quotes := ')';
						if (Length (Tag) > 0) and (Tag <> 'sc') then
							NewContents := NewContents + Quotes + '</' + Tag + '>';
						if Tag = 'A' then
							EndSeeAlso;
					end else begin
						while (P <= Length (FullContents)) and (FullContents [P] = ' ') do
							Inc (P);
						if Tag <> 'center' then begin
							while (P <= Length (FullContents)) and (not (FullContents [P] in [#13, #10])) do begin
								Parameters := Parameters + FullContents [P];
								Inc (P);
							end;
							while (P <= Length (FullContents)) and (FullContents [P] in [#13, #10]) do
								Inc (P);
							TrimWhiteSpace (Parameters);
							if (Tag = 'end') then begin
								Dec (P);
								if FullContents [P] = #10 then
									Dec (P);
								Break
							end else if (Tag = 'c')
								or (Tag = 'comment')
								or (Tag = 'set')
								or (Tag = 'setfilename')
								or (Tag = 'settitle')
								or (Tag = 'title')
								or (Tag = 'subtitle')
								or (Tag = 'cindex')
								or (Tag = 'kindex')
								or (Tag = 'opindex')
								or (Tag = 'findex')
								or (Tag = 'gol')
								or (Tag = 'noindent')
								or (Tag = 'need')
								or (Tag = 'page')
								or (Tag = 'setchapternewpage')
								or (Tag = 'finalout')
								or (Tag = 'printindex')
								or (Tag = 'syncodeindex')
								or (Tag = 'sp')
								or (Tag = 'author')
								or (Tag = 'vskip')
								or (Tag = 'top')
								or (Tag = 'refill')
								or (Tag = 'lowersections')
								or (Tag = 'raisesections')
								or (Tag = 'unnumbered')
								or (Tag = 'unnumberedsec')
								or (Tag = 'contents')
								or (Tag = 'defcodeindex')
								or (Tag = 'dircategory')
								or (Tag = 'tab')
								or (Tag = 'bye')
								or (Tag = 'insertcopying')
								then
							else if (Tag = 'node') then begin
								if not ((Parameters = 'Ln') and (CurNode = 'Line')) then begin
									FinishSection;
									CurNode := Parameters;
									CurSection := '';
									CurSectionTitle := Parameters;
								end;
							end else if (Tag = 'section') or (Tag = 'subsection') or (Tag = 'subsubsection') or (Tag = 'subheading') or (Tag = 'chapter') then begin
								if not ((CurNode = 'Line') and (Pos ('.ln', Parameters) > 0)) then begin
									FinishSection;
									CurSection := Parameters;
									CurSectionTitle := Parameters;
								end;
							end else if (Tag = 'ignore')
								or (Tag = 'tex')
								or (Tag = 'menu')
								or (Tag = 'direntry')
								or (Tag = 'copying')
								or ((Tag = 'ifset') and ((Parameters <> 'M68K') and (Parameters <> 'M680X0') and (Parameters <> 'COFF') and (Parameters <> 'W16') and (Parameters <> 'have-stabs') and (Parameters <> 'cppmanual')))
								then begin
								I := Length (NewContents) + 1;
								ConvertFromP (ListType);
								Delete (NewContents, I, Length (NewContents));
							end else if (Tag = 'group')
								or (Tag = 'ifset')
								or (Tag = 'ifclear')
								or (Tag = 'ifinfo')
								or (Tag = 'iftex')
								or (Tag = 'ifnottex')
								or (Tag = 'ifhtml')
								or (Tag = 'ifnothtml')
								or (Tag = 'macro')
								or (Tag = 'titlepage')
								or (Tag = 'quotation')
								or (Tag = 'display')
								or (Tag = 'detailmenu')
								or (Tag = 'html')
								then
								ConvertFromP (ListType)
							else if (Tag = 'example')
								or (Tag = 'smallexample')
								or (Tag = 'multitable')
								or (Tag = 'format')
								then begin
								NewContents := NewContents + '<PRE>';
								Inc (PreFormatted);
								ConvertFromP (ListType);
								Dec (PreFormatted);
								NewContents := NewContents + '</PRE>';
							end else if (Tag = 'table') or (Tag = 'ftable') or (Tag = 'vtable') then begin
								if CurFile = 'gnuexts' then begin
									S := NewContents;
									I := SecStart;
									SecStart := Length (NewContents) + 1;
									ConvertFromP (ltFiles);
									FinishSection;
									CurSubFile := '';
									SecStart := I;
									NewContents := S;
								end else begin
									if ListType = ltColumns then
										NewContents := NewContents + '<BR><BR>';
									NewContents := NewContents + '<DL>'#13#10;
									ConvertFromP (ltColumns);
									NewContents := NewContents + '</DL>';
									if ListType = ltColumns then
										NewContents := NewContents + '<BR>';
								end;
							end else if (Tag = 'itemize') then begin
								NewContents := NewContents + '<UL>'#13#10'<LI>'#13#10;
								ConvertFromP (ltNormal);
								NewContents := NewContents + '</LI>'#13#10'</UL>';
							end else if (Tag = 'enumerate') then begin
								NewContents := NewContents + '<OL';
								if Length (Parameters) > 0 then begin
									if Parameters [1] in ['0'..'9'] then
										NewContents := NewContents + ' START="' + Parameters + '"'
									else
										NewContents := NewContents + ' TYPE="' + Parameters + '"';
								end;
								NewContents := NewContents + '>'#13#10'<LI>'#13#10;
								ConvertFromP (ltNormal);
								NewContents := NewContents + '</LI>'#13#10'</OL>';
							end else if (Tag = 'item') or (Tag = 'itemx') then begin
								case ListType of
									ltNormal: begin
										NewContents := NewContents + '</LI>'#13#10'<LI>'#13#10;
										Insert (Parameters + #13#10, FullContents, P);
									end;
									ltColumns: begin
										Parameters := HTMLize (Parameters);
										I := Pos ('@', Parameters);
										while I > 0 do begin
											Delete (Parameters, I, 1);
											if (Length (Parameters) >= I) and (Parameters [I] in ['A'..'Z', 'a'..'z']) then begin
												if StartsWith ('var{', Parameters, True, I, True) then begin
													J := Pos ('}', Copy (Parameters, I, Length (Parameters)));
													if J > 0 then begin
														Delete (Parameters, I + J - 1, 1);
														Insert ('</I>', Parameters, I + J - 1);
														Insert ('<I>', Parameters, I);
													end;
												end else begin
													while (Length (Parameters) >= I) and (Parameters [I] <> '{') do
														Delete (Parameters, I, 1);
													if Length (Parameters) >= I then
														Delete (Parameters, I, 1);
													J := Pos ('}', Copy (Parameters, I, Length (Parameters)));
													if J > 0 then
														Delete (Parameters, I + J - 1, 1);
												end;
											end;
											I := Pos ('@', Parameters);
										end;
										NewContents := NewContents + '<DT><B>' + Parameters + '</B>'#13#10'<DD>';
									end;
									ltFiles: begin
										if P > SecStart + 5 then begin
											if Length (CurSubFile) > 0 then
												FinishSection;
											CurSubFile := Parameters;
											if Pos ('__builtin_', CurSubFile) > 0 then
												Delete (CurSubFile, 1, Pos ('__builtin_', CurSubFile) - 1);
											if Pos (' ', CurSubFile) > 0 then
												Delete (CurSubFile, Pos (' ', CurSubFile), Length (CurSubFile));
										end;
									end else
										Insert (Parameters, FullContents, P);
								end;
							end else if (Tag = 'exdent') then
								Insert (Parameters, FullContents, P)
							else if (Tag = 'deftypefn') then begin
								S := NewContents;
								I := SecStart;
								SecStart := Length (NewContents) + 1;
								CurSubFile := Parameters;
								if Pos ('__builtin_', CurSubFile) > 0 then
									Delete (CurSubFile, 1, Pos ('__builtin_', CurSubFile) - 1);
								if Pos (' ', CurSubFile) > 0 then
									Delete (CurSubFile, Pos (' ', CurSubFile), Length (CurSubFile));
								ConvertFromP;
								FinishSection;
								CurSubFile := '';
								SecStart := I;
								NewContents := S;
							end else if (Tag = 'include') then begin
								if FileExists (Parameters) or ((Parameters <> 'asconfig.texi') and (Parameters <> 'md.texi') and (Copy (Parameters, 1, 2) <> 'c-')) then begin
									S := LoadFile (Parameters);
									PreProcessSource (S);
									Insert (S, FullContents, P);
								end;
							end else
								raise ESubStrNotFound.CreateFmt ('Unsupported command "%s"', [Tag]);
						end;
					end;
				end else begin
					if FullContents [P] <> ':' then
						NewContents := NewContents + HTMLizeChar (FullContents [P]);
					Inc (P);
				end;
			end;
		end else begin
			if FullContents [P] <> #10 then begin
				if FullContents [P] in [' ', #13, #9] then begin
					if ((Copy (NewContents, Length (NewContents) - 1, 2) = 'eg') or (Copy (NewContents, Length (NewContents) - 1, 2) = 'ie')) and (Length (NewContents) > 2) and (NewContents [Length (NewContents) - 2] in [' ', #13, #9, '(', '[', '{']) then begin
						Insert ('.', NewContents, Length (NewContents));
						NewContents := NewContents + '.';
					end;
				end;
				if SCase > 0 then
					NewContents := NewContents + HTMLizeChar (UpCase (FullContents [P]))
				else if FullContents [P] in ['´', '`'] then
					NewContents := NewContents + ''''
				else if (FullContents [P] in ['-']) and (Copy (NewContents, Length (NewContents) - 1, 2) = '--') then begin
					NewContents := NewContents + ' ';
					NewContents [Length (NewContents) - 2] := ' ';
				end else if (FullContents [P] = ' ') and (CodeFormat > 0) then
					NewContents := NewContents + '&nbsp;'
				else
					NewContents := NewContents + HTMLizeChar (FullContents [P]);
			end;
			Inc (P);
		end;
	end;
end;

begin
	if Pos ('invoke', ParamStr (1)) > 0 then
		CurFile := 'comopts'
	else if Pos ('as', ParamStr (1)) > 0 then
		CurFile := 'gnuasm'
	else if Pos ('cpp', ParamStr (1)) > 0 then
		CurFile := 'cpp'
	else if Pos ('extend', ParamStr (1)) > 0 then
		CurFile := 'gnuexts'
	else
		CurFile := WithoutExt (ParamStr (1));
	FullContents := LoadFile (ParamStr (1));
	PreProcessSource (FullContents);
	NewContents := '<HTML><BODY>'#13#10;
	try
		CurNode := CurFile;
		CurSection := '';
		CurSectionTitle := '';
		CurSubFile := '';
		P := 1;
		SecStart := Length (NewContents) + 1;
		PreFormatted := 0;
		SCase := 0;
		CodeFormat := 0;
		ConvertFromP;
		if P <= Length (FullContents) then
			raise ESubStrNotFound.Create ('Unexpected closing bracket');
		FinishSection;
	finally
		NewContents := NewContents + #13#10'</BODY></HTML>';
		PostProcessDest (NewContents);
		WriteFile (ChangeFileExt (ParamStr (1), '.html'), NewContents);
	end;
end.

