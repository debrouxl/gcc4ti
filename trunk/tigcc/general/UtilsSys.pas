unit UtilsSys;

interface

uses
	Windows, SysUtils, Classes;

function LastPos (const Substr, S: string): Integer;

var
	AppLocationDir: string; // the directory where the program was started from, with a '\' at the end

implementation

function LastPos;
var
	I: Integer;
begin
	for I := Length (S) - Length (Substr) + 1 downto 1 do begin
		if Copy (S, I, Length (SubStr)) = Substr then begin
			Result := I;
			Exit;
		end;
	end;
	Result := 0;
end;

initialization
	AppLocationDir := ExtractFilePath (ParamStr (0));
end.
