{
  TIGCC Documentation Tools

  Copyright (C) 2002-2004 Sebastian Reichelt

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

program UpdateInfo;

uses
	Windows,
	SysUtils,
	Classes,
	UtilsDos,
	UtilsWin,
	Math,
	HelpSystemMasterUnit in '..\HelpSystemMasterUnit.pas';

function HTMLizeTitle(const Title: string): string;
begin
	Result := HTMLize (Title);
end;

procedure WriteInfoFile(const InfoFile: string);
var
	AvailableSections: TStringList;
	UseQueue: Boolean;
	Queue: TStringList;
	WebContents: string;
	Indent: Integer;
procedure WriteSection(const InfoFile, Section: string; out ContentsStr: string; const ParentSection: string = ''; const ParentTitle: string = ''; const PreviousSection: string = ''; const NextSection: string = '');
var
	Ln,
	I: Integer;
	Sec,
	Line,
	S,
	Title,
	Contents,
	IniContents,
	Body,
	PrepBody,
	FileName,
	Previous,
	Next,
	LeftNav,
	CenterNav,
	RightNav,
	SeeAlso: string;
	SubSections: TStringList;
begin
	Inc (Indent);
	try
		if not UseQueue then begin
			I := AvailableSections.IndexOf (Section);
			if I < 0 then
				raise EFileNotFound.CreateFmt ('Section %s referenced from %s/%s has already been used', [Section, InfoFile, ParentSection])
			else
				AvailableSections.Delete (I);
		end;
		SubSections := TStringList.Create;
		try
			Title := '';
			SeeAlso := '';
			with TStringList.Create do try
				LoadFromFile (SystemFolder + '\Info\' + InfoFile + '\' + Section + '.hss');
				Sec := '';
				for Ln := 0 to Count - 1 do begin
					Line := Strings [Ln];
					if Length (Line) > 0 then begin
						IniReadSec (Line, Sec);
						if Sec = 'main' then begin
							if IniReadString (Line, 'Title') then
								Title := Line
							else if IniReadString (Line, 'Subsections') then
								SubSections.CommaText := Line
							else if IniReadString (Line, 'See Also') then
								SeeAlso := Line;
						end;
					end;
				end;
			finally
				Free;
			end;
			if not UseQueue then begin
				if Section = InfoFile then
					ContentsStr := ''
				else begin
					ContentsStr := '<LI> <OBJECT TYPE="TEXT/SITEMAP">'#13#10
						+ '<PARAM NAME="Name" VALUE="' + HTMLize (Title) + '">'#13#10
						+ '<PARAM NAME="Local" VALUE="' + EncodeInfoLink (InfoFile + '/' + Section, True) + '">'#13#10
						+ '</OBJECT>'#13#10;
				end;
			end;
			with SubSections do
				for I := 0 to Count - 1 do
					if Pos ('/', Strings [I]) > 0 then begin
						if not FileExistsWithCase (SystemFolder + '\Info\' + StringReplace (Strings [I], '/', '\', [rfReplaceAll]) + '.hss') then
							raise EFileNotFound.CreateFmt ('Section %s referenced from %s/%s does not exist', [Strings [I], InfoFile, Section]);
					end else begin
						if not FileExistsWithCase (SystemFolder + '\Info\' + InfoFile + '\' + Strings [I] + '.hss') then
							raise EFileNotFound.CreateFmt ('Section %s referenced from %s/%s does not exist', [Strings [I], InfoFile, Section]);
					end;
			if Length (PreviousSection) > 0 then
				LeftNav := '<A HREF="' + EncodeInfoLink (InfoFile + '/' + PreviousSection, True, 'info/' + InfoFile, Section) + '">Previous</A>'
			else
				LeftNav := '';
			if Length (ParentSection) > 0 then
				CenterNav := '<A HREF="' + EncodeInfoLink (InfoFile + '/' + ParentSection, True, 'info/' + InfoFile, Section) + '">' + HTMLizeTitle (ParentTitle) + '</A>'
			else
				CenterNav := '';
			if Length (NextSection) > 0 then
				RightNav := '<A HREF="' + EncodeInfoLink (InfoFile + '/' + NextSection, True, 'info/' + InfoFile, Section) + '">Next</A>'
			else
				RightNav := '';
			Contents := GetGenericFileStart (HTMLizeTitle (Title), HTMLizeTitle (Title), '', 'info', LeftNav, CenterNav, RightNav);
			if Section <> InfoFile then begin
				if Indent = 2 then
					WebContents := WebContents + '<HR>'#13#10;
				S := IntToStr (Min (Indent, 5));
				WebContents := WebContents + '<H' + S + '><A NAME="' + Section + '"><U>' + HTMLizeTitle (Title) + '</U></A></H' + S + '>'#13#10;
			end;
			IniContents := LoadFile (SystemFolder + '\Info\' + InfoFile + '\' + Section + '.hss');
			S := IniContents;
			DeleteToAfterFirst ('[Description]', S);
			Body := Trim (DeleteToFirst (#13#10'[', S));
			if Length (Body) > 0 then begin
				PrepBody := Body;
				PrepareBody (PrepBody, True, 'info/' + InfoFile, Section, False, True);
				Contents := Contents + '<P CLASS="ITEMDESC">' + PrepBody + #13#10;
				PrepBody := Body;
				PrepareBody (PrepBody, False, 'info/' + InfoFile, Section, True, True);
				WebContents := WebContents + PrepBody + #13#10;
			end;
			S := IniContents;
			DeleteToAfterFirst ('[Top]', S);
			Body := Trim (DeleteToFirst (#13#10'[', S));
			if Length (Body) > 0 then begin
				PrepBody := Body;
				PrepareBody (PrepBody, True, 'info/' + InfoFile, Section, False);
				Contents := Contents + '<P>' + PrepBody + #13#10;
				PrepBody := Body;
				PrepareBody (PrepBody, False, 'info/' + InfoFile, Section, True);
				WebContents := WebContents + PrepBody + #13#10;
			end;
			with SubSections do
				if Count > 0 then begin
					Contents := Contents + '<UL>'#13#10;
					WebContents := WebContents + '<UL>'#13#10;
					for I := 0 to Count - 1 do begin
						with TStringList.Create do try
							if Pos ('/', Subsections.Strings [I]) > 0 then
								LoadFromFile (SystemFolder + '\Info\' + StringReplace (Subsections.Strings [I], '/', '\', [rfReplaceAll]) + '.hss')
							else
								LoadFromFile (SystemFolder + '\Info\' + InfoFile + '\' + Subsections.Strings [I] + '.hss');
							Sec := '';
							for Ln := 0 to Count - 1 do begin
								Line := Strings [Ln];
								if Length (Line) > 0 then begin
									IniReadSec (Line, Sec);
									if Sec = 'main' then begin
										if IniReadString (Line, 'Title') then begin
											S := Subsections.Strings [I];
											if Pos ('/', S) <= 0 then
												S := InfoFile + '/' + S;
											Contents := Contents + '<LI><B><A HREF="' + EncodeInfoLink (S, True, 'info/' + InfoFile, Section) + '">' + HTMLizeTitle (Line) + '</A></B>'#13#10;
											WebContents := WebContents + '<LI><B><A HREF="' + EncodeInfoLink (S, False, 'info/' + InfoFile, Section) + '">' + HTMLizeTitle (Line) + '</A></B>'#13#10;
											if Pos ('/', Subsections.Strings [I]) > 0 then
												S := LoadFile (SystemFolder + '\Info\' + StringReplace (Subsections.Strings [I], '/', '\', [rfReplaceAll]) + '.hss')
											else
												S := LoadFile (SystemFolder + '\Info\' + InfoFile + '\' + Subsections.Strings [I] + '.hss');
											DeleteToAfterFirst ('[Description]', S);
											Body := Trim (DeleteToFirst (#13#10'[', S));
											if Length (Body) > 0 then begin
												PrepBody := Body;
												PrepareBody (PrepBody, True, 'info/' + InfoFile, Section, True);
												Contents := Contents + PrepBody;
												PrepBody := Body;
												PrepareBody (PrepBody, False, 'info/' + InfoFile, Section, True);
												WebContents := WebContents + PrepBody;
											end;
										end;
									end;
								end;
							end;
						finally
							Free;
						end;
					end;
					Contents := Contents + '</UL>'#13#10;
					WebContents := WebContents + '</UL>'#13#10;
				end;
			S := IniContents;
			DeleteToAfterFirst ('[Bottom]', S);
			Body := Trim (DeleteToFirst (#13#10'[', S));
			if Length (Body) > 0 then begin
				PrepBody := Body;
				PrepareBody (PrepBody, True, 'info/' + InfoFile, Section, False);
				Contents := Contents + '<P>' + PrepBody + #13#10;
				PrepBody := Body;
				PrepareBody (PrepBody, False, 'info/' + InfoFile, Section, True);
				WebContents := WebContents + PrepBody + #13#10;
			end;
			if Length (SeeAlso) > 0 then begin
				Contents := Contents + '<P><HR>See also: ' + GetSeeAlsoText (SeeAlso, True, 'info/' + InfoFile, Section) + #13#10;
				WebContents := WebContents + '<P>See also: ' + GetSeeAlsoText (SeeAlso, False, 'info/' + InfoFile, Section) + '</P>'#13#10;
			end;
			Contents := Contents + GetGenericFileEnd;
			if not UseQueue then begin
				FileName := InfoFile;
				if Section <> InfoFile then
					FileName := FileName + '_' + Section;
				WriteFile (DestFolder + '\' + FileName + '.html', Contents);
			end;
			Contents := '';
			with SubSections do
				if Count > 0 then begin
					if UseQueue then begin
						Queue.Add ('Indent=' + IntToStr (Indent));
						for I := 0 to Count - 1 do
							if Pos ('/', Strings [I]) <= 0 then
								Queue.Add (Strings [I]);
					end else
						for I := 0 to Count - 1 do begin
							if Pos ('/', Strings [I]) <= 0 then begin
								if (I - 1 >= 0) and (Pos ('/', Strings [I - 1]) <= 0) then
									Previous := Strings [I - 1]
								else
									Previous := '';
								if (I + 1 < Count) and (Pos ('/', Strings [I + 1]) <= 0) then
									Next := Strings [I + 1]
								else
									Next := '';
								WriteSection (InfoFile, Strings [I], S, Section, Title, Previous, Next);
								Contents := Contents + S;
							end;
						end;
				end;
			if not UseQueue then begin
				if Length (Contents) > 0 then begin
					ContentsStr := ContentsStr + '<UL>'#13#10
						+ Contents
						+ '</UL>'#13#10;
				end;
			end;
		finally
			SubSections.Free;
		end;
	finally
		Dec (Indent);
	end;
end;
var
	Ln: Integer;
	Sec,
	Line,
	S1,
	S2,
	Contents,
	ContentsStr,
	Title,
	Style: string;
	TagCount: Integer;
begin
	Indent := 0;
	AvailableSections := TStringList.Create;
	try
		AvailableSections.Sorted := True;
		UseQueue := False;
		with TFileReferences.Create do try
			SearchForFiles (SystemFolder + '\Info\' + InfoFile + '\*.hss', atStd);
			CopyToStrings (AvailableSections);
		finally
			Free;
		end;
		Title := '';
		Style := 'Tree';
		with TStringList.Create do try
			LoadFromFile (SystemFolder + '\Info\' + InfoFile + '\' + InfoFile + '.hss');
			Sec := '';
			for Ln := 0 to Count - 1 do begin
				Line := Strings [Ln];
				if Length (Line) > 0 then begin
					IniReadSec (Line, Sec);
					if Sec = 'main' then begin
						if IniReadString (Line, 'Title') then
							Title := Line
						else if IniReadString (Line, 'Style') then
							Style := Line;
					end;
				end;
			end;
		finally
			Free;
		end;
		WebContents := GetWebFileStart (HTMLizeTitle (Title), HTMLizeTitle (Title));
		WriteSection (InfoFile, InfoFile, ContentsStr);
		if AvailableSections.Count > 0 then
			raise EFileNotFound.CreateFmt ('File %s contains the following unreferenced sections: %s', [InfoFile, AvailableSections.CommaText]);
		WebContents := WebContents + GetWebFileEnd;
		UseQueue := UpperCase (Style) = 'QUEUE';
		if not UseQueue then
			WriteFile (WebFolder + '\' + InfoFile + '.html', WebContents);
		Contents := LoadFile (DestFolder + '\Contents.hhc');
		S1 := DeleteToFirst ('"' + InfoFile + '.html"', Contents);
		if Length (Contents) > 0 then begin
			S1 := S1 + DeleteToAfterFirst ('</OBJECT>', Contents) + '</OBJECT>';
			try
				S1 := S1 + DeleteToAfterFirst ('<UL>', Contents, 20);
				TagCount := 1;
				repeat
					S2 := DeleteToAfterFirst ('</UL>', Contents);
					Dec (TagCount);
					while Length (S2) > 0 do begin
						DeleteToAfterFirst ('<UL>', S2);
						Inc (TagCount);
					end;
					Dec (TagCount);
				until TagCount <= 0;
			except
				S1 := S1 + #13#10;
			end;
			S1 := S1;
			DeleteWhiteSpace (Contents);
			Contents := S1 + ContentsStr + Contents;
			WriteFile (DestFolder + '\Contents.hhc', Contents);
		end;
		if UseQueue then begin
			WebContents := GetWebFileStart (HTMLizeTitle (Title), HTMLizeTitle (Title));
			Queue := TStringList.Create;
			WriteSection (InfoFile, InfoFile, ContentsStr);
			try
				while Queue.Count > 0 do begin
					if Copy (Queue [0], 1, Length ('Indent=')) = 'Indent=' then begin
						Indent := StrToInt (Copy (Queue [0], Length ('Indent=') + 1, Length (Queue [0])));
						if Indent >= 2 then
							WebContents := WebContents + '<HR>'#13#10;
					end else
						WriteSection (InfoFile, Queue [0], ContentsStr);
					Queue.Delete (0);
				end;
			finally
				Queue.Free;
			end;
			WebContents := WebContents + GetWebFileEnd;
			WriteFile (WebFolder + '\' + InfoFile + '.html', WebContents);
		end;
	finally
		AvailableSections.Free;
	end;
end;

var
	S: string;
	I: Integer;

begin
	try
		if (ParamCount < 1) or (UpperCase (ParamStr (1)) = '/ALL') then begin
			with TFileReferences.Create do try
				SearchForDirs (SystemFolder + '\Info', atStd);
				for I := 0 to Count - 1 do
					with Items [I] do
						WriteInfoFile (WithoutExt (FileName));
			finally
				Free;
			end;
		end else begin
			S := ParamStr (1);
			if Pos ('\', S) > 0 then begin
				S := ExtractFilePath (S);
				S := WithoutExt (ExtractFileName (Copy (S, 1, Length (S) - 1)));
			end;
			WriteInfoFile (S);
		end;
	except
		on E: Exception do
			if not (E is EAbort) then
				MessageBox (0, PChar (E.Message), 'Error', mb_OK or mb_IconError);
	end;
end.
