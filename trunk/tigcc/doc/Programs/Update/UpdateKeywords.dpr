program UpdateKeywords;

uses
	Windows,
	SysUtils,
  Classes,
	UtilsDos,
  UtilsWin,
  HelpSystemMasterUnit in '..\HelpSystemMasterUnit.pas';

var
	Ln: Integer;
	Sec,
	Line,
	S,
	S1,
	S2,
	Title,
	Contents,
	WebContents,
	IniContents,
	Body,
	PrepBody,
	Keyword,
	AllKeywords,
	SeeAlso: string;
	Keywords: TStringList;
	I,
	J: Integer;

begin
	try
		Keywords := TStringList.Create;
		Keywords.Sorted := True;
		Keywords.Duplicates := dupError;
		try
			with TFileReferences.Create do try
				SearchForFiles (SystemFolder + '\Keywords\*.hsk', atStd);
				for I := 0 to Count - 1 do
					with Items [I] do begin
						with TStringList.Create do try
							LoadFromFile (FullName);
							Sec := '';
							for Ln := 0 to Count - 1 do begin
								Line := Strings [Ln];
								if Length (Line) > 0 then begin
									IniReadSec (Line, Sec);
									if Sec = 'main' then begin
										if IniReadString (Line, 'Keywords') then begin
											with TStringList.Create do try
												CommaText := Line;
												for J := 0 to Count - 1 do begin
													Keywords.Add (Strings [J]);
													if (Strings [J] <> WithoutExt (FileName)) and ((not FileExists (SystemFolder + '\Keywords\' + Strings [J] + '.ref')) or (LoadFile (SystemFolder + '\Keywords\' + Strings [J] + '.ref') <> WithoutExt (FileName))) then
														WriteFile (SystemFolder + '\Keywords\' + Strings [J] + '.ref', WithoutExt (FileName));
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
					end;
			finally
				Free;
			end;
			Title := '';
			with TStringList.Create do try
				LoadFromFile (SystemFolder + '\Keywords\Keywords.hss');
				Sec := '';
				for Ln := 0 to Count - 1 do begin
					Line := Strings [Ln];
					if Length (Line) > 0 then begin
						IniReadSec (Line, Sec);
						if Sec = 'main' then begin
							if IniReadString (Line, 'Title') then
								Title := Line;
						end;
					end;
				end;
				IniContents := Text;
			finally
				Free;
			end;
			Contents := GetGenericFileStart (HTMLize (Title), HTMLize (Title), '', 'info');
			WebContents := GetWebFileStart (HTMLize (Title), HTMLize (Title));
			S := IniContents;
			DeleteToAfterFirst ('[Top]', S);
			Body := Trim (DeleteToFirst (#13#10'[', S));
			if Length (Body) > 0 then begin
				PrepBody := Body;
				PrepareBody (PrepBody, True, 'info/keywords');
				Contents := Contents + '<P>' + PrepBody + #13#10;
				PrepBody := Body;
				PrepareBody (PrepBody, False, 'info/keywords', '', True);
				WebContents := WebContents + PrepBody + #13#10;
			end;
			Contents := Contents + '<P><B>'#13#10;
			WebContents := WebContents + '<P><B>'#13#10;
			with Keywords do
				for I := 0 to Count - 1 do begin
					Keyword := Strings [I];
					Contents := Contents + '<A HREF="' + EncodeLink (Keyword, True, 'info/keywords') + '">' + Keyword + '</A>';
					WebContents := WebContents + '<A HREF="' + EncodeLink (Keyword, False, 'info/keywords') + '">' + Keyword + '</A>';
					if I < Count - 1 then begin
						Contents := Contents + '<BR>'#13#10;
						WebContents := WebContents + '<BR>'#13#10;
					end;
				end;
			Contents := Contents + #13#10'</B>'#13#10;
			WebContents := WebContents + #13#10'</B></P>'#13#10;
			S := IniContents;
			DeleteToAfterFirst ('[Bottom]', S);
			Body := Trim (DeleteToFirst (#13#10'[', S));
			if Length (Body) > 0 then begin
				PrepBody := Body;
				PrepareBody (PrepBody, True, 'info/keywords');
				Contents := Contents + '<P>' + PrepBody + #13#10;
				PrepBody := Body;
				PrepareBody (PrepBody, False, 'info/keywords', '', True);
				WebContents := WebContents + PrepBody + #13#10;
			end;
			Contents := Contents + GetGenericFileEnd;
			WriteFile (DestFolder + '\keywords.html', Contents);
			with Keywords do
				for I := 0 to Count - 1 do begin
					Keyword := Strings [I];
					if FileExists (SystemFolder + '\Keywords\' + Keyword + '.hsk') then begin
						AllKeywords := '';
						SeeAlso := '';
						with TStringList.Create do try
							LoadFromFile (SystemFolder + '\Keywords\' + Keyword + '.hsk');
							Sec := '';
							for Ln := 0 to Count - 1 do begin
								Line := Strings [Ln];
								if Length (Line) > 0 then begin
									IniReadSec (Line, Sec);
									if Sec = 'main' then begin
										if IniReadString (Line, 'Keywords') then
											AllKeywords := Line
										else if IniReadString (Line, 'See Also') then
											SeeAlso := Line;
									end;
								end;
							end;
							IniContents := Text;
						finally
							Free;
						end;
						S := 'Keyword';
						if AllKeywords <> Keyword then
							S := S + 's';
						Contents := GetGenericFileStart (AllKeywords, AllKeywords, S, 'info', '', '', '<A HREF="' + EncodeInfoLink ('keywords', True, 'info/keywords', Keyword) + '">Keyword Index</A>');
						WebContents := WebContents + '<HR>'#13#10
							+ '<H2><A NAME="' + Keyword + '"><U>' + AllKeywords + '</U></A></H2>'#13#10;
						S := IniContents;
						DeleteToAfterFirst ('[Description]', S);
						Body := Trim (DeleteToFirst (#13#10'[', S));
						if Length (Body) > 0 then begin
							PrepBody := Body;
							PrepareBody (PrepBody, True, 'info/keywords', Keyword);
							Contents := Contents + '<P CLASS="ITEMDESC">' + PrepBody + #13#10;
							PrepBody := Body;
							PrepareBody (PrepBody, False, 'info/keywords', Keyword, True, True);
							WebContents := WebContents + PrepBody + #13#10;
						end;
						S := IniContents;
						DeleteToAfterFirst ('[Explanation]', S);
						Body := Trim (DeleteToFirst (#13#10'[', S));
						if Length (Body) > 0 then begin
							PrepBody := Body;
							PrepareBody (PrepBody, True, 'info/keywords', Keyword, False);
							Contents := Contents + '<P>' + PrepBody + #13#10;
							PrepBody := Body;
							PrepareBody (PrepBody, False, 'info/keywords', Keyword, True);
							WebContents := WebContents + PrepBody + #13#10;
						end;
						if Length (SeeAlso) > 0 then begin
							Contents := Contents + '<P><HR>See also: ' + GetSeeAlsoText (SeeAlso, True, 'info/keywords', Keyword) + #13#10;
							WebContents := WebContents + '<P>See also: ' + GetSeeAlsoText (SeeAlso, False, 'info/keywords', Keyword) + '</P>'#13#10;
						end;
						Contents := Contents + GetGenericFileEnd;
						WriteFile (DestFolder + '\keywords_' + Keyword + '.html', Contents);
					end;
				end;
			WebContents := WebContents + GetWebFileEnd;
			WriteFile (WebFolder + '\keywords.html', WebContents);
			Contents := LoadFile (DestFolder + '\Contents.hhc');
			S1 := DeleteToFirst ('"keywords.html"', Contents);
			S2 := '';
			S1 := S1 + DeleteToAfterFirst ('</OBJECT>', Contents) + '</OBJECT>';
			try
				S1 := S1 + DeleteToAfterFirst ('<UL>', Contents, 20);
				DeleteToAfterFirst ('</UL>', Contents);
			except
				S1 := S1 + #13#10;
			end;
			S1 := S1 + '<UL>'#13#10;
			S2 := '</UL>' + Contents;
			Contents := '';
			with Keywords do
				for I := 0 to Count - 1 do begin
					S := Strings [I];
					Contents := Contents + '<LI> <OBJECT TYPE="TEXT/SITEMAP">'#13#10
						+ '<PARAM NAME="Name" VALUE="' + S + '">'#13#10
						+ '<PARAM NAME="Local" VALUE="' + EncodeInfoLink ('keywords/' + S, True) + '">'#13#10
						+ '</OBJECT>'#13#10;
				end;
			Contents := S1 + Contents + S2;
			WriteFile (DestFolder + '\Contents.hhc', Contents);
		finally
			Keywords.Free;
		end;
	except
		on E: Exception do
			if not (E is EAbort) then
				MessageBox (0, PChar (E.Message), 'Error', mb_OK or mb_IconError);
	end;
end.
