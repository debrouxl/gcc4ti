program UpdateIndex;

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
	Line: string;
	HeaderFiles,
	List,
	Letters: TStringList;
	HeaderFile,
	Section,
	Identifier,
	Title,
	MainType,
	SubType,
	LinkValue,
	NewIdentifier,
	WebContents,
	TypeStr,
	HeaderStr,
	Definition,
	RealDefinition,
	Alias,
	HdrFile,
	S,
	S1,
	S2: string;
	Letter: Char;
	I,
	J,
	K,
	P,
	Idx,
	FirstNonAlpha: Integer;
	Stream: TFileStream;

procedure WriteStream(const S: string);
begin
	Stream.Write (PChar(S)^, Length (S));
end;

procedure InsertInfoSection(const InfoFile: string; const Section: string = ''; const ParentTitle: string = '');
var
	Ln,
	I: Integer;
	Sec,
	Line,
	Title,
	S: string;
begin
	S := Section;
	if Length (S) <= 0 then
		S := InfoFile;
	Title := '';
	with TStringList.Create do try
		LoadFromFile (SystemFolder + '\Info\' + InfoFile + '\' + S + '.hss');
		Sec := '';
		for Ln := 0 to Count - 1 do begin
			Line := Strings [Ln];
			if Length (Line) > 0 then begin
				IniReadSec (Line, Sec);
				if Sec = 'main' then begin
					if IniReadString (Line, 'Title') then begin
						Title := Line;
						S := StringReplace (HTMLize (Title), '=', '%3D', [rfReplaceAll]) + '=info/' + InfoFile;
						if Length (Section) > 0 then
							S := S + '/' + Section;
						if Length (ParentTitle) > 0 then
							S := S + ', ' + StringReplace (HTMLize (ParentTitle), ',', ';', [rfReplaceAll]);
						List.Add (S);
					end else if IniReadString (Line, 'Index') then begin
						with TStringList.Create do try
							if Line = ',' then
								Text := Line
							else
								CommaText := Line;
							for I := 0 to Count - 1 do begin
								S := StringReplace (HTMLize (Strings [I]), '=', '%3D', [rfReplaceAll]) + '=info/' + InfoFile;
								if Length (Section) > 0 then
									S := S + '/' + Section;
								if Title <> Strings [I] then
									S := S + ', ' + StringReplace (HTMLize (Title), ',', ';', [rfReplaceAll])
								else if Length (ParentTitle) > 0 then
									S := S + ', ' + StringReplace (HTMLize (ParentTitle), ',', ';', [rfReplaceAll]);
								if List.IndexOf (S) < 0 then
									List.Add (S);
							end;
						finally
							Free;
						end;
					end else if IniReadString (Line, 'Subsections') then begin
						with TStringList.Create do try
							CommaText := Line;
							for I := 0 to Count - 1 do
								if Pos ('/', Strings [I]) <= 0 then
									InsertInfoSection (InfoFile, Strings [I], Title);
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

function GetCharCode(Ch: Char): string;
begin
	if Ch in ['a'..'z', 'A'..'Z'] then
		Result := Ch
	else if Ch = '$' then
		Result := 'dollar'
	else if Ch = '_' then
		Result := 'underscore'
	else if Ch = '.' then
		Result := 'dot'
	else
		Result := IntToHex (Ord (Ch), 2) + 'h';
end;

type
	TStringObject = class(TObject)
	public
		S: string;
	end;

begin
	try
		List := TStringList.Create;
		HeaderFiles := TStringList.Create;
		Letters := TStringList.Create;
		try
			List.Duplicates := dupAccept;
			List.Sorted := True;
			HeaderFiles.Sorted := True;
			with TFileReferences.Create do try
				SearchForDirs (SystemFolder + '\Include', atStd);
				CopyFullNamesToStrings (HeaderFiles);
			finally
				Free;
			end;
			List.Add ('Header File Index=info/hdrindex');
			for J := 0 to HeaderFiles.Count - 1 do begin
				HeaderFile := ExtractFileName (HeaderFiles.Strings [J]);
				List.Add (HeaderFile + '=' + HeaderFile + '/');
				with TFileReferences.Create do try
					SearchForFiles (SystemFolder + '\Include\' + HeaderFile + '\*.hsf', atStd);
					for I := 0 to Count - 1 do
						with Items [I] do begin
							Section := WithoutExt (FileName);
							Identifier := '';
							MainType := '';
							SubType := '';
							HeaderStr := '';
							Definition := '';
							RealDefinition := '';
							Alias := '';
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
											else if IniReadString (Line, 'Type') then
												MainType := Line
											else if IniReadString (Line, 'Subtype') then
												SubType := Line
											else if IniReadString (Line, 'Header Files') then
												HeaderStr := Line
											else if IniReadString (Line, 'Definition') then
												Definition := Line
											else if IniReadString (Line, 'Real Definition') then
												RealDefinition := Line
											else if IniReadString (Line, 'Alias') then
												Alias := Line;
										end;
									end;
								end;
							finally
								Free;
							end;
							if Length (Identifier) > 0 then
								List.Add (Identifier + '=' + HeaderFile + '/' + Section + ', , ' + MainType + ', ' + Subtype + ', : ' + HeaderStr);
							if Length (Alias) > 0 then
								List.Add (Alias + '=' + HeaderFile + '/' + Section + ', , ' + MainType + ', ' + Subtype + ', : ' + HeaderStr);
							if UpperCase (SubType) = 'ENUMERATION' then begin
								if Length (RealDefinition) > 0 then
									S := RealDefinition
								else
									S := Definition;
								if Length (S) > 0 then begin
									with TStringList.Create do try
										DeleteToAfterFirst ('{', S);
										CommaText := StringReplace (DeleteToFirst ('}', S), ' ', '', [rfReplaceAll]);
										for K := 0 to Count - 1 do begin
											if Pos ('=', Strings [K]) > 0 then
												Strings [K] := Names [K];
											List.Add (Strings [K] + '=' + HeaderFile + '/' + Section + ', ' + Identifier + ', Constant, , : ' + HeaderStr);
										end;
									finally
										Free;
									end;
								end;
							end;
						end;
				finally
					Free;
				end;
			end;
			List.Add ('C Language Keywords=info/keywords');
			with TFileReferences.Create do try
				SearchForFiles (SystemFolder + '\Keywords\*.hsk', atStd);
				for I := 0 to Count - 1 do
					with Items [I] do begin
						Section := WithoutExt (FileName);
						List.Add (Section + '=info/keywords/' + Section);
					end;
				SearchForFiles (SystemFolder + '\Keywords\*.ref', atStd);
				for I := 0 to Count - 1 do
					with Items [I] do begin
						Section := WithoutExt (FileName);
						List.Add (Section + '=info/keywords/' + Section);
					end;
			finally
				Free;
			end;
			with TFileReferences.Create do try
				SearchForDirs (SystemFolder + '\Info', atStd);
				for I := 0 to Count - 1 do
					with Items [I] do
						InsertInfoSection (WithoutExt (FileName));
			finally
				Free;
			end;
			Stream := TFileStream.Create (DestFolder + '\Index.hhk', fmCreate or fmShareExclusive);
			try
				WriteStream ('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">'#13#10'<HTML>'#13#10'<HEAD>'#13#10'<!-- Sitemap 1.0 -->'#13#10'</HEAD>'#13#10
					+ '<BODY>'#13#10'<UL>'#13#10);
				I := 0;
				with List do
					while I < Count do begin
						S := Strings [I];
						Identifier := StringReplace (DeleteToAfterFirst ('=', S), '%3D', '=', [rfReplaceAll]);
						if Length (Identifier) > 0 then begin
							LinkValue := DeleteToAfterFirst (', ', S);
							Title := DeleteToAfterFirst (', ', S);
							MainType := DeleteToAfterFirst (', ', S);
							SubType := DeleteToAfterFirst (', ', S);
							if Length (S) > 0 then
								DeleteToAfterFirst (': ', S, 0);
							HeaderStr := '';
							while Length (S) > 0 do begin
								HdrFile := DeleteToAfterFirst (', ', S);
								HeaderStr := HeaderStr + '<A HREF="' + EncodeLink (HdrFile + '/', False) + '">' + HdrFile + '</A>';
								if Length (S) > 0 then
									HeaderStr := HeaderStr + ', ';
							end;
							if Length (Title) <= 0 then
								Title := Identifier;
							if (Copy (LinkValue, 1, Length ('info/')) <> 'info/') and (Length (LinkValue) > 0) and (LinkValue [Length (LinkValue)] <> '/') then begin
								TypeStr := MainType;
								if Length (Subtype) > 0 then
									TypeStr := TypeStr + ' (' + Subtype + ')';
								Letter := UpCase (Identifier [1]);
								Idx := Letters.IndexOf (Letter);
								if Idx < 0 then begin
									if Letter in ['A'..'Z'] then begin
										FirstNonAlpha := Letters.Count;
										for Idx := 0 to Letters.Count - 1 do
											if not (Letters.Strings [Idx] [1] in ['A'..'Z']) then begin
												FirstNonAlpha := Idx;
												Break;
											end;
										Letters.InsertObject (FirstNonAlpha, Letter, TStringObject.Create);
										Idx := FirstNonAlpha;
									end else
										Idx := Letters.AddObject (Letter, TStringObject.Create);
								end;
								with TStringObject (Letters.Objects [Idx]) do
									S := S + '<TR><TD><A HREF="' + EncodeLink (LinkValue, False, '', '', Identifier) + '">' + HTMLize (Identifier) + '</A></TD><TD>' + TypeStr + '</TD><TD>' + HeaderStr + '</TD></TR>'#13#10;
							end;
							WriteStream ('<LI> <OBJECT TYPE="TEXT/SITEMAP">'#13#10
								+ '<PARAM NAME="Name" VALUE="' + Identifier + '">'#13#10
								+ '<PARAM NAME="Name" VALUE="' + Title + '">'#13#10
								+ '<PARAM NAME="Local" VALUE="' + EncodeLink (LinkValue, True, '', '', Identifier) + '">'#13#10
								+ '</OBJECT>'#13#10);
							Inc (I);
							if I < Count then begin
								S := Strings [I];
								NewIdentifier := StringReplace (DeleteToAfterFirst ('=', S), '%3D', '=', [rfReplaceAll]);
								if NewIdentifier = Identifier then begin
									WriteStream ('<UL>'#13#10);
									if Title <> Identifier then begin
										WriteStream ('<LI> <OBJECT TYPE="TEXT/SITEMAP">'#13#10
											+ '<PARAM NAME="Name" VALUE="' + Title + '">'#13#10
											+ '<PARAM NAME="Name" VALUE="' + Title + '">'#13#10
											+ '<PARAM NAME="Local" VALUE="' + EncodeLink (LinkValue, True, '', '', Identifier) + '">'#13#10
											+ '</OBJECT>'#13#10);
									end;
									repeat
										if I < Count then begin
											S := Strings [I];
											NewIdentifier := StringReplace (DeleteToAfterFirst ('=', S), '%3D', '=', [rfReplaceAll]);
										end else
											NewIdentifier := '';
										if NewIdentifier = Identifier then begin
											LinkValue := DeleteToAfterFirst (', ', S);
											if Length (S) > 0 then
												Title := S
											else
												Title := Identifier;
											WriteStream ('<LI> <OBJECT TYPE="TEXT/SITEMAP">'#13#10
												+ '<PARAM NAME="Name" VALUE="' + Title + '">'#13#10
												+ '<PARAM NAME="Name" VALUE="' + Title + '">'#13#10
												+ '<PARAM NAME="Local" VALUE="' + EncodeLink (LinkValue, True, '', '', Identifier) + '">'#13#10
												+ '</OBJECT>'#13#10);
											Inc (I);
										end;
									until NewIdentifier <> Identifier;
									WriteStream ('</UL>'#13#10);
								end;
							end;
						end else
							Inc (I);
					end;
				WriteStream ('</UL>'#13#10'</BODY>'#13#10'</HTML>'#13#10);
			finally
				Stream.Free;
			end;
			WebContents := GetWebFileStart ('Alphabetical Index', 'Alphabetical Index', True)
				+ '<P><B>';
			with Letters do
				for I := 0 to Count - 1 do begin
					WebContents := WebContents + '<A HREF="#' + GetCharCode (Strings [I] [1]) + '">' + Strings [I] + '</A>';
					if I < Count - 1 then
						WebContents := WebContents + ' ';
				end;
			WebContents := WebContents + '</B></P>'#13#10;
			with Letters do
				for I := 0 to Count - 1 do begin
					WebContents := WebContents + '<HR>'#13#10
						+ '<H2><A NAME="' + GetCharCode (Strings [I] [1]) + '"><U>' + Strings [I] + '</U></A></H2>'#13#10
						+ '<TABLE WIDTH="100%">'#13#10
						+ '<TR><TD WIDTH="200"><STRONG><U>Identifier</U></STRONG></TD><TD WIDTH="130"><STRONG><U>Type</U></STRONG></TD><TD><STRONG><U>Defined in</U></STRONG></TD></TR>'#13#10
						+ TStringObject(Letters.Objects[I]).S
						+ '</TABLE>'#13#10;
				end;
			WebContents := WebContents + GetWebFileEnd;
			WriteFile (WebFolder + '\catalog.html', WebContents);
		finally
			with Letters do
				for I := Count - 1 downto 0 do begin
					Objects[I].Free;
					Objects [I] := nil;
				end;
			Letters.Free;
			HeaderFiles.Free;
			List.Free;
		end;
	except
		on E: Exception do
			if not (E is EAbort) then
				MessageBox (0, PChar (E.Message), 'Error', mb_OK or mb_IconError);
	end;
end.
