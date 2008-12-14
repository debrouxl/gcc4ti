{
  TIGCC IDE

  Copyright (C) 2004 Fréderic Bour
  Copyright (C) 2004 Sebastian Reichelt

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

unit HtFormatting;

interface

uses Windows, Types, Classes, Graphics, Forms, Controls;

// Show an hint window with formatted text

type
  THtHintWindow = class(THintWindow)
  protected
    procedure Paint; override;
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
  end;

  // Draw text with a small HTML-like formatting
  // Tags: B (bold), I (italic), U (underline) or S (strike-out)
  //
  // You can also use <C:Color> for changing text color

procedure DrawHtTextEx(const Text: string; out PlainText: string; Rect: TRect; Cnv: TCanvas; var Width, Height: Integer; NoColor: Boolean);
procedure DrawHtText(const Text: string; Rect: TRect; Cnv: TCanvas; var Width: Integer; NoColor: Boolean);

procedure DrawMultiLineHtTextEx(const Text: string; out PlainText: string; Rect: TRect; Cnv: TCanvas; var Width, Height: Integer; NoColor: Boolean);
procedure DrawMultiLineHtText(const Text: string; Rect: TRect; Cnv: TCanvas; var Width: Integer; NoColor: Boolean);

implementation

uses StrUtils, MaskUtils, UtilsDos;

procedure DrawHtTextEx(const Text: string; out PlainText: string; Rect: TRect; Cnv: TCanvas; var Width, Height: Integer; NoColor: Boolean);
var
  i, l, p: Integer;
  S: string;
  X, BWidth: Integer;
  C: Char;
  OriginalColor: TColor;
  OriginalStyle: TFontStyles;
  Size: tagSIZE;
begin
  OriginalColor := Cnv.Font.Color;
  OriginalStyle := Cnv.Font.Style;
  l := Length(Text);
  p := 0;
  X := Rect.Left;
  Height := 0;
  PlainText := '';
  BWidth := Cnv.TextWidth('<');
  while p < l do
  begin
    i := p + 1;
    p := PosEx('<', Text, i);
    if p = 0 then
      p := l + 1;

    // Draw Text
    S := Copy(Text, i, p - i);
    Cnv.TextRect(Rect, Rect.Left, Rect.Top, S);
    PlainText := PlainText + S;

    // Update Size
    Size := Cnv.TextExtent(S);
    Inc(Rect.Left, Size.cx);
    if Size.cy > Height then
      Height := Size.cy;

    if p <> l then
    begin
      C := UpCase(Text[p + 1]);
      if C <> 'C' then
      begin
        case C of
          'B': Cnv.Font.Style := Cnv.Font.Style + [fsBold];
          'U': Cnv.Font.Style := Cnv.Font.Style + [fsItalic];
          'I': Cnv.Font.Style := Cnv.Font.Style + [fsUnderline];
          'S': Cnv.Font.Style := Cnv.Font.Style + [fsStrikeOut];
          '/':
            if p + 1 < l then
            begin
              case UpCase(Text[p + 2]) of
                'B': Cnv.Font.Style := Cnv.Font.Style - [fsBold];
                'U': Cnv.Font.Style := Cnv.Font.Style - [fsItalic];
                'I': Cnv.Font.Style := Cnv.Font.Style - [fsUnderline];
                'S': Cnv.Font.Style := Cnv.Font.Style - [fsStrikeOut];
              end;
            end;
          '<':
            begin
              Cnv.TextRect(Rect, Rect.Left, Rect.Top, '<');
              Inc(Rect.Left, BWidth);
            end;
        end;
        p := PosEx('>', Text, p + 1);
        if p = 0 then
          p := l;
      end
      else
      begin
        Inc(P, 3);
        i := PosEx('>', Text, p);
        if i = 0 then
          p := l
        else
        begin
          S := Copy(Text, p, i - p);
          if S <> '' then
          begin
            if not (S[1] in ['0'..'9', '$']) then
              S := 'cl' + S;
            try
              if not NoColor then
                Cnv.Font.Color := StringToColor(S);
            except
            end;
          end;
          p := i;
        end;
      end;
    end
    else
      Break;
  end;
  Width := Rect.Left - X;
  Cnv.Font.Color := OriginalColor;
  Cnv.Font.Style := OriginalStyle;
end;

procedure DrawMultiLineHtTextEx(const Text: string; out PlainText: string; Rect: TRect; Cnv: TCanvas; var Width, Height: Integer; NoColor: Boolean);
var
  Lst: TStringList;
  S: string;
  i, W, H: Integer;
begin
  Lst := TStringList.Create;
  Lst.Text := Text;
  Height := 0;
  Width := 0;
  for i := 0 to Lst.Count - 1 do
  begin
    DrawHtTextEx(Lst[i], S, Rect, Cnv, W, H, NoColor);
    if W > Width then
      Width := W;
    Inc(Height, H);
    Inc(Rect.Top, H);
    Lst[i] := S;
  end;
  PlainText := Lst.Text;
  Lst.Free;
end;

procedure DrawHtText(const Text: string; Rect: TRect; Cnv: TCanvas; var Width: Integer; NoColor: Boolean);
var
  S: string;
  H: Integer;
begin
  DrawHtTextEx(Text, S, Rect, Cnv, Width, H, NoColor);
end;

procedure DrawMultiLineHtText(const Text: string; Rect: TRect; Cnv: TCanvas; var Width: Integer; NoColor: Boolean);
var
  S: string;
  H: Integer;
begin
  DrawMultiLineHtTextEx(Text, S, Rect, Cnv, Width, H, NoColor);
end;

{ THtHintWindow }

function THtHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
  AData: Pointer): TRect;
var
  P: string;
  R: TRect;
  W, H: Integer;
begin
  R := Rect(2, 2, 2, 2);
  Canvas.Font.Color := Screen.HintFont.Color;
  DrawMultiLineHtTextEx(AHint, P, R, Self.Canvas, W, H, False);
  Result := Rect(0, 0, W + 2, H + 2);
end;

procedure THtHintWindow.Paint;
var
  R: TRect;
  W: Integer;
begin
  R := ClientRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Canvas.Font.Color := Screen.HintFont.Color;
  DrawMultiLineHtText(Caption, R, Self.Canvas, W, False);
end;

end.
