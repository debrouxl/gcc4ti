unit TmpltForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MemoComponentUnit;

type
	TTemplate = class(TObject)
	public
		Text: string;
		procedure Apply(Dest: TMemoComponent);
	end;

	TTemplateForm = class(TForm)
		TmpltList: TListBox;
		procedure FormDeactivate(Sender: TObject);
		procedure TmpltListData(Control: TWinControl; Index: Integer;
			var Data: string);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure TmpltListDblClick(Sender: TObject);
		procedure TmpltListKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure FormActivate(Sender: TObject);
	public
		{ Déclarations publiques }
		Templates: TStringList;
		procedure Select;
	end;

var
	TemplateForm: TTemplateForm;

implementation

{$R *.dfm}

uses MasterUnit, UtilsDos, CodeCompletion, CompletionForm;

{ TTemplate }

procedure TTemplate.Apply(Dest: TMemoComponent);
var
  St: Integer;
  i, p: Integer;
  S, Before: string;
  Lst: TStringList;
  T: TTextCell;
begin
  // Indent template
  T := Dest.Selection.StartRowCol;
  Before := Copy(Dest.Lines[T.Row - 1], 1, T.Col - 1);
  Lst := TStringList.Create;
  Lst.Text := Text;
  for i := 1 to Lst.Count - 1 do
    Lst[i] := Before + Lst[i];
	S := Lst.Text;
	if (Copy(S, Length(S) - 1, 2) = #13#10) and (Copy(Text, Length(Text) - 1, 2) <> #13#10) then
		Delete(S, Length(S) - 1, 2);
	Lst.Free;

  p := Pos('|', S);

  st := Dest.Selection.RStart;
  if p = 0 then
    Dest.Selection.Text := S
  else
  begin
    Delete(S, p, 1);
    Dest.Selection.Text := S;
    Inc(St, p - 1);
  end;
  Dest.Selection.RStart := st;
  Dest.Selection.RLength := 0;
end;

{ TTemplateForm }

procedure TTemplateForm.FormDeactivate(Sender: TObject);
begin
  Hide;
end;

procedure TTemplateForm.TmpltListData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  if (Index >= 0) and (Index < Templates.Count) then
    Data := Templates[Index];
end;

procedure TTemplateForm.FormCreate(Sender: TObject);
var
  T: TTemplate;
  F: TStream;
  S: string;
begin
  Templates := TStringList.Create;
  Templates.Sorted := True;
	S := WithBackslash(TIGCCFolder) + TemplatesLocation + 'templates.dat';
  if FileExists(S) then
  try
    F := TFileStream.Create(S, fmOpenRead);
    try
      while ReadString(F, S) do
      begin
        T := TTemplate.Create;
        ReadString(F, T.Text);
        Templates.AddObject(S, T);
      end;
    finally
      F.Free;
    end;
  except
  end;
end;

procedure TTemplateForm.FormDestroy(Sender: TObject);
var
  i: Integer;
  F: TStream;
  T: TTemplate;
begin
  try
    F := TFileStream.Create(WithBackslash(TIGCCFolder) + TemplatesLocation + 'templates.dat', fmCreate);
    try
      for i := 0 to Templates.Count - 1 do
      begin
        T := TTemplate(Templates.Objects[i]);
        if Assigned(T) then
        begin
          WriteString(F, Templates[i]);
          WriteString(F, T.Text);
        end;
      end;
    finally
      F.Free;
    end;
  except
  end;
  for i := 0 to Templates.Count - 1 do
    Templates.Objects[i].Free;
  Templates.Free;
end;

procedure TTemplateForm.TmpltListDblClick(Sender: TObject);
begin
  Select;
end;

procedure TTemplateForm.TmpltListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      Select;
    VK_ESCAPE, VK_CLEAR, VK_BACK, VK_DELETE:
      Hide;
  end;

end;

procedure TTemplateForm.Select;
var
  Idx: Integer;
  M: TMemoComponent;
  T: TTemplate;
begin
  M := CompForm.Editor;
  if Assigned(M) then
  begin
    Idx := TmpltList.ItemIndex;
    if (Idx >= 0) and (Idx < Templates.Count) then
    begin
      T := TTemplate(Templates.Objects[Idx]);
      if Assigned(T) then
        T.Apply(M);
    end;
  end;
  Hide;
end;

procedure TTemplateForm.FormActivate(Sender: TObject);
var
  M: TMemoComponent;
  P: TPoint;
begin
	TmpltList.Count := Templates.Count;
	M := CompForm.Editor;
	if Assigned(M) then
	begin
		TmpltList.ItemIndex := 0;
		P := M.ClientToScreen(M.Selection.StartPoint);
		Left := P.X + 4;
		Top := P.Y + 20;
	end;
end;

end.
