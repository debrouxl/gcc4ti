unit uBatch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl, uEditor, uHSFParser, MasterUnit;

const
  BrowseCaption = 'Select Directory';

type
  TBatchEdit = class(TForm)
    WorkDirLbl: TLabel;
    WorkDirEdit: TEdit;
    WorkDirBtn: TButton;
    HsfDirLbl: TLabel;
    HsfDirEdit: TEdit;
    HsfDirBtn: TButton;
    GoBtn: TButton;
    LogMemo: TMemo;
    EnumCheck: TCheckBox;
    procedure GoBtnClick(Sender: TObject);
    procedure WorkDirBtnClick(Sender: TObject);
    procedure HsfDirBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    Editor: TCEditorForm;
  end;

var
  BatchEdit: TBatchEdit;

implementation

{$R *.dfm}

procedure TBatchEdit.GoBtnClick(Sender: TObject);

  procedure Log(const S: string);
  begin
    LogMemo.Lines.Add(S);
  end;

const
  Valids = faAnyFile and not (faDirectory or faSysFile or faHidden);
var
  Sr: TSearchRec;
  Path, HSFs: string;
  F: TFileStream;
begin
  DoEnum := EnumCheck.Checked;
  LogMemo.Clear;
  if Editor = nil then
    Exit;

  Path := IncludeTrailingPathDelimiter(WorkDirEdit.Text);
  HSFs := IncludeTrailingPathDelimiter(HsfDirEdit.Text);
  Log('Start processing...');
  if FindFirst(Path + '*.h', Valids, Sr) = 0 then
  begin
    repeat
      Log('Found: ' + Sr.Name);
      Editor.Clear;
      if Editor.ParseH(Path + Sr.Name) then
      begin
        Log('Processing ' + Sr.Name + ' ...');
        if DirectoryExists(HSFs + Sr.Name) then
        begin
          ClearCompletion(Editor.Lst);
          ImportDir(HSFs + Sr.Name, Editor, Editor.GetLine);
          Log('Found HSF Files ...');
        end;
        try
          F := TFileStream.Create(Path + ChangeFileExt(Sr.Name, '.ccf'), fmCreate);
          try
            Editor.SaveToStream(F);
          finally
            F.Free;
          end;
        except
          Log('Error while saving ccf for ' + Sr.Name);
        end;
      end;
      Editor.Modified := False;
      Log('');
    until FindNext(Sr) <> 0;
    FindClose(Sr);
  end;
  Editor.Modified := False;
  Log('Finished!');
  DoEnum := True;
end;

procedure TBatchEdit.WorkDirBtnClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := WorkDirEdit.Text;
  if SelectDirectory(BrowseCaption, '', Dir) then
    WorkDirEdit.Text := Dir;
end;

procedure TBatchEdit.HsfDirBtnClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := HsfDirEdit.Text;
  if SelectDirectory(BrowseCaption, '', Dir) then
    HsfDirEdit.Text := Dir;
end;

procedure TBatchEdit.FormCreate(Sender: TObject);
var
  Folder: string;
begin
  Folder := IncludeTrailingPathDelimiter(TIGCCFolder);
  WorkDirEdit.Text := Folder + CIncludeLocation;
  Folder := Folder + 'Src\doc\System\Include';
  if DirectoryExists(Folder) then
    HsfDirEdit.Text := Folder
  else
    HsfDirEdit.Text := TIGCCFolder;
end;

end.
