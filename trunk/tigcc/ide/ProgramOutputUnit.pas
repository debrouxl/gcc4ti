unit ProgramOutputUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, MemoComponentUnit;

type
  TProgramOutputForm = class(TForm)
    OutputMemo: TMemoComponent;
    ErrorMemo: TMemoComponent;
		Splitter: TSplitter;
		Panel1: TPanel;
    Panel2: TPanel;
    procedure FormShow(Sender: TObject);
  private
	public
	end;

implementation

{$R *.DFM}

procedure TProgramOutputForm.FormShow(Sender: TObject);
begin
	OutputMemo.Selection.RStart := OutputMemo.TextLength + 1;
	OutputMemo.Selection.ScrollInView (0);
	ErrorMemo.Selection.RStart := ErrorMemo.TextLength + 1;
	ErrorMemo.Selection.ScrollInView (0);
end;

end.
