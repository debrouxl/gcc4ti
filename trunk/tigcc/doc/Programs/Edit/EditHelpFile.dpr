program EditHelpFile;

uses
	Forms,
	MainUnit in 'MainUnit.pas' {MainForm},
	Consts in '..\..\consts.pas';

{$R *.RES}

begin
	Application.Initialize;
	Application.Title := 'Help System Editor';
	Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
