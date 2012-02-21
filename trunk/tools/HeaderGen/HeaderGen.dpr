program HeaderGen;

uses
  Vcl.Forms,
  frmMain in 'frmMain.pas' {MainForm},
  LibXmlParser in '..\..\3rdparty\LibXmlParser.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
