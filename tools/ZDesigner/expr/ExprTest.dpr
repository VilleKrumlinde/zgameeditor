program ExprTest;

uses
  Forms,
  CocoBase in 'CocoBase.pas',
  fTestZc in 'fTestZc.pas' {fmTestZc},
  mwStringHashList in 'mwStringHashList.pas',
  Zc in 'Zc.PAS',
  uSymTab in '..\uSymTab.pas',
  ZClasses in '..\..\..\ZClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmTestZc, fmTestZc);
  Application.Run;
end.
