unit uHelp;

interface

procedure ShowHelp(const Topic : string);

procedure GoUrl(const Url : string);

implementation

{$ifndef ZgeLazarus}
uses ShellApi,Forms,Windows;
{$endif}

const
  HelpRoot = 'http://www.zgameeditor.org/';

procedure GoUrl(const Url : string);
begin
  {$ifndef ZgeLazarus}
  ShellExecuteW(Application.MainForm.Handle,'open',PChar( Url ),nil,nil,SW_SHOWDEFAULT);
  {$endif}
end;

procedure ShowHelp(const Topic : string);
var
  HelpPage : string;
begin
  if Topic='' then
    HelpPage := HelpRoot
  else
    HelpPage := HelpRoot + 'index.php/' + Topic;
  GoUrl(HelpPage);
end;

end.
