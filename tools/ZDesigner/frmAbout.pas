{Copyright (c) 2008 Ville Krumlinde

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

unit frmAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TAboutForm = class(TForm)
    SplashPanel: TPanel;
    Button1: TButton;
    Label1: TLabel;
    NameLabel: TLabel;
    VersionLabel: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses frmEditor, uHelp;

{$R *.dfm}

procedure TAboutForm.FormCreate(Sender: TObject);
var
  Prog,ProgParam : string;
begin
  Prog := ExtractFilePath(Application.ExeName) + 'about.bin';
  ProgParam := '-p ' + IntToStr(SplashPanel.Handle);
  //Must use winexec or createproces because shellexecute cannot start
  //programs that doesn't end with '.exe'.
  //Process is closed automatically when its parent window is destroyed.
  WinExec(PAnsiChar(AnsiString(Prog + ' ' + ProgParam)), SW_SHOWNORMAL);

  NameLabel.Caption := frmEditor.AppName;
  VersionLabel.Caption := frmEditor.AppVersion;
end;

procedure TAboutForm.Label6Click(Sender: TObject);
begin
  uHelp.GoUrl( (Sender as TLabel).Caption );
end;

end.
