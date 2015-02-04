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
  Dialogs, StdCtrls, ExtCtrls, GLPanel, ZExpressions;

type
  TAboutForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    { Private declarations }
    Glp : TGLPanelZGE;
    Clicked : TDefineVariable;
    procedure OnUpdateData(Sender: TObject);
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses frmEditor, uHelp, Renderer;

{$R *.dfm}

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //Post an extra mouseup to avoid app being closed immediately if reopened
  PostMessage(Glp.Handle,WM_LBUTTONUP,0,0);
  Application.ProcessMessages;
end;


procedure TAboutForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_DLGFRAME;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  R : TRenderText;
begin
  Glp := TGLPanelZGE.Create(Self);
  Glp.Align := alClient;
  Glp.SharedHrc := (Owner as TEditorForm).Glp.GetHrc;
  Glp.Parent := Self;
  Glp.OnUpdateData := Self.OnUpdateData;

  Glp.LoadApp((Owner as TEditorForm).ExePath + 'Projects\about.zgeproj');
  if Glp.App.BindComponent<TRenderText>('VersionText',R) then
  begin
    R.SetString('Text','Version ' + frmEditor.AppVersion);
  end;

  Glp.App.BindComponent<TDefineVariable>('Clicked',Self.Clicked);
end;

procedure TAboutForm.OnUpdateData(Sender: TObject);
begin
  if Assigned(Self.Clicked) and (Clicked.IntValue<>0) then
    Close;
end;

end.
