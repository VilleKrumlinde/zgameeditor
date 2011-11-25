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

unit frm3dsImportOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TImport3dsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    NamePrefixEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    UpDown1: TUpDown;
    MeshScaleEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    NameExampleLabel: TLabel;
    ColorsCheckBox: TCheckBox;
    AutoCenterCheckBox: TCheckBox;
    AutoScaleCheckBox: TCheckBox;
    DownloadURLLabel: TLabel;
    InvertNormalsCheckBox: TCheckBox;
    TexCoordsCheckBox: TCheckBox;
    SingleMeshCheckBox: TCheckBox;
    procedure NamePrefixEditChange(Sender: TObject);
    procedure AutoScaleCheckBoxClick(Sender: TObject);
    procedure DownloadURLLabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetValidatedName(const S : string) : string;
  end;

var
  Import3dsForm: TImport3dsForm;

implementation

{$R *.dfm}

uses uHelp;

procedure TImport3dsForm.AutoScaleCheckBoxClick(Sender: TObject);
begin
  MeshScaleEdit.Enabled := not AutoScaleCheckBox.Checked;
end;

procedure TImport3dsForm.DownloadURLLabelClick(Sender: TObject);
begin
  GoUrl( 'http://www.zgameeditor.org/index.php/Howto/Import3dsfile' );
end;

function TImport3dsForm.GetValidatedName(const S: string): string;
var
  I : integer;
begin
  Result := LowerCase(Trim(S));
  while (Length(Result)>0) and CharInSet(Result[1],['0'..'9']) do
    Delete(Result,1,1);
  I := 1;
  while I<=Length(Result) do
  begin
    if CharInSet(Result[I],['a'..'z','0'..'9','_']) then
      Inc(I)
    else
      Delete(Result,I,1);   
  end;
  if Length(Result)>0 then
    Result[1] := UpCase(Result[1]);
end;

procedure TImport3dsForm.NamePrefixEditChange(Sender: TObject);
var
  S : string;
begin
  S := GetValidatedName(NamePrefixEdit.Text);
  NameExampleLabel.Caption := '"' + S + 'Model", "' + S + 'Mesh" etc.';
end;

end.
