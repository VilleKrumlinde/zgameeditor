unit frmMemoEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls;

type
  TMemoEditForm = class(TForm)
    Memo1: TMemo;
    OkButton: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MemoEditForm: TMemoEditForm;

implementation

{$R *.dfm}

end.
