unit frmMemoEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

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
