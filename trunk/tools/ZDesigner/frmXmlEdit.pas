unit frmXmlEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TXmlEditForm = class(TForm)
    Memo1: TMemo;
    OkButton: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  XmlEditForm: TXmlEditForm;

implementation

{$R *.dfm}

end.
