unit frmCustomPropEditBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DesignerGUI, ZClasses;

type
  TCustomPropEditBaseForm = class(TForm)
    DetachButton: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    TreeNode : TZComponentTreeNode;
    Component : TZComponent;
    Prop : TZProperty;
    procedure SaveChanges; virtual;
  end;

  TCustomPropEditBaseFormClass = class of TCustomPropEditBaseForm;

var
  CustomPropEditBaseForm: TCustomPropEditBaseForm;

implementation

{$R *.dfm}

{ TCustomPropEditBaseForm }

procedure TCustomPropEditBaseForm.SaveChanges;
begin

end;

end.
