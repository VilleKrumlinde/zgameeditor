unit frmCompEditBase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ZClasses, DesignerGUI;

type
  TCompEditFrameBase = class(TFrame)
  protected
  private
    { Private declarations }
  public
    { Public declarations }
    Component : TZComponent;
    TreeNode : TZComponentTreeNode;
    NeedRefreshTreeNode : boolean;
    procedure SetComponent(C : TZComponent; TreeNode : TZComponentTreeNode); virtual;
    procedure OnPropChanged; virtual;
    procedure OnEditorClose; virtual;
    procedure OnKeyPress(var Key : char); virtual;
    procedure SetProjectChanged;
  end;

implementation

uses frmEditor;

{$R *.dfm}

{ TCompEditFrameBase }

procedure TCompEditFrameBase.OnEditorClose;
begin
  //
end;

procedure TCompEditFrameBase.OnKeyPress(var Key: char);
begin
  //
end;

procedure TCompEditFrameBase.OnPropChanged;
begin
  //
end;

procedure TCompEditFrameBase.SetComponent(C: TZComponent; TreeNode : TZComponentTreeNode);
begin
  Component := C;
  Self.TreeNode := TreeNode;
end;

procedure TCompEditFrameBase.SetProjectChanged;
begin
  (Owner as TEditorForm).SetFileChanged(True);
end;

end.
