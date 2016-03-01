unit frmScriptedCompEditBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCompEditBase, GLPanel, ZClasses, DesignerGui,
  ZApplication, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TScriptedCompEditFrameBase = class(TCompEditFrameBase)
    Panel3: TPanel;
    HelpButton: TButton;
    InfoLabel: TLabel;
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnBindData(Sender : TObject);
    procedure OnUpdateData(Sender : TObject);
  protected
    Glp : TGLPanelZGE;
    ScriptName : string;
    procedure BindData; virtual;
    procedure UpdateData; virtual;
  public
    procedure SetComponent(C: TZComponent; TreeNode: TZComponentTreeNode); override;
    destructor Destroy; override;
  end;

implementation

uses frmEditor, ZPlatform;

{$R *.dfm}

{ TScriptedCompEditFrameBase }

procedure TScriptedCompEditFrameBase.SetComponent(C: TZComponent;
  TreeNode: TZComponentTreeNode);
begin
  inherited;

  Glp := TGLPanelZGE.Create(Self);
  Glp.Align := alClient;
  Glp.SharedHrc := (Owner as TEditorForm).Glp.GetHrc;
  Glp.Parent := Self;
  Glp.OnBindData := Self.OnBindData;
  Glp.OnUpdateData := Self.OnUpdateData;

  Glp.LoadApp((Owner as TEditorForm).ExePath + 'Editors\' + ScriptName);
end;

procedure TScriptedCompEditFrameBase.UpdateData;
begin

end;

procedure TScriptedCompEditFrameBase.BindData;
begin

end;

destructor TScriptedCompEditFrameBase.Destroy;
begin
  inherited;
end;

procedure TScriptedCompEditFrameBase.HelpButtonClick(Sender: TObject);
begin
  inherited;
  HtmlHelp(0,Application.HelpFile + '::/' + InfoLabel.Hint + '.html', HH_DISPLAY_TOPIC, 0);
end;

procedure TScriptedCompEditFrameBase.OnBindData(Sender: TObject);
begin
  Self.BindData;
end;

procedure TScriptedCompEditFrameBase.OnUpdateData(Sender: TObject);
begin
  Self.UpdateData;
end;

end.
