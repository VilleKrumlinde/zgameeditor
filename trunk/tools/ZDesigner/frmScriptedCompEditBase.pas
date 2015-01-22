unit frmScriptedCompEditBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCompEditBase, GLPanel, ZClasses, DesignerGui,
  ZApplication, Vcl.ExtCtrls;

type
  TScriptedCompEditFrameBase = class(TCompEditFrameBase)
    RenderTimer: TTimer;
    procedure RenderTimerTimer(Sender: TObject);
  private
    { Private declarations }
    Glp : TGLPanel;
    OldGlWindowProc : TWndMethod;
    procedure OnGlDraw(Sender: TObject);
    procedure LoadScript;
    procedure GlWindowProc(var Message: TMessage);
  protected
    EditorApp : TZApplication;
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

procedure TScriptedCompEditFrameBase.RenderTimerTimer(Sender: TObject);
begin
  Glp.Invalidate;
end;

procedure TScriptedCompEditFrameBase.SetComponent(C: TZComponent;
  TreeNode: TZComponentTreeNode);
begin
  inherited;

  Glp := TGLPanel.Create(Self);
  Glp.Align := alClient;
  Glp.SharedHrc := (Owner as TEditorForm).Glp.GetHrc;
  Glp.OnGLDraw := Self.OnGlDraw;
  OldGlWindowProc := Glp.WindowProc;
  Glp.WindowProc := GlWindowProc;
  Glp.Parent := Self;

  LoadScript;
end;

procedure TScriptedCompEditFrameBase.UpdateData;
begin

end;

procedure TScriptedCompEditFrameBase.BindData;
begin

end;

destructor TScriptedCompEditFrameBase.Destroy;
begin
  FreeAndNil(EditorApp);
  inherited;
end;

procedure TScriptedCompEditFrameBase.LoadScript;
var
  A : TZApplication;
begin
  A := ComponentManager.LoadXmlFromFile( (Owner as TEditorForm).ExePath + 'Editors\' + ScriptName ) as TZApplication;
  A.OnGetLibraryPath := (Owner as TEditorForm).OnGetLibraryPath;
  A.Compile;

  Self.EditorApp := A;
  BindData;

  A.DesignerReset;
  A.DesignerStart(Glp.Width,Glp.Height, 0);

  RenderTimer.Interval := 25;
  RenderTimer.Enabled := True;
end;

procedure TScriptedCompEditFrameBase.OnGlDraw(Sender: TObject);
begin
  if EditorApp<>nil then
  begin
    Self.UpdateData;

    EditorApp.ScreenWidth := Glp.Width;
    EditorApp.ScreenHeight := Glp.Height;
    EditorApp.UpdateViewport;
    EditorApp.WindowHandle := Glp.Handle;

    try
      //Update app
      EditorApp.Main;
    except
      on E : Exception do
      begin
        FreeAndNil(EditorApp);
        raise;
      end;
    end;
  end;
end;

procedure TScriptedCompEditFrameBase.GlWindowProc(var Message: TMessage);
begin
  Platform_DesignerWindowProc( pointer(@Message) );
  OldGlWindowProc(Message);
end;



end.
