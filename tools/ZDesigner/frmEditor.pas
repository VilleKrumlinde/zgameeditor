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

//Main form in ZDesigner

unit frmEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ZClasses, DesignerGui, GLPanel, ComCtrls, Menus, StdCtrls,
  SynEdit, ActnList, ImgList, frmSoundEdit, frmCompEditBase, Contnrs,
  uSymTab, frmMusicEdit, ZLog, Buttons, StdActns, XPMan, ExtCtrls, ToolWin;

type
  TBuildBinaryKind = (bbNormal,bbScreenSaver,bbActiveX,bbNormalLinux,bbNormalOsx86);

  TEditorForm = class(TForm)
    SaveDialog: TSaveDialog;
    Timer1: TTimer;
    LeftPanel: TPanel;
    TreePanel: TPanel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    CustomPropEditorsPageControl: TPageControl;
    TabSheet1: TTabSheet;
    TrackBar1: TTrackBar;
    TabSheet2: TTabSheet;
    PropEditorPanel: TPanel;
    ViewerPanel: TPanel;
    Splitter2: TSplitter;
    Label1: TLabel;
    TabSheet3: TTabSheet;
    ExprCompileButton: TButton;
    Splitter3: TSplitter;
    ExprPanel : TPanel;
    ActionList1: TActionList;
    AddComponentAction: TAction;
    TreePopupMenu: TPopupMenu;
    Addcomponent2: TMenuItem;
    ViewerPageControl: TPageControl;
    ViewerGlTabSheet: TTabSheet;
    ViewerSoundTabSheet: TTabSheet;
    ActionImageList: TImageList;
    RotateModelPanel: TPanel;
    ViewRotateXTrackBar: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    ZoomTrackBar: TTrackBar;
    DeleteComponentAction: TAction;
    DeleteComponentAction1: TMenuItem;
    AppControlPanel: TPanel;
    UpdateTimeCheckBox: TCheckBox;
    ResetComponentAction: TAction;
    N1: TMenuItem;
    ResetComponentAction1: TMenuItem;
    ResetModelButton: TButton;
    ViewerMusicTabSheet: TTabSheet;
    CopyComponentAction: TAction;
    PasteComponentAction: TAction;
    N2: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    MusicEditFrame1: TMusicEditFrame;
    WireframeCheckBox: TCheckBox;
    MoveUpComponentAction: TAction;
    MoveDownComponentAction: TAction;
    Moveup1: TMenuItem;
    Movedown1: TMenuItem;
    LowerRightPanel: TPanel;
    LogPanel: TPanel;
    LogListBox: TListBox;
    Splitter4: TSplitter;
    SaveProjectAction: TAction;
    LockShowAction: TAction;
    Lockshow1: TMenuItem;
    NewProjectAction: TAction;
    FileExitAction: TFileExit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Newproject1: TMenuItem;
    Save1: TMenuItem;
    N3: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    XPManifest1: TXPManifest;
    FileOpenAction: TFileOpen;
    Open1: TMenuItem;
    ToolButton8: TToolButton;
    EditCopyAction: TEditCopy;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    EditPasteAction: TEditPaste;
    ToolButton11: TToolButton;
    FileSaveAsAction: TFileSaveAs;
    SaveAs1: TMenuItem;
    RunExeAction: TAction;
    Run1: TMenuItem;
    Run2: TMenuItem;
    FileSaveBinaryAsAction: TAction;
    SaveBinaryMenuItem: TMenuItem;
    ViewerBlankTabSheet: TTabSheet;
    CompileErrorLabel: TStaticText;
    ActionDisabledImageList: TImageList;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    GenerateScreenSaverAction: TAction;
    GenerateScreenSaverAction1: TMenuItem;
    N4: TMenuItem;
    GenerateReleaseAction: TAction;
    Buildreleaseversionsmallest1: TMenuItem;
    AboutAction: TAction;
    About1: TMenuItem;
    GenerateReleaseSSAction: TAction;
    Buildreleasescreensaverversionsmallest1: TMenuItem;
    N5: TMenuItem;
    Help1: TMenuItem;
    ResetCameraButton: TButton;
    Onlinehelp1: TMenuItem;
    N6: TMenuItem;
    SoundEditFrame1: TSoundEditFrame;
    AppPreviewStartAction: TAction;
    AppStartButton: TBitBtn;
    AppPreviewStopAction: TAction;
    AppStopButton: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ForumsMenuItems: TMenuItem;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    N7: TMenuItem;
    NewWindow1: TMenuItem;
    FileNewWindowAction: TAction;
    ExprHelpButton: TButton;
    ReopenMenuItem: TMenuItem;
    GenerateActiveXAction: TAction;
    BuildandcompressAciveX1: TMenuItem;
    Import3dsAction: TAction;
    Import3dsAction1: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    ViewTranslateLabel: TLabel;
    ShaderTabSheet: TTabSheet;
    CompileShaderButton: TButton;
    ShaderPanel: TPanel;
    Label6: TLabel;
    GenerateReleaseLinuxAction: TAction;
    BuildandcompressLinuxbinary1: TMenuItem;
    GenerateReleaseOsx86Action: TAction;
    BuildMacOSXIntelbinary1: TMenuItem;
    ShowSettingsAction: TAction;
    ools1: TMenuItem;
    Settings2: TMenuItem;
    FindComponentAction: TAction;
    Findcomponent1: TMenuItem;
    NormalsCheckBox: TCheckBox;
    Panel4: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SaveBinaryMenuItemClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Update1Click(Sender: TObject);
    procedure LockShowActionExecute(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure ExprCompileButtonClick(Sender: TObject);
    procedure GenerateEXEClick(Sender: TObject);
    procedure AddComponentActionExecute(Sender: TObject);
    procedure AddComponentActionUpdate(Sender: TObject);
    procedure ViewRotateXTrackBarChange(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);
    procedure DeleteComponentActionExecute(Sender: TObject);
    procedure DeleteComponentActionUpdate(Sender: TObject);
    procedure ResetComponentActionExecute(Sender: TObject);
    procedure ResetModelButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CopyComponentActionUpdate(Sender: TObject);
    procedure CopyComponentActionExecute(Sender: TObject);
    procedure PasteComponentActionExecute(Sender: TObject);
    procedure PasteComponentActionUpdate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure MoveUpComponentActionExecute(Sender: TObject);
    procedure MoveUpComponentActionUpdate(Sender: TObject);
    procedure MoveDownComponentActionExecute(Sender: TObject);
    procedure MoveDownComponentActionUpdate(Sender: TObject);
    procedure LogListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SaveProjectActionExecute(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure FileOpenActionAccept(Sender: TObject);
    procedure FileSaveAsActionAccept(Sender: TObject);
    procedure LockShowActionUpdate(Sender: TObject);
    procedure GenerateScreenSaverActionExecute(Sender: TObject);
    procedure GenerateReleaseActionExecute(Sender: TObject);
    procedure AboutActionExecute(Sender: TObject);
    procedure GenerateReleaseSSActionExecute(Sender: TObject);
    procedure UpdateTimeCheckBoxClick(Sender: TObject);
    procedure ResetCameraButtonClick(Sender: TObject);
    procedure Onlinehelp1Click(Sender: TObject);
    procedure AppPreviewStartActionExecute(Sender: TObject);
    procedure AppPreviewStopActionExecute(Sender: TObject);
    procedure ForumsMenuItemsClick(Sender: TObject);
    procedure FileNewWindowActionExecute(Sender: TObject);
    procedure ExprHelpButtonClick(Sender: TObject);
    procedure GenerateActiveXActionExecute(Sender: TObject);
    procedure Import3dsActionExecute(Sender: TObject);
    procedure CompileShaderButtonClick(Sender: TObject);
    procedure GenerateReleaseLinuxActionExecute(Sender: TObject);
    procedure GenerateReleaseOsx86ActionExecute(Sender: TObject);
    procedure FileSaveAsActionBeforeExecute(Sender: TObject);
    procedure ShowSettingsActionExecute(Sender: TObject);
    procedure FindComponentActionExecute(Sender: TObject);
    procedure NormalsCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
    Ed : TZPropertyEditor;
    Glp : TCustomGLPanel;
    Selected,ShowNode : TZComponent;
    LockShow : boolean;
    Root : TZComponent;
    FloatEdit : TEdit;
    MinFloat,MaxFloat : single;
    ExprEdit : TEdit;
    CurrentFileName : string;
    ExprSynEdit,ShaderSynEdit : TSynEdit;
    ViewRotate,ViewTranslate : TZVector3f;
    IsAppRunning : boolean;
    OldGlWindowProc : TWndMethod;
    CompEditor : TCompEditFrameBase;  //Current component editor, nil if none
    CompEditorTreeNode : TZComponentTreeNode;
    _FileChanged : boolean;
    ExePath : string;
    RenderAborted : boolean;
    MruList : TStringList;
    RendererInitCalled : boolean;
    PredefinedConstants : TObjectList;
    PackerProg,PackerParams : string;
    ShowOpCodes : boolean;
    ZcGlobalNames : TObjectList;
    procedure SelectComponent(C : TZComponent);
    procedure DrawZBitmap;
    procedure DrawMesh;
    procedure DrawModel;
    procedure OnGlDraw(Sender : TObject);
    procedure OnPropValueChange;
    procedure OnTreeSelectItem(Sender : TObject; Node : TTreeNode);
    procedure OnTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    function CompileAll : boolean;
    procedure ReadProjectSettingsFromIni;
    procedure WriteProjectSettingsToIni;
    procedure SetShowNode(Node : TZComponent);
    procedure GlWindowProc(var Message: TMessage);
    procedure OnTreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure OnTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SetFileChanged(Value : Boolean);
    procedure RefreshSymbolTable;
    procedure DoCompile(Node: TZComponentTreeNode;  const Expr: TZPropertyValue; Prop : TZProperty);
    procedure OnGLPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnGLPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnGLPanelMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    function InsertAndRenameComponent(InsertC: TZComponent;
      DestTreeNode: TZComponentTreeNode) : TZComponentTreeNode;
    procedure OnReceiveLogMessage(Log: TLog; Mess: TLogString);
    procedure OpenProject(const FileName: string);
    procedure NewProject;
    function CloseProject: boolean;
    procedure OnExprChanged(Sender: TObject);
    procedure BuildBinary(const PlayerName, OutputName: string);
    procedure ExecToolAndWait(const ExeFile, ParamString: string);
    procedure BuildRelease(Kind : TBuildBinaryKind);
    procedure ResetCamera;
    procedure ReadAppSettingsFromIni;
    procedure WriteAppSettingsToIni;
    function VerifyToolExists(const ToolName, ToolUrl, ExeFile : string): boolean;
    procedure SetCurrentFileName(const F : string);
    procedure ReplaceResource(const ExeFile, OutFile, DataFile: string);
    procedure RefreshMenuFromMruList;
    procedure OnMruItemClick(Sender: TObject);
    procedure ReplaceActiveXGuid(const OutFile: string);
    procedure BuildActiveXTypelibrary(const ClsGuid: TGUID; const OutFile: string);
    procedure ReplaceTypeLibResource(const InFile, OutFile, DataFile: string);
  public
    Tree : TZComponentTreeView;
    SymTab : TSymbolTable;
    //Custom editors
    procedure ShowFloatEditor(Edit : TEdit; IsScalar : boolean);
    procedure ShowExprEditor(Edit : TEdit);
    procedure ShowShaderEditor(Edit : TEdit);
    procedure HideEditor;
    procedure ValidateNewName(const OldName,NewName : string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  EditorForm : TEditorForm;

const
  AppName = 'ZGameEditor';
  AppVersion = '1.9.3b';
  ZgeProjExtension = '.zgeproj';

implementation

{$R *.dfm}

uses Math, ZOpenGL, BitmapProducers, ZBitmap, Meshes, Renderer, ExprEdit, ZExpressions,
  ShellApi, SynHighlighterCpp,frmSelectComponent, AudioComponents, IniFiles, ZPlatform, ZApplication,
  dmCommon, frmAbout, uHelp, frmToolMissing, Clipbrd, unitPEFile,unitResourceDetails,
  ActiveX, u3dsFile, AudioPlayer, frmSettings, unitResourceGraphics, Zc_Ops;

{ TEditorForm }

constructor TEditorForm.Create(AOwner: TComponent);
var
  Con : TDefineConstant;
begin
  inherited Create(AOwner);

  //Zc expressions needs '.' set
  Application.UpdateFormatSettings := False;
  DecimalSeparator := '.';

  Ed := TZPropertyEditor.Create(Self);
  Ed.Align := alClient;
  Ed.OnPropValueChanged := Self.OnPropValueChange;
  Ed.Parent := PropEditorPanel;

  Tree := TZComponentTreeView.Create(Self);
  Tree.Align := alClient;
  Tree.Parent := TreePanel;
  Tree.OnChange := OnTreeSelectItem;
  Tree.OnChanging := OnTreeChanging;
  Tree.PopupMenu := TreePopupMenu;
  Tree.Images := CommonModule.CompIconsImageList;
  Tree.OnDragOver := OnTreeDragOver;
  Tree.OnDragDrop := OnTreeDragDrop;
  Tree.DragMode := dmAutomatic;
  Tree.RightClickSelect := True;

  Glp := TCustomGLPanel.Create(Self);
  Glp.Align := alClient;
  Glp.Parent := ViewerGlTabSheet;
  Glp.OnGLDraw := Self.OnGlDraw;
  //Byt ut windowproc mot vår platform_windowproc
  OldGlWindowProc := Glp.WindowProc;
  Glp.WindowProc := GlWindowProc;
  Glp.OnMouseDown := OnGLPanelMouseDown;
  Glp.OnMouseUp := OnGLPanelMouseUp;
  Glp.OnMouseMove := OnGLPanelMouseMove;
  Glp.TabStop := True;
  //Mousewheel måste sättas på formuläret annars tar det inte
  //Glp.OnMouseWheel := OnGLPanelMouseWheel;
  Self.OnMouseWheel := OnGLPanelMouseWheel;

  Platform_DesignerSetDC(Glp.Canvas.Handle, Glp.Handle);
  Platform_InitAudio;

  ExePath := ExtractFilePath(Application.ExeName);
  SaveDialog.InitialDir := ExePath + 'Projects';

  ExprSynEdit := TSynEdit.Create(Self);
  ExprSynEdit.Highlighter := TSynCppSyn.Create(Self);
  ExprSynEdit.Align := alClient;
  ExprSynEdit.Gutter.Visible := False;
  ExprSynEdit.Parent := ExprPanel;
  ExprSynEdit.OnChange := OnExprChanged;

  ShaderSynEdit := TSynEdit.Create(Self);
  ShaderSynEdit.Highlighter := TSynCppSyn.Create(Self);
  ShaderSynEdit.Align := alClient;
  ShaderSynEdit.Gutter.Visible := False;
  ShaderSynEdit.Parent := ShaderPanel;
  ShaderSynEdit.OnChange := OnExprChanged;

  SymTab := TSymbolTable.Create;

  PredefinedConstants := TObjectList.Create(True);
  Con := TDefineConstant.Create(nil);
  Con.Name := 'PI';
  Con.Value := PI;
  PredefinedConstants.Add(Con);

  ResetCamera;

  ZLog.SetReceiverFunc(OnReceiveLogMessage);

  AboutAction.Caption := 'About ' + AppName;

  MruList := TStringList.Create;
  MruList.StrictDelimiter := True;
  MruList.Duplicates := dupIgnore;

  ZcGlobalNames := TObjectList.Create(True);

  //öppna editor direkt
  if (ParamCount=1) and FileExists(ParamStr(1)) then
    OpenProject(ParamStr(1))
  else
    NewProject;

  ReadAppSettingsFromIni;

  RefreshMenuFromMruList;

  SaveBinaryMenuItem.Visible := DebugHook<>0;
  ShowOpCodes := DebugHook<>0;

  Platform_InitGlobals;  //Nollställ timer etc

  Assert(Self.SoundEditFrame1.Osc1WaveformCombo.Items.Count>0,'Dåligt bygge: osc count=0');
end;

procedure TEditorForm.OnMruItemClick(Sender : TObject);
begin
  if CloseProject then
    OpenProject((Sender as TMenuItem).Hint);
end;

procedure TEditorForm.RefreshMenuFromMruList;
var
  M : TMenuItem;
  I : integer;
begin
  ReopenMenuItem.Clear;
  for I := 0 to MruList.Count - 1 do
  begin
    if CompareText(MruList[I],CurrentFileName)=0 then
      Continue;
    M := TMenuItem.Create(ReopenMenuItem);
    M.Caption := '&' + IntToStr(I) + ' ' + MruList[I];
    M.Hint := MruList[I];
    M.OnClick := OnMruItemClick;
    ReopenMenuItem.Add(M);
  end;
  ReopenMenuItem.Enabled := ReopenMenuItem.Count>0;
end;

procedure TEditorForm.OpenProject(const FileName : string);

  function InNewProject : TZApplication;
  begin
    Result := TZApplication.Create(nil);
    Result.Name:='App';
    Result.Caption:=AppName + ' application';
  end;

begin
  //Mixer properties must be reset to default between projects because they are
  //global data.
  AudioPlayer.DesignerResetMixer;

  if (FileName='') or (not FileExists(FileName)) then
  begin //New project
    Root := InNewProject;
    SetCurrentFileName('');
  end
  else
  begin
    try
      Self.Root := ComponentManager.LoadXmlFromFile( FileName );
    except
      on E : Exception do
        begin
          //Try to recover by creating a new project
          OpenProject('');
          ShowMessage(E.Message);
          Exit;
        end;
    end;
    SetCurrentFileName(FileName);
  end;

  //Assign ZApp-global to current application
  ZApp := Root as TZApplication;

  Ed.RootComponent := Self.Root;
  Tree.SetRootComponent(Self.Root);
//  SelectComponent(Self.Root);

  RefreshSymbolTable;

//  Root.Update;

  //Sätt till nytt värde så att form.caption ändras
  _FileChanged := True;
  SetFileChanged(False);

  ViewerPageControl.ActivePage := ViewerBlankTabSheet;

  ReadProjectSettingsFromIni;

  //Must compile directly after load because no zc-instructions are saved in the xml
  CompileAll;
end;

procedure TEditorForm.ResetCamera;
var
  M : TMesh;
begin
  FillChar(ViewTranslate,SizeOf(ViewTranslate),0);
  FillChar(ViewRotate,SizeOf(ViewRotate),0);
  ViewTranslate[2] := -8;

  if (ShowNode is TMesh) then
  begin
    M := (ShowNode as TMesh);
    M.BeforeRender; //force refresh bounds
    ViewTranslate[0] := -M.BoundSphere.Center[0];
    ViewTranslate[1] := -M.BoundSphere.Center[1];
    //http://groups.google.com/group/comp.graphics.api.opengl/browse_thread/thread/1c5ea0c3b5a3dbcb/e7908ba6696ad57c?lnk=st&q=opengl+view+whole+object&rnum=6&hl=en#e7908ba6696ad57c
    ViewTranslate[2] := -M.BoundSphere.Center[2] - M.BoundSphere.Radius / Tan(45.0 / 2);
  end;
end;

procedure TEditorForm.ResetCameraButtonClick(Sender: TObject);
begin
  ResetCamera;
end;

procedure TEditorForm.OnReceiveLogMessage(Log : TLog; Mess : TLogString);
begin
  while LogListBox.Items.Count>500 do
    LogListBox.Items.Delete(0);
  LogListBox.Items.AddObject(Mess,Log);
  LogListBox.ItemIndex := LogListBox.Items.Count-1;
  LogListBox.Repaint;
end;

procedure TEditorForm.RefreshSymbolTable;
var
  List : TStringList;
  I : integer;
  Con : TDefineConstant;
begin
  ZcGlobalNames.Clear;
  SymTab.ClearAll;

  for I := 0 to PredefinedConstants.Count - 1 do
  begin
    Con := TDefineConstant(PredefinedConstants[I]);
    SymTab.Add(Con.Name,Con);
  end;
  SymTab.PushScope;

  List := TStringList.Create;
  try
    //todo: skippa getobjectnames, den här rutinen ska själv loopa root
    GetObjectNames(Root,List);
    for I := 0 to List.Count-1 do
      SymTab.Add( List[I], List.Objects[I] );
  finally
    List.Free;
  end;
end;

procedure TEditorForm.ReadAppSettingsFromIni;
var
  Ini : TIniFile;
  Section,S : string;
begin
  Ini := TIniFile.Create( ChangeFileExt(Application.ExeName,'.ini') );
  try
    Section := 'Designer';

    S := Ini.ReadString(Section,'LastOpenedProject','');
    if (S<>'') and (CurrentFileName='') and (not FindCmdLineSwitch('blank')) then
    begin
      CloseProject;
      OpenProject(S);
    end;

    S := Ini.ReadString(Section,'LastOpenedPath', '');
    if S='' then
      S := Self.ExePath + 'Projects';
    FileOpenAction.Dialog.InitialDir := S;

    S := Ini.ReadString(Section,'MruList', '');
    MruList.CommaText := S;

    Self.Width := Max(Ini.ReadInteger(Section,'Width',Self.Width),100);
    Self.Height := Max(Ini.ReadInteger(Section,'Height',Self.Height),100);
    if Ini.ReadBool(Section,'IsMaximized',False) then
      Self.WindowState:=wsMaximized;

    LowerRightPanel.Height := Max(Ini.ReadInteger(Section,'LowerRightPanel.Height',LowerRightPanel.Height),100);
    LogPanel.Width := Max(Ini.ReadInteger(Section,'LogPanel.Width',LogPanel.Width),20);

    Self.PackerProg := Ini.ReadString(Section,'PackerProg','{$toolpath}upx.exe');
    Self.PackerParams := Ini.ReadString(Section,'PackerParams','{$exename}');
  finally
    Ini.Free;
  end;
end;

procedure TEditorForm.WriteAppSettingsToIni;
var
  Ini : TIniFile;
  Section,S : string;
begin
  Ini := TIniFile.Create( ChangeFileExt(Application.ExeName,'.ini') );
  try
    Section := 'Designer';
    Ini.WriteString(Section,'LastOpenedProject',CurrentFileName);

    S := ExtractFilePath(CurrentFileName);
    if S='' then
      S:= ExtractFilePath(FileOpenAction.Dialog.FileName);
    if S<>'' then
      Ini.WriteString(Section,'LastOpenedPath',S);

    Ini.WriteInteger(Section,'Width',Self.Width);
    Ini.WriteInteger(Section,'Height',Self.Height);

    Ini.WriteBool(Section,'IsMaximized', Self.WindowState=wsMaximized);

    Ini.WriteString(Section,'MruList', MruList.CommaText);

    Ini.WriteInteger(Section,'LowerRightPanel.Height',LowerRightPanel.Height);
    Ini.WriteInteger(Section,'LogPanel.Width',LogPanel.Width);

    Ini.WriteString(Section,'PackerProg', Self.PackerProg);
    Ini.WriteString(Section,'PackerParams', Self.PackerParams);

  finally
    Ini.Free;
  end;
end;

procedure TEditorForm.ReadProjectSettingsFromIni;
var
  Ini : TIniFile;
  SelectedIndex,I,J : integer;
  Section,S : string;
  L : TStringList;
  Expanded : array of integer;
  Node,SelectedNode : TTreeNode;
begin
  Ini := TIniFile.Create( ChangeFileExt(Application.ExeName,'.ini') );
  try
    Section := 'Project: ' + ExtractFileName(CurrentFileName);

    S := Ini.ReadString(Section,'ExpandedNodes','');
    if S<>'' then
    begin
      L := TStringList.Create;
      try
        L.CommaText := S;
        SetLength(Expanded,L.Count);
        for I := 0 to L.Count-1 do
          Expanded[I] := StrToIntDef(L[I],0);
      finally
        L.Free;
      end;
    end;

    SelectedIndex := Ini.ReadInteger(Section,'LastSelectedNodeIndex',-1);
    if (SelectedIndex>-1) or (Length(Expanded)>0) then
    begin
      SelectedNode := nil;
      for I := 0 to Tree.Items.Count-1 do
      begin
        Node := Tree.Items[I];
        if Node.AbsoluteIndex=SelectedIndex then
          SelectedNode := Node;
        for J := 0 to High(Expanded) do
        begin
          if Node.AbsoluteIndex=Expanded[J] then
            Node.Expanded := True;
        end;
      end;
      if SelectedNode<>nil then
      begin
        SelectedNode.MakeVisible;
        Tree.Selected := SelectedNode;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TEditorForm.WriteProjectSettingsToIni;
var
  Ini : TIniFile;
  I : integer;
  Node : TTreeNode;
  L : TStringList;
  ExpandedNodes,Section : string;
begin
  if CurrentFileName='' then
    Exit; //don't save for blank project
  L := TStringList.Create;
  try
    for I := 0 to Tree.Items.Count-1 do
    begin
      Node := Tree.Items[I];
      if Node.Expanded then
        L.Add( IntToStr(Node.AbsoluteIndex) );
    end;
    ExpandedNodes := L.CommaText;
  finally
    L.Free;
  end;
  Ini := TIniFile.Create( ChangeFileExt(Application.ExeName,'.ini') );
  try
    Section := 'Project: ' + ExtractFileName(CurrentFileName);
    if Tree.ZSelected<>nil then
      Ini.WriteInteger(Section,'LastSelectedNodeIndex',Tree.ZSelected.AbsoluteIndex);
    Ini.WriteString(Section,'ExpandedNodes',ExpandedNodes);
  finally
    Ini.Free;
  end;
end;

procedure TEditorForm.OnGlDraw(Sender: TObject);
begin
  if ShowNode=nil then
    Exit;

  if (ShowNode is TZApplication) and (IsAppRunning) then
  begin
    try
      //Set window size to make sure camera ratio calculations are correct
      ZApplication.ScreenWidth := Glp.Width;
      ZApplication.ScreenHeight := Glp.Height;
      //Update app
      (ShowNode as TZApplication).Main;
    except
      on E : EZHalted do
      begin
        AppPreviewStopAction.Execute;
        raise;
      end;
    end;
  end
  else if not RenderAborted then
  begin
    //Gör update på hela trädet för att prop-ändringar skall slå igenom
    Root.Update;

    ViewTranslateLabel.Caption := FloatToStr( RoundTo(ViewTranslate[0],-1) ) + #13 +
      FloatToStr( RoundTo(ViewTranslate[1],-1) ) + #13 +
      FloatToStr( RoundTo(ViewTranslate[2],-1) );

    if not RendererInitCalled then
    begin
      Renderer.InitRenderer;
      RendererInitCalled := True;
    end;
    glViewport(0, 0, Glp.Width, Glp.Height);

    //ShowNode.Update;
    if {(ShowNode is TBitmapProducer) or }(ShowNode is TZBitmap)then
      DrawZBitmap
    else if {(ShowNode is TMeshProducer) or }(ShowNode is TMesh) then
      DrawMesh
    else if (ShowNode is TModel) then
      DrawModel
    else
    begin
      //Prevent displaying junk
      glClearColor(0.5,0.5,0.5,0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    end;
  end;
end;

procedure TEditorForm.SelectComponent(C: TZComponent);
begin
  Selected := C;
  if not LockShow then
    SetShowNode(Selected);
  Ed.SetComponent(C);
  RenderAborted := False;
  ViewerPanel.Refresh;
//  if Assigned(Ed.WantsFocus) and Ed.WantsFocus.CanFocus and Ed.Parent.Enabled and Visible then
//    Ed.WantsFocus.SetFocus;
end;

procedure TEditorForm.SetShowNode(Node : TZComponent);
begin
  if CompEditor<>nil then
    CompEditor.OnEditorClose;

  CompEditor := nil;
  CompEditorTreeNode := nil;

  ShowNode := Node;
  if ShowNode is AudioComponents.TSound then
  begin
    ViewerPageControl.ActivePage := ViewerSoundTabSheet;
    CompEditor := SoundEditFrame1;
  end
  else if ShowNode is AudioComponents.TMusic then
  begin
    ViewerPageControl.ActivePage := ViewerMusicTabSheet;
    CompEditor := MusicEditFrame1;
  end
  else if (ShowNode is TZBitmap) or
    (ShowNode is TMesh) or
    (ShowNode is TZApplication) or
    (ShowNode is TModel) then
  begin
    RotateModelPanel.Visible := (ShowNode is TModel) or (ShowNode is TMesh);
    AppControlPanel.Visible := ShowNode is TZApplication;
//  ZLog.GetLog(Self.ClassName).Write( inttostr(byte(AppControlPanel.Visible)));
    ViewerPageControl.ActivePage := ViewerGlTabSheet;
    ResetCamera;
  end
  else
    ViewerPageControl.ActivePage := ViewerBlankTabSheet;

  if CompEditor<>nil then
    CompEditor.SetComponent(ShowNode,Tree.ZSelected);

end;

procedure TEditorForm.FileNewWindowActionExecute(Sender: TObject);
var
  S,P : string;
begin
  S := Application.ExeName;
  P := '-blank';
  ShellExecute(Handle, 'open',PChar(S), PChar(P), nil, SW_SHOWDEFAULT);
end;

procedure TEditorForm.FileOpenActionAccept(Sender: TObject);
begin
  if CloseProject then
    OpenProject(FileOpenAction.Dialog.FileName);
end;

procedure TEditorForm.FileSaveAsActionAccept(Sender: TObject);
begin
  SetCurrentFileName( FileSaveAsAction.Dialog.FileName );
  SetFileChanged(True);
  SaveProjectAction.Execute;
end;

procedure TEditorForm.FileSaveAsActionBeforeExecute(Sender: TObject);
begin
  //Suggest current filename for Save As
  FileSaveAsAction.Dialog.FileName := Self.CurrentFileName;
end;

procedure TEditorForm.FindComponentActionExecute(Sender: TObject);
var
  S : string;
  C : TZComponent;
  I : integer;
  Node : TZComponentTreeNode;
begin
  if InputQuery('Find component','Enter name of component to search for',S) then
  begin
    C := SymTab.Lookup(S) as TZComponent;
    if C<>nil then
    begin
      for I := 0 to Tree.Items.Count-1 do
      begin
        Node := Tree.Items[I] as TZComponentTreeNode;
        if Node.Component=C then
        begin
          Node.Expand(True);
          Tree.Selected := Node;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;

  //Need to remove synedit first, otherwise synedit assertion fails
  Self.RemoveComponent(ExprSynEdit);
  ExprSynEdit.Free;
  ExprSynEdit:=nil;

  Self.RemoveComponent(ShaderSynEdit);
  ShaderSynEdit.Free;
  ShaderSynEdit:=nil;
end;

procedure TEditorForm.OnPropValueChange;
begin
  //Värde har ändrats i propertyeditorn
  Glp.Invalidate;
  if CompEditor<>nil then
    CompEditor.OnPropChanged;
  SetFileChanged(True);
end;

procedure TEditorForm.OnTreeSelectItem(Sender: TObject; Node : TTreeNode);
begin
  if (Tree.ZSelected<>nil) and (Tree.ZSelected.Component<>nil) then
  begin
    SelectComponent( Tree.ZSelected.Component );
    if GetAsyncKeyState(VK_SHIFT)<0 then
      LockShowAction.Execute;
  end
  else
    //Dölj property editor om ingen component är selectad
    Ed.SetComponent(nil);
end;


procedure TEditorForm.OnTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  //Körs innan selected byts
  if (CompEditor<>nil) and CompEditor.NeedRefreshTreeNode then
  begin
    Tree.RefreshNode(CompEditor.TreeNode,CompEditor.Component);
    SetFileChanged(True);
    CompEditor.NeedRefreshTreeNode := False;
    AllowChange:=False;
    Exit;
  end;

  //Stop app if another node is selected
  if IsAppRunning and (not LockShow) then
    AppPreviewStopAction.Execute;

  if Assigned(ActiveControl) and
    (ActiveControl is TEdit) and
    Assigned((ActiveControl as TEdit).OnExit) then
    //Spara stringedit
    (ActiveControl as TEdit).OnExit(ActiveControl)
  else if ActiveControl=ExprSynEdit then
  begin
    //Spara expression
    if ExprCompileButton.Enabled then ExprCompileButton.Click;
  end
  else if ActiveControl=ShaderSynEdit then
  begin
    //Spara shader
    if CompileShaderButton.Enabled then CompileShaderButton.Click;
  end;

  AllowChange:=True;
end;


procedure TEditorForm.SaveBinaryMenuItemClick(Sender: TObject);
var
  Stream : TMemoryStream;
begin
  if not CompileAll then
    Exit;
  if not SaveDialog.Execute then
    Exit;
  Stream := ComponentManager.SaveBinaryToStream(Root) as TMemoryStream;
  try
    Stream.SaveToFile(SaveDialog.FileName);
  finally
    Stream.Free;
  end;
end;

procedure TEditorForm.DrawZBitmap;
var
  W,H : integer;
  B : TZBitmap;
  Owns : boolean;
begin
  if ShowNode is TZBitmap then
  begin
    Owns := False;
    B := (ShowNode as TZBitmap);
  end
  else
  begin
    Exit;
//    Owns := True;
//    B := (ShowNode as TBitmapProducer).ProduceOutput as TZBitmap;
  end;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);

  glViewport(0, 0, Glp.Width, Glp.Height);

  glClearColor(0.5,0.5,0.5,0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if B=nil then
    Exit;

  glDisable( GL_LIGHTING );

  glLoadIdentity();

  glScalef(2.0 / Glp.Width, -2.0 / Glp.Height, 1.0);

  //rita en quad
  glPushMatrix;

  glEnable(GL_TEXTURE_2D);
  B.UseTextureBegin;

  W := (Min(Glp.Width,Glp.Height) div 2) - 8;
  H := W;

  W := Min(Round(W * B.PixelWidth/B.PixelHeight),W);
  H := Min(Round(H * B.PixelHeight/B.PixelWidth),H);

  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);

  //För TexCoords gäller: Y=1 Top, Y=0 Bottom
  glBegin(GL_QUADS);
    //x.
    //..
    glTexCoord2f(0.0, 1.0);
    glVertex2f(-W,-H);

    //..
    //x.
    glTexCoord2f(0.0, 0.0);
    glVertex2f(-W,H);

    //..
    //.x
    glTexCoord2f(1.0, 0.0);
    glVertex2f(W,H);

    //.x
    //..
    glTexCoord2f(1.0, 1.0);
    glVertex2f(W,-H);
  glEnd();

  glDisable(GL_TEXTURE_2D);

  //B.UseTextureEnd;
  if Owns then
    B.Free;

  glPopMatrix;

//  glFlush;
end;

procedure SetupGLShading;
const
{  FlightIntensity=0.5;
  Light0Diffuse  : array[0..3] of single = (FlightIntensity,FlightIntensity,FlightIntensity,1);
  FlightSpecularIntensity=0.7;
  Light0Specular  : array[0..3] of single = (FlightSpecularIntensity,FlightSpecularIntensity,FlightSpecularIntensity,1);}

  AmbientLight : array[0..3] of single = (0.4, 0.4, 0.4, 1.0);
  DiffuseMaterial : array[0..3] of single = (0.5, 0.5, 0.5, 1);
  LightPos : array[0..3] of single = (0, 0, 1, 0);
  //exempel från http://rush3d.com/reference/opengl-redbook-1.1/chapter06.html
  no_mat : array[0..3] of single = (0.0, 0.0, 0.0, 1.0 );
  mat_ambient : array[0..3] of single = (0.7, 0.7, 0.7, 1.0 );
  mat_ambient_color : array[0..3] of single = (0.8, 0.8, 0.2, 1.0 );
  mat_diffuse : array[0..3] of single = ( 0.1, 0.5, 0.8, 1.0 );
  mat_specular : array[0..3] of single = ( 0.2, 0.2, 0.2, 1.0 );
  AllOne : array[0..3] of single = ( 1, 1, 1, 1 );
  no_shininess = 0;
  low_shininess = 5;
  high_shininess = 100;
  mat_emission : array[0..3] of single = (0.3, 0.2, 0.2, 0.0);
begin
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  {  http://sjbaker.org/steve/omniv/opengl_lighting.html
  Set GL_LIGHT_0's position to something like 45 degrees to the 'vertical'. Coordinate (1,1,0) should work nicely in most cases.
  Set GL_LIGHT_0's Ambient color to 0,0,0,1 (default)
  Set GL_LIGHT_0's Diffuse color to 1,1,1,1 (default)
  Set GL_LIGHT_0's Specular color to 1,1,1,1 (default)
  Set the glLightModel's global ambient to 0.2,0.2,0.2,1 (this is the default).
  Don't set any other glLight or glLightModel options - just let them default.
* Enable GL_LIGHTING and GL_LIGHT_0.
* Enable GL_COLOR_MATERIAL and set glColorMaterial to GL_AMBIENT_AND_DIFFUSE. This means that glMaterial will control the polygon's specular and emission colours and the ambient and diffuse will both be set using glColor.
* Set the glMaterial's Specular colour to 1,1,1,1
  Set the glMaterial's Emission colour to 0,0,0,1 (default)
  Set the glColor to whatever colour you want each polygon to basically appear to be. That sets the Ambient and Diffuse to the same value which is what you generally want.
  Using Alpha with lighting enabled.
  }

  glLightModelfv( GL_LIGHT_MODEL_AMBIENT, @AmbientLight );

  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );

  glEnable ( GL_COLOR_MATERIAL );
  glColorMaterial ( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );

  glMaterialfv(GL_FRONT, GL_SPECULAR, @mat_specular);
  glMateriali(GL_FRONT, GL_SHININESS, low_shininess);

//    glMaterialfv(GL_FRONT, GL_EMISSION, @no_mat);

    glEnable(GL_CULL_FACE);

//    glPolygonMode(GL_FRONT,GL_FILL);
//    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glEnable(GL_NORMALIZE);
end;

procedure TEditorForm.DrawMesh;
var
  M : TMesh;
begin
  if ShowNode is TMesh then
    M := (ShowNode as TMesh)
  else
    Exit;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  //calculate the aspect ratio of the window
  //we'll use a perspective matrix to view our scene
  gluPerspective(45.0, Glp.Width/Glp.Height, 0.1, 200.0);
  glMatrixMode(GL_MODELVIEW);

//  glShadeModel(GL_SMOOTH);

  glViewport(0, 0, Glp.Width, Glp.Height);

  glClearColor(0.5,0.5,0.5,0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if M=nil then
    Exit;

  glPushAttrib(GL_ALL_ATTRIB_BITS);

    if WireframeCheckBox.Checked then
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE)
    else
      SetupGLShading;

    // now here is where we'd translate and rotate the cube
    //reset our modelview matrix
    glLoadIdentity();

    //translate
    glTranslatef(ViewTranslate[0], ViewTranslate[1], ViewTranslate[2]);

    //rotate
    glRotatef( ViewRotate[0] , 1.0, 0.0, 0.0);
    glRotatef( ViewRotate[1] , 0.0, 1.0, 0.0);
    glRotatef( ViewRotate[2] , 0.0, 0.0, 1.0);

    //set our color to be red
    glColor3f(1.0, 0.0, 0.0);

    Renderer.RenderMesh(M);

  glPopAttrib();

//  glFlush;
end;


procedure TEditorForm.DrawModel;
var
  Model : TModel;
begin
  Model := (ShowNode as TModel);

  glViewport(0, 0, Glp.Width, Glp.Height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  //calculate the aspect ratio of the window
  //we'll use a perspective matrix to view our scene
  gluPerspective(45.0, Glp.Width/Glp.Height, 0.1, 200.0);
//  glTranslatef(0,0,-10);
  glMatrixMode(GL_MODELVIEW);

  glShadeModel(GL_SMOOTH);

  glClearColor(0.5,0.5,0.5,0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glPushAttrib(GL_ALL_ATTRIB_BITS);

    if WireframeCheckBox.Checked then
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE)
    else
      SetupGLShading;

    // now here is where we'd translate and rotate the cube
    //reset our modelview matrix
    glLoadIdentity();

    //translate
    glTranslatef(ViewTranslate[0], ViewTranslate[1], ViewTranslate[2]);

    //rotate
    glRotatef( ViewRotate[0] , 1.0, 0.0, 0.0);
    glRotatef( ViewRotate[1] , 0.0, 1.0, 0.0);
    glRotatef( ViewRotate[2] , 0.0, 0.0, 1.0);

    if UpdateTimeCheckBox.Checked then
    begin
      ZApp.UpdateTime;
      try
        Model.DesignerUpdate;
      except
        on E : EZHalted do
        begin
          UpdateTimeCheckBox.Checked:=False;
          raise;
        end;
      end;
    end
    else
    begin
      ZApp.Clock.DeltaTime := 0;
      ZApp.DeltaTime := 0;
    end;
    Renderer.Render_Begin;
    try
      //Undo translation so that the model is in screen center
      glTranslatef(-Model.Position[0],-Model.Position[1],-Model.Position[2]);
      Renderer.RenderModel(Model);
    except
      on E : EZHalted do
      begin //Detect errors in onrender-list
        RenderAborted := True;
        raise;
      end;
    end;
    Renderer.Render_End;

  glPopAttrib();

//  glFlush;
end;

procedure TEditorForm.Timer1Timer(Sender: TObject);
begin
  //Only draw when topmost
  if GetActiveWindow=Handle then
    Glp.Invalidate;
end;

procedure TEditorForm.Update1Click(Sender: TObject);
begin
  Timer1.Enabled := (Sender as TMenuItem).Checked;
end;

procedure TEditorForm.UpdateTimeCheckBoxClick(Sender: TObject);
begin
  ResetModelButton.Enabled := UpdateTimeCheckBox.Checked;
end;

destructor TEditorForm.Destroy;
begin
  inherited;
  SymTab.Free;
  MruList.Free;
  PredefinedConstants.Free;
  ZcGlobalNames.Free;
end;

procedure TEditorForm.LockShowActionExecute(Sender: TObject);
begin
  if not LockShow then
  begin
    LockShow := True;
    SetShowNode(Selected);
    Tree.LockShowNode := Tree.Selected;
    Tree.Refresh;
    SelectComponent(ShowNode);
  end
  else
  begin
    LockShow := False;
    ShowNode := nil;
    Tree.LockShowNode := nil;
    Tree.Repaint;
    if Assigned(Tree.ZSelected.Component) then
      SetShowNode(Tree.ZSelected.Component);
  end;
end;

procedure TEditorForm.LockShowActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := LockShow or
    (Assigned(Tree.Selected) and
    Assigned(Tree.ZSelected.Component));

  (Sender as TAction).Checked:= LockShow;
end;

procedure TEditorForm.TrackBar1Change(Sender: TObject);
var
  NewValue,Scale : single;
begin
  if FloatEdit=nil then
    Exit;
  Scale := (MaxFloat - MinFloat) / TrackBar1.Max;
  NewValue := MinFloat + (TrackBar1.Position * Scale);
  FloatEdit.Text := FloatToStr( RoundTo(NewValue,-2) );
end;

procedure TEditorForm.ShowFloatEditor(Edit: TEdit; IsScalar : boolean);
var
  CurValue : single;
begin
  CustomPropEditorsPageControl.ActivePageIndex := 1;
  FloatEdit := nil;
  CurValue := StrToFloatDef(Edit.Text,0);
  if IsScalar then
  begin
    MinFloat := 0;
    MaxFloat := 1;
  end
  else
  begin
    MinFloat := Min(CurValue / 10, CurValue - 5);
    MaxFloat := Max(CurValue * 5, CurValue + 5);
  end;
  TrackBar1.Position := Round((CurValue-MinFloat) / ((MaxFloat-MinFloat)/TrackBar1.Max));
  FloatEdit := Edit;
end;

procedure TEditorForm.HideEditor;
begin
  CustomPropEditorsPageControl.ActivePageIndex := 0;
end;

procedure TEditorForm.ShowExprEditor(Edit: TEdit);
begin
  ExprEdit := Edit;
  ExprEdit.ReadOnly := True;
  ExprSynEdit.Text := Edit.Text;
  ExprCompileButton.Enabled := False;
  CustomPropEditorsPageControl.ActivePageIndex := 2;
end;

procedure TEditorForm.ShowSettingsActionExecute(Sender: TObject);
var
  F : TSettingsForm;
begin
  F := TSettingsForm.Create(Self);
  try
    F.PackerEdit.Text := Self.PackerProg;
    F.PackerParamsEdit.Text := Self.PackerParams;

    if F.ShowModal=mrOk then
    begin
      Self.PackerProg := F.PackerEdit.Text;
      Self.PackerParams := F.PackerParamsEdit.Text;
    end;
  finally
    F.Free;
  end;
end;

procedure TEditorForm.ShowShaderEditor(Edit: TEdit);
begin
  ExprEdit := Edit;
  ExprEdit.ReadOnly := True;
  ShaderSynEdit.Text := Edit.Text;
  CompileShaderButton.Enabled := False;
  CustomPropEditorsPageControl.ActivePage := ShaderTabSheet;
end;

procedure TEditorForm.OnExprChanged(Sender: TObject);
begin
  ExprCompileButton.Enabled := True;
  CompileShaderButton.Enabled := True;
end;

procedure TEditorForm.CompileShaderButtonClick(Sender: TObject);
begin
  //Spara ändrad text i edit-ruta
  ExprEdit.Text := TrimRight(ShaderSynEdit.Text);
  ExprEdit.OnExit(ExprEdit);
end;

procedure TEditorForm.ExprCompileButtonClick(Sender: TObject);
var
  C : TZComponent;
  Prop : TZProperty;
  PropValue : TZPropertyValue;
  Success : boolean;
  I : integer;
begin
  //Spara ändrad text i edit-ruta
  ExprEdit.Text := TrimRight(ExprSynEdit.Text);
  ExprEdit.OnExit(ExprEdit);
  //Propvärde har nu sparats i component
  //Läs tillbaka propvärde för kompilering
  C := Selected;
  Prop := C.GetProperties.GetByName(ExprEdit.Hint);
  C.GetProperty(Prop,PropValue);
  Success:=False;
  try
    if C is TZLibrary then
      Success := CompileAll
    else
    begin
      DoCompile(Tree.ZSelected,PropValue,Prop);
      Success:=True;
    end;
  except
    on E : EParseError do
    begin
      ExprSynEdit.CaretXY := Point(E.Col-1,E.Line);
      ExprSynEdit.BlockBegin := Point(0,E.Line);
      ExprSynEdit.BlockEnd := Point(0,E.Line+1);
      ExprSynEdit.SetFocus;
      //ShowMessage( E.Message );
      CompileErrorLabel.Caption := E.Message;
      ZLog.GetLog(Self.ClassName).Write(E.Message);
    end;
    on E : ECodeGenError do
    begin
      CompileErrorLabel.BevelKind := bkTile;
      CompileErrorLabel.Caption := E.Message;
      ZLog.GetLog(Self.ClassName).Write(E.Message);
    end;
  end;
//  Tree.RefreshNode(Tree.Selected,Selected);
  if Success then
  begin
    ExprCompileButton.Enabled := False;
    CompileErrorLabel.Caption := '';
    CompileErrorLabel.BevelKind := bkNone;
    if ShowOpCodes then
    begin
      ZLog.GetLog(Self.ClassName).Write('----');
      for I := 0 to PropValue.ExpressionValue.Code.Count - 1 do
        ZLog.GetLog(Self.ClassName).Write( (PropValue.ExpressionValue.Code[I] as TExpBase).ExpAsText );
    end;
  end;
end;

procedure TEditorForm.ExprHelpButtonClick(Sender: TObject);
begin
  uHelp.ShowHelp('Main/WritingExpressions');
end;

procedure TEditorForm.DoCompile(Node : TZComponentTreeNode; const Expr : TZPropertyValue; Prop : TZProperty);
var
  C : TZComponent;
  CurParent : TZComponentTreeNode;
  Model : TModel;
begin
  if Prop.IsDefaultValue(Expr) then
  begin
    //Generate no code for an empty expression
    Expr.ExpressionValue.Code.Clear;
    Exit;
  end;

  C := Node.Component;
  CurParent := Node.Parent as TZComponentTreeNode;
  Model := nil;
  //Om det finns en model-parent så skriv den till symbol 'CurrentModel'
  //så att den kan användas i uttryck.
  while CurParent<>nil do
  begin
    if Assigned(CurParent.Component) and
      (CurParent.Component is TModel) then
    begin
      Model := CurParent.Component as TModel;
      Break;
    end;
    CurParent := CurParent.Parent as TZComponentTreeNode;
  end;
  if Assigned(Model) then
    SymTab.Add('CurrentModel',Model);
  try
    Compile(C,Expr.ExpressionValue,SymTab,Prop.ReturnType,ZcGlobalNames);
  finally
    if Assigned(Model) then
      SymTab.Remove('CurrentModel');
  end;
end;

function TEditorForm.CompileAll : boolean;
var
  I,J,CompiledCount : integer;
  Node : TZComponentTreeNode;
  C : TZComponent;
  Prop : TZProperty;
  PropValue : TZPropertyValue;
  PropList : TZPropertyList;
  Success : boolean;
begin
  CompiledCount := 0;
  Tree.Items.BeginUpdate;
  Success := True;

  for I := 0 to ZcGlobalNames.Count - 1 do
  begin
    //Remove user function names from symtab
    if SymTab.Contains(TZcOpFunction(ZcGlobalNames[I]).Id) then
      SymTab.Remove(TZcOpFunction(ZcGlobalNames[I]).Id);
  end;
  ZcGlobalNames.Clear;

  try
    I := 0;
    while I<Tree.Items.Count do
    begin
      Node := Tree.Items[I] as TZComponentTreeNode;
      if Assigned(Node.Component) then
      begin
        C := Node.Component;
        PropList := C.GetProperties;
        for J := 0 to PropList.Count-1 do
        begin
          Prop := TZProperty(PropList[J]);
          if Prop.PropertyType=zptExpression then
          begin
            C.GetProperty(Prop,PropValue);
            //Compile(C,PropValue.ExpressionValue,Root,SymTab);
            try
              DoCompile(Node,PropValue,Prop);
            except on E : Exception do
              begin
                ShowMessage( 'Error in expression for node: ' + Node.Component.GetDisplayName + ' '#13 + E.Message );
                Node.Expand(True);
                Tree.Selected := Node;
                Success := False;
              end;
            end;
            //Ifall expressioninstruktioner visas så måste träd refreshas
            //Men då tar omkompilering 5 sekunder
  //          Tree.RefreshNode(Node,C);
            Inc(CompiledCount);
          end;
        end;
      end;
      Inc(I);
    end;
  finally
    Tree.Items.EndUpdate;
  end;
  ZLog.GetLog(Self.ClassName).Write('Compiled expressions: ' + IntToStr(CompiledCount));
  Result := Success;
end;

procedure TEditorForm.BuildBinary(const PlayerName,OutputName : string);
const
  Magic : integer = $01020304;
var
  M1,M2 : TMemoryStream;
  IsPiggy : boolean;
begin
  IsPiggy := (PlayerName<>'');
  if not CompileAll then
    Exit;
  M2 := ComponentManager.SaveBinaryToStream(Root) as TMemoryStream;
  M1 := TMemoryStream.Create;
  try
    if IsPiggy then
      M1.LoadFromFile(ExePath + PlayerName);
    M1.Position := M1.Size;
    M2.SaveToStream(M1);
    if IsPiggy then
      M1.Write(Magic,4);
    M1.SaveToFile(OutputName);
  finally
    M1.Free;
    M2.Free;
  end;
  ZLog.GetLog(Self.ClassName).Write('File generated: ' + OutputName);
end;

procedure TEditorForm.GenerateActiveXActionExecute(Sender: TObject);
begin
  BuildRelease(bbActiveX);
end;

procedure TEditorForm.GenerateEXEClick(Sender: TObject);
var
  OutFile : string;
begin
  if CurrentFileName='' then
    OutFile := ExePath + 'untitled.exe'
  else
    OutFile := ChangeFileExt(CurrentFileName,'.exe');
  BuildBinary('player.bin',OutFile);
  //Kör den skapade filen
  ShellExecute(Handle, 'open',PChar(OutFile), nil, nil, SW_SHOWNORMAL);
end;

procedure TEditorForm.GenerateScreenSaverActionExecute(Sender: TObject);
var
  OutFile : string;
begin
  if CurrentFileName='' then
    OutFile := ExePath + 'untitled.scr'
  else
    OutFile := ChangeFileExt(CurrentFileName,'.scr');
  BuildBinary('player_ss.bin',OutFile);
  ShowMessage('Created file: ' + OutFile);
end;

function TEditorForm.VerifyToolExists(const ToolName,ToolUrl,ExeFile : string) : boolean;
var
  D : TToolMissingForm;
begin
  if not FileExists(ExeFile) then
  begin
    D := TToolMissingForm.Create(Self);
    D.ExeNameLabel.Caption := ExtractFileName(ExeFile);
    D.ToolPathLabel.Caption := ExtractFilePath(ExeFile);
    D.DownloadURLLabel.Caption := ToolUrl;
    D.ToolNameLabel.Caption := ToolName;
    D.ShowModal;
    Result := False;
  end else
    Result := True;
end;

procedure TEditorForm.ExecToolAndWait(const ExeFile,ParamString : string);
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  ZLog.GetLog(Self.ClassName).Write(ExeFile + ' ' + ParamString);
  FillChar(SEInfo, SizeOf(SEInfo), 0) ;
  SEInfo.cbSize := SizeOf(TShellExecuteInfo) ;
  with SEInfo do
  begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(ExeFile) ;
    lpParameters := PChar(ParamString) ;
    // lpDirectory := PChar(StartInString) ;
    nShow := SW_SHOWNORMAL;
  end;
  if ShellExecuteEx(@SEInfo) then begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(SEInfo.hProcess, ExitCode) ;
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
  end
  else
    ShowMessage('Error ' + ExeFile);
end;

procedure TEditorForm.ReplaceResource(const ExeFile,OutFile,DataFile : string);
var
  M : TPEResourceModule;
  R : TResourceDetails;
  IconR : TIconGroupResourceDetails;
  NewData : TMemoryStream;
begin
  M := TPEResourceModule.Create;
  try
    M.LoadFromFile( ExeFile );

    R := M.FindResource('10','DATA_FILE',1053);
    Assert(R<>nil);

    NewData := TMemoryStream.Create;
    try
      NewData.LoadFromFile(DataFile);
      R.ChangeData(NewData);
    finally
      NewData.Free;
    end;

    //Remove the other two resource (packageinfo), saves about 1kb
    //todo: remove more resources if ocx
    if ExtractFileExt(OutFile)<>'.ocx' then
    begin
      R := M.FindResource('10','DVCLAL',0);
      if R<>nil then
        M.DeleteResource(M.IndexOfResource(R));
      R := M.FindResource('10','PACKAGEINFO',0);
      if R<>nil then
        M.DeleteResource(M.IndexOfResource(R));

//      M.DeleteResource(1);
//      M.DeleteResource(1);
    end;

    if not ZApp.ShowOptionsDialog then
    begin
      //Remove dialog resource if not used
      R := M.FindResource('5','IDDLG_OPTIONSDIALOG',1053);
      if R<>nil then
        M.DeleteResource(M.IndexOfResource(R));
    end;

    //http://msdn.microsoft.com/en-us/library/ms997538.aspx
    //lägg till ico-file ifall den finns som prop på application
    if ZApp.Icon.Size>0 then
    begin
      IconR := TIconGroupResourceDetails.CreateNew(M,1053,'1');
      NewData := TMemoryStream.Create;
      try
        NewData.Write(ZApp.Icon.Data^,ZApp.Icon.Size);
        NewData.Position:=0;
        IconR.LoadImageFromStream(NewData);
      finally
        NewData.Free;
      end;
      //IconR.LoadImage(ExePath + 'test.ico');
    end;

    M.SaveToFile( OutFile );
  finally
    M.Free;
  end;
end;

procedure TEditorForm.ReplaceTypeLibResource(const InFile,OutFile,DataFile : string);
var
  M : TPEResourceModule;
  R : TResourceDetails;
  NewData : TMemoryStream;
begin
  M := TPEResourceModule.Create;
  try
    M.LoadFromFile( InFile );

    R := M.FindResource('TYPELIB','1',0);
    Assert(R<>nil);

    NewData := TMemoryStream.Create;
      NewData.LoadFromFile(DataFile);
      R.ChangeData(NewData);
    NewData.Free;

    M.SaveToFile( OutFile );
  finally
    M.Free;
  end;
end;

procedure TEditorForm.BuildActiveXTypelibrary(const ClsGuid : TGUID; const OutFile : string);
const
//  ZClsGuid : TGUID = '{49F96FB9-0D7F-4E4E-ADBC-442EAB031529}';
  ZIntGuid : TGUID = '{8C1FB6F5-666A-44D3-A98C-8B24C1B13CE1}';
  ITypeInfoGuid : TGUID = '{00020401-0000-0000-C000-000000000046}';
  IDispatchGuid : TGUID = '{00020400-0000-0000-C000-000000000046}';
var
  LibGUID : TGUID;
  Tlb : ICreateTypeLib2;
  StdOleTlb : ITypeLib;
  Cls,Intf: ICreateTypeInfo;
  IntfTypeInfo,DispatchTypeInfo  : ITypeInfo;
  RefType : HRefType;
begin
  //Create typelib
  CoCreateGuid(LibGUID);
  CreateTypeLib2(SYS_WIN32, StringToOleStr(OutFile), Tlb);

  Tlb.SetGuid(LibGUID);
  Tlb.SetLcid(0);
  Tlb.SetName('ZgeActiveX');

  //Create reference to IDispatch in stdole32
  LoadTypeLib('stdole32.tlb',StdOleTlb);
  StdOleTlb.GetTypeInfoOfGuid(IDispatchGuid,DispatchTypeInfo);

  //Create interface. Must inherit IDispatch.
  Tlb.CreateTypeInfo('IZgeActiveXControl', TKIND_INTERFACE, Intf);
  Intf.SetGuid(ZIntGuid);
  Intf.AddRefTypeInfo(DispatchTypeInfo, RefType);
  Intf.AddImplType(0,RefType);
  Intf.QueryInterface(ITypeInfoGuid,IntfTypeInfo);

  //Create coclass. Implements IZgeActiveXControl.
  Tlb.CreateTypeInfo('ZgeActiveXControl', TKIND_COCLASS, Cls);
//  ClsGuid := ZClsGuid;
  Cls.SetGuid(ClsGUID);
  Cls.SetTypeFlags(TYPEFLAG_FCANCREATE);

  Cls.AddRefTypeInfo(IntfTypeInfo, RefType);
  Cls.AddImplType(0,RefType);
  Cls.SetImplTypeFlags(0,IMPLTYPEFLAG_FDEFAULT);

  Tlb.SaveAllChanges;
end;

procedure TEditorForm.ReplaceActiveXGuid(const OutFile : string);
const
  OriginalG : TGUID = '{49F96FB9-0D7F-4E4E-ADBC-442EAB031529}';
  HtmlPage : string =
'<HTML>'#13#10 +
'<H1> Sample page for using the "%s" ActiveX-control </H1><p>'#13#10 +
'<a href="http://www.google.com">Google</a>  <a href="http://www.zgameeditor.org">ZGameEditor</a>'#13#10 +
'<HR><center><P>'#13#10 +
'<OBJECT'#13#10 +
'   classid="clsid:%s"'#13#10 +
'   codebase="%s#Version=-1,-1,-1,-1"'#13#10 +
'   width=800'#13#10 +
'   height=600'#13#10 +
'   align=center'#13#10 +
'   hspace=0'#13#10 +
'   vspace=0'#13#10 +
'>'#13#10 +
'</OBJECT>'#13#10 +
'</HTML>'#13#10;
var
  Mem : TMemorySTream;
  I,ReplaceCount : integer;
  P : PChar;
  NewG : TGUID;
  NewGBuf : array[0..100] of WideChar;
  NewGString,S : string;
  TempTlbFile : string;
begin
  //Every ZGE-application must have its own guid, otherwise IE will not
  //load another ocx with the same guid.

  //New GUID for the player.
  CoCreateGuid(NewG);

  //Rebuild and replace the typelibrary within the player-module
  //(cannot do a binary replace inside a typelibrary so we must rebuild it)
  TempTlbFile := ExePath + 'temp.tlb';
  BuildActiveXTypelibrary(NewG,TempTlbFile);
  ReplaceTypeLibResource(OutFile,OutFile,TempTlbFile);
  DeleteFile(TempTlbFile);
  //Delete backupfile generated by resource-code
  DeleteFile(ChangeFileExt(OutFile,'.~ocx'));

  //Binary replace of the oldguid to the new
  Mem := TMemoryStream.Create;
  try
    Mem.LoadFromFile(OutFile);
    P := Mem.Memory;
    ReplaceCount := 0;
    for I := 0 to Mem.Size - SizeOf(TGUID) - 1 do
    begin
      if CompareMem(@OriginalG,P,SizeOf(TGUID)) then
      begin
        Move(NewG,P^,SizeOf(TGUID));
        Inc(ReplaceCount);
      end;
      Inc(P);
    end;
    Assert(ReplaceCount>0);
    Mem.SaveToFile(OutFile);

    //Genereate HTM-page with the new GUID
    StringFromGUID2(NewG,NewGBuf,39);
    NewGString := Copy(WideCharToString(NewGBuf),2,36);
    S := Format(HtmlPage,[ ExtractFileName(OutFile), NewGString, ExtractFileName(OutFile) ]);
    Mem.Clear;
    Mem.Write(pointer(S)^,Length(S));
    Mem.SaveToFile(ChangeFileExt(OutFile,'.htm'));
  finally
    Mem.Free;
  end;
end;

procedure TEditorForm.BuildRelease(Kind : TBuildBinaryKind);
var
  OutFile,TempFile,Tool,ToolParams,PlayerName,Ext : string;
  ToolPath : string;
  UsePiggyback : boolean;

  function InGetSize : integeR;
  var
    Handle : THandle;
  begin
    Handle := FileOpen(PChar(OutFile),fmOpenRead);
    Result := GetFileSize(Handle,nil);
    FileClose(Handle);
  end;

begin
  UsePiggyback := False;
  case Kind of
    bbNormal :
      begin
        Ext := 'exe';
        PlayerName := ExePath + 'player.bin';
      end;
    bbScreenSaver :
      begin
        Ext := 'scr';
        PlayerName := ExePath + 'player_ss.bin'
      end;
    bbActiveX :
      begin
        Ext := 'ocx';
        PlayerName := ExePath + 'player_activex.bin';
      end;
    bbNormalLinux :
      begin
        Ext := '';
        PlayerName := 'player_linux.bin';
        UsePiggyback := True;
      end;
    bbNormalOsx86 :
      begin
        Ext := '';
        PlayerName := 'player_osx86.bin';
        UsePiggyback := True;
      end;
  end;

  if CurrentFileName='' then
    OutFile := ExePath + 'untitled.' + Ext
  else
    //Must expand filename because we need absolute path when calling tools, not relative paths like .\projects
    OutFile := ChangeFileExt(ExpandFileName(CurrentFileName),'.' + Ext);

  if FileExists(OutFile) then
    DeleteFile(OutFile);

  if UsePiggyback then
  begin
    BuildBinary(PlayerName,OutFile);
  end
  else
  begin
    TempFile := ExePath + 'temp.dat';
    BuildBinary('',TempFile);
    ReplaceResource(PlayerName,OutFile,TempFile);
    DeleteFile(TempFile);
  end;

  if Kind=bbActiveX then
    ReplaceActiveXGuid(OutFile);

  //Kör upx på allt utom Linux (linuxbinärer med piggyback hanteras ej av upx)
  if (Kind<>bbNormalLinux) and (Kind<>bbNormalOsx86) then
  begin
    //Upx -v %1
    ToolPath := ExePath + 'tools\';

//    Tool := ToolPath + 'upx.exe';
    Tool := StringReplace(PackerProg,'{$toolpath}',ToolPath,[rfReplaceAll, rfIgnoreCase]);
    if not VerifyToolExists('Packer','',Tool) then
      Exit;
//    ToolParams := '-v "' + OutFile + '"';
    ToolParams := StringReplace(PackerParams,'{$exename}','"' + OutFile + '"',[rfReplaceAll, rfIgnoreCase]);
    ExecToolAndWait(Tool,ToolParams);
  end;

  //Need to update filedate, the file created by reshacker has same date as original
  FileSetDate(OutFile,DateTimeToFileDate(Now));

  if Kind=bbActiveX then
  begin
    case Application.MessageBox( PChar('Created files: '#13#13 +
      OutFile + #13 +
      ChangeFileExt(OutFile,'.htm') + #13#13 +
      'Size: ' + IntToStr(InGetSize div 1024) + ' kb' + #13#13 +
      'Click YES to open the html-file in Internet Explorer to test the ActiveX-control.')
       ,
      PChar(Caption), MB_YESNO) of
      IDYES : ShellExecute(Handle, 'open',  PChar('iexplore.exe'), PChar(ChangeFileExt(OutFile,'.htm')), nil, SW_SHOWDEFAULT);
      IDNO : ;
    end;
{    ShowMessage('Created files: '#13#13 +
      O
      utFile + #13 +
      ChangeFileExt(OutFile,'.htm') + #13#13 +
      'Size: ' + IntToStr(InGetSize div 1024) + ' kb' );
    ShellExecute(Handle, 'open',  PChar('iexplore.exe'), PChar(ChangeFileExt(OutFile,'.htm')), nil, SW_SHOWDEFAULT);}
  end else
    ShowMessage('Created file: '#13#13'  ' + OutFile + #13#13 + 'Size: ' + IntToStr(InGetSize div 1024) + ' kb' );
end;

procedure TEditorForm.GenerateReleaseActionExecute(Sender: TObject);
begin
  BuildRelease(bbNormal);
end;


procedure TEditorForm.GenerateReleaseLinuxActionExecute(Sender: TObject);
begin
  BuildRelease(bbNormalLinux);
end;

procedure TEditorForm.GenerateReleaseOsx86ActionExecute(Sender: TObject);
begin
  BuildRelease(bbNormalOsx86);
end;

procedure TEditorForm.GenerateReleaseSSActionExecute(Sender: TObject);
begin
  BuildRelease(bbScreenSaver);
end;

procedure TEditorForm.AboutActionExecute(Sender: TObject);
begin
  AboutForm := TAboutForm.Create(Self);
  try
    AboutForm.Caption := 'About ' + AppName;
    AboutForm.ShowModal;
  finally
    AboutForm.Free;
  end;
end;

procedure TEditorForm.AddComponentActionExecute(Sender: TObject);
var
  C : TZComponent;
  Ci : TZComponentInfo;
  OwnerList : TZComponentList;
  Prop : TZProperty;
  ParentComps,ParentLists : TStringList;
  CurParent : TZComponentTreeNode;
  S : string;
  I : integer;
begin
  //Ta reda på vilken lista som nod ska läggas till
  if not Assigned(Tree.Selected) then
    Exit;
  if Assigned(Tree.ZSelected.ComponentList) then
  begin
    OwnerList := Tree.ZSelected.ComponentList;
    //ParentC := (Tree.ZSelected.Parent as TZComponentTreeNode).Component;
    Prop := Tree.ZSelected.Prop;
  end
{  else if (TObject(Tree.Selected.Data) is TZComponent) then
  begin
    //Todo: får endast ha en nested
    //gör c.DesignerGetDefaultList, nil om flera finns
    Selected.GetProperty(Selected.GetProperties.GetByType(zptComponentList),Value);
    OwnerList := Value.ComponentListValue;
    ParentC := Selected;
    Prop := nil;
  end }
  else
    Exit;

  ParentComps := TStringList.Create;
  ParentLists := TStringList.Create;
  try
    CurParent := Tree.ZSelected;
    while CurParent<>nil do
    begin
      if Assigned(CurParent.Component) then
        ParentComps.Add(ComponentManager.GetInfo(CurParent.Component).ZClassName)
      else if Assigned(CurParent.ComponentList) then
        ParentLists.Add(CurParent.Text);
      CurParent := CurParent.Parent as TZComponentTreeNode;
    end;

    //Ta reda på vilken klass som ska läggas till
    if not Assigned(SelectComponentForm) then
      SelectComponentForm := TSelectComponentForm.Create(Self);
//    if Prop=nil then
      SelectComponentForm.FilterBy(ParentComps,ParentLists,Prop);
//    else
//      SelectComponentForm.FilterBy(Prop.ChildClasses);
    if SelectComponentForm.ShowModal<>mrOk then
      Exit;
    Ci := SelectComponentForm.GetSelectedClass;
  finally
    ParentComps.Free;
    ParentLists.Free;
  end;

  C := Ci.ZClass.Create(OwnerList);

  if Ci.AutoName then
  begin  //Give unique name
    for I := 1 to 100 do
    begin
      S := Ci.ZClassName + IntToStr(I);
      if not SymTab.Contains(S) then
      begin
        C.Name := S;
        SymTab.Add(S,C);
        Break;
      end;
    end;
  end;

  Tree.AddNode(C,Tree.Selected).Selected := True;
  //Signalera till parentlistan att den är ändrad
  TZComponentTreeNode(Tree.Selected.Parent).ComponentList.Change;
  SetFileChanged(True);
end;

procedure TEditorForm.AddComponentActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Tree.Focused and Assigned(Tree.Selected) and Assigned(Tree.ZSelected.ComponentList);
end;

procedure TEditorForm.ViewRotateXTrackBarChange(Sender: TObject);
var
  TB : TTrackbar;
begin
  TB := Sender as TTrackBar;
  ViewRotate[ TB.Tag ] := TB.Position * ( (360) / TB.Max );
end;

procedure TEditorForm.ZoomTrackBarChange(Sender: TObject);
var
  TB : TTrackbar;
begin
  TB := Sender as TTrackBar;
  ViewTranslate[2] := -1 - TB.Position * ( 10 / TB.Max );
end;

procedure TEditorForm.DeleteComponentActionExecute(Sender: TObject);
var
  C,CurC : TZComponent;
  List : TObjectList;
  I : integer;
begin
  C := Tree.ZSelected.Component;
  if HasReferers(Root, C) then
  begin
    ShowMessage('Cannot delete, other components refers to this component.');
    Exit;
  end;

  if C=ShowNode then
  begin
    ShowNode := nil;
    Tree.LockShowNode := nil;
  end;

  //Remove all names from symboltable
  List := TObjectList.Create(False);
  try
    GetAllObjects(C,List);
    for I := 0 to List.Count - 1 do
    begin
      CurC := List[I] as TZComponent;
      if CurC.Name<>'' then
        SymTab.Remove(CurC.Name);
    end;
  finally
    List.Free;
  end;

  C.Free;

  //Signalera till parentlistan att den är ändrad
  TZComponentTreeNode(Tree.Selected.Parent).ComponentList.Change;
  Tree.ZSelected.Delete;
  SelectComponent(nil);
  SetFileChanged(True);
end;

procedure TEditorForm.DeleteComponentActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (ActiveControl = Tree) and
    Assigned(Tree.Selected) and
    Assigned(Tree.ZSelected.Component) and
    (Tree.ZSelected.Component<>Root) and
    (not (Tree.ZSelected.Component is TZApplication));
end;

procedure TEditorForm.AppPreviewStartActionExecute(Sender: TObject);
begin
  if (Tree.ZSelected.Component<>ZApp) and (ShowNode<>ZApp) then
    Exit;
  if not CompileAll then
    Exit;
  try
    ZApp.DesignerReset;  //Reset timer-components etc
    ZApp.DesignerStart(Glp.Width,Glp.Height);
  except
    on E : EZHalted do
    begin
      AppPreviewStopAction.Execute;
      raise;
    end;
  end;
  AppPreviewStartAction.ShortCut := 0;
  AppPreviewStopAction.ShortCut := 32781;
  AppPreviewStartAction.Enabled := False;
  AppPreviewStopAction.Enabled := True;
  Glp.SetFocus;
  IsAppRunning := True;
end;

procedure TEditorForm.AppPreviewStopActionExecute(Sender: TObject);
begin
  ZApp.DesignerStop;
  AppPreviewStartAction.ShortCut := 32781;
  AppPreviewStopAction.ShortCut := 0;
  AppPreviewStartAction.Enabled := True;
  AppPreviewStopAction.Enabled := False;
  IsAppRunning := False;
end;

procedure TEditorForm.ResetComponentActionExecute(Sender: TObject);
begin
  if Assigned(Tree.ZSelected.ComponentList) then
    Tree.ZSelected.ComponentList.DesignerReset
  else if Assigned(Tree.ZSelected.Component) then
    Tree.ZSelected.Component.DesignerReset;
end;

procedure TEditorForm.ResetModelButtonClick(Sender: TObject);
begin
  if ShowNode<>nil then
  begin
    ZApp.Clock.Time := 0;
    ShowNode.DesignerReset;
  end;
end;

procedure TEditorForm.GlWindowProc(var Message: TMessage);
begin
  if IsAppRunning then
    Platform_DesignerWindowProc( pointer(@Message) );
  OldGlWindowProc(Message);
end;

procedure TEditorForm.OnTreeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  FromNode,ToNode : TTreeNode;
  IsCopy : boolean;
begin
  IsCopy := GetAsyncKeyState(VK_CONTROL)<0;

  Accept:=False;
  FromNode:=Tree.Selected;
  ToNode:=Tree.DropTarget;
  if (FromNode=nil) or (ToNode=nil) then
    Exit;
  if ToNode.HasAsParent(FromNode) and (not IsCopy) then
    Exit;
  if not Assigned((FromNode as TZComponentTreeNode).Component) then
    Exit;
  if not Assigned((ToNode as TZComponentTreeNode).ComponentList) then
    Exit;
  Accept:=True;
  if IsCopy then
    Tree.DragCursor := crMultiDrag //todo borde vara dragplus ikon, men den finns ej?
  else
    Tree.DragCursor := crDrag;
end;

procedure TEditorForm.OnTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  FromNode,ToNode : TTreeNode;
  SourceC : TZComponent;
  DestList : TZComponentList;
  IsCopy : boolean;
begin
  IsCopy := GetAsyncKeyState(VK_CONTROL)<0;
  FromNode:=Tree.Selected;
  ToNode:=Tree.DropTarget;
  if (FromNode=nil) or (ToNode=nil) then
    Exit;
  if ToNode.HasAsParent(FromNode) and (not IsCopy) then
    Exit;
  SourceC := (FromNode as TZComponentTreeNode).Component;
  if not Assigned(SourceC) then
    Exit;
  DestList := (ToNode as TZComponentTreeNode).ComponentList;
  if not Assigned(DestList) then
    Exit;

  if GetAsyncKeyState(VK_CONTROL)<0 then
    //Kopiera komponent ifall CTRL är nedtryckt
    InsertAndRenameComponent(SourceC.Clone,ToNode as TZComponentTreeNode)
  else
  begin
    //Flytta zcomponent
    SourceC.OwnerList.RemoveComponent(SourceC);
    DestList.AddComponent(SourceC);

    //Flytta trädnoder
    FromNode.MoveTo(ToNode,naAddChild);
  end;
  SetFileChanged(True);
end;

procedure TEditorForm.SetCurrentFileName(const F: string);
const
  MruListMax=8;
begin
  CurrentFileName := F;
  Platform_DesignerSetFilePath( ExtractFilePath(CurrentFileName) );
  //Add to MRU-list
  if F<>'' then
  begin
    if MruList.IndexOf(F)>-1 then
      MruList.Delete(MruList.IndexOf(F));
    MruList.Insert(0,F);
    while MruList.Count>MruListMax do
      MruList.Delete(MruListMax);
    RefreshMenuFromMruList;
  end;
end;

procedure TEditorForm.SetFileChanged(Value: Boolean);
var
  S,Fn : string;
begin
  if _FileChanged = Value then
    Exit;
  _FileChanged := Value;

  if CurrentFileName<>'' then
    Fn := ExtractFileName(CurrentFileName)
  else
    Fn := '[untitled project]';
  if _FileChanged then
    Fn := Fn + ' * ';

  S := Fn + ' - ' + AppName + ' ' + AppVersion;
  Self.Caption := S;
  Application.Title := S;
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  WriteAppSettingsToIni;
  CanClose := CloseProject();
  if CanClose then
    Platform_ShutdownAudio;
end;

procedure TEditorForm.ValidateNewName(const OldName, NewName: string);
var
  C : TZComponent;
  I : integer;
begin
  C := Tree.ZSelected.Component;
  if CompareText(NewName,'CurrentModel')=0 then
  begin
    ShowMessage('Name cannot be CurrentModel');
    Abort;
  end;
  for I := 1 to Length(NewName) do
  begin
    if not (NewName[I] in ['a'..'z','A'..'Z','_','0'..'9']) then
    begin
      ShowMessage('Name contains invalid characters: ' + NewName);
      Abort;
    end;
  end;
  if (NewName<>'') and (NewName[1] in ['0'..'9']) then
  begin
    ShowMessage('Name cannot begin with a digit: ' + NewName);
    Abort;
  end;
  if (NewName<>'') and SymTab.Contains(NewName) and (SymTab.Lookup(NewName)<>C) then
  begin
    ShowMessage('An component with this name already exists: ' + NewName);
    Abort;
  end;
  if Trim(OldName)<>'' then
    SymTab.Remove(OldName);
  if NewName<>'' then
    SymTab.Add(NewName,C);
end;

procedure TEditorForm.CopyComponentActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Tree.Focused and
    Assigned(Tree.Selected) and
    Assigned(Tree.ZSelected.Component) and
    (Tree.ZSelected.Component<>Root);
end;

procedure TEditorForm.CopyComponentActionExecute(Sender: TObject);
var
  Stream : TMemoryStream;
  S : string;
begin
  Stream := ComponentManager.SaveXmlToStream(Tree.ZSelected.Component) as TMemoryStream;
  try
    SetLength(S,Stream.Size);
    Stream.Position := 0;
    Stream.Read(S[1],Stream.Size);
    S := 'ZZDC' + S;
    Clipboard.SetTextBuf(PChar(S));
  finally
    Stream.Free;
  end;
end;

procedure TEditorForm.Import3dsActionExecute(Sender: TObject);
var
  D : TOpenDialog;
  Imp : T3dsImport;
  Parent,Node : TZComponentTreeNode;
begin
  D := TOpenDialog.Create(Self);
  try
    D.Filter := '3D-studio files (*.3ds)|*.3ds';
    D.DefaultExt := '*.3ds';
    if not D.Execute then
      Exit;
    Imp := T3dsImport.Create(D.FileName);
    try
      //ZApp.Content.AddComponent( Imp.Import );
      Parent := Tree.FindNodeForComponentList(ZApp.Content);
      Assert(Parent<>nil,'Can''t find app.content node');
      Node := InsertAndRenameComponent(Imp.Import, Parent);
      Node.Expand(False);
      //Auto-select the Model-component
      Tree.Selected := Node.GetLastChild.GetLastChild;
    finally
      Imp.Free;
    end;
  finally
    D.Free;
  end;
end;

function TEditorForm.InsertAndRenameComponent(InsertC : TZComponent;
  DestTreeNode : TZComponentTreeNode) : TZComponentTreeNode;
var
  DestList : TZComponentList;
  C : TZComponent;
  List : TObjectList;
  I,J,FirstCopyNr : integer;
  NewName,StartName,S : string;
  NewNameFound : boolean;
begin
  Result := nil;
  DestList := DestTreeNode.ComponentList;
  List := TObjectList.Create(False);
  try
    GetAllObjects(InsertC,List);
    //Loop all objects in clone and rename components that would produce duplicate names
    for I := 0 to List.Count-1 do
    begin
      //Name     CopyName
      //(blank)  (blank)
      //Button   Button (ifall ledigt)
      //Button   Button1
      //Button58 Button59
      C := List[I] as TZComponent;
      if C.Name='' then
        Continue;

      if SymTab.Contains(C.Name) then
      begin
        StartName := C.Name;
        S := '';
        while (Length(StartName)>0) and (StartName[Length(StartName)] in ['0'..'9']) do
        begin
          S := StartName[Length(StartName)] + S;
          Delete(StartName,Length(StartName),1);
        end;
        FirstCopyNr := StrToIntDef(S,0);
        NewNameFound := False;
        for J := FirstCopyNr + 1 to FirstCopyNr + 1000 do
        begin
          NewName := StartName + IntToStr(J);
          if not SymTab.Contains(NewName) then
          begin
            C.Name := NewName;
            NewNameFound := True;
            Break;
          end;
        end;
        if not NewNameFound then
        begin
          ShowMessage('Could not find unique name for component: ' + C.Name);
          Exit;
        end;
      end;

      SymTab.Add(C.Name,C);
    end;
  finally
    List.Free;
  end;
  DestList.AddComponent(InsertC);
  SetFileChanged(True);
  Result := Tree.AddNode(InsertC,DestTreeNode)as TZComponentTreeNode;
end;

procedure TEditorForm.PasteComponentActionExecute(Sender: TObject);
var
  DestTreeNode : TZComponentTreeNode;
  S : string;
  C : TZComponent;
begin
  DestTreeNode := Tree.ZSelected;

  if Clipboard.HasFormat(CF_TEXT) then
  begin
    S := Clipboard.AsText;
    if Copy(S,1,4)='ZZDC' then
    begin
      Delete(S,1,4);
      C := ComponentManager.LoadXmlFromString(S,SymTab);
      InsertAndRenameComponent(C,DestTreeNode);
    end;
  end;
end;

procedure TEditorForm.PasteComponentActionUpdate(Sender: TObject);
  function HasData :  boolean;
  begin
    Result := False;
    if Clipboard.HasFormat(CF_TEXT) then
    begin
      if Copy(Clipboard.AsText,1,4)='ZZDC' then
        Result := True;
    end;
  end;
begin
  (Sender as TAction).Enabled := Tree.Focused and
    Assigned(Tree.Selected) and
    Assigned(Tree.ZSelected.ComponentList) and
    HasData;
end;

procedure TEditorForm.MoveUpComponentActionExecute(Sender: TObject);
var
  C,Tmp : TObject;
  L : TZComponentList;
  I : integer;
begin
  C:=Tree.ZSelected.Component;
  L:=(Tree.Selected.Parent as TZComponentTreeNode).ComponentList;
  I := L.IndexOf(C);
  Tmp := L[I-1];
  L[I-1] := C;
  L[I] := Tmp;
  Tree.Selected.MoveTo(Tree.Selected.Parent.Item[Tree.Selected.Index-1],naInsert);
  SetFileChanged(True);
end;

procedure TEditorForm.MoveDownComponentActionExecute(Sender: TObject);
var
  C,Tmp : TObject;
  L : TZComponentList;
  I : integer;
begin
  C:=Tree.ZSelected.Component;
  L:=(Tree.Selected.Parent as TZComponentTreeNode).ComponentList;
  I := L.IndexOf(C);
  Tmp := L[I+1];
  L[I+1] := C;
  L[I] := Tmp;
  if I<L.Count-2 then
    Tree.Selected.MoveTo(Tree.Selected.Parent.Item[Tree.Selected.Index+2],naInsert)
  else
    Tree.Selected.MoveTo(Tree.Selected.Parent,naAddChild);
  SetFileChanged(True);
end;

procedure TEditorForm.MoveUpComponentActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Tree.Focused and
    Assigned(Tree.Selected) and
    Assigned(Tree.ZSelected.Component) and
    (Tree.Selected.Index>0);
end;

function TEditorForm.CloseProject : boolean;
begin
  Result := True;
  if not Assigned(Root) then
    Exit;

  if _FileChanged then
  begin
    case Application.MessageBox('File has changed. Save changes?', PChar(Self.Caption), MB_YESNOCANCEL) of
      IDYES :  SaveProjectAction.Execute;
      IDNO :  ;
      IDCANCEL :
        begin
          Result := False;
          Exit;
        end;
    end;
  end;

  WriteProjectSettingsToIni;

  SetShowNode(nil);

  Tree.Selected := nil;
  LockShow := False;
//  Selected := nil;
  SelectComponent(nil);

  if IsAppRunning then
    AppPreviewStopAction.Execute;
  ZApp.Terminate;
  Root.Free;
  Root := nil;
  ZApp := nil;
end;

procedure TEditorForm.NewProject;
begin
  if CloseProject then
    OpenProject('');
end;

procedure TEditorForm.NewProjectActionExecute(Sender: TObject);
begin
  NewProject;
end;

procedure TEditorForm.NormalsCheckBoxClick(Sender: TObject);
begin
  Renderer.NormalsVisible := (Sender as TCheckBox).Checked;
end;

procedure TEditorForm.MoveDownComponentActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Tree.Focused and
    Assigned(Tree.Selected) and
    Assigned(Tree.ZSelected.Component) and
    Assigned(Tree.Selected.Parent) and
    (Tree.Selected.Index<Tree.Selected.Parent.Count-1);
end;

procedure TEditorForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and (not (ActiveControl is TSynEdit)) and
    (not (ActiveControl is TCustomMemo)) and
    (not IsAppRunning) then
  begin
    //Enter as tab
    Key := #0; // Eat the Beep
    SelectNext(ActiveControl AS TWinControl, True, True) // Forward
  end;
  if Assigned(CompEditor) then
    CompEditor.OnKeyPress(Key);
end;


var
  LockMouse : TPoint;

procedure TEditorForm.OnGLPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Tmp : TPoint;
  DeltaX,DeltaY : integer;
begin
  //From ESS-doll
  if not Glp.MouseCapture then
    Exit;

  Tmp := Glp.ClientToScreen( Point(X,Y) );
  DeltaX := tmp.X - lockmouse.X;
  DeltaY := tmp.Y - lockmouse.Y;

  if ((Abs(deltaX)+Abs(deltaY))>2) then
  begin
    if (Shift = [ssLeft]) then
    begin
      // Moving the mouse on the X axis should move the doll around the Y axis.
      ViewRotate[1] := Frac((ViewRotate[1] - (deltaX/4))/360)*360;
      // Moving the mouse on the Y axis should move the doll around the X axis.
      ViewRotate[0] := Frac((ViewRotate[0] - (deltaY/4))/360)*360;
    end else if (Shift = [ssRight]) then
    begin
      ViewTranslate[0] := ViewTranslate[0] + deltaX/120;
      ViewTranslate[1] := ViewTranslate[1] - deltaY/120;
    end else if (Shift = [ssRight,ssLeft]) then
    begin
      ViewTranslate[2] := ViewTranslate[2] - (deltaY/120);
    end;
    SetCursorPos(LockMouse.X,LockMouse.Y);
  end;

end;

procedure TEditorForm.OnGLPanelMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Glp.MouseInClient then
  begin
    ViewTranslate[2] := ViewTranslate[2] + (WheelDelta/240);
    Handled := True;
  end
  else
    Handled := False;
end;

procedure TEditorForm.Onlinehelp1Click(Sender: TObject);
begin
  uHelp.ShowHelp('');
end;

procedure TEditorForm.OnGLPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Glp.MouseCapture := True;
  LockMouse := Glp.ClientToScreen( Point(X,Y) );
  ShowCursor(False);
end;

procedure TEditorForm.OnGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Glp.MouseCapture := False;
  ShowCursor(True);
  Glp.SetFocus;
end;

procedure TEditorForm.LogListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  LogColors : array[0..5] of TColor =
// ($0D00C4,$BE00C1,$BF0069,$BC0003,$BA4700,$404040);
//  (clRed,clGreen,clAqua,clBlue,clOlive,clNavy);
  ($BAFFCA,$A647FF,$0ACCA2,$C0C0C0,$07997B,$909090);
  LogChars : string = 'ox#/\+-*';
var
  C : TCanvas;
  Log : TLog;
  S : string;
  I : integer;
begin
  Log := (Control as TListBox).Items.Objects[Index] as TLog;
  if Log=nil then
    Exit;

  C := (Control as TListBox).Canvas;

  //Clear area in listbox-color, this avoids highlighting selected
  C.Brush.Color := (Control as TListBox).Color;
  C.FillRect(Rect);

//  C.Font.Style:=[fsBold];
  C.Font.Color := clWhite;
  C.TextOut(Rect.Left + 2, Rect.Top, Copy(LogChars[ (Log.ID mod Length(LogChars))+1 ] ,1,1));

  C.Font.Color := LogColors[ Log.ID mod High(LogColors) ];
  S := (Control as TListBox).Items[Index];
  C.TextOut(Rect.Left + 12, Rect.Top, S);
  I := C.TextWidth(S) + 16;
  if I>(Control as TListBox).ScrollWidth then
    (Control as TListBox).ScrollWidth := I;
end;

procedure TEditorForm.SaveProjectActionExecute(Sender: TObject);
begin
//  if MessageDlg('Save to file: ' + CurrentFileName + '?',mtConfirmation,mbOKCancel,0)<>mrOk then
//    Exit;
  if CurrentFileName='' then
    FileSaveAsAction.Execute
  else
    ComponentManager.SaveXml(Root,CurrentFileName);
  SetFileChanged(False);
end;



procedure TEditorForm.ForumsMenuItemsClick(Sender: TObject);
begin
  GoUrl('http://www.emix8.org/forum/');
end;

end.