{Copyright (c) 2008-2012 Ville Krumlinde

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
  uSymTab, frmMusicEdit, ZLog, Buttons, StdActns, ExtCtrls,
  ToolWin, SynCompletionProposal, frmBitmapEdit, frmMeshEdit, unitPEFile,
  Jpeg, Vcl.Themes, ZApplication;

type
  TBuildBinaryKind = (bbNormal,bbNormalUncompressed,bbScreenSaver,bbScreenSaverUncompressed,
    bbNormalLinux,bbNormalOsx86,bbNormalAndroid);

  TEditorForm = class(TForm)
    SaveDialog: TSaveDialog;
    Timer1: TTimer;
    LeftPanel: TPanel;
    TreePanel: TGroupBox;
    Splitter1: TSplitter;
    CustomPropEditorsPageControl: TPageControl;
    TabSheet1: TTabSheet;
    TrackBar1: TTrackBar;
    TabSheet2: TTabSheet;
    PropEditorPanel: TGroupBox;
    ViewerPanel: TPanel;
    Splitter2: TSplitter;
    Label1: TLabel;
    TabSheet3: TTabSheet;
    ExprCompileButton: TButton;
    Splitter3: TSplitter;
    ExprPanel: TGroupBox;
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
    NewProjectMenuItem: TMenuItem;
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
    AppPreviewStopAction: TAction;
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
    Import3dsAction: TAction;
    Import3dsAction1: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    ViewTranslateLabel: TLabel;
    ShaderTabSheet: TTabSheet;
    CompileShaderButton: TButton;
    ShaderPanel: TGroupBox;
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
    ShowCompilerDetailsAction: TAction;
    N10: TMenuItem;
    N11: TMenuItem;
    Panel2: TPanel;
    N12: TMenuItem;
    UndoDeleteAction: TAction;
    Undodelete1: TMenuItem;
    AddFromLibraryMenuItem: TMenuItem;
    ViewerBitmapTabSheet: TTabSheet;
    BitmapEditFrame1: TBitmapEditFrame;
    Import3DSfile1: TMenuItem;
    ViewerMeshTabSheet: TTabSheet;
    MeshEditFrame1: TMeshEditFrame;
    RemoveUnusedMenuItem: TMenuItem;
    LogPopupMenu: TPopupMenu;
    LogCopytoclipboardMenuItem: TMenuItem;
    ForceRefreshAction: TAction;
    Refresh1: TMenuItem;
    Contents1: TMenuItem;
    HelpContentsAction: TAction;
    N13: TMenuItem;
    DetailedBuildReportMenuItem: TMenuItem;
    EditXmlAction: TAction;
    EditasXML1: TMenuItem;
    ToolButton14: TToolButton;
    ToolButton17: TToolButton;
    Edit1: TMenuItem;
    Undodelete2: TMenuItem;
    N14: TMenuItem;
    DisableComponentAction: TAction;
    Disablecomponent1: TMenuItem;
    BoundsCheckBox: TCheckBox;
    DisableShadersCheckBox: TCheckBox;
    DisableFBOCheckBox: TCheckBox;
    StyleMenuItem: TMenuItem;
    OpenStyleMenuItem: TMenuItem;
    N15: TMenuItem;
    OpenStyleDialog: TOpenDialog;
    AppStartButton: TButton;
    AppStopButton: TButton;
    emptyproject1: TMenuItem;
    N16: TMenuItem;
    GenerateAndroidAction: TAction;
    BuildAndroidbinaryexperimental1: TMenuItem;
    AndroidRunAction: TAction;
    AndroidRunproject1: TMenuItem;
    N17: TMenuItem;
    AndroidBuildDebugApkAction: TAction;
    AndroidBuildAPK1: TMenuItem;
    AndroidBuildReleaseApkAction: TAction;
    AndroidBuildAPKrelease1: TMenuItem;
    LogClearMenuItem: TMenuItem;
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
    procedure Import3dsActionExecute(Sender: TObject);
    procedure CompileShaderButtonClick(Sender: TObject);
    procedure GenerateReleaseLinuxActionExecute(Sender: TObject);
    procedure GenerateReleaseOsx86ActionExecute(Sender: TObject);
    procedure FileSaveAsActionBeforeExecute(Sender: TObject);
    procedure ShowSettingsActionExecute(Sender: TObject);
    procedure FindComponentActionExecute(Sender: TObject);
    procedure NormalsCheckBoxClick(Sender: TObject);
    procedure ShowCompilerDetailsActionExecute(Sender: TObject);
    procedure UndoDeleteActionUpdate(Sender: TObject);
    procedure UndoDeleteActionExecute(Sender: TObject);
    procedure AddFromLibraryMenuItemClick(Sender: TObject);
    procedure LogListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LogCopytoclipboardMenuItemClick(Sender: TObject);
    procedure ForceRefreshActionExecute(Sender: TObject);
    procedure ExprPanelClick(Sender: TObject);
    procedure HelpContentsActionExecute(Sender: TObject);
    procedure EditXmlActionExecute(Sender: TObject);
    procedure DisableComponentActionExecute(Sender: TObject);
    procedure DisableComponentActionUpdate(Sender: TObject);
    procedure BoundsCheckBoxClick(Sender: TObject);
    procedure DisableShadersCheckBoxClick(Sender: TObject);
    procedure DisableFBOCheckBoxClick(Sender: TObject);
    procedure OnChooseStyleMenuItemClick(Sender: TObject);
    procedure OpenStyleMenuItemClick(Sender: TObject);
    procedure GenerateAndroidActionExecute(Sender: TObject);
    procedure AndroidRunActionExecute(Sender: TObject);
    procedure AndroidBuildDebugApkActionExecute(Sender: TObject);
    procedure AndroidBuildReleaseApkActionExecute(Sender: TObject);
    procedure LogClearMenuItemClick(Sender: TObject);
  private
    { Private declarations }
    Ed : TZPropertyEditor;
    Selected,ShowNode : TZComponent;
    LockShow : boolean;
    Root : TZComponent;
    FloatEdit : TEdit;
    MinFloat,MaxFloat : single;
    ExprEditBox : TEdit;
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
    PackerProg,PackerParams : string;
    AndroidSdkPath,AndroidSdCardPath,AndroidAntPath,
    AndroidKeystorePath,AndroidKeystoreAlias : string;
    GuiLayout : integer;
    UndoNodes,UndoIndices : TObjectList;
    UndoParent : TZComponentTreeNode;
    SysLibrary : TZComponent;
    AutoComp,ParamComp : TSynCompletionProposal;
    Log : TLog;
    ZApp : TZApplication;
    procedure SelectComponent(C : TZComponent);
    procedure DrawZBitmap;
    procedure DrawMesh;
    procedure DrawModel;
    procedure OnGlDraw(Sender : TObject);
    procedure OnPropValueChange;
    procedure OnTreeSelectItem(Sender : TObject; Node : TTreeNode);
    procedure OnTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    function CompileAll(ThrowOnFail : boolean = False) : boolean;
    procedure ReadProjectSettingsFromIni;
    procedure WriteProjectSettingsToIni;
    procedure SetShowNode(Node : TZComponent);
    procedure GlWindowProc(var Message: TMessage);
    procedure OnTreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure OnTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoCompile(Node: TZComponentTreeNode;  const Expr: TZPropertyValue; Prop : TZProperty);
    procedure OnGLPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnGLPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnGLPanelMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    function InsertAndRenameComponent(InsertC: TZComponent;
      DestTreeNode: TZComponentTreeNode; Index : integer = -1) : TZComponentTreeNode;
    procedure OnReceiveLogMessage(Log: TLog; Mess: TLogString; Level : TLogLevel);
    procedure OpenProject(const FileName: string; const IsTemplate : boolean = False);
    procedure NewProject(const FromTemplate : string = '');
    function CloseProject: boolean;
    procedure OnExprChanged(Sender: TObject);
    procedure BuildBinary(const PlayerName, OutputName: string);
    procedure ExecToolAndWait(const ExeFile, ParamString: string);
    function BuildRelease(Kind : TBuildBinaryKind) : string;
    procedure ResetCamera;
    procedure ReadAppSettingsFromIni;
    procedure WriteAppSettingsToIni;
    function VerifyToolExists(const ToolName, ToolUrl, ExeFile : string): boolean;
    procedure SetCurrentFileName(const F : string);
    procedure ReplaceResource(const ExeFile, OutFile, DataFile: string; UseCodeRemoval : boolean);
    procedure RefreshMenuFromMruList;
    procedure OnMruItemClick(Sender: TObject);
    procedure DrawOnRenderComponent;
    procedure WipeUndoHistory;
    procedure LoadSysLibrary;
    procedure OnAddFromLibraryItemClick(Sender: TObject);
    procedure AddNewComponentToTree(C: TZComponent);
    procedure AutoCompOnExecute(Kind: SynCompletionType; Sender: TObject;  var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure ParamAutoCompOnExecute(Kind: SynCompletionType; Sender: TObject;  var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure OnShaderExprChanged(Sender: TObject);
    procedure DoChangeTreeFocus(var Message : TMessage); message WM_USER + 1;
    procedure OnGlInit(Sender: TObject);
    procedure OnAppException(Sender: TObject; E: Exception);
    procedure RemoveUnusedCode(Module: TPEModule);
    procedure FindCurrentModel(Node: TZComponentTreeNode; var Model: TZComponent);
    procedure ClearRoot;
    procedure SetRoot(C: TZComponent);
    procedure SaveCurrentEdits;
    procedure BuildStyleMenu;
    procedure SwitchToStyle(const StyleName: string; const StyleHandle : TStyleManager.TStyleServicesHandle);
    procedure OnTreeRecreate(Sender : TObject);
    function OnGetLibraryPath: string;
    procedure FillNewMenuTemplateItems;
    procedure BuildAndroidApk(const IsDebug : boolean);
  public
    Glp : TGLPanel;
    Tree : TZComponentTreeView;
    procedure SetFileChanged(Value : Boolean);
    //Custom editors
    procedure ShowFloatEditor(Edit : TEdit; IsScalar : boolean);
    procedure ShowExprEditor(Edit : TEdit);
    procedure ShowShaderEditor(Edit : TEdit);
    procedure HideEditor;
    procedure ValidateNewName(const OldName,NewName : string);
    procedure FindComponentAndFocusInTree(const CName: string); overload;
    procedure FindComponentAndFocusInTree(C: TZComponent); overload;
    procedure RefreshCompEditorTreeNode;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  EditorForm : TEditorForm;

const
  AppName = 'ZGameEditor';
  AppVersion = '3.1b';
  ZgeProjExtension = '.zgeproj';

implementation

{$R *.dfm}

uses Math, ZOpenGL, BitmapProducers, ZBitmap, Meshes, Renderer, Compiler, ZExpressions,
  ShellApi, SynEditHighlighter, SynHighlighterCpp, SynHighlighterZc,frmSelectComponent, AudioComponents, IniFiles, ZPlatform,
  dmCommon, frmAbout, uHelp, frmToolMissing, Clipbrd, unitResourceDetails,
  u3dsFile, AudioPlayer, frmSettings, unitResourceGraphics, Zc_Ops,
  SynEditTypes, SynEditSearch, frmXmlEdit, frmArrayEdit, System.Types, System.IOUtils,
  Generics.Collections, frmAndroidApk;

{ TEditorForm }

constructor TEditorForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ZLog.SetReceiverFunc(OnReceiveLogMessage);

  Math.SetExceptionMask(exAllArithmeticExceptions);

  Self.Log := ZLog.GetLog(Self.ClassName);
  Log.Write( IntToStr(SizeOf(Pointer)*8) + ' bit version' );

  //Zc expressions needs '.' set
  Application.UpdateFormatSettings := False;
  FormatSettings.DecimalSeparator := '.';

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
  Tree.MultiSelect := True;
  Tree.MultiSelectStyle := [msControlSelect, msShiftSelect, msSiblingOnly];
  Tree.OnRecreate := OnTreeRecreate;

  //InitAudio needs hwnd of main window to work
  Platform_DesignerSetDC(0,Self.Handle);
  Platform_InitAudio;

  Glp := TGLPanel.Create(Self);
  Glp.Align := alClient;
  Glp.OnGLDraw := Self.OnGlDraw;
  //Byt ut windowproc mot vår platform_windowproc
  OldGlWindowProc := Glp.WindowProc;
  Glp.WindowProc := GlWindowProc;
  Glp.OnMouseDown := OnGLPanelMouseDown;
  Glp.OnMouseUp := OnGLPanelMouseUp;
  Glp.OnMouseMove := OnGLPanelMouseMove;
  Glp.TabStop := True;
  Glp.OnGlInit := Self.OnGlInit;
  Glp.Parent := ViewerGlTabSheet;
  Glp.ForceInitGL;
  //Mousewheel måste sättas på formuläret annars tar det inte
  //Glp.OnMouseWheel := OnGLPanelMouseWheel;
  Self.OnMouseWheel := OnGLPanelMouseWheel;

  ExePath := ExtractFilePath(Application.ExeName);
  SaveDialog.InitialDir := ExePath + 'Projects';

  ExprSynEdit := TSynEdit.Create(Self);
  ExprSynEdit.Align := alClient;
  ExprSynEdit.Gutter.Visible := True;
  ExprSynEdit.Gutter.ShowLineNumbers := True;
  ExprSynEdit.Parent := ExprPanel;
  ExprSynEdit.OnChange := OnExprChanged;
  ExprSynEdit.Highlighter := TSynZcSyn.Create(Self);
  ExprSynEdit.WantTabs := True;
  ExprSynEdit.TabWidth := 2;
  ExprSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  ExprSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ExprSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;

  //SynEdit autocompletion
  AutoComp := TSynCompletionProposal.Create(Self);
  AutoComp.Editor := ExprSynEdit;
  AutoComp.EndOfTokenChr := '+-/*=()[]. @';
  AutoComp.TriggerChars := 'abcdefghijklmnopqrstuvxyz.@';
  AutoComp.ShortCut := 16416;
  AutoComp.OnExecute := AutoCompOnExecute;
  AutoComp.Options := DefaultProposalOptions + [scoCaseSensitive,scoUseBuiltInTimer,scoUseInsertList,scoUsePrettyText];
  AutoComp.TimerInterval := 2000;

  //SynEdit autocompletion for parameters
  ParamComp := TSynCompletionProposal.Create(Self);
  ParamComp.DefaultType := ctParams;
  ParamComp.Options := [scoLimitToMatchedText, scoUseBuiltInTimer];
  ParamComp.TriggerChars := '(';
  ParamComp.EndOfTokenChr := '';
  ParamComp.ShortCut := 24608;
  ParamComp.Editor := ExprSynEdit;
  ParamComp.OnExecute := ParamAutoCompOnExecute;
  ParamComp.TimerInterval := 2000;

  ShaderSynEdit := TSynEdit.Create(Self);
  ShaderSynEdit.Highlighter := TSynCppSyn.Create(Self);
  ShaderSynEdit.Align := alClient;
  ShaderSynEdit.Gutter.Visible := True;
  ShaderSynEdit.Gutter.ShowLineNumbers := True;
  ShaderSynEdit.Parent := ShaderPanel;
  ShaderSynEdit.OnChange := OnShaderExprChanged;
  ShaderSynEdit.WantTabs := True;
  ShaderSynEdit.TabWidth := 2;
  ShaderSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  ShaderSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ShaderSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;

  Application.HelpFile := ExePath + 'ZGameEditor.chm';

  ResetCamera;

  AboutAction.Caption := 'About ' + AppName;

  MruList := TStringList.Create;
  MruList.StrictDelimiter := True;
  MruList.Duplicates := dupIgnore;

  Platform_InitGlobals;  //Nollställ timer etc

  Application.OnException := OnAppException;

  SaveBinaryMenuItem.Visible := DebugHook<>0;
  ShowCompilerDetailsAction.Checked := DebugHook<>0;
  DetailedBuildReportMenuItem.Visible := DebugHook<>0;

  UndoNodes := TObjectList.Create(True);
  UndoIndices := TObjectList.Create(False);

  BuildStyleMenu;
  ReadAppSettingsFromIni;
  RefreshMenuFromMruList;
  FillNewMenuTemplateItems;
end;

procedure OnTaskdialogHyperlinkClick(this : pointer; sender : tobject);
begin
  GoUrl( (Sender as TTaskDialog).URL );
end;

procedure ShowMessageWithLink(const S1,S2 : string);
var
  D: TTaskDialog;
  M : TMethod;
begin
  if (Win32MajorVersion >= 6) then
  begin
    //Display dialog with hyperlinks on Vista or higher
    D := TTaskDialog.Create(nil);
    try
      D.Flags := [tfEnableHyperlinks,tfUseHiconMain];
      D.Text := S1 + #13#13 + S2;
      D.CommonButtons := [tcbOk];
      D.Title := AppName;
      D.Caption := AppName;
      D.CustomMainIcon := Application.Icon;
      M.Data := D;
      M.Code := @OnTaskdialogHyperlinkClick;
      D.OnHyperlinkClicked := TNotifyEvent(M);
      D.Execute;
    finally
      D.Free;
    end;
  end else
    //Display without hyperlinks
    ShowMessage(S1);
end;

procedure TEditorForm.FillNewMenuTemplateItems;
var
  Name : string;
  Mi : TMenuItem;
begin
  for Name in TDirectory.GetFiles(ExePath + 'Templates','*.zgeproj') do
  begin
    Mi := TMenuItem.Create(Self);
    Mi.Action := NewProjectAction;
    Mi.ShortCut := 0;
    Mi.Hint := Name;
    Mi.Caption := TPath.GetFileNameWithoutExtension(Name);
    NewProjectMenuItem.Add(Mi);
  end;
end;


procedure TEditorForm.OnAppException(Sender : TObject; E: Exception);
begin
  if E is EZHalted then
  begin
    Log.Error(E.Message);
    ZExpressions.ResetScriptState;
  end
  else
    Application.ShowException(E);
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

procedure TEditorForm.OpenProject(const FileName : string; const IsTemplate : boolean = False);
var
  C : TZComponent;

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
    C := InNewProject;
    SetCurrentFileName('');
  end
  else
  begin
    try
      C := ComponentManager.LoadXmlFromFile( FileName );
    except
      on E : Exception do
        begin
          //Try to recover by creating a new project
          OpenProject('');
          ShowMessage(E.Message);
          Exit;
        end;
    end;
    if IsTemplate then
      SetCurrentFileName('')
    else
      SetCurrentFileName(FileName);
  end;

  //Assign ZApp-global to current application
  SetRoot(C);

  //Read settings last. Must be after compileall because it selects nodes which
  //will call RefreshContent that requires to have expressions already compiled.
  ReadProjectSettingsFromIni;
  Tree.Invalidate;
end;

function TEditorForm.OnGetLibraryPath : string;
begin
  Result := Self.ExePath + 'Lib'
end;

procedure TEditorForm.SetRoot(C : TZComponent);
begin
  Self.Root := C;

  ZApp := Root as TZApplication;
  ZApp.OnGetLibraryPath := Self.OnGetLibraryPath;
  Ed.RootComponent := Self.Root;
  Tree.SetRootComponent(Self.Root);
//  SelectComponent(Self.Root);

  //Needed for platform wndproc
  SetWindowLongPtr(Glp.Handle,GWL_USERDATA, NativeInt(ZApp) );

  ZApp.RefreshSymbolTable;

  //Sätt till nytt värde så att form.caption ändras
  _FileChanged := True;
  SetFileChanged(False);

  ViewerPageControl.ActivePage := ViewerBlankTabSheet;

  //Must compile directly after load because no zc-instructions are saved in the xml
  CompileAll;

  //Initial tree update. Must be after compileall.
  //Slows down opening project but without this call the walls in FpsDemo
  //become black when WallModel is selected.
  if Self.Visible then
  begin
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    Root.Update;
    CheckGLError;
  end;
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

type
  TListLogItem = class
  public
    Level : TLogLevel;
    Log : TLog;
    Msg : string;
  end;

procedure TEditorForm.OnReceiveLogMessage(Log : TLog; Mess : TLogString; Level : TLogLevel);
var
  I : integer;
  Tmp : TStringList;

  procedure InAddOne(const S : String);
  var
    Data : TListLogItem;
  begin
    while LogListBox.Items.Count>2000 do
    begin
      LogListBox.Items.Objects[0].Free;
      LogListBox.Items.Delete(0);
    end;
    Data := TListLogItem.Create;
    Data.Log := Log;
    Data.Msg := Mess;
    Data.Level := Level;
    LogListBox.Items.AddObject(S,Data);
  end;

begin
  if Pos(#12,Mess)>0 then
  begin
    LogClearMenuItemClick(nil);
    Mess := StringReplace(Mess,#12,'',[rfReplaceAll]);
  end;
  if Pos(#10,Mess)=0 then
    InAddOne(Mess)
  else
  begin
    Tmp := TStringList.Create;
    Tmp.Text := Mess;
    for I := 0 to Tmp.Count - 1 do
      InAddOne(Tmp[I]);
    Tmp.Free;
  end;
  LogListBox.ItemIndex := LogListBox.Items.Count-1;
  LogListBox.Invalidate;
end;

procedure TEditorForm.ReadAppSettingsFromIni;
var
  Ini : TIniFile;
  Section,S : string;
begin
  Ini := TIniFile.Create( ChangeFileExt(Application.ExeName,'.ini') );
  try
    Section := 'Designer';

    Self.Width := Max(Ini.ReadInteger(Section,'Width',Self.Width),100);
    Self.Height := Max(Ini.ReadInteger(Section,'Height',Self.Height),100);
    if Ini.ReadBool(Section,'IsMaximized',False) then
      Self.WindowState:=wsMaximized;

    GuiLayout := Min(Ini.ReadInteger(Section,'GuiLayout',0),1);
    if GuiLayout=0 then
    begin
      PropEditorPanel.Parent := LeftPanel;
      PropEditorPanel.Align := alBottom;
      TreePanel.Align := alClient;
      Splitter3.Parent := LeftPanel;
      Splitter3.Align := alBottom;
      Splitter3.Cursor := crVSplit;
      PropEditorPanel.Height := Self.Height div 2;
    end;

    S := Ini.ReadString(Section, 'Style', TStyleManager.ActiveStyle.Name);
    if S<>TStyleManager.ActiveStyle.Name then
      SwitchToStyle(S,nil);

    if (ParamCount=1) and FileExists(ParamStr(1)) then
      OpenProject(ParamStr(1))
    else
    begin
      S := Ini.ReadString(Section,'LastOpenedProject','');
      if (S<>'') and (CurrentFileName='') and (not FindCmdLineSwitch('blank')) then
      begin
        CloseProject;
        OpenProject(S);
      end else
        NewProject;
    end;

    S := Ini.ReadString(Section,'LastOpenedPath', '');
    if S='' then
      S := Self.ExePath + 'Projects';
    FileOpenAction.Dialog.InitialDir := S;

    S := Ini.ReadString(Section,'MruList', '');
    MruList.CommaText := S;

    LowerRightPanel.Height := Max(Ini.ReadInteger(Section,'LowerRightPanel.Height',LowerRightPanel.Height),100);
    LogPanel.Width := Max(Ini.ReadInteger(Section,'LogPanel.Width',LogPanel.Width),20);
    LeftPanel.Width := Max(Ini.ReadInteger(Section,'LeftPanel.Width',LeftPanel.Width),20);

    Self.PackerProg := Ini.ReadString(Section,'PackerProg','{$toolpath}upx.exe');
    Self.PackerParams := Ini.ReadString(Section,'PackerParams','{$exename}');

    Self.AutoComp.TimerInterval := Ini.ReadInteger(Section,'CodeCompletionDelay',2000);
    Self.ParamComp.TimerInterval := Self.AutoComp.TimerInterval;

    Self.AndroidSdkPath := Ini.ReadString(Section,'AndroidSdkPath','');
    Self.AndroidSdCardPath := Ini.ReadString(Section,'AndroidSdCardPath','/sdcard/');
    Self.AndroidAntPath := Ini.ReadString(Section,'AndroidAntPath','');

    Self.AndroidKeystorePath := Ini.ReadString(Section,'AndroidKeystorePath','');
    Self.AndroidKeystoreAlias := Ini.ReadString(Section,'AndroidKeystoreAlias','');
  finally
    Ini.Free;
  end;
end;

procedure TEditorForm.WipeUndoHistory;
begin
  UndoNodes.Clear;
  UndoIndices.Clear;
  UndoParent := nil;
end;

procedure TEditorForm.WriteAppSettingsToIni;
var
  Ini : TIniFile;
  Section,S,FName : string;
begin
  FName := ChangeFileExt(Application.ExeName,'.ini');
  Ini := TIniFile.Create( FName );
  try
    try
      Section := 'Designer';
      Ini.WriteString(Section,'LastOpenedProject',CurrentFileName);

      Ini.WriteInteger(Section,'GuiLayout',GuiLayout);

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
      Ini.WriteInteger(Section,'LeftPanel.Width',LeftPanel.Width);

      Ini.WriteString(Section,'PackerProg', Self.PackerProg);
      Ini.WriteString(Section,'PackerParams', Self.PackerParams);

      Ini.WriteInteger(Section,'CodeCompletionDelay',Self.AutoComp.TimerInterval);

      Ini.WriteString(Section,'Style', TStyleManager.ActiveStyle.Name);

      Ini.WriteString(Section,'AndroidSdkPath',Self.AndroidSdkPath);
      Ini.WriteString(Section,'AndroidSdCardPath',Self.AndroidSdCardPath);
      Ini.WriteString(Section,'AndroidAntPath',Self.AndroidAntPath);

      Ini.WriteString(Section,'AndroidKeystorePath',Self.AndroidKeystorePath);
      Ini.WriteString(Section,'AndroidKeystoreAlias',Self.AndroidKeystoreAlias);
    except
      ShowMessage('Could not save settings to file: ' + FName);
    end;
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

procedure TEditorForm.OnGlInit(Sender: TObject);
var
  P : PAnsiChar;
  I : integer;
begin
  Renderer.InitRenderer;

  DisableShadersCheckBox.Enabled := ShadersSupported;
  DisableFBOCheckBox.Enabled := FbosSupported;

  if not ShadersSupported then
    Log.Write('GL shaders not supported')
  else
  begin
    P := glGetString(GL_SHADING_LANGUAGE_VERSION);
    if P<>nil then
      Log.Write('GL shaders: ' + P)
  end;
  if not MultiTextureSupported then
    Log.Write('GL multitexture not supported')
  else
  begin
    glGetIntegerv(GL_MAX_TEXTURE_UNITS,@I);
    Log.Write('GL texture units: ' + IntToStr(I))
  end;
  if not VbosSupported then
    Log.Write('GL VBOs not supported');
  if not FbosSupported then
    Log.Write('GL FBOs not supported');
end;

procedure TEditorForm.OnGlDraw(Sender: TObject);
begin
  if ShowNode=nil then
    Exit;

  //Set window size to make sure camera ratio calculations are correct
  if ZApp<>nil then
  begin
    ZApp.ScreenWidth := Glp.Width;
    ZApp.ScreenHeight := Glp.Height;
  end;

  if (ShowNode is TZApplication) and (IsAppRunning) then
  begin
    try
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
//    Root.Update;
    ShowNode.Update;

    glViewport(0, 0, Glp.Width, Glp.Height);

    //todo: delphi 2010 needs this line
    Set8087CW($133F);
    ViewTranslateLabel.Caption := FloatToStr( RoundTo(ViewTranslate[0],-1) ) + #13 +
      FloatToStr( RoundTo(ViewTranslate[1],-1) ) + #13 +
      FloatToStr( RoundTo(ViewTranslate[2],-1) );

    if {(ShowNode is TBitmapProducer) or }(ShowNode is TZBitmap)then
      DrawZBitmap
    else if {(ShowNode is TMeshProducer) or }(ShowNode is TMesh) then
      DrawMesh
    else if (ShowNode is TModel) then
      DrawModel
    else if ((ShowNode is TStateBase) or (ShowNode is TZApplication))then
      DrawOnRenderComponent
    else
    begin
      //Prevent displaying junk
      glClearColor(ZApp.PreviewClearColor.V[0],ZApp.PreviewClearColor.V[1],ZApp.PreviewClearColor.V[2],0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    end;
  end;
end;

procedure TEditorForm.SelectComponent(C: TZComponent);
var
  OldFocus : TWinControl;

  function InCheckForGraphParent : boolean;
  //Do not change shownode if parent is bitmap or mesh
  //This will keep the graph-diagram visible while editing producers
  var
    CurParent : TZComponentTreeNode;
  begin
    Result := False;
    if Tree.ZSelected=nil then
      Exit;
    CurParent := Tree.ZSelected.Parent as TZComponentTreeNode;
    while CurParent<>nil do
    begin
      if (CurParent.Component is TZBitmap) or (CurParent.Component is TMesh) then
      begin
        Result := True;
        Exit;
      end;
      CurParent := CurParent.Parent as TZComponentTreeNode;
    end;
  end;

begin
  Selected := C;
  if (not LockShow) and (not InCheckForGraphParent) then
    SetShowNode(Selected);
  Ed.SetComponent(C);
  RenderAborted := False;
  ViewerPanel.Refresh;
  if (C<>nil) and Assigned(Ed.WantsFocus) and
    Ed.WantsFocus.CanFocus and
    Ed.Parent.Enabled and
    Visible and
    (Self.ActiveControl<>Ed.WantsFocus)
    then
  begin
    //Focus editor to make code-editor visible
    //Then focus back to tree to make tree-navigation with cursorkeys possible
    OldFocus := Self.ActiveControl;
    Ed.WantsFocus.SetFocus;
    if Assigned(OldFocus) and (OldFocus.Visible) then
      Self.ActiveControl := OldFocus;
  end;
end;

procedure TEditorForm.SetShowNode(Node : TZComponent);
begin
  if CompEditor<>nil then
    CompEditor.OnEditorClose;

  if IsAppRunning and (not (Node is TZApplication)) then
    AppPreviewStopAction.Execute;

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
  else if ShowNode is TZBitmap then
  begin
    ViewerPageControl.ActivePage := ViewerBitmapTabSheet;
    CompEditor := BitmapEditFrame1;
  end
  else if ShowNode is TMesh then
  begin
    ViewerPageControl.ActivePage := ViewerMeshTabSheet;
    CompEditor := MeshEditFrame1;
  end
  else if {(ShowNode is TZBitmap) or}
    {(ShowNode is TMesh) or}
    (ShowNode is TZApplication) or
    (ShowNode is TModel) or
    (ShowNode is TStateBase) then
  begin
    RotateModelPanel.Visible := (ShowNode is TModel) or (ShowNode is TMesh);// or (ShowNode is TStateBase);
    AppControlPanel.Visible := ShowNode is TZApplication;
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
begin
  if InputQuery('Find component','Enter name of component to search for',S) then
    FindComponentAndFocusInTree(S);
end;

procedure TEditorForm.DoChangeTreeFocus(var Message : TMessage);
var
  Node : TZComponentTreeNode;
begin
  Node := TZComponentTreeNode(Message.LParam);
  Node.Expand(True);
  Tree.Selected := Node;
  Tree.SetFocus;
end;

procedure TEditorForm.FindComponentAndFocusInTree(const CName : string);
var
  C : TZComponent;
begin
  C := Zapp.SymTab.Lookup(CName) as TZComponent;
  if C<>nil then
    FindComponentAndFocusInTree(C);
end;

procedure TEditorForm.FindComponentAndFocusInTree(C : TZComponent);
var
  I : integer;
  Node : TZComponentTreeNode;
begin
  {
    Find a component then post a win-message to change focus.
    This is neccessary because this method is called from controls in the
    property-editor, and those controls are destroyed when changing tree-focus
    causing access violation.
  }
  for I := 0 to Tree.Items.Count-1 do
  begin
    Node := Tree.Items[I] as TZComponentTreeNode;
    if Node.Component=C then
    begin
      PostMessage(Self.Handle,WM_USER + 1,0,Integer(Node));
      Break;
    end;
  end;
end;

procedure TEditorForm.ForceRefreshActionExecute(Sender: TObject);
begin
  if Assigned(Tree.ZSelected.ComponentList) then
    Tree.ZSelected.ComponentList.Change
  else if Assigned(Tree.ZSelected.Component) then
    Tree.ZSelected.Component.Change;
  Glp.Invalidate;
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I : integer;
begin
  Action := caFree;

  //Need to remove synedit first, otherwise synedit assertion fails
  Self.RemoveComponent(ExprSynEdit);
  ExprSynEdit.Free;
  ExprSynEdit:=nil;

  Self.RemoveComponent(ShaderSynEdit);
  ShaderSynEdit.Free;
  ShaderSynEdit:=nil;

  for I := 0 to LogListBox.Items.Count - 1 do
  begin
    LogListBox.Items.Objects[I].Free;
    LogListBox.Items.Objects[I] := nil;
  end;
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
    SelectComponent( Tree.ZSelected.Component )
  else
    //Dölj property editor om ingen component är selectad
    Ed.SetComponent(nil);
end;

procedure TEditorForm.RefreshCompEditorTreeNode;
begin
  Tree.RefreshNode(CompEditor.TreeNode,CompEditor.Component);
  CompEditor.TreeNode.Expand(False);
  CompEditor.NeedRefreshTreeNode := False;
  SetFileChanged(True);
end;

procedure TEditorForm.OnTreeChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  //Körs innan selected byts
  if (CompEditor<>nil) and CompEditor.NeedRefreshTreeNode then
  begin
    RefreshCompEditorTreeNode();
    AllowChange:=False;
    Exit;
  end;

  //Stop app if another node is selected
  if IsAppRunning and (not LockShow) then
    AppPreviewStopAction.Execute;

  SaveCurrentEdits;

  AllowChange:=True;
end;

procedure TEditorForm.SaveCurrentEdits;
begin
  if Assigned(ActiveControl) and
    (ActiveControl is TEdit) and
    Assigned((ActiveControl as TEdit).OnExit) then
    //Spara stringedit
    (ActiveControl as TEdit).OnExit(ActiveControl)
  else if ActiveControl=ExprSynEdit then
  begin
  end
  else if ActiveControl=ShaderSynEdit then
  begin
  end;

  //Save expression
  if ExprCompileButton.Enabled then ExprCompileButton.Click;

  //Save shader
  if CompileShaderButton.Enabled then CompileShaderButton.Click;
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
  AmbientLight : array[0..3] of single = (0.4, 0.4, 0.4, 1.0);
  //exempel från http://rush3d.com/reference/opengl-redbook-1.1/chapter06.html
  mat_specular : array[0..3] of single = ( 0.2, 0.2, 0.2, 1.0 );
  no_shininess = 0;
  low_shininess = 5;
  high_shininess = 100;
begin
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  glLightModelfv( GL_LIGHT_MODEL_AMBIENT, @AmbientLight );

  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );

  glEnable ( GL_COLOR_MATERIAL );
  glColorMaterial ( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );

  glMaterialfv(GL_FRONT, GL_SPECULAR, @mat_specular);
  glMateriali(GL_FRONT, GL_SHININESS, low_shininess);

  glEnable(GL_CULL_FACE);

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

    if MeshEditFrame1.WireframeCheckBox.Checked then
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


procedure TEditorForm.DrawOnRenderComponent;
var
  OnRender : TZComponentList;
begin
  OnRender := nil;
  if ShowNode is TStateBase then
    OnRender := (ShowNode as TStateBase).OnRender
  else if (ShowNode is TZApplication) then
    OnRender := (ShowNode as TZApplication).OnRender;

  if OnRender=nil then
    Exit;

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  try
    ZApp.DesignerSetUpView;

    Renderer.Render_Begin;
    try
      OnRender.ExecuteCommands;
    except
      on E : EZHalted do
      begin //Detect errors in onrender-list
        RenderAborted := True;
        raise;
      end;
    end;
    Renderer.Render_End;
  finally
    glPopAttrib();
  end;
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

  glClearColor(ZApp.PreviewClearColor.V[0],ZApp.PreviewClearColor.V[1],ZApp.PreviewClearColor.V[2],0);
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
var
  H : HWND;
begin
  //Only draw when topmost (or editing arrays)
  H := GetActiveWindow;
  if ((H=Handle) or (Assigned(ArrayEditForm) and (H=ArrayEditForm.Handle)))
     and (Glp.Tag=0) then
    Glp.Invalidate;
end;

procedure TEditorForm.UndoDeleteActionExecute(Sender: TObject);
var
  C : TZComponent;
  Index : integer;
begin
  while UndoNodes.Count>0 do
  begin
    C := UndoNodes[UndoNodes.Count-1] as TZComponent;
    UndoNodes.Extract( C );

    Index := Integer(UndoIndices[UndoIndices.Count-1]);
    UndoIndices.Delete(UndoIndices.Count-1);

    InsertAndRenameComponent(C,UndoParent,Index);
  end;

  WipeUndoHistory;
end;

procedure TEditorForm.UndoDeleteActionUpdate(Sender: TObject);
begin
  UndoDeleteAction.Enabled := UndoNodes.Count>0;
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
  MruList.Free;
  Renderer.CleanUp;
  UndoNodes.Free;
  UndoIndices.Free;
  SysLibrary.Free;
end;

procedure TEditorForm.FindCurrentModel(Node: TZComponentTreeNode; var Model: TZComponent);
var
  CurParent: TZComponentTreeNode;
begin
  CurParent := Node.Parent as TZComponentTreeNode;
  Model := nil;
  //Om det finns en model-parent så skriv den till symbol 'CurrentModel'
  //så att den kan användas i uttryck.
  while CurParent <> nil do
  begin
    if Assigned(CurParent.Component) and (CurParent.Component is TModel) then
    begin
      Model := CurParent.Component as TModel;
      Break;
    end;
    CurParent := CurParent.Parent as TZComponentTreeNode;
  end;
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

procedure TEditorForm.HelpContentsActionExecute(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TEditorForm.HideEditor;
begin
  CustomPropEditorsPageControl.ActivePageIndex := 0;
end;

procedure TEditorForm.ShowCompilerDetailsActionExecute(Sender: TObject);
begin
  ShowCompilerDetailsAction.Checked := not ShowCompilerDetailsAction.Checked;
end;

procedure TEditorForm.ShowExprEditor(Edit: TEdit);
begin
  ExprEditBox := Edit;
  ExprEditBox.ReadOnly := True;
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
    F.GuiLayoutCombo.ItemIndex := Self.GuiLayout;
    F.UpDown1.Position := Self.AutoComp.TimerInterval;
    F.AndroidSdkPathEdit.Text := Self.AndroidSdkPath;
    F.AndroidSdCardPathEdit.Text := Self.AndroidSdCardPath;
    F.AndroidAntPathEdit.Text := Self.AndroidAntPath;
    F.AndroidKeystorePathEdit.Text := Self.AndroidKeystorePath;
    F.AndroidKeystoreAliasEdit.Text := Self.AndroidKeystoreAlias;
    if F.ShowModal=mrOk then
    begin
      Self.PackerProg := F.PackerEdit.Text;
      Self.PackerParams := F.PackerParamsEdit.Text;
      Self.GuiLayout := F.GuiLayoutCombo.ItemIndex;
      Self.AutoComp.TimerInterval := F.UpDown1.Position;
      Self.ParamComp.TimerInterval := Self.AutoComp.TimerInterval;
      Self.AndroidSdkPath := F.AndroidSdkPathEdit.Text;
      Self.AndroidSdCardPath := F.AndroidSdCardPathEdit.Text;
      Self.AndroidAntPath := F.AndroidAntPathEdit.Text;
      Self.AndroidKeystorePath := F.AndroidKeystorePathEdit.Text;
      Self.AndroidKeystoreAlias := F.AndroidKeystoreAliasEdit.Text;
    end;
  finally
    F.Free;
  end;
end;

procedure TEditorForm.ShowShaderEditor(Edit: TEdit);
begin
  ExprEditBox := Edit;
  ExprEditBox.ReadOnly := True;
  ShaderSynEdit.Text := Edit.Text;
  CompileShaderButton.Enabled := False;
  CustomPropEditorsPageControl.ActivePage := ShaderTabSheet;
end;

procedure TEditorForm.OnExprChanged(Sender: TObject);
begin
  ExprCompileButton.Enabled := True;
end;

procedure TEditorForm.OnShaderExprChanged(Sender: TObject);
begin
  CompileShaderButton.Enabled := True;
end;

procedure TEditorForm.CompileShaderButtonClick(Sender: TObject);
begin
  CompileShaderButton.Enabled := False;
  //Spara ändrad text i edit-ruta
  ExprEditBox.Text := TrimRight(ShaderSynEdit.Text);
  ExprEditBox.OnExit(ExprEditBox);
end;

procedure TEditorForm.ExprCompileButtonClick(Sender: TObject);
var
  C : TZComponent;
  Prop : TZProperty;
  PropValue : TZPropertyValue;
  Success : boolean;
  I : integer;
begin
  ExprCompileButton.Enabled := False;
  //Spara ändrad text i edit-ruta
  ExprEditBox.Text := TrimRight(ExprSynEdit.Text);
  ExprEditBox.OnExit(ExprEditBox);
  //Propvärde har nu sparats i component
  //Läs tillbaka propvärde för kompilering
  C := Selected;
  Prop := C.GetProperties.GetByName(ExprEditBox.Hint);
  C.GetProperty(Prop,PropValue);
  Success:=False;
  try
    if (C is TZLibrary) or (C is TZExternalLibrary) then
      CompileAll(True)
    else
      DoCompile(Tree.ZSelected,PropValue,Prop);
    Success:=True;
  except
    on E : EParseError do
    begin
      ExprSynEdit.CaretXY := BufferCoord(E.Col-1,E.Line);
      ExprSynEdit.BlockBegin := BufferCoord(0,E.Line);
      ExprSynEdit.BlockEnd := BufferCoord(0,E.Line+1);
      ExprSynEdit.SetFocus;
      //ShowMessage( E.Message );
      CompileErrorLabel.Caption := E.Message;
      Log.Write(E.Message);
    end;
    on E : ECodeGenError do
    begin
      CompileErrorLabel.BevelKind := bkTile;
      CompileErrorLabel.Caption := E.Message;
      Log.Write(E.Message);
    end;
  end;
//  Tree.RefreshNode(Tree.Selected,Selected);
  if Success then
  begin
    CompileErrorLabel.Caption := '';
    CompileErrorLabel.BevelKind := bkNone;
    if ShowCompilerDetailsAction.Checked and (not (C is TZExternalLibrary)) then
    begin
      ZLog.GetLog(Self.ClassName).Write(Compiler.CompileDebugString);
      ZLog.GetLog(Self.ClassName).Write('----');
      for I := 0 to PropValue.ExpressionValue.Code.Count - 1 do
        Log.Write( (PropValue.ExpressionValue.Code[I] as TExpBase).ExpAsText );
    end;
  end;
end;

procedure TEditorForm.ExprHelpButtonClick(Sender: TObject);
begin
  HtmlHelp(0,Application.HelpFile + '::writingexpressions.html', HH_DISPLAY_TOPIC, 0);
//  uHelp.ShowHelp('Main/WritingExpressions');
end;

procedure TEditorForm.ExprPanelClick(Sender: TObject);
var
  X : integer;
begin
  X := LowerRightPanel.Height;
  LowerRightPanel.Height := Panel2.Height;
  Panel2.Height := X;
end;

procedure TEditorForm.DoCompile(Node : TZComponentTreeNode; const Expr : TZPropertyValue; Prop : TZProperty);
begin
  ZApp.CompileProperty(Node.Component,Expr,Prop);
end;

function TEditorForm.CompileAll(ThrowOnFail : boolean = False) : boolean;
var
  Node : TZComponentTreeNode;
begin
  Result := True;
  try
    ZApp.Compile;
  except
    on E : EZcErrorBase do
    begin
      if ThrowOnFail then
        raise
      else
      begin
        Node := Tree.FindNodeForComponent(E.Component);
        ShowMessage( 'Error in expression for node: ' + String(Node.Component.GetDisplayName) + ' '#13 + E.Message );
        Node.Expand(True);
        Tree.Selected := Node;
        Result := False;
      end;
    end;
  end;
end;

procedure TEditorForm.BoundsCheckBoxClick(Sender: TObject);
begin
  Renderer.CollisionBoundsVisible := (Sender as TCheckBox).Checked;
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

  //Set the NoSound-flag if no sound is used, this means we can remove all audio code later
  ZApp.NoSound := FindInstanceOf(Self.Root, TSound)=nil;

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
  Log.Write('File generated: ' + OutputName);
end;

procedure TEditorForm.GenerateAndroidActionExecute(Sender: TObject);
var
  OutFile : string;
begin
  OutFile := BuildRelease(bbNormalAndroid);
  ShowMessageWithLink('Created file: '#13#13 + OutFile,
    '<A HREF="' + ExtractFilePath(OutFile) + '">Open containing folder</A>' + #13#13 +
    'To run this file on Android devices see this <A HREF="http://www.emix8.org/forum/viewtopic.php?t=874">forum thread.</A>');
end;

procedure TEditorForm.AndroidRunActionExecute(Sender: TObject);
var
  OutFile,BatFile,ApkPath : string;
begin
  if Self.AndroidSdkPath='' then
  begin
    ShowMessage('AndroidSdkPath not set. Check settings.');
    Exit;
  end;
  OutFile := BuildRelease(bbNormalAndroid);

  ApkPath := ExePath + 'Android\ZGEAndroid-debug.apk';

  SetEnvironmentVariable('ANDROID_SDK_PATH',PWideChar(Self.AndroidSdkPath));
  SetEnvironmentVariable('ZZDC_PATH',PWideChar(OutFile));
  SetEnvironmentVariable('APK_PATH',PWideChar(ApkPath));

  BatFile := '/c "' + ExePath + 'Android\r.bat' + '"';
  ShellExecute(Handle, 'open', 'cmd', PChar(BatFile), nil, SW_SHOWDEFAULT);
end;

procedure TEditorForm.GenerateEXEClick(Sender: TObject);
var
  OutFile : string;
begin
  OutFile := BuildRelease(bbNormalUncompressed);
  //Kör den skapade filen
  ShellExecute(Handle, 'open',PChar(OutFile), nil, nil, SW_SHOWNORMAL);
end;

procedure TEditorForm.GenerateScreenSaverActionExecute(Sender: TObject);
begin
  BuildRelease(bbScreenSaverUncompressed);
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

procedure TEditorForm.EditXmlActionExecute(Sender: TObject);
var
  F : TXmlEditForm;
  Sa : AnsiString;
  Su : string;
  Stream : TMemoryStream;
  SymTemp : TSymbolTable;
  C : TZComponent;
begin
  Stream := ComponentManager.SaveXmlToStream(Self.Root) as TMemoryStream;
  try
    SetLength(Sa,Stream.Size);
    Stream.Position := 0;
    Stream.Read(Sa[1],Stream.Size);
  finally
    Stream.Free;
  end;
  if not Assigned(XmlEditForm) then
    XmlEditForm := TXmlEditForm.Create(Self);
  F := XmlEditForm;
  SymTemp := TSymbolTable.Create;
  try
    F.SynEdit.Text := String(Sa);
    F.SynEdit.Modified := False;
    repeat
      if (F.ShowModal=mrOk) and F.SynEdit.Modified then
      begin
        Su := F.SynEdit.Text;
        SymTemp.ClearAll;
        C := nil;
        try
          C := ComponentManager.LoadXmlFromString(Su,SymTemp);
          if not (C is TZApplication) then
            raise Exception.Create('Root component must be ZApplication');
        except
          on E : Exception do
          begin
            ShowMessage(E.ToString);
            C.Free;
            Continue;
          end;
        end;

        ClearRoot;
        SetRoot(C);
        SetFileChanged(True);
      end;

      Break;
    until False;
  finally
    SymTemp.Free;
  end;
end;

procedure TEditorForm.ExecToolAndWait(const ExeFile,ParamString : string);
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  Log.Write(ExeFile + ' ' + ParamString);
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


procedure TEditorForm.ReplaceResource(const ExeFile,OutFile,DataFile : string; UseCodeRemoval : boolean);
var
  M : TPEResourceModule;
  R : TResourceDetails;
  IconR : TIconGroupResourceDetails;
  NewData : TMemoryStream;
begin
  M := TPEResourceModule.Create;
  try
    M.LoadFromFile( AnsiString(ExeFile) );

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
    if ExtractFileExt(OutFile)<>'.ocx' then
    begin
      R := M.FindResource('10','DVCLAL',0);
      if R<>nil then
        M.DeleteResource(M.IndexOfResource(R))
      else
        Log.Warning('Resource not found');
      R := M.FindResource('10','PACKAGEINFO',0);
      if R<>nil then
        M.DeleteResource(M.IndexOfResource(R))
      else
        Log.Warning('Resource not found');
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

    if UseCodeRemoval then
      RemoveUnusedCode(M);

    //Important, otherwise Win7 won't recognize program icon
    M.SortResources;

    M.SaveToFile( AnsiString(OutFile) );
  finally
    M.Free;
  end;
end;

function TEditorForm.BuildRelease(Kind : TBuildBinaryKind) : string;
var
  OutFile,TempFile,Tool,ToolParams,PlayerName,Ext : string;
  ToolPath : string;
  UsePiggyback,UseCodeRemoval : boolean;

  function InGetSize : integer;
  var
    Handle : THandle;
  begin
    Handle := FileOpen(OutFile,fmOpenRead or fmShareDenyNone);
    Result := GetFileSize(Handle,nil);
    FileClose(Handle);
  end;

var
  C : TDefineConstant;
begin
  UsePiggyback := False;
  UseCodeRemoval := False;
  case Kind of
    bbNormal, bbNormalUncompressed :
      begin
        Ext := 'exe';
        PlayerName := ExePath + 'player.bin';
        UseCodeRemoval := RemoveUnusedMenuItem.Checked;
      end;
    bbScreenSaver, bbScreenSaverUncompressed :
      begin
        Ext := 'scr';
        PlayerName := ExePath + 'player_ss.bin';
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

  if Kind=bbNormalAndroid then
  begin
    OutFile := ExePath + 'zzdc.dat';
  end
  else
  begin
    if CurrentFileName='' then
      OutFile := ExePath + 'untitled.' + Ext
    else
      //Must expand filename because we need absolute path when calling tools, not relative paths like .\projects
      OutFile := ChangeFileExt(ExpandFileName(CurrentFileName),'.' + Ext);
  end;

  if FileExists(OutFile) then
    DeleteFile(OutFile);

  C := ZApp.SymTab.Lookup('android') as TDefineConstant;
  C.Value := IfThen(Kind=bbNormalAndroid, 1, 0);

  if Kind=bbNormalAndroid then
    BuildBinary('',OutFile)
  else if UsePiggyback then
  begin
    if not FileExists(ExePath + PlayerName) then
    begin
      ShowMessage('Player file missing for chosen platform (they are not included in the ZGE beta version).');
      Exit;
    end;
    BuildBinary(PlayerName,OutFile);
  end
  else
  begin
    TempFile := ExePath + 'temp.dat';
    BuildBinary('',TempFile);
    ReplaceResource(PlayerName,OutFile,TempFile,UseCodeRemoval);
    DeleteFile(TempFile);
  end;

  C.Value := 0;
  if Kind=bbNormalAndroid then
    //Need to recompile afterwards to remove constant in designer
    CompileAll;

  //linuxbinärer med piggyback hanteras ej av upx
  if Kind in [bbNormal,bbScreenSaver] then
  begin
    //Upx -v %1
    ToolPath := ExePath + 'tools\';

    Tool := StringReplace(PackerProg,'{$toolpath}',ToolPath,[rfReplaceAll, rfIgnoreCase]);
    if not VerifyToolExists('Packer','',Tool) then
      Exit;

    ToolParams := StringReplace(PackerParams,'{$exename}','"' + OutFile + '"',[rfReplaceAll, rfIgnoreCase]);
    ExecToolAndWait(Tool,ToolParams);
  end;

  case Kind of
    bbNormalUncompressed: ;
    bbNormal,bbScreenSaver,bbScreenSaverUncompressed :
      ShowMessageWithLink('Created file: '#13#13 + OutFile + #13#13 + 'Size: ' + IntToStr(InGetSize div 1024) + ' kb',
        '<A HREF="' + ExtractFilePath(OutFile) + '">Open containing folder</A>' );
    bbNormalLinux:
      ShowMessageWithLink('Created file: '#13#13 + OutFile,
        '<A HREF="' + ExtractFilePath(OutFile) + '">Open containing folder</A>' + #13#13 +
        'To run this file on Linux see <A HREF="http://www.zgameeditor.org/index.php/Howto/GenCrossPlatform">Generate files for Linux and OS X</A>');
    bbNormalOsx86:
      ShowMessageWithLink('Created file: '#13#13 + OutFile,
        '<A HREF="' + ExtractFilePath(OutFile) + '">Open containing folder</A>' + #13#13 +
        'To run this file on Mac see <A HREF="http://www.zgameeditor.org/index.php/Howto/GenCrossPlatform">Generate files for Linux and OS X</A>');
  end;

  //Return created filename
  Result := OutFile;
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
  Prop : TZProperty;
  ParentComps,ParentLists : TStringList;
  CurParent : TZComponentTreeNode;
begin
  //Ta reda på vilken lista som nod ska läggas till
  if not Assigned(Tree.Selected) then
    Exit;
  if Assigned(Tree.ZSelected.ComponentList) then
  begin
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

  C := Ci.ZClass.Create(nil);
  AddNewComponentToTree(C);
end;

procedure TEditorForm.AddNewComponentToTree(C : TZComponent);
var
  Ci : TZComponentInfo;
  S : string;
begin
  Ci := ComponentManager.GetInfo(C);

  if Ci.AutoName then
  begin  //Give unique name
    S := ZApp.SymTab.MakeUnique(Ci.ZClassName);
    C.SetString('Name',AnsiString(S));
    ZApp.SymTab.Add(S,C);
  end;

  Tree.AddNode(C,Tree.Selected).Selected := True;
  TZComponentTreeNode(Tree.Selected.Parent).ComponentList.AddComponent(C);
  TZComponentTreeNode(Tree.Selected.Parent).ComponentList.Change;
  SetFileChanged(True);
  if CompEditor<>nil then
    CompEditor.OnTreeChanged;
end;

procedure TEditorForm.AddComponentActionUpdate(Sender: TObject);
var
  B : boolean;
begin
  B :=Tree.Focused and Assigned(Tree.Selected) and Assigned(Tree.ZSelected.ComponentList);
  (Sender as TAction).Enabled := B;
  AddFromLibraryMenuItem.Enabled := B;
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
  I,J : integer;
  Node : TZComponentTreeNode;
  NodeList : TObjectList;
begin
  WipeUndoHistory;
  UndoParent := Tree.ZSelected.Parent as TZComponentTreeNode;

  NodeList := Tree.SortSelections;
  try
    Tree.Selected := nil;
    for J := 0 to NodeList.Count - 1 do
    begin
      Node := NodeList[J] as TZComponentTreeNode;

      C := Node.Component;
      if HasReferers(Root, C) then
      begin
        ShowMessage('Cannot delete, other components refers to this component.');
        Exit;
      end;

      if C=ShowNode then
      begin
        ShowNode := nil;
        Tree.LockShowNode := nil;
        LockShow := False;
      end;

      //Remove all names from symboltable
      List := TObjectList.Create(False);
      try
        GetAllObjects(C,List);
        for I := 0 to List.Count - 1 do
        begin
          CurC := List[I] as TZComponent;
          if CurC.Name<>'' then
            ZApp.SymTab.Remove(String(CurC.Name));
        end;
      finally
        List.Free;
      end;

      UndoNodes.Add(C);
      UndoParent.ComponentList.RemoveComponent(C);
      UndoIndices.Add( TObject(UndoParent.IndexOf(Node)) );
      Node.Delete;
    end;
  finally
    NodeList.Free;
  end;

  //Signalera till parentlistan att den är ändrad
  UndoParent.ComponentList.Change;
  SelectComponent(nil);
  SetFileChanged(True);
  if (CompEditor<>nil) and (Sender<>CompEditor) then
    CompEditor.OnTreeChanged;
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
  if ((Tree.ZSelected<>nil) and (Tree.ZSelected.Component<>ZApp)) and (ShowNode<>ZApp) then
    Exit;
  if not CompileAll then
    Exit;
  try
    ZExpressions.ResetScriptState;
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
  //Needed because of styles-bug: http://stackoverflow.com/questions/9580563/disabling-tbutton-issue-on-a-vcl-styled-form
  AppStartButton.Perform(CM_RECREATEWND, 0, 0);
  AppStopButton.Perform(CM_RECREATEWND, 0, 0);
end;

procedure TEditorForm.AppPreviewStopActionExecute(Sender: TObject);
begin
  ZApp.DesignerStop;
  //Call reset here to unload dll's loaded with ExternalLibrary
  ZApp.DesignerReset;
  AppPreviewStartAction.ShortCut := 32781;
  AppPreviewStopAction.ShortCut := 0;
  AppPreviewStartAction.Enabled := True;
  AppPreviewStopAction.Enabled := False;
  IsAppRunning := False;
  //Needed because of styles-bug: http://stackoverflow.com/questions/9580563/disabling-tbutton-issue-on-a-vcl-styled-form
  AppStartButton.Perform(CM_RECREATEWND, 0, 0);
  AppStopButton.Perform(CM_RECREATEWND, 0, 0);
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
    ZApp.Time := 0;
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

procedure TEditorForm.OnTreeRecreate(Sender: TObject);
begin
  //With Style-changes (internal delphi style or windows style) the tree gets recreated
  //Need to rebuild treenodes otherwise state is lost
  if Self.Root<>nil then
    Tree.SetRootComponent(Self.Root);
end;

procedure TEditorForm.OnTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  FromNode,ToNode : TTreeNode;
  SourceC : TZComponent;
  DestList : TZComponentList;
  IsCopy : boolean;
  I : integer;
  Nodes : TObjectList;
begin
  IsCopy := GetAsyncKeyState(VK_CONTROL)<0;

  Nodes := Tree.SortSelections;
  try
    for I := 0 to Nodes.Count - 1 do
    begin
      FromNode:=Nodes[I] as TTreeNode;
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

      if IsCopy then
        //Kopiera komponent ifall CTRL är nedtryckt
        InsertAndRenameComponent(SourceC.Clone,ToNode as TZComponentTreeNode)
      else
      begin
        //Flytta zcomponent
        SourceC.OwnerList.Change;
        SourceC.OwnerList.RemoveComponent(SourceC);

        DestList.AddComponent(SourceC);
        DestList.Change;

        //Flytta trädnoder
        FromNode.MoveTo(ToNode,naAddChild);

        if CompEditor<>nil then
          CompEditor.OnTreeChanged;
      end;
    end;
  finally
    Nodes.Free;
  end;
  SetFileChanged(True);
end;

procedure TEditorForm.SetCurrentFileName(const F: string);
const
  MruListMax=8;
begin
  CurrentFileName := F;
  if F<>'' then
  begin
    Platform_DesignerSetFilePath( AnsiString( ExtractFilePath(CurrentFileName) ) );
    SetCurrentDir( ExtractFilePath(CurrentFileName) );
    //Add to MRU-list
    if MruList.IndexOf(F)>-1 then
      MruList.Delete(MruList.IndexOf(F));
    MruList.Insert(0,F);
    while MruList.Count>MruListMax do
      MruList.Delete(MruListMax);
  end;
  RefreshMenuFromMruList;
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
    if not CharInSet(NewName[I],['a'..'z','A'..'Z','_','0'..'9']) then
    begin
      ShowMessage('Name contains invalid characters: ' + NewName);
      Abort;
    end;
  end;
  if (NewName<>'') and CharInSet(NewName[1],['0'..'9']) then
  begin
    ShowMessage('Name cannot begin with a digit: ' + NewName);
    Abort;
  end;
  if (NewName<>'') and ZApp.SymTab.Contains(NewName) and (ZApp.SymTab.Lookup(NewName)<>C) then
  begin
    ShowMessage('An component with this name already exists: ' + NewName);
    Abort;
  end;
  if (NewName='') and HasReferers(Root, C, False) then
  begin
    ShowMessage('Cannot set the name to blank, other components refer to this component.');
    Abort;
  end;
  if Trim(OldName)<>'' then
    ZApp.SymTab.Remove(OldName);
  if NewName<>'' then
    ZApp.SymTab.Add(NewName,C);
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
  S : AnsiString;
  C : TZComponent;
  Group : TLogicalGroup;
  I : integer;
  Nodes : TObjectList;
begin
  Group := nil;
  if Tree.SelectionCount>1 then
  begin
    //Copy several: Create group component as root for the copies
    Group := TLogicalGroup.Create(nil);
    Group.SetString('Name', AnsiString('#'));
    Nodes := Tree.SortSelections;
    try
      for I := 0 to Tree.SelectionCount - 1 do
        Group.Children.AddComponent( (Nodes[I] as TZComponentTreeNode).Component.Clone );
    finally
      Nodes.Free;
    end;
    C := Group;
  end
  else
  begin
    //Copy single node
    C := Tree.ZSelected.Component;
  end;

  Stream := ComponentManager.SaveXmlToStream(C) as TMemoryStream;
  try
    SetLength(S,Stream.Size);
    Stream.Position := 0;
    Stream.Read(S[1],Stream.Size);
    S := 'ZZDC' + S;
    Clipboard.SetTextBuf( PChar(String(S)) );
  finally
    Stream.Free;
    if Assigned(Group) then
      Group.Free;
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
      Imp.Import;
      Node := InsertAndRenameComponent(Imp.ResultModelGroup, Parent);
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
  DestTreeNode : TZComponentTreeNode;
  Index : integer = -1) : TZComponentTreeNode;
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

  if (Index>=0) and (Index>=DestList.Count) then
    Index := -1;

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

      if ZApp.SymTab.Contains(String(C.Name)) then
      begin
        StartName := String(C.Name);
        S := '';
        while (Length(StartName)>0) and CharInSet(StartName[Length(StartName)],['0'..'9']) do
        begin
          S := StartName[Length(StartName)] + S;
          Delete(StartName,Length(StartName),1);
        end;
        FirstCopyNr := StrToIntDef(S,0);
        NewNameFound := False;
        for J := FirstCopyNr + 1 to FirstCopyNr + 1000 do
        begin
          NewName := StartName + IntToStr(J);
          if not ZApp.SymTab.Contains(NewName) then
          begin
            C.SetString('Name',AnsiString(NewName));
            NewNameFound := True;
            Break;
          end;
        end;
        if not NewNameFound then
        begin
          ShowMessage('Could not find unique name for component: ' + String(C.Name));
          Exit;
        end;
      end;

      ZApp.SymTab.Add(String(C.Name),C);
    end;
  finally
    List.Free;
  end;
  if Index=-1 then
    DestList.AddComponent(InsertC)
  else
    DestList.InsertComponent(InsertC,Index);
  DestList.Change;
  SetFileChanged(True);
  Result := Tree.AddNode(InsertC,DestTreeNode,Index)as TZComponentTreeNode;
  if CompEditor<>nil then
    CompEditor.OnTreeChanged;
end;

procedure TEditorForm.PasteComponentActionExecute(Sender: TObject);
var
  DestTreeNode : TZComponentTreeNode;
  S : string;
  C : TZComponent;
  Group : TLogicalGroup;
begin
  DestTreeNode := Tree.ZSelected;

  if Clipboard.HasFormat(CF_TEXT) then
  begin
    S := Clipboard.AsText;
    if Copy(S,1,4)='ZZDC' then
    begin
      Delete(S,1,4);
      C := ComponentManager.LoadXmlFromString(S,ZApp.SymTab);
      if (C is TLogicalGroup) and ((C as TLogicalGroup).Name='#') then
      begin //Paste several components
        Group := (C as TLogicalGroup);
        while Group.Children.Count>0 do
        begin
          C := Group.Children.GetComponent(0);
          Group.Children.RemoveComponent(C);
          InsertAndRenameComponent(C,DestTreeNode);
        end;
        Group.Free;
      end
      else //Paste single component
        InsertAndRenameComponent(C,DestTreeNode);
      CompileAll;
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
  Node : TTreeNode;
begin
  C:=Tree.ZSelected.Component;
  L:=(Tree.Selected.Parent as TZComponentTreeNode).ComponentList;
  I := L.IndexOf(C);
  Tmp := L[I-1];
  L[I-1] := C;
  L[I] := Tmp;
  L.Change;

  Node := Tree.Selected;
  Tree.ClearSelection(True);
  //Must set selected to false before moving, otherwise error in comctrls when using keyboard shortcut
  Node.Selected := False;
  Node.MoveTo(Node.Parent.Item[Node.Index-1],naInsert);
  Node.Selected := True;

  SetFileChanged(True);
  if CompEditor<>nil then
    CompEditor.OnTreeChanged;
end;

procedure TEditorForm.MoveDownComponentActionExecute(Sender: TObject);
var
  C,Tmp : TObject;
  L : TZComponentList;
  I : integer;
  Node : TTreeNode;
begin
  C:=Tree.ZSelected.Component;
  L:=(Tree.Selected.Parent as TZComponentTreeNode).ComponentList;
  I := L.IndexOf(C);
  Tmp := L[I+1];
  L[I+1] := C;
  L[I] := Tmp;
  L.Change;

  Node := Tree.Selected;
  Tree.ClearSelection(True);
  Node.Selected := False;
  if I<L.Count-2 then
    Node.MoveTo(Node.Parent.Item[Node.Index+2],naInsert)
  else
    Node.MoveTo(Node.Parent,naAddChild);
  Node.Selected := True;

  SetFileChanged(True);
  if CompEditor<>nil then
    CompEditor.OnTreeChanged;
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

  WriteProjectSettingsToIni;

  //This force current expression-editor to be saved
  Tree.Selected := nil;

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

  ClearRoot;
end;

procedure TEditorForm.ClearRoot;
begin
  AudioPlayer.DesignerResetMixer;

  SetShowNode(nil);

  WipeUndoHistory;

  LockShow := False;
  SelectComponent(nil);

  if IsAppRunning then
    AppPreviewStopAction.Execute;
  ZApp.Terminate;
  Root.Free;
  Root := nil;
  ZApp := nil;
end;

procedure TEditorForm.NewProject(const FromTemplate : string = '');
begin
  if CloseProject then
    OpenProject(FromTemplate,FromTemplate<>'');
end;

procedure TEditorForm.NewProjectActionExecute(Sender: TObject);
begin
  NewProject( ((Sender as TAction).ActionComponent as TMenuItem).Hint);
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
    SelectNext(ActiveControl as TWinControl, True, True) // Forward
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
  if (not Glp.MouseCapture) or IsAppRunning then
    Exit;
  //From ESS-doll

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
  if not IsAppRunning then
  begin
    Glp.MouseCapture := True;
    LockMouse := Glp.ClientToScreen( Point(X,Y) );
    ShowCursor(False);
  end;
end;

procedure TEditorForm.OnGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not IsAppRunning then
  begin
    Glp.MouseCapture := False;
    ShowCursor(True);
  end;
  Glp.SetFocus;
end;

procedure TEditorForm.LogListBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
const Levels : array[TLogLevel] of string = ('Normal','Warning','Error','UserTrace');
var
  Point : TPoint;
  Index : Integer;
  S : string;
  Log : TLog;
  Data : TListLogItem;
begin
  Point.X := X;
  Point.Y := Y;
  Index := LogListBox.ItemAtPos(Point,True);
  if Index<>-1 then
  begin
    Data := LogListBox.Items.Objects[Index] as TListLogItem;
    if Data=nil then
      Exit;
    Log := Data.Log;
    S := Format('Level: %s'#13'Log: %s'#13'Message: %s',
      [ Levels[ Data.Level ], Log.Name, Data.Msg ]);
    LogListBox.Hint := S;
    Point := LogListBox.ClientToScreen(Point);
    Application.ActivateHint(Point);
  end;
end;

procedure TEditorForm.LogClearMenuItemClick(Sender: TObject);
var
  I : integer;
begin
  for I := 0 to LogListBox.Items.Count-1 do
    LogListBox.Items.Objects[I].Free;
  LogListBox.Items.Clear;
end;

procedure TEditorForm.LogCopytoclipboardMenuItemClick(Sender: TObject);
var
  //Data : TListLogItem;
  I : integer;
  S : string;
begin
  S := '';
  for I := 0 to LogListBox.Items.Count - 1 do
  begin
    //Data := LogListBox.Items.Objects[Index] as TListLogItem;
    S := S + LogListBox.Items[I] + #13#10;
  end;
  Clipboard.AsText := S;
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
  Data : TListLogItem;
begin
  Data := LogListBox.Items.Objects[Index] as TListLogItem;
  if Data=nil then
    Exit;
  Log := Data.Log;
  if Log=nil then
    Exit;

  C := (Control as TListBox).Canvas;

  if Data.Level in [lleWarning,lleError] then
  begin
    C.Font.Style:=[fsBold];
    C.Brush.Color := clWhite;
    if Data.Level=lleWarning then
      C.Font.Color := (Control as TListBox).Color
    else
      C.Font.Color := clRed;
  end
  else
  begin
    C.Font.Style:=[];
    C.Brush.Color := (Control as TListBox).Color;
    C.Font.Color := clWhite;
  end;

  //Clear area in listbox-color, this avoids highlighting selected
  C.FillRect(Rect);

  C.TextOut(Rect.Left + 2, Rect.Top, LogChars[ (Log.ID mod Length(LogChars))+1 ] );

//  C.Font.Color := LogColors[ Log.ID mod High(LogColors) ];
  S := (Control as TListBox).Items[Index];
  C.TextOut(Rect.Left + 12, Rect.Top, S);
  I := C.TextWidth(S) + 16;
  if I>(Control as TListBox).ScrollWidth then
    (Control as TListBox).ScrollWidth := I;
end;


procedure TEditorForm.SaveProjectActionExecute(Sender: TObject);
begin
  SaveCurrentEdits;
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


procedure TEditorForm.LoadSysLibrary;
var
  S : string;

  procedure InAddItems(List : TZComponentList; Parent : TMenuItem);
  var
    M : TMenuItem;
    C : TZComponent;
    I : integer;
  begin
    for I := 0 to List.Count - 1 do
    begin
      C := List.GetComponent(I);
      M := TMenuItem.Create(AddFromLibraryMenuItem);
      M.Caption := String(C.Comment);
      Parent.Add(M);
      if (C is TLogicalGroup) and (string(C.Name)='') then
        InAddItems((C as TLogicalGroup).Children,M)
      else
      begin
        M.OnClick := OnAddFromLibraryItemClick;
        M.Tag := Integer(C);
      end;
    end;
  end;

begin
  S := ExePath + 'Library.xml';
  if not FileExists(S) then
  begin
    Log.Write( 'Lib file missing: ' + S );
    Exit;
  end;
  AddFromLibraryMenuItem.Clear;
  SysLibrary := ComponentManager.LoadXmlFromFile(S);
  InAddItems((SysLibrary as TZApplication).Content,AddFromLibraryMenuItem);
end;

procedure TEditorForm.OnAddFromLibraryItemClick(Sender: TObject);
var
  M : TMenuItem;
  C : TZComponent;
begin
  M := Sender as TMenuItem;
  C := TZComponent(M.Tag).Clone;
  InsertAndRenameComponent(C,Tree.ZSelected);
  CompileAll;
  C.Change;
end;

procedure TEditorForm.AddFromLibraryMenuItemClick(Sender: TObject);
begin
  if SysLibrary=nil then
    LoadSysLibrary;
end;

procedure AutoCompAddOne(const S : string; Item : TObject; Context : pointer);
var
  C : TSynCompletionProposal;
  Desc : string;

  function InGetSig(Func : TZcOpFunctionBase) : string;
  var
    I : integer;
    Arg : TZcOpArgumentVar;
  begin
    Result := Func.Id + '(';
    for I := 0 to Func.Arguments.Count - 1 do
    begin
      Arg := Func.Arguments[I] as TZcOpArgumentVar;
      if I>0 then
        Result := Result + ',';
      if Arg.Typ.IsPointer then
        Result := Result + 'ref ';
      Result := Result + GetZcTypeName(Arg.Typ) + ' ' + Arg.Id;
    end;
    Result := Result + ') : ' + GetZcTypeName(Func.ReturnType);
  end;

begin
  C := TSynCompletionProposal(Context);
  Desc := '';
  if (Item is TZComponent) then
    Desc := String((Item as TZComponent).GetDisplayName)
  else if (Item is TZcOpFunctionBase) then
    Desc := InGetSig(Item as TZcOpFunctionBase)
  else
    Desc := S;
  C.ItemList.Add(Desc);
  C.InsertList.Add(S);
end;

procedure TEditorForm.AutoCompOnExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  Comp : TSynCompletionProposal;
  Line,Ident,PropName : string;
  I,J,K : integer;
  C : TZComponent;
  PropList : TZPropertyList;
  Prop : TZProperty;

  procedure InAdd(const Items : array of string);
  var
    S : string;
  begin
    for S in Items do
    begin
      Comp.InsertList.Add(S);
      Comp.ItemList.Add(S);
    end;
  end;

  function InGetC(S : string) : TZComponent;
  begin
    if SameText(S,'this') then
      Result := Tree.ZSelected.Component
    else if SameText(S,'CurrentModel') then
    begin
      FindCurrentModel(Tree.ZSelected,Result);
    end
    else
      Result := ZApp.SymTab.Lookup(S) as TZComponent;
  end;

var
  Id : TZClassIds;
  Ci : TZComponentInfo;
begin
  Comp := Sender as TSynCompletionProposal;
  Comp.ItemList.Clear;
  Comp.InsertList.Clear;

  Line := Comp.Editor.LineText;
  I := Min(Comp.Editor.CaretX-1,Length(Line));
  while (I>0) and CharInSet(Line[I],['a'..'z','A'..'Z','_','0'..'9']) do
    Dec(I);
  if (I>0) and (Line[I]='.') then
  begin
    J := I-1;
    while (J>0) and CharInSet(Line[J],['a'..'z','A'..'Z','_','0'..'9']) do
      Dec(J);
    Ident := Copy(Line,J+1,I-J-1);
    if (J>0) and (Line[J]='.') then
    begin
      PropName := Ident;
      K := J-1;
      while (K>0) and CharInSet(Line[K],['a'..'z','A'..'Z','_','0'..'9']) do
        Dec(K);
      Ident := Copy(Line,K+1,J-K-1);
      C := InGetC(Ident);
      if C<>nil then
      begin
        PropList := C.GetProperties;
        Prop := PropList.GetByName(PropName);
        if Prop<>nil then
        begin
          //List members of a property
          if Prop.PropertyType=zptColorf then
            InAdd(['R','G','B','A']);
          if Prop.PropertyType=zptVector3f then
            InAdd(['X','Y','Z']);
        end;
      end;
    end else
    begin
      //List properties of an identifier
      C := InGetC(Ident);
      if C<>nil then
      begin
        PropList := C.GetProperties;
        for I := 0 to PropList.Count - 1 do
        begin
          Prop := TZProperty(PropList[I]);
          if (Prop.PropertyType in [zptPropertyRef,zptComponentList,zptExpression]) or
            (Prop.Name='ObjId') then
            Continue;
          if Prop.ExcludeFromBinary then
            Continue;
          InAdd([Prop.Name]);
        end;
      end;
    end;
  end else if (I>0) and (Line[I]='@') then
  begin
    for Id := Low(TZClassIds) to High(TZClassIds) do
    begin
      Ci := ComponentManager.GetInfoFromId(Id);
      if Ci.ZClass.InheritsFrom(TCommand) and (not Ci.ZClass.InheritsFrom(TContentProducer)) then
        InAdd([ComponentManager.GetInfoFromId(Id).ZClassName]);
    end;
  end
  else
  begin
    //List global identifiers
    ZApp.SymTab.Iterate(AutoCompAddOne,Comp);
    InAdd(['CurrentModel','this','string','int','float','while','for']);
  end;
  (Comp.ItemList as TStringList).Sort;
  (Comp.InsertList as TStringList).Sort;
end;

procedure TEditorForm.ParamAutoCompOnExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  I,J,K,PCount,PIndex : integer;
  C : char;
  S,Line,Tmp : string;
  Comp : TSynCompletionProposal;
  O : TObject;
  Func : TZcOpFunctionBase;
  Arg : TZcOpArgumentVar;
begin
  Comp := TSynCompletionProposal(Sender);
  Line := Comp.Editor.LineText;
  I := Min(Comp.Editor.CaretX,Length(Line));
  PCount := 0;
  PIndex := 0;
  CanExecute := False;
  while I>0 do
  begin
    C := Line[I];
    case C  of
      '(' :
        if PCount=0 then
        begin
          J := I-1;
          while (J>0) and CharInSet(Line[J] ,['a'..'z','A'..'Z','_','0'..'9']) do
            Dec(J);
          S := Copy(Line,J+1,I-J-1);
          O := ZApp.SymTab.Lookup(S);
          if (O<>nil) and (O is TZcOpFunctionBase) then
          begin
            Func := O as TZcOpFunctionBase;
            Tmp := '';
            for K := 0 to Func.Arguments.Count - 1 do
            begin
              Arg := Func.Arguments[K] as TZcOpArgumentVar;
              if K>0 then
                Tmp := Tmp + ',';
              Tmp := Tmp + '"';
              if Arg.Typ.IsPointer then
                Tmp := Tmp + 'ref ';
              Tmp := Tmp + GetZcTypeName(Arg.Typ) + ' ' + Arg.Id + '"';
            end;
            Comp.Form.CurrentIndex := PIndex;
            Comp.ItemList.Clear;
            Comp.ItemList.Add(Tmp);
            CanExecute := True;
          end;
          Break;
        end
        else
          Dec(PCount);
      ')' : Inc(PCount);
      ',' : if PCount=0 then Inc(PIndex);
    end;
    Dec(I);
  end;
end;


type
  TMapName = class
    Name,MapUnitName,MapClassName,MapMethodName : string;
    Start : integer;
    Size : integer;
  end;

procedure TEditorForm.RemoveUnusedCode(Module : TPEModule);
var
  TotalRemovedBytes,TotalKeptBytes,I,J,FirstLine : integer;
  Section : TImageSection;
  Stream : TMemoryStream;
  MapNames : TObjectList;
  B : byte;
  S,MapFile : string;
  Splitter,Lines : TStringList;
  Item : TMapName;
  Id : TZClassIds;

  Infos : PComponentInfoArray;
  Ci : TZComponentInfo;
  UsedComponents,ClassesToRemove,NamesToRemove : TStringList;
  NamesKept,AllObjects : TObjectList;
  NeedJpeg,DisplayDetailedReport : boolean;
begin
  DisplayDetailedReport := DetailedBuildReportMenuItem.Checked;

  Section := Module.ImageSection[0];
  if Section.SectionName<>'.text' then
  begin
    Log.Warning('wrong section');
    Exit;
  end;

  MapFile := ExePath + 'zzdc.map';
  if not FileExists(MapFile) then
  begin
    Log.Error('map file not found');
    Exit;
  end;

  Lines := TStringList.Create;
  MapNames := TObjectList.Create(True);
  NamesToRemove := TStringList.Create;
  ClassesToRemove := TStringList.Create;
  UsedComponents := TStringList.Create;
  Splitter := TStringList.Create;
  Splitter.Delimiter := '.';
  try
    Lines.LoadFromFile(MapFile);
    FirstLine := Lines.IndexOf('  Address             Publics by Name');
    if FirstLine=-1 then
    begin
      Log.Error('error in map file');
      Exit;
    end;
    for I := FirstLine+2 to Lines.Count - 1 do
    begin
      S := Trim(Lines[I]);
      if Length(S)=0 then
        Break;
      if Copy(S,1,4)<>'0001' then
        Continue;
      Item := TMapName.Create;
      Item.Name := Copy(S,21,255);
      Item.Start := StrToInt('$' + Copy(S,6,8));
      Splitter.DelimitedText := Item.Name;
      if Splitter.Count=3 then
      begin
        Item.MapUnitName := Splitter[0];
        Item.MapClassName := Splitter[1];
        Item.MapMethodName := Splitter[2];
        if Length(Item.MapClassName)=0 then
          Item.MapClassName := Item.MapMethodName;
      end else if Splitter.Count=2 then
      begin
        Item.MapUnitName := Splitter[0];
      end;
      MapNames.Add(Item);
    end;
    MapNames.SortList(
      //Sort on start address
      function (Item1, Item2: Pointer): Integer
      var
        I1,I2 : integer;
        N1,N2 : TMapName;
      begin
        N1 := TMapName(Item1);
        N2 := TMapName(Item2);
        I1 := N1.Start;
        I2 := N2.Start;
        Result := I1-I2;
      end
    );

    for I := 0 to MapNames.Count - 2 do
    begin
      Item := MapNames[I] as TMapName;
      Item.Size := TMapName(MapNames[I+1]).Start - Item.Start;
    end;

    //Get names of used classes
    NeedJpeg := False;
    AllObjects := TObjectList.Create(False);
    try
      GetAllObjects(Self.Root,AllObjects);
      UsedComponents.Sorted := True;
      UsedComponents.Add('TAudioMixer');
      UsedComponents.Add('TMaterial');
      UsedComponents.Add('TMaterialTexture');
      for I := 0 to AllObjects.Count - 1 do
      begin
        UsedComponents.Add(TZComponent(AllObjects[I]).ClassName);
        if (AllObjects[I] is TBitmapFromFile) and ((AllObjects[I] as TBitmapFromFile).FileFormat=bffJpeg) then
          NeedJpeg := True;
        if (AllObjects[I] is TExpInvokeComponent) then
          UsedComponents.Add(ComponentManager.GetInfoFromId(TZClassIds((AllObjects[I] as TExpInvokeComponent).InvokeClassId)).ZClass.ClassName);
      end;
    finally
      AllObjects.Free;
    end;
    if UsedComponents.IndexOf('TRenderText')>=0 then
    begin
      UsedComponents.Add('TZBitmap');
      UsedComponents.Add('TFont');
    end;
    if UsedComponents.IndexOf('TMeshLoop')>=0 then
      UsedComponents.Add('TMeshCombine');
    if UsedComponents.IndexOf('TRenderNet')>=0 then
      UsedComponents.Add('TMesh');
    if UsedComponents.IndexOf('TExpInitLocalArray')>=0 then
      UsedComponents.Add('TDefineArray');
    if not NeedJpeg then
      ClassesToRemove.Add('TNjDecoder');

    //NamesToRemove = AllNames - UsedNames
    Infos := ZClasses.ComponentManager.GetAllInfos;
    for Id := Low(TComponentInfoArray) to High(TComponentInfoArray) do
    begin
      Ci := TZComponentInfo(Infos[Id]);
      if UsedComponents.IndexOf(Ci.ZClass.ClassName)=-1 then
        ClassesToRemove.Add(Ci.ZClass.ClassName);
    end;
    if UsedComponents.IndexOf('TMeshImplicit')=-1 then
      ClassesToRemove.Add('TImpProcess');
    ClassesToRemove.Add('TDefineConstant');
    if not ZApp.ShowOptionsDialog then
    begin
      NamesToRemove.Add('ZPlatform.Options');
      NamesToRemove.Add('ZPlatform.Platform_ShowOptionDialog');
    end;
    if UsedComponents.IndexOf('TSound')=-1 then
    begin
      NamesToRemove.Add('AudioPlayer.UpdateModulators');
      NamesToRemove.Add('AudioPlayer.RenderVoice');
      NamesToRemove.Add('AudioPlayer.SetVoiceFrameConstants');
      NamesToRemove.Add('AudioPlayer.UpdateEnvelope');
      NamesToRemove.Add('AudioPlayer.UpdateLfo');
      NamesToRemove.Add('AudioPlayer.UpdateFrame');
      NamesToRemove.Add('AudioPlayer.UpdateFrame');
      ClassesToRemove.Add('TAudioMixer');
      NamesToRemove.Add('ZPlatform.Platform_InitAudio');
      NamesToRemove.Add('AudioPlayer.EmitSoundsInEmitList');
      NamesToRemove.Add('AudioPlayer.RenderToMixBuffer');
      NamesToRemove.Add('ZPlatform.Platform_ShutdownAudio');
      NamesToRemove.Add('AudioPlayer.RenderChannel');
      NamesToRemove.Add('AudioPlayer.ChannelApplyDelay');
      NamesToRemove.Add('ZPlatform.PlaybackThread_Execute');
    end;
    if UsedComponents.IndexOf('TWebOpen')=-1 then
    begin
      NamesToRemove.Add('ZPlatform.Platform_NetOpen');
      NamesToRemove.Add('ZPlatform.Platform_NetRead');
    end;

    //NamesToRemove.Add('System.@HandleAnyException');
    //NamesToRemove.Add('System.@FinalizeArray');

    //ok, start removing
    NamesKept := TObjectList.Create(False);
    Stream := Section.RawData;
    TotalRemovedBytes := 0;
    for I := 0 to MapNames.Count - 1 do
    begin
      Item := TMapName(MapNames[I]);
      if (Item.Size=0) then
      begin
        if DisplayDetailedReport then
          NamesKept.Add(Item);
        Continue;
      end;
      if (ClassesToRemove.IndexOf(Item.MapClassName)=-1) and
        (NamesToRemove.IndexOf(Item.Name)=-1) then
      begin
        if DisplayDetailedReport then
          NamesKept.Add(Item);
        Continue;
      end;
      Stream.Seek(Item.Start,soBeginning);
      B := $90; //nop
      {$ifdef CPU386}
      //TODO: this does not work on 64-bit, not sure why
      for J := 0 to Item.Size - 1 do
        Stream.Write(B,1);
      {$endif}
      Inc(TotalRemovedBytes,Item.Size);
      //Log.Write(Item.Name);
    end;
    Log.Write('Removed: ' + IntToStr(TotalRemovedBytes) );

    if DisplayDetailedReport then
    begin
      NamesKept.SortList(
        //Sort on size
        function (Item1, Item2: Pointer): Integer
        var
          I1,I2 : integer;
          N1,N2 : TMapName;
        begin
          N1 := TMapName(Item1);
          N2 := TMapName(Item2);
          I1 := N1.Size;
          I2 := N2.Size;
          Result := I2-I1;
        end
      );

      TotalKeptBytes := 0;
      for I := 0 to NamesKept.Count - 1 do
      begin
        Item := TMapName(NamesKept[I]);
        Inc(TotalKeptBytes,Item.Size);
        Log.Write(IntToStr(Item.Size) + ' ' + Item.Name);
      end;
      Log.Write('Kept: ' + IntToStr(TotalKeptBytes) );
    end;
    NamesKept.Free;

  finally
    Lines.Free;
    MapNames.Free;
    ClassesToRemove.Free;
    NamesToRemove.Free;
    UsedComponents.Free;
    Splitter.Free;
  end;

end;

procedure TEditorForm.DisableComponentActionExecute(Sender: TObject);
var
  Nodes : TObjectList;
  I : integer;
  Node : TZComponentTreeNode;
begin
  Nodes := Tree.SortSelections;
  try
    for I := 0 to Nodes.Count-1 do
    begin
      Node := (Nodes[I] as TZComponentTreeNode);
      Node.Component.DesignDisable := not Node.Component.DesignDisable;
      Node.RefreshNodeName;
    end;
  finally
    Nodes.Free;
  end;
end;

procedure TEditorForm.DisableComponentActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (ActiveControl = Tree) and
    Assigned(Tree.Selected) and
    Assigned(Tree.ZSelected.Component) and
    (Tree.ZSelected.Component<>Root) and
    (not (Tree.ZSelected.Component is TZApplication));
  if (Sender as TAction).Enabled then
    (Sender as TAction).Checked := Tree.ZSelected.Component.DesignDisable;
end;

procedure TEditorForm.DisableFBOCheckBoxClick(Sender: TObject);
begin
  FbosSupported := not (Sender as TCheckBox).Checked;
end;

procedure TEditorForm.DisableShadersCheckBoxClick(Sender: TObject);
begin
  ShadersSupported := not (Sender as TCheckBox).Checked;
end;

//http://stackoverflow.com/questions/9978106/vcl-styles-menu-hotkey-inconsistency
type
  TFormStyleHookFix= class (TFormStyleHook)
  procedure CMDialogChar(var Message: TWMKey); message CM_DIALOGCHAR;
  end;

  TFormStyleHookHelper= class  helper for TFormStyleHook
  private
     function CheckHotKeyItem(ACharCode: Word): Boolean;
  end;

{ TFormStyleHookFix }

procedure TFormStyleHookFix.CMDialogChar(var Message: TWMKey);
begin
   if ((Message.KeyData and $20000000) <> 0 ) and (CheckHotKeyItem(Message.CharCode)) then
    begin
      Message.Result := 1;
      Handled := True;
    end
end;

{ TFormStyleHookHelper }
function TFormStyleHookHelper.CheckHotKeyItem(ACharCode: Word): Boolean;
begin
  Result:=False;
  if Self.FMainMenuBarHook<>nil then
   Result:=Self.FMainMenuBarHook.CheckHotKeyItem(ACharCode);
end;

procedure TEditorForm.SwitchToStyle(const StyleName : string; const StyleHandle : TStyleManager.TStyleServicesHandle);

{  procedure RecolorHighlighter(H : TSynCustomHighlighter);
  var
    I : integer;
    A : TSynHighlighterAttributes;
  begin
    H.WhitespaceAttribute.Background := StyleServices.GetSystemColor(clWindow);
    for I := 0 to H.AttrCount-1 do
    begin
      A := H.Attribute[I];
      if A.Style=[fsBold] then
        A.Foreground := StyleServices.GetSystemColor(clWindowText)
      else if A.Style=[fsItalic] then
        A.Foreground := StyleServices.GetSystemColor(clGrayText)
      else
        A.Foreground := StyleServices.GetSystemColor(clWindowText);
    end;
  end;}

begin
  //Make sure we leave the currently selected component because nodes will be recreated
  Tree.Selected := nil;
  TStyleManager.Engine.RegisterStyleHook(TEditorForm, TFormStyleHookFix);
  TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TMemoStyleHook);
  if StyleHandle=nil then
  begin
    TStyleManager.TrySetStyle( StyleName );
    if Assigned(StyleMenuItem.Find(StyleName)) then
      StyleMenuItem.Find(StyleName).Checked := True;
  end
  else
    TStyleManager.SetStyle( StyleHandle );
  Application.ProcessMessages;
//  RecolorHighlighter(ExprSynEdit.Highlighter);
//  RecolorHighlighter(ShaderSynEdit.Highlighter);
end;

procedure TEditorForm.OnChooseStyleMenuItemClick(Sender: TObject);
var
  M : TMenuItem;
begin
  M := (Sender as TMenuItem);
  M.Checked := True;
  SwitchToStyle(M.Hint,nil);
end;

procedure TEditorForm.OpenStyleMenuItemClick(Sender: TObject);
begin
  if OpenStyleDialog.Execute(Self.Handle) then
  begin
    SwitchToStyle('', TStyleManager.LoadFromFile(OpenStyleDialog.FileName));
  end;
end;

procedure TEditorForm.BuildStyleMenu;
var
  M : TMenuItem;
  S : string;
begin
  for S in TDirectory.GetFiles(ExePath + 'Styles','*.vsf') do
  begin
    TStyleManager.LoadFromFile(S);
  end;

  for S in TStyleManager.StyleNames do
  begin
    M := TMenuItem.Create(Self);
    M.OnClick := Self.OnChooseStyleMenuItemClick;
    M.Hint := S;
    M.Caption := S;
    M.RadioItem := True;
    StyleMenuItem.Add(M);
  end;
end;

procedure TEditorForm.AndroidBuildDebugApkActionExecute(Sender: TObject);
begin
  BuildAndroidApk(True);
end;

procedure TEditorForm.AndroidBuildReleaseApkActionExecute(Sender: TObject);
begin
  BuildAndroidApk(False);
end;

procedure TEditorForm.BuildAndroidApk(const IsDebug : boolean);
var
  TemplatePath,ProjectPath,OutFile : string;
  Lookups : TDictionary<string,string>;

  procedure MPath(const P : string);
  begin
    if not TDirectory.Exists(P) then
      TDirectory.CreateDirectory(P);
  end;

  procedure MCopy(const Name : string; const DoReplace : boolean = False);
  var
    Src,Dst,Key,S : string;
    I : integer;
    L : TStringList;
  begin
    Src := TemplatePath + Name;
    Dst := ProjectPath + Name;
    if not DoReplace then
    begin
      if TFile.Exists(Dst) and
        (TFile.GetLastWriteTime(Dst)>=TFile.GetLastWriteTime(Src)) then
        //no need to copy
      else
        TFile.Copy(Src,Dst,True);
    end
    else
    begin
      L := TStringList.Create;
      try
        L.LoadFromFile(Src);
        for I := 0 to L.Count-1 do
        begin
          S := L[I];
          if Pos('$',S)=0 then
            Continue;
          for Key in Lookups.Keys do
          begin
            if Pos(Key,S)>0 then
              S := StringReplace(S,Key,Lookups[Key],[rfReplaceAll])
          end;
          L[I] := S;
        end;
        L.SaveToFile(Dst);
      finally
        L.Free;
      end;
    end;
  end;

var
  Params,ApkFileName : string;
  F : TAndroidApkForm;
begin
  if Self.AndroidSdkPath='' then
  begin
    ShowMessage('AndroidSdkPath not set. Check settings.');
    Exit;
  end;
  if Self.AndroidAntPath='' then
  begin
    ShowMessage('AndroidAntPath not set. Check settings.');
    Exit;
  end;

  F := TAndroidApkForm.Create(Self);
  try
    F.PackageNameEdit.Text := String(Self.ZApp.AndroidPackageName);
    F.AppNameEdit.Text := String(Self.ZApp.Caption);
    F.VersionNameEdit.Text := String(Self.ZApp.AndroidVersionName);
    F.VersionNumberEdit.Text := IntToStr(Self.ZApp.AndroidVersionNumber);
    F.OrientationComboBox.ItemIndex := IfThen(Self.ZApp.AndroidPortrait,1,0);
    if F.ShowModal=mrCancel then
      Exit;
    Self.ZApp.SetString('AndroidPackageName',AnsiString(F.PackageNameEdit.Text));
    Self.ZApp.SetString('Caption',AnsiString(F.AppNameEdit.Text));
    Self.ZApp.SetString('AndroidVersionName',AnsiString(F.VersionNameEdit.Text));
    Self.ZApp.AndroidVersionNumber := StrToInt(F.VersionNumberEdit.Text);
    Self.ZApp.AndroidPortrait := (F.OrientationComboBox.ItemIndex=1);
    Self.SetFileChanged(True);
  finally
    F.Free;
  end;

  if String(Self.ZApp.AndroidPackageName)='' then
  begin
    ShowMessage('App.AndroidPackageName not set. Set the property and try again.');
    Exit;
  end;

  TemplatePath := Self.ExePath + 'Android\Template\';

  if CurrentFileName='' then
    ProjectPath := Self.ExePath + 'Android\Projects\'
  else
    ProjectPath := ExtractFilePath( ExpandFileName(CurrentFileName) );
  ProjectPath := ProjectPath + String(Self.ZApp.AndroidPackageName) + '\';

  Lookups := TDictionary<string,string>.Create;
  try
    Lookups.Add('$sdkpath$', StringReplace(Self.AndroidSdkPath,'\','/',[rfReplaceAll]) );
    Lookups.Add('$sdkpath_normal$',Self.AndroidSdkPath);
    Lookups.Add('$package$', String(Self.ZApp.AndroidPackageName) );
    Lookups.Add('$title$', String(Self.ZApp.Caption) );
    Lookups.Add('$versionname$', String(Self.ZApp.AndroidVersionName) );
    Lookups.Add('$versionnumber$', IntToStr(Self.ZApp.AndroidVersionNumber) );
    if Self.ZApp.AndroidPortrait then
      Lookups.Add('$orientation$', 'portrait')
    else
      Lookups.Add('$orientation$', 'landscape');

    if not IsDebug then
    begin
      Lookups.Add('$keystorepath$', StringReplace(Self.AndroidKeystorePath,'\','/',[rfReplaceAll]) );
      Lookups.Add('$keystorealias$', Self.AndroidKeystoreAlias );
    end;

    ApkFileName := ProjectPath + 'bin\' + String(Self.ZApp.Caption);
    if IsDebug then
      ApkFileName := ApkFileName + '-debug.apk'
    else
    begin
      if Length(Self.AndroidKeystorePath)>0 then
        ApkFileName := ApkFileName + '-release.apk'
      else
        ApkFileName := ApkFileName + '-release-unsigned.apk';
    end;
    Lookups.Add('$apkpath$',ApkFileName);

    MPath(ProjectPath);
    MPath(ProjectPath + 'libs');
    MPath(ProjectPath + 'libs\armeabi');
    MPath(ProjectPath + 'assets');
    MPath(ProjectPath + 'res');
    MPath(ProjectPath + 'res\drawable-ldpi');
    MPath(ProjectPath + 'src');

    MCopy('libs\armeabi\libzgeandroid.so');
    MCopy('src\Zge.java');
    MCopy('src\ZgeActivity.java');
    MCopy('res\drawable-ldpi\icon.png');

    MCopy('default.properties');
    MCopy('local.properties',True);
    MCopy('AndroidManifest.xml',True);
    MCopy('build.xml',True);
    if IsDebug then
      MCopy('run.bat',True);
    if (not IsDebug) and (Length(Self.AndroidKeystorePath)>0)  then
      MCopy('ant.properties',True);

    OutFile := BuildRelease(bbNormalAndroid);
    TFile.Copy(OutFile,ProjectPath + 'assets\zzdc.dat',True);

    Params := '-buildfile "' + ProjectPath + 'build.xml" ';
    if IsDebug then
      Params := Params + 'debug'
    else
      Params := Params + 'release';

    SetEnvironmentVariable('ANT_TOOL',PWideChar(TPath.Combine(Self.AndroidAntPath,'bin\ant')));
    SetEnvironmentVariable('ANT_PARAMS',PWideChar(Params));
    ExecToolAndWait('cmd','/c "' + ExePath + 'Android\m.bat"');
  finally
    Lookups.Free;
  end;

  if TFile.Exists(ApkFileName) then
    ShowMessageWithLink('Created APK file: '#13#13 + ApkFileName,
      '<A HREF="' + ProjectPath + '">Open project folder</A>' + #13#13 +
      'To run this file on Android devices see this <A HREF="http://www.emix8.org/forum/viewtopic.php?t=874">forum thread.</A>');
end;

end.
