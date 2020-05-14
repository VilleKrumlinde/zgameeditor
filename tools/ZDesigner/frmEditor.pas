{Copyright (c) 2008- Ville Krumlinde

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

{$DEFINE ZZDC_FPC} //Runtime engine built with Freepascal

uses
  Windows, Messages, SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, ZClasses, DesignerGui, GLPanel, Vcl.ComCtrls, Vcl.Menus, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ImgList, frmSoundEdit, frmCompEditBase, Contnrs,
  uSymTab, frmMusicEdit, ZLog, Vcl.Buttons, Vcl.StdActns, Vcl.ExtCtrls,
  Vcl.ToolWin, SynCompletionProposal, frmBitmapEdit, frmMeshEdit, unitPEFile,
  Vcl.Imaging.Jpeg, Vcl.Themes, ZApplication, GLDrivers, System.Actions,
  Vcl.Imaging.pngimage, ZBitmap, Generics.Collections, CommCtrl,
  System.ImageList, frmCustomPropEditBase;

type
  TBuildBinaryKind = (bbNormal,bbNormalUncompressed,bbScreenSaver,bbScreenSaverUncompressed,
    bbNormalLinux,bbNormalOsx86,bbNormalAndroid);

  TPropEditKey = record
    Comp : TZComponent;
    Prop : TZProperty;
    constructor Create(Comp : TZComponent; Prop : TZProperty);
  end;

  TEditorForm = class(TForm)
    SaveDialog: TSaveDialog;
    Timer1: TTimer;
    LeftPanel: TPanel;
    TreePanel: TGroupBox;
    Splitter1: TSplitter;
    PropListPanel: TPanel;
    ViewerPanel: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    ActionList1: TActionList;
    AddComponentAction: TAction;
    TreePopupMenu: TPopupMenu;
    Addcomponent2: TMenuItem;
    ViewerPageControl: TPageControl;
    ViewerGlTabSheet: TTabSheet;
    ViewerCompTabSheet: TTabSheet;
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
    CopyComponentAction: TAction;
    PasteComponentAction: TAction;
    N2: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
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
    ReopenMenuItem: TMenuItem;
    Import3dsAction: TAction;
    Import3dsAction1: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    ViewTranslateLabel: TLabel;
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
    Import3DSfile1: TMenuItem;
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
    GamutImage: TImage;
    EnableThreadedProcessingMenuItem: TMenuItem;
    ImportBitmapAction: TAction;
    Importbitmap1: TMenuItem;
    ImportAudioAction: TAction;
    ImportAudio1: TMenuItem;
    Panel1: TPanel;
    CompEditorParentPanel: TPanel;
    DetachCompEditorButton: TButton;
    PropListParent: TGroupBox;
    QuickCompListView: TListView;
    Panel3: TPanel;
    QuickCompListParent: TPanel;
    PropEditParentPanel: TPanel;
    LogShowTraceOnly: TMenuItem;
    Findunsedcomponents1: TMenuItem;
    HighDPIImageListContainer: TImageList;
    EnableFunctionInlining: TMenuItem;
    OpenAllProjectsMenuItem: TMenuItem;
    EvalEdit: TEdit;
    BuildZ80MenuItem: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SaveBinaryMenuItemClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Update1Click(Sender: TObject);
    procedure LockShowActionExecute(Sender: TObject);
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
    procedure FileOpenActionBeforeExecute(Sender: TObject);
    procedure EnableThreadedProcessingMenuItemClick(Sender: TObject);
    procedure WmActivate(var M : TMessage); message WM_ACTIVATE;
    procedure ImportBitmapActionExecute(Sender: TObject);
    procedure ImportAudioActionExecute(Sender: TObject);
    procedure DetachCompEditorButtonClick(Sender: TObject);
    procedure DetachPropEditorButtonClick(Sender: TObject);
    procedure QuickCompListViewClick(Sender: TObject);
    procedure QuickCompListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LogShowTraceOnlyClick(Sender: TObject);
    procedure Findunsedcomponents1Click(Sender: TObject);
    procedure EnableFunctionInliningClick(Sender: TObject);
    procedure OpenAllProjectsMenuItemClick(Sender: TObject);
    procedure EvalEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure QuickCompListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure BuildZ80MenuItemClick(Sender: TObject);
  private
    { Private declarations }
    Ed : TZPropertiesEditor;
    Selected,ShowNode : TZComponent;
    LockShow : boolean;
    Root : TZComponent;
    CurrentFileName : string;
    ViewRotate,ViewTranslate : TZVector3f;
    IsAppRunning : boolean;
    OldGlWindowProc : TWndMethod;
    CompEditor : TCompEditFrameBase;  //Current component editor, nil if none
    CompEditorTreeNode : TZComponentTreeNode;
    PropEditor : TCustomPropEditBaseForm; //Current property editor, nil if none
    _FileChanged : boolean;
    RenderAborted : boolean;
    MruList : TStringList;
    PackerProg,PackerParams : string;
    AndroidSdkPath,AndroidSdCardPath,AndroidAntPath,
    AndroidKeystorePath,AndroidKeystoreAlias : string;
    GuiLayout : integer;
    UndoNodes,UndoIndices : TObjectList;
    UndoParent : TZComponentTreeNode;
    SysLibrary : TZComponent;
    SynEditFontSize,AutoCompTimerInterval : integer;
    Log : TLog;
    DetachedCompEditors : TObjectDictionary<TZComponent,TForm>;
    DetachedPropEditors : TObjectDictionary<TPropEditKey,TForm>;
    MainScaling : integer;
    EvalHistory : TStringList;
    QuickCompEnabledList : array of boolean;
    procedure SelectComponent(C : TZComponent);
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
    procedure CommitAllEdits;
    function CloseProject: boolean;
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
    procedure AddNewComponentToTree(C: TZComponent; SelectIt : boolean = true);
    procedure AutoCompOnExecute(Kind: SynCompletionType; Sender: TObject;  var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure ParamAutoCompOnExecute(Kind: SynCompletionType; Sender: TObject;  var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
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
    procedure FillNewMenuTemplateItems;
    procedure BuildAndroidApk(const IsDebug : boolean);
    procedure AddOneLogString(const S: string; Log : TLog; Level : TLogLevel);
    procedure WMDROPFILES(var msg : TWMDropFiles) ; message WM_DROPFILES;
    procedure ImportBitmaps(Files: TStringList);
    procedure ImportAudioFiles(Files: TStringList);
    procedure ImportModelFiles(Files: TStringList);
    function MakeCompEditor(Kind: TCompEditFrameBaseType): TCompEditFrameBase;
    procedure OnDetachedCompEditorClose(Sender: TObject; var Action: TCloseAction);
    procedure OnDetachedPropEditorClose(Sender: TObject; var Action: TCloseAction);
    procedure FillQuickCompList;
    procedure OnPropEditFocusControl(Sender: TObject; Prop : TZProperty; Component : TZComponent);
    procedure ResizeImageListImagesforHighDPI(const imgList: TImageList);
    procedure ParseEvalExpression(const Expr : string);
    procedure FilterQuickCompList;
    procedure BuildZ80(OutFile : string);
  protected
    procedure CreateWnd; override;
  public
    Glp : TGLPanel;
    Tree : TZComponentTreeView;
    ZApp : TZApplication;
    GamutZBitmap : TZBitmap;
    Driver : TGLDriverBase;
    ExePath : string;
    procedure SetFileChanged(Value : Boolean);
    procedure ValidateNewName(const OldName,NewName : string);
    procedure FindComponentAndFocusInTree(const CName: string); overload;
    procedure FindComponentAndFocusInTree(C: TZComponent); overload;
    procedure RefreshCompEditorTreeNode;
    function OnGetLibraryPath: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  EditorForm : TEditorForm;

const
  AppName = 'ZGameEditor';
  AppVersion = '4.0b';
  ZgeProjExtension = '.zgeproj';

procedure SetupGLShading;

implementation

{$R *.dfm}

uses Math, ZOpenGL, BitmapProducers, Meshes, Renderer, Compiler, ZExpressions,
  ShellApi, SynEditHighlighter, SynHighlighterZc,frmSelectComponent, AudioComponents, IniFiles, ZPlatform,
  dmCommon, frmAbout, uHelp, frmToolMissing, Vcl.Clipbrd, unitResourceDetails,
  u3dsFile, AudioPlayer, frmSettings, unitResourceGraphics, Zc_Ops,
  SynEditTypes, SynEditSearch, frmXmlEdit, frmArrayEdit, System.Types, System.IOUtils,
  frmAndroidApk, Winapi.Imm, Vcl.ExtDlgs, frmSpriteSheetEdit, frmTileSetEdit,
  frmExprPropEdit, frmShaderPropEdit, frmFloatPropEdit, SynEdit, uObjFile, ZFile;

{ TEditorForm }

constructor TEditorForm.Create(AOwner: TComponent);

  procedure LoadGamutBitmap;
  var
    B : TPngImage;
    Zb : TZBitmap;
    Bf : TBitmapFromFile;
    Value : TZPropertyValue;
    X,Y : integer;
    M : TMemoryStream;
    PPixel : PRGBQuad;
  begin
    M := TMemoryStream.Create;
    try
      B := Self.GamutImage.Picture.Graphic as TPngImage;
      for Y := 0 to B.Height-1 do
      begin
        PPixel := B.ScanLine[Y];
        for X := 0  to B.Width-1 do
        begin
          M.Write(PPixel.rgbRed,1);
          M.Write(PPixel.rgbGreen,1);
          M.Write(PPixel.rgbBlue,1);
          Inc(NativeUInt(PPixel),3);
        end;
      end;
      Value.BinaryValue.Size := M.Size;
      GetMem(Value.BinaryValue.Data,M.Size);
      Move(M.Memory^,Value.BinaryValue.Data^,M.Size);
    finally
      M.Free;
    end;

    Zb := TZBitmap.Create(nil);
    Zb.Width := 16;
    Zb.Height := 16;
    Bf := TBitmapFromFile.Create(Zb.Producers);
    Bf.SetProperty( Bf.GetProperties.GetByName('BitmapFile'), Value );

    Self.GamutZBitmap := Zb;
  end;

begin
  Driver := CreateDriver(glbFixed);
  inherited Create(AOwner);
  ZLog.SetReceiverFunc(OnReceiveLogMessage);

  Math.SetExceptionMask(exAllArithmeticExceptions);

  Self.Log := ZLog.GetLog(Self.ClassName);
  Log.Write( IntToStr(SizeOf(Pointer)*8) + ' bit version' );

  LoadGamutBitmap;
  DetachedCompEditors := TObjectDictionary<TZComponent,TForm>.Create([doOwnsValues]);
  DetachedPropEditors := TObjectDictionary<TPropEditKey,TForm>.Create([doOwnsValues]);

  //Zc expressions needs '.' set
  Application.UpdateFormatSettings := False;
  FormatSettings.DecimalSeparator := '.';

  Ed := TZPropertiesEditor.Create(Self);
  Ed.Align := alClient;
  Ed.OnPropValueChanged := Self.OnPropValueChange;
  Ed.OnPropEditFocusControl := Self.OnPropEditFocusControl;
  Ed.Parent := PropListParent;

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

  Application.HelpFile := ExePath + 'ZGameEditor.chm';

  //Remove chm-blocking (see http://stackoverflow.com/questions/11438634/opening-a-chm-file-produces-navigation-to-the-webpage-was-canceled)
  if FileExists(Application.HelpFile+':Zone.Identifier') then
    DeleteFile(Application.HelpFile+':Zone.Identifier');

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
  OpenAllProjectsMenuItem.Visible := DebugHook<>0;

  UndoNodes := TObjectList.Create(True);
  UndoIndices := TObjectList.Create(False);

  TStyleManager.Engine.RegisterStyleHook(TCustomSynEdit, TMemoStyleHook);

  BuildStyleMenu;
  ReadAppSettingsFromIni;
  RefreshMenuFromMruList;
  FillNewMenuTemplateItems;

  FillQuickCompList;

  EvalHistory := TStringList.Create;
end;


procedure TEditorForm.OnPropEditFocusControl(Sender: TObject; Prop : TZProperty; Component : TZComponent);

  function MakePropEditor(T : TCustomPropEditBaseFormClass) : TCustomPropEditBaseForm;
  var
    F : TCustomPropEditBaseForm;
  begin
    F := T.Create(Self);

    F.ScaleBy(Self.MainScaling,100);

    F.Prop := Prop;
    F.Component := Component;
    F.TreeNode := Tree.Selected as TZComponentTreeNode;
    F.DetachButton.OnClick := Self.DetachPropEditorButtonClick;
    F.Parent := PropEditParentPanel;
    F.Align := alClient;
    F.Show;
    Self.PropEditor := F;
    Result := F;
  end;

  procedure ShowExprEditor(Edit: TEdit);
  var
    F: TExprPropEditForm;
  begin
    F := MakePropEditor(TExprPropEditForm) as TExprPropEditForm;

    F.ExprPanel.Caption := F.ExprPanel.Caption + ' (' + Prop.Name + ')';

    F.ExprSynEdit.Text := Component.GetProperty(Prop).ExpressionValue.Source;
    F.ExprSynEdit.ResetModificationIndicator;
    F.AutoComp.OnExecute := AutoCompOnExecute;
    F.ParamComp.OnExecute := ParamAutoCompOnExecute;
    F.ExprSynEdit.Font.Size := Self.SynEditFontSize;
    F.AutoComp.TimerInterval := Self.AutoCompTimerInterval;
    F.ParamComp.TimerInterval := Self.AutoCompTimerInterval;
    F.ExprCompileButton.OnClick := Self.ExprCompileButtonClick;
  end;

  procedure ShowShaderEditor(Edit: TEdit);
  var
    F: TShaderPropEditForm;
  begin
    F := MakePropEditor(TShaderPropEditForm) as TShaderPropEditForm;

    F.ShaderPanel.Caption := F.ShaderPanel.Caption + ' (' + Prop.Name + ')';
    F.ShaderSynEdit.Text := String(Component.GetProperty(Prop).StringValue);
    F.ShaderSynEdit.ResetModificationIndicator;

    F.ShaderSynEdit.Font.Size := Self.SynEditFontSize;
    F.CompileShaderButton.OnClick := Self.CompileShaderButtonClick;
  end;

  procedure ShowFloatEditor(Edit: TEdit; IsScalar : boolean);
  var
    F: TFloatPropEditForm;
    CurValue : single;
  begin
    F := MakePropEditor(TFloatPropEditForm) as TFloatPropEditForm;

    CurValue := StrToFloatDef(Edit.Text,0);
    if IsScalar then
    begin
      F.MinFloat := 0;
      F.MaxFloat := 1;
    end
    else
    begin
      F.MinFloat := Min(CurValue / 10, CurValue - 5);
      F.MaxFloat := Max(CurValue * 5, CurValue + 5);
    end;
    F.TrackBar1.Position := Round((CurValue-F.MinFloat) / ((F.MaxFloat-F.MinFloat)/F.TrackBar1.Max));
    F.FloatEdit := Edit;
    F.PropIndex := Edit.Tag;
  end;

var
  Key : TPropEditKey;
begin
  if PropEditor<>nil then
  begin
    PropEditor.SaveChanges;
    FreeAndNil(PropEditor);
  end;

  Key := TPropEditKey.Create(Component,Prop);
  if DetachedPropEditors.ContainsKey(Key) then
    DetachedPropEditors[Key].BringToFront
  else if (Sender is TEdit) and
    (Prop.PropertyType in [zptFloat,zptScalar,zptRectf,zptVector3f]) then
    ShowFloatEditor(Sender as TEdit,Prop.PropertyType=zptScalar)
  else if (Sender is TEdit) and (TEdit(Sender).Tag=100) then
    ShowExprEditor(Sender as TEdit)
  else if (Sender is TEdit) and (TEdit(Sender).Tag=101) then
    ShowShaderEditor(Sender as TEdit);
end;

procedure TEditorForm.FillQuickCompList;
var
  Infos : PComponentInfoArray;
  Ci : TZComponentInfo;
  I : TZClassIds;
  Item : TListItem;
begin
  Infos := ZClasses.ComponentManager.GetAllInfos;
  for I := Low(TComponentInfoArray) to High(TComponentInfoArray) do
  begin
    if I=AnyComponentClassId then
      Continue;
    Ci := TZComponentInfo(Infos[I]);
    Assert(Ci<>nil, 'Component info=nil. Component class removed?');
    if Ci.NoUserCreate then
      Continue;
    Item := QuickCompListView.Items.Add;
    Item.Caption := Ci.ZClassName;
    Item.Data := Ci;
    Item.ImageIndex := Ci.ImageIndex;
  end;
  SetLength(QuickCompEnabledList,QuickCompListView.Items.Count);
//  ListView_SetColumnWidth(QuickCompListView.Handle, 0, 200);
end;

procedure TEditorForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles(WindowHandle, True);
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

procedure TEditorForm.OpenAllProjectsMenuItemClick(Sender: TObject);
var
  Path,FileName : string;
begin
  //Debug code to test opening all projects
  Path := Self.ExePath + 'Projects';
  for FileName in TDirectory.GetFiles(Path,'*.zgeproj',TSearchOption.soAllDirectories) do
  begin
    CloseProject;
    OutputDebugString(PWideChar(FileName));
    OpenProject(FileName);
  end;
end;

procedure TEditorForm.OpenProject(const FileName : string; const IsTemplate : boolean = False);
var
  C : TZComponent;

  function InNewProject : TZApplication;
  begin
    Result := TZApplication.Create(nil);
    Result.Name:='App';
    Result.FileVersion := AppFileVersion;
    Result.RefreshSymbolTable;
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
  Tree.Items[0].Selected := True;  //Select "App" component as default
  //After scaling, hscroll position is sometimes not left 0
  Tree.Perform(WM_HSCROLL, MakeWParam(SB_PAGELEFT, 0), 0);
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

  //Sätt till nytt värde så att form.caption ändras
  _FileChanged := True;
  SetFileChanged(False);

  ViewerPageControl.ActivePage := ViewerBlankTabSheet;

  try
    //Must compile directly after load because no zc-instructions are saved in the xml
    CompileAll;
  finally
    TThread.Synchronize(nil,
      procedure
      begin
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
        try
          //App needs driver to allow editor to preview graphics
          ZApp.Driver := GLDrivers.CreateDriver(ZApp.GLBase);
          //ZApp.DesignerStart(Glp.Width,Glp.Height);
          //ZApp.DesignerStop;
        except
          on E : EZHalted do ;
        end;
        CheckGLError;
      end
    );
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

procedure TEditorForm.AddOneLogString(const S : string; Log : TLog; Level : TLogLevel);
var
  Data : TListLogItem;
begin
  while LogListBox.Items.Count>10000 do
  begin
    LogListBox.Items.Objects[0].Free;
    LogListBox.Items.Delete(0);
  end;
  Data := TListLogItem.Create;
  Data.Log := Log;
  Data.Msg := S;
  Data.Level := Level;
  LogListBox.Items.AddObject(S,Data);
end;

procedure TEditorForm.OnReceiveLogMessage(Log : TLog; Mess : TLogString; Level : TLogLevel);
begin
  TThread.Synchronize(nil,
    procedure
    var
      I : integer;
      Tmp : TStringList;
    begin
      if Self.LogShowTraceOnly.Checked and (Level<>lleUserTrace) then
        Exit;
      if Pos(#12,Mess)>0 then
      begin
        LogClearMenuItemClick(nil);
        Mess := StringReplace(Mess,#12,'',[rfReplaceAll]);
      end;
      if Pos(#10,Mess)=0 then
        AddOneLogString(Mess,Log,Level)
      else
      begin
        Tmp := TStringList.Create;
        Tmp.Text := Mess;
        for I := 0 to Tmp.Count - 1 do
          AddOneLogString(Tmp[I],Log,Level);
        Tmp.Free;
      end;
      LogListBox.ItemIndex := LogListBox.Items.Count-1;
    end
  );
end;

procedure TEditorForm.ReadAppSettingsFromIni;
var
  Ini : TIniFile;
  Section,S : string;
  OldState : TWindowState;
begin
  Ini := TIniFile.Create( ChangeFileExt(Application.ExeName,'.ini') );
  try
    Section := 'Designer';

    Self.Width := Max(Ini.ReadInteger(Section,'Width',Self.Width),100);
    Self.Height := Max(Ini.ReadInteger(Section,'Height',Self.Height),100);
    if Ini.ReadBool(Section,'IsMaximized',False) then
      Self.WindowState:=wsMaximized;

    Self.MainScaling := Ini.ReadInteger(Section,'Scaling',100);

    OldState := Self.WindowState; //ChangeScale switches from Maximized to Normal
    ChangeScale(Self.MainScaling,100);
    if Self.MainScaling<>100 then
    begin
      ResizeImageListImagesforHighDPI(Self.ActionImageList);
      ResizeImageListImagesforHighDPI(Self.ActionDisabledImageList);
    end;
    Self.WindowState := OldState;

    GuiLayout := Min(Ini.ReadInteger(Section,'GuiLayout',1),1);
    if GuiLayout=0 then
    begin
      PropListPanel.Parent := LeftPanel;
      PropListPanel.Align := alBottom;
      TreePanel.Align := alClient;
      Splitter3.Parent := LeftPanel;
      Splitter3.Align := alBottom;
      Splitter3.Cursor := crVSplit;
      PropListPanel.Height := Self.Height div 2;
    end;

    SynEditFontSize := Ini.ReadInteger(Section,'CodeEditorFontSize',10);

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

    LowerRightPanel.Height := Min(Max(Ini.ReadInteger(Section,'LowerRightPanel.Height',LowerRightPanel.Height),100),Screen.Height);
    LogPanel.Width := Min(Max(Ini.ReadInteger(Section,'LogPanel.Width',LogPanel.Width),20),Screen.Width);
    LeftPanel.Width := Min(Max(Ini.ReadInteger(Section,'LeftPanel.Width',LeftPanel.Width),20),Screen.Width);

    Self.PackerProg := Ini.ReadString(Section,'PackerProg','{$toolpath}upx.exe');
    Self.PackerParams := Ini.ReadString(Section,'PackerParams','{$exename}');

    Self.AutoCompTimerInterval := Ini.ReadInteger(Section,'CodeCompletionDelay',2000);

    Self.AndroidSdkPath := Ini.ReadString(Section,'AndroidSdkPath','');
    Self.AndroidSdCardPath := Ini.ReadString(Section,'AndroidSdCardPath','/sdcard/');
    Self.AndroidAntPath := Ini.ReadString(Section,'AndroidAntPath','');

    Self.AndroidKeystorePath := Ini.ReadString(Section,'AndroidKeystorePath','');
    Self.AndroidKeystoreAlias := Ini.ReadString(Section,'AndroidKeystoreAlias','');

    EnableThreadedProcessingMenuItem.Checked := Ini.ReadBool(Section,'UseThreadedProcessing',True);
    EnableThreadedProcessingMenuItem.OnClick(EnableThreadedProcessingMenuItem);
  finally
    Ini.Free;
  end;
end;

procedure TEditorForm.ResizeImageListImagesforHighDPI(const imgList: TImageList);
//Credits: http://zarko-gajic.iz.hr/resizing-delphis-timagelist-bitmaps-to-fit-high-dpi-scaling-size-for-menus-toolbars-trees-etc/
const
  DevImgSIZE = 16;
var
  ii : integer;
  mb, ib, sib, smb : TBitmap;
begin
  if Self.MainScaling=100 then Exit;

  //clear images
  highDPIImageListContainer.Clear;

  //add from source image list
  for ii := 0 to -1 + imgList.Count do
    highDPIImageListContainer.AddImage(imgList, ii);

  //set size to match DPI size (like 250% of 16px = 40px)
  imgList.SetSize(MulDiv(DevImgSIZE, Self.MainScaling, 100), MulDiv(DevImgSIZE, Self.MainScaling, 100));

  //add images back to original ImageList stretched (if DPI scaling > 150%) or centered (if DPI scaling <= 150%)
  for ii := 0 to -1 + highDPIImageListContainer.Count do
  begin
    sib := TBitmap.Create; //stretched (or centered) image
    smb := TBitmap.Create; //stretched (or centered) mask
    try
      sib.Width := imgList.Width;
      sib.Height := imgList.Height;
      sib.Canvas.FillRect(sib.Canvas.ClipRect);
      smb.Width := imgList.Width;
      smb.Height := imgList.Height;
      smb.Canvas.FillRect(smb.Canvas.ClipRect);

      ib := TBitmap.Create;
      mb := TBitmap.Create;
      try
        ib.Width := DevImgSIZE;
        ib.Height := DevImgSIZE;
        ib.Canvas.FillRect(ib.Canvas.ClipRect);

        mb.Width := DevImgSIZE;
        mb.Height := DevImgSIZE;
        mb.Canvas.FillRect(mb.Canvas.ClipRect);

        ImageList_DrawEx(highDPIImageListContainer.Handle, ii, ib.Canvas.Handle, 0, 0, ib.Width, ib.Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
        ImageList_DrawEx(highDPIImageListContainer.Handle, ii, mb.Canvas.Handle, 0, 0, mb.Width, mb.Height, CLR_NONE, CLR_NONE, ILD_MASK);

        if Screen.PixelsPerInch * 100 / 96 <= 150 then //center if <= 150%
        begin
          sib.Canvas.Draw((sib.Width - ib.Width) DIV 2, (sib.Height - ib.Height) DIV 2, ib);
          smb.Canvas.Draw((smb.Width - mb.Width) DIV 2, (smb.Height - mb.Height) DIV 2, mb);
        end
        else //stretch if > 150%
        begin
          sib.Canvas.StretchDraw(Rect(0, 0, sib.Width, sib.Width), ib);
          smb.Canvas.StretchDraw(Rect(0, 0, smb.Width, smb.Width), mb);
        end;
      finally
        ib.Free;
        mb.Free;
      end;

      imgList.Add(sib, smb);
    finally
      sib.Free;
      smb.Free;
    end;
  end;
end;

procedure TEditorForm.WipeUndoHistory;
begin
  UndoNodes.Clear;
  UndoIndices.Clear;
  UndoParent := nil;
end;

procedure TEditorForm.WmActivate(var M: TMessage);
begin
  if M.WParam=WA_ACTIVE then
  begin
    //From Kjell: Avoid keeping a input method to this window
    ImmAssociateContext(Self.Handle,0);
  end;
  inherited;
end;

procedure TEditorForm.WMDROPFILES(var msg: TWMDropFiles);
var
  i, fileCount: integer;
  fileName: array[0..MAX_PATH] of char;
  ModelFiles,BitmapFiles,AudioFiles : TStringList;
  BmMatch,AudioMatch,ModelMatch : string;
begin
  BitmapFiles := TStringList.Create;
  AudioFiles := TStringList.Create;
  ModelFiles := TStringList.Create;

  BmMatch := '.png .jpg .jpeg .bmp .gif';
  AudioMatch := '.ogg';
  ModelMatch := '.3ds .obj';

  fileCount:=DragQueryFile(msg.Drop, $FFFFFFFF, fileName, MAX_PATH);
  for i := 0 to fileCount - 1 do
  begin
    DragQueryFile(msg.Drop, i, fileName, MAX_PATH);
    if Pos(LowerCase(ExtractFileExt(FileName)),BmMatch)>0 then
      BitmapFiles.Add(Filename)
    else if Pos(LowerCase(ExtractFileExt(FileName)),AudioMatch)>0 then
      AudioFiles.Add(Filename)
    else if Pos(LowerCase(ExtractFileExt(FileName)),ModelMatch)>0 then
      ModelFiles.Add(Filename)
  end;
  DragFinish(msg.Drop);

  if BitmapFiles.Count>0 then
    ImportBitmaps(BitmapFiles);
  if AudioFiles.Count>0 then
    ImportAudioFiles(AudioFiles);
  if ModelFiles.Count>0 then
    ImportModelFiles(ModelFiles);

  BitmapFiles.Free;
  AudioFiles.Free;
  ModelFiles.Free;
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

      Ini.WriteInteger(Section,'CodeEditorFontSize',SynEditFontSize);

      Ini.WriteInteger(Section,'Scaling',Self.MainScaling);

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

      Ini.WriteInteger(Section,'CodeCompletionDelay',Self.AutoCompTimerInterval);

      Ini.WriteString(Section,'Style', TStyleManager.ActiveStyle.Name);

      Ini.WriteString(Section,'AndroidSdkPath',Self.AndroidSdkPath);
      Ini.WriteString(Section,'AndroidSdCardPath',Self.AndroidSdCardPath);
      Ini.WriteString(Section,'AndroidAntPath',Self.AndroidAntPath);

      Ini.WriteString(Section,'AndroidKeystorePath',Self.AndroidKeystorePath);
      Ini.WriteString(Section,'AndroidKeystoreAlias',Self.AndroidKeystoreAlias);

      Ini.WriteBool(Section,'UseThreadedProcessing',EnableThreadedProcessingMenuItem.Checked);
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
  Driver.InitGL;

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
var
  IsRenderComponent : boolean;
begin
  if ShowNode=nil then
    Exit;

  //Set window size to make sure camera ratio calculations are correct
  if ZApp<>nil then
  begin
    ZApp.ScreenWidth := Glp.Width;
    ZApp.ScreenHeight := Glp.Height;
    ZApp.UpdateViewport;
    ZApp.WindowHandle := Glp.Handle;
  end;

  try
    if (ShowNode is TZApplication) and (IsAppRunning) then
    begin
      try
        //Update app
        (ShowNode as TZApplication).Main;
      except
        on E : Exception do
        begin
          AppPreviewStopAction.Execute;
          raise;
        end;
      end;
    end
    else if not RenderAborted then
    begin
      IsRenderComponent := ((ShowNode is TStateBase) or (ShowNode is TZApplication));

      if not IsRenderComponent then
        //Gör update för att prop-ändringar skall slå igenom
        //Men inte för appar och states, då körs update-kod t.ex. med centerMouse();
        ShowNode.Update;

      glViewport(0, 0, Glp.Width, Glp.Height);

      //todo: delphi 2010 needs this line
      Set8087CW($133F);
      ViewTranslateLabel.Caption := FloatToStr( RoundTo(ViewTranslate[0],-1) ) + #13 +
        FloatToStr( RoundTo(ViewTranslate[1],-1) ) + #13 +
        FloatToStr( RoundTo(ViewTranslate[2],-1) );

      if (ShowNode is TModel) then
        DrawModel
      else if IsRenderComponent then
        DrawOnRenderComponent
      else
      begin
        //Prevent displaying junk
        glClearColor(ZApp.PreviewClearColor.V[0],ZApp.PreviewClearColor.V[1],ZApp.PreviewClearColor.V[2],0);
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      end;

      //Run GC because rendering might generate garbage (expressions are executed)
      if ManagedHeap_GetAllocCount>0 then
        ManagedHeap_GarbageCollect;
    end;
  except
    on E : Exception do
    begin
      RenderAborted := True;
      Log.Error(E.Message);
    end;
  end;
end;

procedure TEditorForm.SelectComponent(C: TZComponent);

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

var
  TreeHadFocus : boolean;
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
    TreeHadFocus := Self.ActiveControl=Self.Tree;
    Ed.WantsFocus.SetFocus;
    if TreeHadFocus then
      Self.Tree.SetFocus;
  end;
end;

function TEditorForm.MakeCompEditor(Kind : TCompEditFrameBaseType) : TCompEditFrameBase;
begin
  Result := Kind.Create(Self);
  Result.Name := '';
  Result.Parent := CompEditorParentPanel;
  Result.Align := alClient;
  DetachCompEditorButton.Visible := True;
end;

procedure TEditorForm.SetShowNode(Node : TZComponent);
begin
  if CompEditor<>nil then
  begin
    CompEditor.OnEditorClose;
    FreeAndNil(CompEditor);
  end;

  if IsAppRunning and (not (Node is TZApplication)) then
    AppPreviewStopAction.Execute;

  CompEditorTreeNode := nil;

  ShowNode := Node;

  if DetachedCompEditors.ContainsKey(ShowNode) then
  begin
    DetachedCompEditors[ShowNode].BringToFront;
    ViewerPageControl.ActivePage := ViewerBlankTabSheet;
  end
  else if ShowNode is AudioComponents.TSound then
  begin
    ViewerPageControl.ActivePage := ViewerCompTabSheet;
    CompEditor := MakeCompEditor(TSoundEditFrame);
  end
  else if ShowNode is AudioComponents.TMusic then
  begin
    ViewerPageControl.ActivePage := ViewerCompTabSheet;
    CompEditor := MakeCompEditor(TMusicEditFrame);
  end
  else if ShowNode is TZBitmap then
  begin
    ViewerPageControl.ActivePage := ViewerCompTabSheet;
    CompEditor := MakeCompEditor(TBitmapEditFrame);
  end
  else if ShowNode is TMesh then
  begin
    ViewerPageControl.ActivePage := ViewerCompTabSheet;
    CompEditor := MakeCompEditor(TMeshEditFrame);
  end
  else if ShowNode is TSpriteSheet then
  begin
    ViewerPageControl.ActivePage := ViewerCompTabSheet;
    CompEditor := MakeCompEditor(TSpriteSheetEditFrame);
  end
  else if ShowNode is TTileSet then
  begin
    ViewerPageControl.ActivePage := ViewerCompTabSheet;
    CompEditor := MakeCompEditor(TTileSetEditFrame);
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

procedure TEditorForm.FileOpenActionBeforeExecute(Sender: TObject);
begin
  FileOpenAction.Dialog.InitialDir := ExtractFilePath( Self.CurrentFileName );
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
  begin
    if (Tree.ZSelected.Component is TCommand) and (not (Tree.ZSelected.Component is TContentProducer)) then
      TCommand(Tree.ZSelected.Component).Execute
    else
      Tree.ZSelected.Component.Change;
  end;
  Glp.Invalidate;
end;

procedure TEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I : integer;
begin
  Action := caFree;

  //Need to remove synedit first, otherwise synedit assertion fails
  for I := 0 to LogListBox.Items.Count - 1 do
  begin
    LogListBox.Items.Objects[I].Free;
    LogListBox.Items.Objects[I] := nil;
  end;
end;

procedure TEditorForm.OnPropValueChange;
var
  F : TForm;
  C : TCompEditFrameBase;
begin
  //Värde har ändrats i propertyeditorn
  Glp.Invalidate;

  if CompEditor<>nil then
    CompEditor.OnPropChanged;

  for F in DetachedCompEditors.Values do
  begin
    C := F.Controls[0] as TCompEditFrameBase;
    C.OnPropChanged;
  end;

  SetFileChanged(True);
end;

procedure TEditorForm.FilterQuickCompList;
var
  Ci : TZComponentInfo;
  Prop : TZProperty;
  ParentComps,ParentLists : TStringList;
  CurParent : TZComponentTreeNode;
  Item : TListItem;
  I,J : integer;
  PassedFilter,Enabled : boolean;
begin
  if (not Assigned(Tree.Selected)) or (not Assigned(Tree.ZSelected.ComponentList)) then
    Exit;

  Prop := Tree.ZSelected.Prop;

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

    for I := 0 to QuickCompListView.Items.Count - 1 do
    begin
      Item := QuickCompListView.Items[I];
      Ci := TZComponentInfo(Item.Data);

      Enabled := True;
      if (Ci.NeedParentComp<>'') and (ParentComps.IndexOf(Ci.NeedParentComp)=-1) then
        Enabled := False;

      if High(Prop.ChildClasses)>-1 then
      begin
        PassedFilter :=False;
        for J := 0 to High(Prop.ChildClasses) do
          if (Ci.ZClass = Prop.ChildClasses[J]) or
            (Ci.ZClass.InheritsFrom(Prop.ChildClasses[J])) then
          begin
            PassedFilter := True;
            Break;
          end;
        if not PassedFilter then
          Enabled := False;
      end;

      QuickCompEnabledList[I] := Enabled;
    end;

  finally
    ParentComps.Free;
    ParentLists.Free;
  end;
  QuickCompListView.Invalidate;
end;

procedure TEditorForm.OnTreeSelectItem(Sender: TObject; Node : TTreeNode);
begin
  if (Tree.ZSelected<>nil) and (Tree.ZSelected.Component<>nil) then
  begin
    PropListParent.Visible := True;
    QuickCompListParent.Visible := False;
    SelectComponent( Tree.ZSelected.Component )
  end
  else if (Tree.ZSelected<>nil) and (Tree.ZSelected.ComponentList<>nil) then
  begin
    PropListParent.Visible := False;
    QuickCompListParent.Visible := True;
    FilterQuickCompList;
    Ed.SetComponent(nil);
  end
  else
  begin
    PropListParent.Visible := False;
    QuickCompListParent.Visible := False;
    Ed.SetComponent(nil);
  end;
end;

procedure TEditorForm.RefreshCompEditorTreeNode;

  procedure DoOne(CompEditor : TCompEditFrameBase);
  begin
    Tree.RefreshNode(CompEditor.TreeNode,CompEditor.Component);
    CompEditor.TreeNode.Expand(False);
    CompEditor.NeedRefreshTreeNode := False;
  end;

var
  F : TForm;
  C : TCompEditFrameBase;
begin
  //Värde har ändrats i propertyeditorn
  Glp.Invalidate;

  if CompEditor<>nil then
    DoOne(CompEditor);
  for F in DetachedCompEditors.Values do
  begin
    C := F.Controls[0] as TCompEditFrameBase;
    DoOne(C);
  end;

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
var
  F : TForm;
begin
  if Assigned(ActiveControl) and
    (ActiveControl is TEdit) and
    Assigned((ActiveControl as TEdit).OnExit) then
    //Spara stringedit
    (ActiveControl as TEdit).OnExit(ActiveControl);

  if Assigned(PropEditor) then
    PropEditor.SaveChanges;

  for F in DetachedPropEditors.Values do
    (F as TCustomPropEditBaseForm).SaveChanges;
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

    ZApp.Driver.EnableMaterial(DefaultMaterial);
    try
      OnRender.ExecuteCommands;
    except
      on E : Exception do
      begin //Detect errors in onrender-list
        RenderAborted := True;
        raise;
      end;
    end;
    ZApp.Driver.EnableMaterial(DefaultMaterial);
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

  ZApp.Driver.MatrixMode(GL_PROJECTION);
  ZApp.Driver.LoadIdentity();
  //calculate the aspect ratio of the window
  //we'll use a perspective matrix to view our scene
  ZApp.Driver.Perspective(45.0, Glp.Width/Glp.Height, 0.1, 200.0);
//  glTranslatef(0,0,-10);
  ZApp.Driver.MatrixMode(GL_MODELVIEW);

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
    ZApp.Driver.LoadIdentity();

    //translate
    ZApp.Driver.Translate(ViewTranslate[0], ViewTranslate[1], ViewTranslate[2]);

    //rotate
    ZApp.Driver.Rotate( ViewRotate[0] , 1.0, 0.0, 0.0);
    ZApp.Driver.Rotate( ViewRotate[1] , 0.0, 1.0, 0.0);
    ZApp.Driver.Rotate( ViewRotate[2] , 0.0, 0.0, 1.0);

    if UpdateTimeCheckBox.Checked then
    begin
      ZApp.UpdateTime;
      try
        Model.DesignerUpdate;
      except
        on E : Exception do
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
    ZApp.Driver.EnableMaterial(DefaultMaterial);
    try
      //Undo translation so that the model is in screen center
      ZApp.Driver.Translate(-Model.Position[0],-Model.Position[1],-Model.Position[2]);
      Renderer.RenderModel(Model);
    except
      on E : Exception do
      begin //Detect errors in onrender-list
        RenderAborted := True;
        raise;
      end;
    end;

  glPopAttrib();

  glFlush;
end;

procedure TEditorForm.Timer1Timer(Sender: TObject);
begin
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
  DetachedCompEditors.Free;
  DetachedPropEditors.Free;
  MruList.Free;
  Renderer.CleanUp;
  UndoNodes.Free;
  UndoIndices.Free;
  SysLibrary.Free;
  Driver.Free;
  GamutZBitmap.Free;
  EvalHistory.Free;
end;


procedure TEditorForm.OnDetachedCompEditorClose(Sender: TObject; var Action: TCloseAction);
var
  C : TCompEditFrameBase;
begin
  C := (Sender as TForm).Controls[0] as TCompEditFrameBase;
  if DetachedCompEditors.ContainsKey(C.Component) then
  begin
    C.OnEditorClose;
    DetachedCompEditors.Remove(C.Component);
  end;
end;

procedure TEditorForm.OnDetachedPropEditorClose(Sender: TObject; var Action: TCloseAction);
var
  F : TCustomPropEditBaseForm;
begin
  F := (Sender as TCustomPropEditBaseForm);
  if DetachedPropEditors.ContainsKey(TPropEditKey.Create(F.Component,F.Prop)) then
  begin
    F.SaveChanges;
    DetachedPropEditors.Remove(TPropEditKey.Create(F.Component,F.Prop));
  end;
end;

procedure TEditorForm.DetachCompEditorButtonClick(Sender: TObject);
var
  F : TForm;
begin
  F := TForm.CreateNew(Self);
  F.Caption := 'Editing: ' + String(CompEditor.Component.GetDisplayName);

  F.SetBounds(CompEditor.ClientToScreen(Point(0,0)).X,CompEditor.ClientToScreen(Point(0,0)).Y,CompEditor.Width,CompEditor.Height);
  F.Position := poDesigned;
  CompEditor.Parent := F;
  F.Show;
  F.OnClose := Self.OnDetachedCompEditorClose;
  F.KeyPreview := True;
  F.OnKeyPress := Self.OnKeyPress;
  DetachedCompEditors.Add(CompEditor.Component,F);
  DetachCompEditorButton.Visible := False;
  CompEditor := nil;
end;

procedure TEditorForm.DetachPropEditorButtonClick(Sender: TObject);
var
  F : TCustomPropEditBaseForm;
  R : TRect;
begin
  F := Self.PropEditor;
  F.Caption := 'Editing: ' + String(F.Component.GetDisplayName) + ' ' + F.Prop.Name;

  R := F.BoundsRect;
//  F.SetBounds(CompEditor.ClientToScreen(Point(0,0)).X,CompEditor.ClientToScreen(Point(0,0)).Y,CompEditor.Width,CompEditor.Height);
//  F.Position := poDesigned;
//  CompEditor.Parent := F;
//  F.Show;
  F.Align := alNone;
  F.Parent := nil;

  F.SetBounds(PropEditParentPanel.ClientToScreen(Point(0,0)).X,PropEditParentPanel.ClientToScreen(Point(0,0)).Y,R.Width,R.Height);
  F.Position := poDesigned;

  F.BorderStyle := bsSizeable;
  F.OnClose := Self.OnDetachedPropEditorClose;
  DetachedPropEditors.Add( TPropEditKey.Create(F.Component,F.Prop) ,F);
  F.DetachButton.Visible := False;

  PropEditor := nil;
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

procedure TEditorForm.Findunsedcomponents1Click(Sender: TObject);
var
  I,J : integer;
  L : TObjectList;
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  C : TZComponent;
  Counts : TDictionary<TZComponent,Integer>;
  S : string;
begin
  CompileAll(True);
  L := TObjectList.Create(False);
  Counts := TDictionary<TZComponent,Integer>.Create;
  try
    GetAllObjects(Self.Root,L);
    for I := 0 to L.Count-1 do
    begin
      C := TZComponent(L[I]);

      if (Length(C.Name)>0)
        and (not (C is TDefineConstant))
        and (not (C is TCommand))
        and (not (C is TZApplication))
        and (not (C is TLogicalGroup))
      then
      begin
        if not Counts.ContainsKey(C) then
          Counts.Add(C,0);
      end;

      PropList := C.GetProperties;
      for J := 0 to PropList.Count-1 do
      begin
        Prop := TZProperty(PropList[J]);
        case Prop.PropertyType of
          zptComponentRef :
            begin
              Value := C.GetProperty(Prop);
              if Value.ComponentValue=nil then
                Continue;
              Counts.AddOrSetValue(Value.ComponentValue,1);
            end;
        end;
      end;
    end;

    S := '';
    for C in Counts.Keys do
    begin
      if (Counts[C]=0) and (not HasReferers(Root,C)) then
      begin
        if S='' then
          S := string(C.Name)
        else
          S := S + #13 + string(C.Name);
      end;
    end;
    if S='' then
      ShowMessage('No unused components found')
    else
      ShowMessage('The following components are not used in the project:'#13#13 + S);

  finally
    L.Free;
    Counts.Free;
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

procedure TEditorForm.HelpContentsActionExecute(Sender: TObject);
begin
  HtmlHelp(0,Application.HelpFile, HH_DISPLAY_TOC, 0);
end;

procedure TEditorForm.ShowCompilerDetailsActionExecute(Sender: TObject);
begin
  ShowCompilerDetailsAction.Checked := not ShowCompilerDetailsAction.Checked;
end;

procedure TEditorForm.LogShowTraceOnlyClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := not (Sender as TMenuItem).Checked;
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
    F.UpDown1.Position := Self.AutoCompTimerInterval;
    F.AndroidSdkPathEdit.Text := Self.AndroidSdkPath;
    F.AndroidSdCardPathEdit.Text := Self.AndroidSdCardPath;
    F.AndroidAntPathEdit.Text := Self.AndroidAntPath;
    F.AndroidKeystorePathEdit.Text := Self.AndroidKeystorePath;
    F.AndroidKeystoreAliasEdit.Text := Self.AndroidKeystoreAlias;
    F.ScalingCombo.ItemIndex := F.ScalingCombo.Items.IndexOf( IntToStr(Self.MainScaling) );

    if F.ShowModal=mrOk then
    begin
      Self.PackerProg := F.PackerEdit.Text;
      Self.PackerParams := F.PackerParamsEdit.Text;
      Self.GuiLayout := F.GuiLayoutCombo.ItemIndex;
      Self.AutoCompTimerInterval := F.UpDown1.Position;
      Self.AndroidSdkPath := F.AndroidSdkPathEdit.Text;
      Self.AndroidSdCardPath := F.AndroidSdCardPathEdit.Text;
      Self.AndroidAntPath := F.AndroidAntPathEdit.Text;
      Self.AndroidKeystorePath := F.AndroidKeystorePathEdit.Text;
      Self.AndroidKeystoreAlias := F.AndroidKeystoreAliasEdit.Text;
      Self.MainScaling := StrToIntDef(F.ScalingCombo.Text,100);
    end;
  finally
    F.Free;
  end;
end;

procedure TEditorForm.CompileShaderButtonClick(Sender: TObject);
var
  F : TShaderPropEditForm;
  S : AnsiString;
  Value : TZPropertyValue;
begin
  F := (Sender as TComponent).Owner as TShaderPropEditForm;

  F.CompileShaderButton.Enabled := False;

  Value := F.Component.GetProperty(F.Prop);
  S := AnsiString(TrimRight(F.ShaderSynEdit.Text));
  if Value.StringValue<>S then
  begin
    Value.StringValue := S;
    F.Component.SetProperty(F.Prop,Value);
    SetFileChanged(True);
    try
      (F.Component as TShader).UseShader; //Force a compile
      F.HideError;
      glUseProgram(0); //Then turn it off
      OnPropValueChange;
      if F.Component=Self.Tree.ZSelected.Component then
        Self.Ed.SetComponent(F.Component); //This will force rebuildgui to show latest values
    except
      on E : EShaderException do
      begin
        F.ShowError(E.Message);
      end;
    end;
  end;
end;

procedure TEditorForm.ExprCompileButtonClick(Sender: TObject);
var
  C : TZComponent;
  Success : boolean;
  I : integer;
  Node : TZComponentTreeNode;
  F : TExprPropEditForm;
  Value : TZPropertyValue;
  S : string;
begin
  F := (Sender as TComponent).Owner as TExprPropEditForm;

  F.ExprCompileButton.Enabled := False;

  Value := F.Component.GetProperty(F.Prop);
  S := TrimRight(F.ExprSynEdit.Text);

  if Value.ExpressionValue.Source=S then
    Exit;
  Value.ExpressionValue.Source:=S;

  F.Component.SetProperty(F.Prop,Value);
  SetFileChanged(True);

  C := F.Component;
  Success:=False;
  try
    if (C is TZLibrary) or (C is TZExternalLibrary) then
      CompileAll(True)
    else
      DoCompile(F.TreeNode,Value,F.Prop);
    OnPropValueChange;
    Success:=True;
  except
    on E : EParseError do
    begin
      if E.Component<>C then
      begin
        Node := Tree.FindNodeForComponent(E.Component);
        ShowMessage( 'Error in expression for node: ' + String(Node.Component.GetDisplayName) + ' '#13 + E.Message );
        Node.Expand(True);
        Tree.Selected := Node;
      end else
      begin
        F.ExprSynEdit.CaretXY := BufferCoord(E.Col-1,E.Line);
        F.ExprSynEdit.BlockBegin := BufferCoord(0,E.Line);
        F.ExprSynEdit.BlockEnd := BufferCoord(0,E.Line+1);
        F.ExprSynEdit.SetFocus;
        //ShowMessage( E.Message );
        F.ShowError(E.Message);
        Log.Write(E.Message);
      end;
    end;
    on E : ECodeGenError do
    begin
      if E.Component<>C then
      begin
        Node := Tree.FindNodeForComponent(E.Component);
        ShowMessage( 'Error in expression for node: ' + String(Node.Component.GetDisplayName) + ' '#13 + E.Message );
        Node.Expand(True);
        Tree.Selected := Node;
      end else
      begin
        F.CompileErrorLabel.BevelKind := bkTile;
        F.ShowError(E.Message);
        Log.Write(E.Message);
      end;
    end;
  end;
//  Tree.RefreshNode(Tree.Selected,Selected);
  if Success then
  begin
    F.HideError;
    if ShowCompilerDetailsAction.Checked and (not (C is TZExternalLibrary)) then
    begin
      ZLog.GetLog(Self.ClassName).Write(Compiler.CompileDebugString);
      ZLog.GetLog(Self.ClassName).Write('----');
      if Value.ExpressionValue.Code.Count<100 then
        for I := 0 to Value.ExpressionValue.Code.Count - 1 do
          Log.Write( (Value.ExpressionValue.Code[I] as TExpBase).ExpAsText );
    end;

    if F.Component=Self.Tree.ZSelected.Component then
      Self.Ed.SetComponent(F.Component); //This will force rebuildgui to show latest values
  end;
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
  CommitAllEdits;
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
    F.SynEdit.Font.Size := Self.SynEditFontSize;
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

procedure TEditorForm.EnableFunctionInliningClick(Sender: TObject);
begin
  CompilerOptions.InliningEnabled := (Sender as TMenuItem).Checked;
end;

procedure TEditorForm.EnableThreadedProcessingMenuItemClick(Sender: TObject);
begin
  ZClasses.Tasks.Enabled := (Sender as TMenuItem).Checked;
end;

procedure TEditorForm.EvalEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=13 then
  begin
    Key := 0;
    Self.EvalHistory.Add(EvalEdit.Text);
    EvalEdit.Tag := Self.EvalHistory.Count-1;
    Self.ParseEvalExpression( EvalEdit.Text );
  end;
  if EvalHistory.Count>0 then
  begin
    if Key=VK_UP then
    begin
      if EvalEdit.Tag=0 then
        EvalEdit.Tag := EvalHistory.Count-1
      else
        EvalEdit.Tag := (EvalEdit.Tag - 1) mod EvalHistory.Count;
      EvalEdit.Text := EvalHistory[ EvalEdit.Tag ];
      Key := 0;
    end;
    if Key=VK_DOWN then
    begin
      EvalEdit.Tag := (EvalEdit.Tag + 1) mod EvalHistory.Count;
      EvalEdit.Text := EvalHistory[ EvalEdit.Tag ];
      Key := 0;
    end;
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
    M.LoadFromFile( ExeFile );

    R := M.FindResource('10','42',1053);
    Assert(R<>nil);

    NewData := TMemoryStream.Create;
    try
      NewData.LoadFromFile(DataFile);
      R.ChangeData(NewData);
    finally
      NewData.Free;
    end;

    //Remove the other two resource (packageinfo), saves about 1kb
{$IFNDEF ZZDC_FPC}
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
{$ENDIF}

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

    M.SaveToFile( OutFile );
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
  C,DebugC : TDefineConstant;
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

  DebugC := ZApp.SymTab.Lookup('debug') as TDefineConstant;
  DebugC.Value := 0;

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
  DebugC.Value := 1;

  //Need to recompile afterwards to reset constants in designer
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
    begin
      SelectComponentForm := TSelectComponentForm.Create(Self);
      SelectComponentForm.ScaleBy(Self.MainScaling,100);
    end;

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

procedure TEditorForm.AddNewComponentToTree(C : TZComponent; SelectIt : boolean = true);
var
  Ci : TZComponentInfo;
  S : string;
  Node : TTreeNode;
begin
  Ci := ComponentManager.GetInfo(C);

  if Ci.AutoName then
  begin  //Give unique name
    S := ZApp.SymTab.MakeUnique(Ci.ZClassName);
    C.SetString('Name',AnsiString(S));
    ZApp.SymTab.Add(S,C);
  end;

  TZComponentTreeNode(Tree.Selected).ComponentList.AddComponent(C);
  TZComponentTreeNode(Tree.Selected).ComponentList.Change;

  Node := Tree.AddNode(C,Tree.Selected);
  if SelectIt then
    Node.Selected := SelectIt;
  Node.Parent.Expanded := True;

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

      if DetachedCompEditors.ContainsKey(C) then
        DetachedCompEditors.Remove(C);

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
  Timer1.Interval := 15; //Try 60fps in preview
  IsAppRunning := True;
  //Needed because of styles-bug: http://stackoverflow.com/questions/9580563/disabling-tbutton-issue-on-a-vcl-styled-form
  AppStartButton.Perform(CM_RECREATEWND, 0, 0);
  AppStopButton.Perform(CM_RECREATEWND, 0, 0);
  Self.ShowHint := False;  //http://www.emix8.org/forum/viewtopic.php?t=1045
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
  Timer1.Interval := 30; //Restore 30fps in preview (to avoid fan spinning on laptops)
  //Needed because of styles-bug: http://stackoverflow.com/questions/9580563/disabling-tbutton-issue-on-a-vcl-styled-form
  AppStartButton.Perform(CM_RECREATEWND, 0, 0);
  AppStopButton.Perform(CM_RECREATEWND, 0, 0);
  Self.ShowHint := True;
  if ZApp.HasScriptCreatedComponents then
  begin
    ZApp.HasScriptCreatedComponents := False;
    Tree.SetRootComponent(Self.Root);
  end;
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
  MruListMax=16;
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

procedure TEditorForm.ImportModelFiles(Files: TStringList);
var
  Imp : T3dsImport;
  ObjImp : TObjImport;
  Parent,Node : TZComponentTreeNode;
  S : string;
begin
  for S in Files do
  begin
    Parent := Tree.FindNodeForComponentList(ZApp.Content);
    Assert(Parent<>nil,'Can''t find app.content node');
    if ExtractFileExt(S).ToLower='.3ds' then
    begin
      Imp := T3dsImport.Create(S);
      try
        Imp.Import(True);
        Node := InsertAndRenameComponent(Imp.ResultModelGroup, Parent);
        Node.Expand(False);
        //Auto-select the Model-component
        Tree.Selected := Node.GetLastChild.GetLastChild;
      finally
        Imp.Free;
      end;
    end else if ExtractFileExt(S).ToLower='.obj' then
    begin
      ObjImp := TObjImport.Create(S);
      try
        ObjImp.Import;
        Node := InsertAndRenameComponent(ObjImp.ResultMesh, Parent);
        Node.Expand(False);
        //Auto-select the Mesh-component
        Tree.Selected := Node;
      finally
        ObjImp.Free;
      end;
    end;
  end;
end;

procedure TEditorForm.Import3dsActionExecute(Sender: TObject);
var
  D : TOpenDialog;
  L : TStringList;
begin
  D := TOpenDialog.Create(Self);
  L := TStringList.Create;
  try
    D.Filter := 'All 3d-models|*.3ds;*.obj|3D-studio files (*.3ds)|*.3ds|OBJ files (*.obj)|*.obj';
    D.Options := D.Options + [ofAllowMultiSelect];
//    D.DefaultExt := '*.3ds;*.obj';
    if not D.Execute then
      Exit;

    L.AddStrings( D.Files );
    if L.Count>0 then
      ImportModelFiles(L);
  finally
    D.Free;
    L.Free;
  end;
end;

procedure TEditorForm.ImportBitmaps(Files: TStringList);
var
  Parent: TZComponentTreeNode;
  BmFile: TBitmapFromFile;
  Node: TZComponentTreeNode;
  S: string;
  M : TMemoryStream;
begin
  M := TMemoryStream.Create;
  for S in Files do
  begin
    M.Clear;
    BmFile := nil;
    GetPictureStream(BmFile, S, M);
    if M.Size > 0 then
    begin
      BmFile.BitmapFile.Size := M.Size;
      GetMem(BmFile.BitmapFile.Data, M.Size);
      Move(M.Memory^, BmFile.BitmapFile.Data^, M.Size);
    end;
    Parent := Tree.FindNodeForComponentList(ZApp.Content);
    Node := InsertAndRenameComponent(BmFile.GetOwner, Parent);
    Node.Expand(False);
    Tree.Selected := Node;
  end;
  M.Free;
end;

procedure TEditorForm.ImportAudioActionExecute(Sender: TObject);
var
  D : TOpenDialog;
  Files : TStringList;
begin
  D := TOpenDialog.Create(Self);
  Files := TStringList.Create;
  try
    D.Title := 'Pick audio file to import';
    D.Filter := 'OGG-files (*.ogg)|*.ogg';
    D.DefaultExt := '*.ogg';
    D.Options := D.Options + [ofAllowMultiSelect];
    if not D.Execute then
      Exit;
    Files.AddStrings( D.Files );
    ImportAudioFiles(Files);
  finally
    D.Free;
    Files.Free;
  end;
end;

procedure TEditorForm.ImportAudioFiles(Files: TStringList);
var
  Parent: TZComponentTreeNode;
  Snd : TSound;
  Smp : TSample;
  Import: TSampleImport;
  Node: TZComponentTreeNode;
  S: string;
  M : TMemoryStream;
begin
  M := TMemoryStream.Create;
  for S in Files do
  begin
    M.Clear;
    M.LoadFromFile(S);

    Snd := TSound.Create(nil);
    Snd.SetString('Name','Sound1');

    Smp := TSample.Create(nil);
    Smp.SetString('Name','Sample1');
    Import := TSampleImport.Create(Smp.Producers);
    Import.SetString('Comment','Imported from ' + AnsiString(ExtractFileName(S)));
    Import.SampleFileFormat := sffOGG;

    Import.SampleData.Size := M.Size;
    GetMem(Import.SampleData.Data, M.Size);
    Move(M.Memory^, Import.SampleData.Data^, M.Size);

    Snd.Voice.SampleRef := Smp;
    Snd.Voice.UseSampleHz := True;

    Parent := Tree.FindNodeForComponentList(ZApp.Content);

    InsertAndRenameComponent(Smp, Parent);
    Node := InsertAndRenameComponent(Snd, Parent);

    Tree.Selected := Node;
  end;
  M.Free;
end;


procedure TEditorForm.ImportBitmapActionExecute(Sender: TObject);
var
  D : TOpenPictureDialog;
  Files : TStringList;
begin
  D := TOpenPictureDialog.Create(Self);
  D.Options := D.Options + [ofAllowMultiSelect];
  Files := TStringList.Create;
  try
    if not D.Execute then
      Exit;
    Files.AddStrings( D.Files );
    ImportBitmaps(Files);
  finally
    D.Free;
    Files.Free;
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
      C._ZApp := Self.ZApp;
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

procedure TEditorForm.QuickCompListViewClick(Sender: TObject);
var
  Ci : TZComponentInfo;
  C : TZComponent;
begin
  if QuickCompListView.Selected=nil then
    Exit;
  Ci := TZComponentInfo(QuickCompListView.Selected.Data);
  C := Ci.ZClass.Create(nil);
  AddNewComponentToTree(C, QuickCompListView.Tag<>0);
end;

procedure TEditorForm.QuickCompListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
//Reference: http://theroadtodelphi.wordpress.com/2012/03/14/vcl-styles-and-owner-draw/
var
  LStyles   : TCustomStyleServices;
  LColor    : TColor;
  Fs : TThemedTreeView;
begin
  LStyles := StyleServices;

  if QuickCompEnabledList[Item.Index] then
    Fs := ttItemNormal
  else
    Fs := ttItemDisabled;
  if not LStyles.GetElementColor(LStyles.GetElementDetails(Fs), ecTextColor, LColor) or (LColor = clNone) then
  begin
    if QuickCompEnabledList[Item.Index] then
	    LColor := LStyles.GetSystemColor(clWindowText)
    else
	    LColor := LStyles.GetSystemColor(clGrayText);
  end;

  Sender.Canvas.Font.Color  := LColor;
  Sender.Canvas.Brush.Color := LStyles.GetStyleColor(scListView);
end;

procedure TEditorForm.QuickCompListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  QuickCompListView.Tag := Ord(ssCtrl in Shift);
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
    Node.MoveTo(Node.Parent.GetLastChild,naAdd);
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

procedure TEditorForm.CommitAllEdits;
var
  F : TForm;
  C : TCompEditFrameBase;
begin
  //Close and save all detached editors
  for F in DetachedCompEditors.Values do
  begin
    C := F.Controls[0] as TCompEditFrameBase;
    C.OnEditorClose;
  end;
  DetachedCompEditors.Clear;

  if PropEditor<>nil then
    FreeAndNil(PropEditor);
  for F in DetachedPropEditors.Values do
  begin
    (F as TCustomPropEditBaseForm).SaveChanges;
  end;
  DetachedPropEditors.Clear;
end;

function TEditorForm.CloseProject : boolean;
begin
  Result := True;
  if not Assigned(Root) then
    Exit;

  WriteProjectSettingsToIni;

  ShowNode := nil;
  Tree.LockShowNode := nil;

  CommitAllEdits;

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

  try
    ZApp.Terminate;
  except
    on E : Exception do ShowMessage(E.Message);
  end;

  try
    Root.Free;
  except
    on E : Exception do ShowMessage(E.Message);
  end;
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
var
  F : TForm;
begin
  if (ActiveControl=EvalEdit) and (Key=#13) then
  begin
    Key := #0; //Eat the beep
    Exit;
  end;
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
  for F in DetachedCompEditors.Values do
    (F.Controls[0] as TCompEditFrameBase).OnKeyPress(Key);
end;

var
  LockMouse : TPoint;

procedure TEditorForm.OnGLPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Tmp : TPoint;
  DeltaX,DeltaY : integer;
begin
  //Skip mouse capture because it does not work well with tablets (according to Kjell)
  if {(not Glp.MouseCapture) or }(not ((ssLeft in Shift) or (ssRight in Shift))) or  IsAppRunning then
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
//    SetCursorPos(LockMouse.X,LockMouse.Y);

    LockMouse := Tmp;
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
//    Glp.MouseCapture := True;
    LockMouse := Glp.ClientToScreen( Point(X,Y) );
//    ShowCursor(False);
  end;
end;

procedure TEditorForm.OnGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not IsAppRunning then
  begin
//    Glp.MouseCapture := False;
//    ShowCursor(True);
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

//  C.TextOut(Rect.Left + 2, Rect.Top, LogChars[ (Log.ID mod Length(LogChars))+1 ] );

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
        M.Tag := NativeInt(C);
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
      if I > 0 then
        Result := Result + ', ';
      if Arg.Typ.IsPointer then
        Result := Result + 'ref ';
      if Arg.Typ.Kind=zctArray then
      begin
        if Arg.Id = '' then
          Result := Result + ZcTypeNames[TDefineArray(Arg.Typ.TheArray)._Type] + '[]'
        else
          Result := Result + ZcTypeNames[TDefineArray(Arg.Typ.TheArray)._Type] + '[] ' + Arg.Id;
      end else
      if Arg.Id = '' then
        Result := Result + GetZcTypeName(Arg.Typ)
      else
        Result := Result + GetZcTypeName(Arg.Typ) + ' ' + Arg.Id;
    end;
    Result := Result + ') : ' + GetZcTypeName(Func.ReturnType);
  end;

var
  Ins : string;
begin
  C := TSynCompletionProposal(Context);
  Desc := '';
  Ins := S;
  if (Item is TZComponent) then
    Desc := String((Item as TZComponent).GetDisplayName)
  else if (Item is TZcOpFunctionBase) then
  begin
    if (Item is TZcOpFunctionBuiltIn) and (TZcOpFunctionBuiltIn(Item).FuncId=fcGenLValue) then
      Exit; //Skip internal function
    Desc := InGetSig(Item as TZcOpFunctionBase);
    Ins := (Item as TZcOpFunctionBase).Id; //Use the unmangled-name
  end
  else
    Desc := S;
  C.ItemList.AddObject(Desc,TObject(PChar(Ins)));
  C.InsertList.Add(Ins);
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
      Comp.ItemList.AddObject(S,TObject(PChar(S)));
    end;
  end;

  function InGetC(S : string) : TZComponent;
  var
    P : TObject;
  begin
    Result := nil;
    if SameText(S,'this') then
      Result := Tree.ZSelected.Component
    else if SameText(S,'CurrentModel') then
    begin
      FindCurrentModel(Tree.ZSelected,Result);
    end
    else
    begin
      P := ZApp.SymTab.Lookup(S);
      if Assigned(P) and (P is TZComponent) then
        Result := P as TZComponent;
    end;
  end;

var
  Id : TZClassIds;
  Ci : TZComponentInfo;
  Cv : TDefineVariable;
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
        if C is TDefineVariable then
        begin //If var of ref-type, show props of referenced type instead (i.e. Bitmap bmp)
          Cv := C as TDefineVariable;
          if Cv._Type=zctReference then
            PropList := ComponentManager.GetInfoFromId(Cv._ReferenceClassId).GetProperties;
        end;
        for I := 0 to PropList.Count - 1 do
        begin
          Prop := TZProperty(PropList[I]);
          if (Prop.PropertyType in [zptComponentList,zptExpression]) or
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
      if Id=AnyComponentClassId then
        Continue;
      Ci := ComponentManager.GetInfoFromId(Id);
      if Assigned(Ci) and Ci.ZClass.InheritsFrom(TCommand) and (not Ci.ZClass.InheritsFrom(TContentProducer)) then
        InAdd([ComponentManager.GetInfoFromId(Id).ZClassName]);
    end;
  end
  else
  begin
    //List global identifiers
    ZApp.SymTab.Iterate(AutoCompAddOne, Comp);
    InAdd(['CurrentModel','this','string','int','float','while','for','vec2','vec3','vec4','mat4']);
  end;
  (Comp.InsertList as TStringList).Sort;
  (Comp.ItemList as TStringList).Sort;
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

procedure TEditorForm.ParseEvalExpression(const Expr: string);
var
  TargetCode : TZComponentList;
  Ret : TCodeReturnValue;
  RetType : TZcDataType;
  S : string;
begin
  TargetCode := TZComponentList.Create(Self.ZApp);
  try
    try
      RetType := Compiler.CompileEvalExpression(Expr + ';',Self.ZApp,TargetCode);
    except
      on E : Exception do
      begin
        Log.Write(E.Message);
        Exit;
      end;
    end;
    Ret := ZExpressions.RunCode(TargetCode);
    S := '';
    case RetType.Kind of
      zctFloat: S := FloatToStr(PFloat(@Ret.PointerValue)^);
      zctInt,zctByte : S := IntToStr(NativeInt(Ret.PointerValue));
      zctString: S := String(PAnsiChar(Ret.PointerValue));
    end;
    if S<>'' then
      Log.Write(S);
  finally
    TargetCode.Free;
  end;
end;

type
  TMapName = class
    Name,MapUnitName,MapClassName,MapMethodName : string;
    Start : integer;
    Size : integer;
    Section : integer;
  end;

function BytePos(const Pattern: PByte; PatternLength : integer; const Buffer: PByte; const BufLen: integer): PByte;
var
  i: integer;
  j: integer;
  OK: boolean;
begin
  result := nil;
  for i := 0 to BufLen - PatternLength do
    if PByte(Buffer + i)^ = Pattern[0] then
    begin
      OK := true;
      for j := 1 to PatternLength - 1 do
        if PByte(Buffer + i + j)^ <> Pattern[j] then
        begin
          OK := false;
          break
        end;
      if OK then
        Exit(Buffer + i);
    end;
end;

function SortOnLength(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Length(List[Index2])-Length(List[Index1]);
end;

procedure TEditorForm.RemoveUnusedCode(Module : TPEModule);
var
  TotalRemovedBytes,TotalKeptBytes,I,J,FirstLine,SectionNr : integer;
  Section,DataSection : TImageSection;
  Stream : TMemoryStream;
  MapNames : TObjectList;
  B : byte;
  S,MapFile : string;
  Splitter,Lines : TStringList;
  Item : TMapName;
  Id : TZClassIds;
  NameAnsi : ansistring;
  P : PByte;

  Infos : PComponentInfoArray;
  Ci : TZComponentInfo;
  UsedComponents,ClassesToRemove,NamesToRemove,UnitsToRemove : TStringList;
  NamesKept,AllObjects : TObjectList;
  NeedJpeg,NeedArray,NeedOgg,DisplayDetailedReport : boolean;

  function MakeFastList : TStringList;
  begin
    Result := TStringList.Create;
    Result.Sorted := True;
    Result.Duplicates := dupIgnore;
  end;

begin
  {$IFDEF ZZDC_FPC}
  Exit; //TODO: check if this still works after building ZZDC with Freepascal
  {$ENDIF}

  DisplayDetailedReport := DetailedBuildReportMenuItem.Checked;

  Section := Module.ImageSection[0];
  DataSection := Module.ImageSection[2];
  if (Section.SectionName<>'.text') or (DataSection.SectionName<>'.data') then
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
  NamesToRemove := MakeFastList;
  ClassesToRemove := MakeFastList;
  UsedComponents := MakeFastList;
  UnitsToRemove := MakeFastList;
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

      SectionNr := StrToIntDef(Copy(S,1,4),-1);
      if (SectionNr<>1) and (SectionNr<>3) then
        Continue;

      Item := TMapName.Create;
      Item.Name := Copy(S,21,255);
      Item.Start := StrToInt('$' + Copy(S,6,8));
      Item.Section := SectionNr;
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
        I1 := (N1.Section shl 24) + N1.Start;
        I2 := (N2.Section shl 24) + N2.Start;
        Result := I1-I2;
      end
    );

    for I := 0 to MapNames.Count - 2 do
    begin
      Item := MapNames[I] as TMapName;
      if Item.Section=TMapName(MapNames[I+1]).Section then
        Item.Size := TMapName(MapNames[I+1]).Start - Item.Start;
    end;

    //Get names of used classes
    NeedJpeg := False;
    NeedArray := False;
    NeedOgg := False;
    AllObjects := TObjectList.Create(False);
    try
      AllObjects.Capacity := 1024;
      GetAllObjects(Self.Root,AllObjects);
      UsedComponents.Sorted := True;
      UsedComponents.Add('TAudioMixer');
      UsedComponents.Add('TMaterial');
      UsedComponents.Add('TMaterialTexture');
      for I := 0 to AllObjects.Count - 1 do
      begin
        UsedComponents.Add(TZComponent(AllObjects[I]).ClassName);
        if (AllObjects[I] is TBitmapFromFile) and ((AllObjects[I] as TBitmapFromFile).FileFormat=bffJpeg) then
          NeedJpeg := True
        else if (AllObjects[I] is TExpInvokeComponent) then
          UsedComponents.Add(ComponentManager.GetInfoFromId(TZClassIds((AllObjects[I] as TExpInvokeComponent).InvokeClassId)).ZClass.ClassName)
        else if (AllObjects[I] is TDefineVariable) and ((AllObjects[I] as TDefineVariable)._Type in [zctVec2,zctVec3,zctVec4,zctMat4])  then
          NeedArray := True
        else if (AllObjects[I] is TExpMat4FuncCall) and ((AllObjects[I] as TExpMat4FuncCall).Kind in [fcMatMultiply..fcVec4])  then
          NeedArray := True
        else if (AllObjects[I] is TBitmapExpression) or (AllObjects[I] is TMeshExpression) then
          NeedArray := True
        else if (AllObjects[I] is TExpConvert) and (TExpConvert(AllObjects[I]).Kind in [eckPropToVec3,eckPropToVec4]) then
          NeedArray := True
        else if (AllObjects[I] is TSampleImport) and ((AllObjects[I] as TSampleImport).SampleFileFormat=sffOGG) then
          NeedOgg := True
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
    if NeedArray or (UsedComponents.IndexOf('TExpInitLocalArray')>=0) then
      UsedComponents.Add('TDefineArray');
    if not NeedJpeg then
      ClassesToRemove.Add('TNjDecoder');

    if ZApp.GLBase=glbFixed then
      ClassesToRemove.Add('TGLDriverProgrammable')
    else
      ClassesToRemove.Add('TGLDriverFixed');

    //NamesToRemove = AllNames - UsedNames
    Infos := ZClasses.ComponentManager.GetAllInfos;
    for Id := Low(TComponentInfoArray) to High(TComponentInfoArray) do
    begin
      Ci := TZComponentInfo(Infos[Id]);
      if Assigned(Ci) and (UsedComponents.IndexOf(Ci.ZClass.ClassName)=-1) then
        ClassesToRemove.Add(Ci.ZClass.ClassName);
    end;
    if UsedComponents.IndexOf('TMeshImplicit')=-1 then
      ClassesToRemove.Add('TImpProcess');
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
      NamesToRemove.Add('AudioPlayer.EmitNote');
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

    if not NeedOGG then
    begin
      UnitsToRemove.Add('BeRoAudioOGGVorbisTremor');
      NamesToRemove.Add('AudioComponents.ogg_fread');
      NamesToRemove.Add('AudioComponents.ogg_fseek');
      NamesToRemove.Add('AudioComponents.ogg_ftell');
      NamesToRemove.Add('AudioComponents.ogg_fclose');
    end;

    //NamesToRemove.Add('System.@HandleAnyException');
    //NamesToRemove.Add('System.@FinalizeArray');

    //ok, start removing
    NamesKept := TObjectList.Create(False);
    TotalRemovedBytes := 0;
    Stream := nil;
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
        (NamesToRemove.IndexOf(Item.Name)=-1) and
        (UnitsToRemove.IndexOf(Item.MapUnitName)=-1) then
      begin
        if DisplayDetailedReport then
          NamesKept.Add(Item);
        Continue;
      end;
      case Item.Section of
        1 : Stream := Section.RawData;
        3 : Stream := DataSection.RawData;
      else
        Assert(false);
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
    Log.Write('Removed code: ' + IntToStr(TotalRemovedBytes) );

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
    UnitsToRemove.Free;
    UsedComponents.Free;
    Splitter.Free;
  end;

  //--

  //Remove type names
  NamesToRemove := TStringList.Create;
  Infos := ZClasses.ComponentManager.GetAllInfos;
  for Id := Low(TComponentInfoArray) to High(TComponentInfoArray) do
  begin
    Ci := TZComponentInfo(Infos[Id]);
    if Assigned(Ci) then
      NamesToRemove.Add(Ci.ZClass.ClassName)
  end;
  NamesToRemove.CustomSort(SortOnLength);

  TotalRemovedBytes := 0;
  Stream := Section.RawData;
  Stream.Position := 0;
  for I := 0 to NamesToRemove.Count-1 do
  begin
    NameAnsi := AnsiString(NamesToRemove[I]);
    while True do
    begin
      P := BytePos( PByte(@NameAnsi[1]), Length(NameAnsi) ,Stream.Memory, Stream.Size);
      if P<>nil then
      begin
        FillChar(P^, Length(NameAnsi), 0);
        Inc(TotalRemovedBytes,Length(NameAnsi));
      end
      else
        Break;
    end;
  end;
  Log.Write('Removed type names: ' + IntToStr(TotalRemovedBytes) );

  NamesToRemove.Free;
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

procedure TEditorForm.BuildZ80MenuItemClick(Sender: TObject);
var
  OutFile : string;
begin
  if CurrentFileName='' then
    OutFile := ExePath + 'untitled.z80'
  else
    OutFile := ChangeFileExt(ExpandFileName(CurrentFileName),'.z80');
  try
    wglMakeCurrent(Glp.Canvas.Handle,Glp.GetHrc);
    BuildZ80(OutFile);
  except on E : Exception do
    ShowMessage('Z80 code generation failed.'#13'Only a very limited set of ZGE features is supported for Z80.'#13'Please check the Z80 demo projects.'+#13+#13+E.Message);
  end;
end;

procedure TEditorForm.BuildZ80(OutFile : string);
type
  TRtlRoutine = (rrtSwitch);
  TFixUp =
    record
      Offset,Target : integer;
    end;
  TResourceFixUp =
    record
      Resource : TZComponent;
      Offset : integer;
    end;
  TRtlFixUp =
    record
      Routine : TRtlRoutine;
      Offset : integer;
    end;
var
  C : TZComponent;
  Z80Code : TMemoryStream;
  Z80File : TMemoryStream;
  Fixups : array of TFixUp;
  ResourceFixups : array of TResourceFixUp;
  RtlFixups : array of TRtlFixUp;
  RedundantLoadsRemoved : integer;
  InstructionOffsets : TList;
  CodeBase,VarBase,VarSize : integer;

  procedure InCode(const Code : array of byte; Stream : TMemoryStream = nil);
  var
    B : integer;
  begin
    if Stream=nil then
      Stream := Z80Code;
    for B in Code do
      Stream.Write(B,1);
  end;

  procedure InCodeWord(const W : word; Stream : TMemoryStream = nil);
  begin
    if Stream=nil then
      Stream := Z80Code;
    Stream.Write(W,2);
  end;

  procedure InCodeString(S : string; Stream : TMemoryStream = nil);
  var
    Buf : array of byte;
    LastOpCode : byte;
  begin
    S := StringReplace(S,' ','',[rfReplaceAll]);
    LastOpCode := 0;
    if Stream=nil then
    begin
      Stream := Z80Code;
      if Stream.Position>0 then
        LastOpCode := (PByte(Stream.Memory)+Stream.Position-1)^;
    end;
    SetLength(Buf,Length(S) div 2);
    HexToBin(PWideChar(S),Buf[0],Length(Buf));

    if (LastOpCode=$e5) and (Length(Buf)>0) then
    begin
      if Buf[0]=$e1 then
      begin //Remove pair: push hl, pop hl
        Inc(RedundantLoadsRemoved);
        Stream.Position := Stream.Position-1;
        Stream.Write(Buf[1],Length(Buf)-1);
        Exit;
      end;
      if Buf[0]=$d1 then
      begin //Replace "push hl, pop de" with "ex de, hl"
        Inc(RedundantLoadsRemoved);
        Stream.Position := Stream.Position-1;
        Buf[0] := $eb;
      end;
    end;

    Stream.Write(Buf[0],Length(Buf));
  end;

  procedure InAddFixup(const Destination : integer);
  var
    I : integer;
  begin
    I := Length(Fixups);
    SetLength(Fixups,I+1);
    Fixups[I].Offset := Z80Code.Position;
    Fixups[I].Target := Destination;
    InCodeWord(0);
  end;

  procedure InAddResourceFixup(const Resource : TZComponent);
  var
    I : integer;
  begin
    I := Length(ResourceFixups);
    SetLength(ResourceFixups,I+1);
    ResourceFixups[I].Offset := Z80Code.Position;
    ResourceFixups[I].Resource := Resource;
    InCodeWord(0);
  end;

  procedure InAddRtlFixup(const Routine : TRtlRoutine);
  var
    I : integer;
  begin
    I := Length(RtlFixups);
    SetLength(RtlFixups,I+1);
    RtlFixups[I].Offset := Z80Code.Position;
    RtlFixups[I].Routine:= Routine;
    InCodeWord(0);
  end;

  procedure InWriteBitmap_Zx(Stream : TMemoryStream; B : TZBitmap);
  var
    P,OriginalP : PLongWord;
    PixelCount,X,Y : integer;
    OutByte : byte;
  begin
    PixelCount := B.PixelHeight * B.PixelWidth;
    GetMem(OriginalP,PixelCount * 4);
    B.UseTextureBegin;
    glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_UNSIGNED_BYTE,OriginalP);
    for Y := 0 to B.PixelHeight-1 do
    begin
      P := OriginalP;
      Inc(P,(B.PixelHeight-1-Y) * B.PixelWidth); //Image is upside down
      OutByte := 0;
      for X := 0 to B.PixelWidth-1 do
      begin
        if P^=$FF000000 then
          OutByte := (OutByte shl 1)
        else
          OutByte := (OutByte shl 1) or 1;
        if ((X and 7)=7) then
          Stream.Write(OutByte,1);
        Inc(P);
      end;
    end;
    FreeMem(OriginalP);
  end;

  procedure InWriteBitmap_Sms(Stream : TMemoryStream; B : TZBitmap);
  var
    P,OriginalP : PLongWord;
    PixelCount,X,Y,TileX,TileY,Bit,I,J : integer;
    Color,OutByte : byte;
    //Map all possible colors to paletteindex, $ff=unused.
    AllColors : array[0..63] of byte;
    PixelRow : array[0..7] of byte;
    ColorsUsed,ColorIndex : integer;
    SavePosPal,SavePosEnd : integer;
  begin
    FillChar(AllColors,SizeOf(AllColors),$ff);
    PixelCount := B.PixelHeight * B.PixelWidth;
    GetMem(OriginalP,PixelCount * 4);
    B.UseTextureBegin;
    glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_UNSIGNED_BYTE,OriginalP);
    CheckGLError;

    //Make room for palette first
    SavePosPal := Stream.Position;
    OutByte := 0;
    for I := 0 to 15 do
      Stream.Write(OutByte,1);

    ColorsUsed := 0;
    Y := 0;
    while Y<B.PixelHeight do
    begin
      X := 0;
      while X<B.PixelWidth do
      begin
        P := OriginalP;
        Inc(P,(B.PixelHeight-1-Y) * B.PixelWidth); //Image is upside down
        Inc(P,X);
        for TileY := 0 to 8-1 do
        begin
          OutByte := 0;
          for TileX := 0 to 8-1 do
          begin
            Color := ((P^ and 255) shr 6) or
              ((((P^ shr 8) and 255) shr 6) shl 2) or
              ((((P^ shr 16) and 255) shr 6) shl 4);
            if AllColors[Color]=$FF then
            begin
              AllColors[Color] := ColorsUsed;
              ColorIndex := ColorsUsed;
              Inc(ColorsUsed);
            end else
              ColorIndex := AllColors[Color];
            PixelRow[TileX] := ColorIndex;
            Inc(P);
          end;
          for Bit := 0 to 3 do
          begin
            OutByte := 0;
            for I := 0 to 7 do
              OutByte := (OutByte shl 1) or ((PixelRow[I] shr Bit) and 1);
            Stream.Write(OutByte,1);
          end;
          Dec(P,B.PixelWidth+8);
        end;
        Inc(X,8);
      end;
      Inc(Y,8);
    end;
    FreeMem(OriginalP);
    if ColorsUsed>16 then
      Log.Write('Too many unique colors: ' + String(B.Name));

    //Write palette
    SavePosEnd := Stream.Position;
    Stream.Position := SavePosPal;
    for I := 0 to 15 do
    begin
      Color := 0;
      for J := 0 to High(AllColors) do
      begin
        if AllColors[J]=I then
        begin
          Color := J;
          Break;
        end;
      end;
      Stream.Write(Color,1);
    end;
    Stream.Position := SavePosEnd;
  end;

  procedure InFail(const Msg : string);
  begin
    raise Exception.Create(Msg);
  end;

  procedure InGenList(Exp : TZComponentList; IsFunc : boolean);
  var
    I,J : integer;
    W : word;
    StringConstant : TExpStringConstant;
    IntConstant : TExpConstantInt;
    Switch : TExpSwitchTable;
    Ar : TDefineArray;
  begin
    StringConstant := nil;
    IntConstant := nil;
    I := 0;
    while I<Exp.Count do
    begin
      while InstructionOffsets.Count<I do
        InstructionOffsets.Add(nil);
      InstructionOffsets.Add( pointer( Z80Code.Position ) );

      C := Exp[I] as TZComponent;
      if (C is TExpLoadComponent) then
      begin
        if (TExpLoadComponent(C).Component is TExpStringConstant) then
        begin
          Assert(Exp[I+1] is TExpLoadPropOffset);
          Assert(Exp[I+2] is TExpAddToPointer);
          Assert(Exp[I+3] is TExpMisc);
          Inc(I,4); //Skip the rest of the load constant, load propoffset, addtopointer, ptrdereference
          //if next is TExternal 'emit' then hold in stringconstant, else push string address
          if (Exp[I] is TExpExternalFuncCall) and
            (TExpExternalFuncCall(Exp[I]).FuncName='emit') then
            StringConstant := TExpLoadComponent(C).Component as TExpStringConstant
          else
          begin
            InCode([$21]);  //ld hl,
            InAddResourceFixup( TExpLoadComponent(C).Component );
            InCodeString('e5');  //push hl
          end;
          Continue;
        end;
      end else if (C is TExpExternalFuncCall) then
      begin
        if TExpExternalFuncCall(C).FuncName='emit' then
        begin
          InCodeString(String(StringConstant.Value));
        end else if TExpExternalFuncCall(C).FuncName='getResourceAddress' then
        begin
          InCode([$21]);  //ld hl,nn
          InAddResourceFixup( (Exp[I-1] as TExpLoadComponent).Component );
          InCodeString('e5');  //push hl
        end else if TExpExternalFuncCall(C).FuncName='push' then
        begin
          //do nothing
        end else if TExpExternalFuncCall(C).FuncName='pushString' then
        begin
          //do nothing
        end else if TExpExternalFuncCall(C).FuncName='emitByte' then
        begin
          InCode([ IntConstant.Constant ]);
        end
        else
          InFail('Invalid func call');
      end else if (C is TExpConstantInt) then
      begin
        //if next is TExternal 'emitByte' then hold in variable, else push integer value
        if (Exp[I+1] is TExpExternalFuncCall) and
          (TExpExternalFuncCall(Exp[I+1]).FuncName='emitByte') then
          IntConstant := C as TExpConstantInt
        else
        begin
          InCode([$21]);  //ld hl,
          InCodeWord(TExpConstantInt(C).Constant);
          InCodeString('e5');  //push hl
        end;
      end else if (C is TExpAccessLocal) then
      begin
        W := VarBase + TExpAccessLocal(C).Index*2;
        case TExpAccessLocal(C).Kind of
        loLoad :
          begin
            InCode([$2a]);  //ld hl,(nn)
            InCodeWord(W);
            InCodeString('e5');  //push hl
          end;
        loStore :
          begin
            InCodeString('e1 22'); //pop hl, ld (nn),hl
            InCodeWord(W);
          end;
        else
          InFail('Invalid TExpAccessLocal');
        end;
      end else if (C is TExpJump) then
      begin
        //Compared with codegen from z88dk
        case TExpJump(C).Kind of
          jsJumpEQ :
            begin
              Assert( TExpJump(C)._Type=jutInt );
              InCodeString('e1 d1 a7 ed 52 ca');  //pop hl, pop de, and a (clear carry), sbc hl,de, jp z,nn
              InAddFixup(I + TExpJump(C).Destination);
            end;
          jsJumpNE :
            begin
              Assert( TExpJump(C)._Type=jutInt );
              InCodeString('e1 d1 a7 ed 52 c2');  //pop hl, pop de, and a, sbc hl,de, jp nz,nn
              InAddFixup(I + TExpJump(C).Destination);
            end;
          jsJumpGE :
            begin //https://retrocomputing.stackexchange.com/questions/9163/comparing-signed-numbers-on-z80-8080-in-assembly
              Assert( TExpJump(C)._Type=jutInt );
              InCodeString('d1 e1 a7 ed 52 d2');  //pop de, pop hl, and a, sbc hl,de, jp nc,nn
              InAddFixup(I + TExpJump(C).Destination);
            end;
          jsJumpGT :
            begin
              Assert( TExpJump(C)._Type=jutInt );
              InCodeString('e1 d1 a7 ed 52 da');  //pop hl, pop de, and a, sbc hl,de, jp c,nn
              InAddFixup(I + TExpJump(C).Destination);
            end;
          jsJumpLT :
            begin
              Assert( TExpJump(C)._Type=jutInt );
              InCodeString('d1 e1 a7 ed 52 fa');  //pop de, pop hl, and a, sbc hl,de, jp m,nn
              InAddFixup(I + TExpJump(C).Destination);
            end;
          jsJumpLE :
            begin
              Assert( TExpJump(C)._Type=jutInt );
              InCodeString('e1 d1 a7 ed 52 d2');  //pop hl, pop de, and a, sbc hl,de, jp nc,nn
              InAddFixup(I + TExpJump(C).Destination);
            end;
          jsJumpAlways :
            begin
              InCode([$c3]);  //jp
              InAddFixup(I + TExpJump(C).Destination);
            end;
        else
          InFail('invalid TExpJump');
        end;
      end else if (C is TExpOpBinaryInt) then
      begin
        case TExpOpBinaryInt(C).Kind of
          vbkPlus :
            begin
              InCodeString('e1 d1 19 e5');  //pop hl, pop de, add hl,de, push hl
            end;
          vbkMinus :
            begin
              InCodeString('d1 e1 a7 ed 52 e5');  //pop hl, pop de, and a, sbc hl,de, push hl
            end;
          vbkBinaryAnd :
            begin
              InCodeString('e1d17ba56f2600e5'); //pop hl, pop de, ld a,e, and l, ld l,a, ld h,0, push hl
            end;
          vbkBinaryOr :
            begin
              InCodeString('e1d17bb56f2600e5'); //pop hl, pop de, ld a,e, or l, ld l,a, ld h,0, push hl
            end;
          vbkBinaryShiftLeft :
            begin
              InCodeString('d1e14378b72806cb25cb1410fae5'); //pop de, pop hl, ld b,e, jr z skip, sla l, rl h, djnz next, push hl
            end;
          vbkBinaryShiftRight :
            begin
              InCodeString('d1e14378b72806cb3ccb1d10fae5'); //pop de, pop hl, ld b,e, jr z skip, srl h, rr l, djnz next, push hl
            end;
        else
          InFail('wrong TExpOpBinaryInt');
        end;
      end else if (C is TExpArrayGetElement) then
      begin
        Ar := (Exp[I-1] as TExpLoadComponent).Component as TDefineArray;
        Assert(Ar._Type=zctByte,'Wrong array type');
        Assert(Ar.Dimensions=dadOne,'Only 1D array supported');
        InCode([$21]);  //ld hl,nn
        InAddResourceFixup( Ar );
        InCodeString('d1 19 e5'); //pop de, add hl,de, push hl
      end else if (C is TExpAssign1) then
      begin
        InCodeString('d1 e1 73'); //pop de, pop hl, ld (hl),e
      end else if (C is TExpMisc) then
      begin
        case (C as TExpMisc).Kind of
          emNop : ;
          emPtrDeref1 :
            begin
              InCodeString('e1 6e 26 00 e5'); //pop hl, ld l,(hl), ld h,0, push hl
            end;
        else
          InFail('Unsupported misc instruction: ' + C.ClassName);
        end;
      end else if (C is TExpReturn) or (C is TExpStackFrame) then
      begin
        if (C is TExpStackFrame) then
          Inc(VarSize, TExpStackFrame(C).Size*2);
        //ignore these
      end else if (C is TExpSwitchTable) then
      begin
        Switch := (C as TExpSwitchTable);
        if (Switch.LowBound<0) or (Switch.HighBound>128) then
          InFail('case statements must be in range 0-128');

        InCodeString('e1'); //pop hl

        if Switch.LowBound<>0 then
        begin
          InCode([$3e,Switch.LowBound,$bd,$da]); //ld a,lowbound, cp l, jp c,nn
          InAddFixup(I + Switch.DefaultOrExit);
        end;

        InCode([$3e,Switch.HighBound,$bd,$fa]); //ld a,highbound, cp l, jp m,nn
        InAddFixup(I + Switch.DefaultOrExit);

        InCode([$cd]); //call
        InAddRtlFixup(rrtSwitch);

        //emit jumptable
        for J := 0 to Switch.HighBound-Switch.LowBound do
          InAddFixup(I + PIntegerArray(Switch.Jumps.Data)^[J]);

      end else
        InFail('Unsupported instruction: ' + C.ClassName);

      Inc(I);
    end;
  end;
{
  https://www.asm80.com/
}
var
  Zex : TZExpression;
  I : integer;
  ResourceNames : TStringList;
  Target : (z80Spectrum,z80MasterSystem);
  Ar : TDefineArray;
  RtlAddress : array[TRtlRoutine] of word;
begin
  if not CompileAll then
    Exit;

  Target := z80Spectrum;

  C := Self.ZApp.SymTab.Lookup('Z80Platform') as TZComponent;
  if Assigned(C) and (C is TDefineConstant) then
  begin
    if TDefineConstant(C).StringValue='ZXSpectrum' then
      Target := z80Spectrum
    else if TDefineConstant(C).StringValue='MasterSystem' then
      Target := z80MasterSystem
    else
      InFail('Unknown target: ' + String(TDefineConstant(C).StringValue));
  end;

  Z80Code := TMemoryStream.Create;
  InstructionOffsets := TList.Create;

  case Target of
    z80Spectrum:
      begin
        CodeBase := 30000;
        VarBase := 60000;
      end;
    z80MasterSystem:
      begin
        CodeBase := 0;
        VarBase := $c000;
      end;
  else
    begin
      CodeBase := 0;
      VarBase := 0;
    end;
  end;
  VarSize := 0;

  //Init
  case Target of
    z80Spectrum:
      InCode([$3e,$02,$cd,$01,$16]); //open print channel
    z80MasterSystem:
      InCode([$31,$f0,$df,$f3,$ed,$56]); //ld sp, $dff0, di, im 1
  end;

//InCode([$18,$fe]); //infinite loop

  Zex := nil;
  for I := 0 to Self.ZApp.OnLoaded.ComponentCount-1 do
  begin
    if Self.ZApp.OnLoaded[I] is TZExpression then
    begin
      Zex := Self.ZApp.OnLoaded[I] as TZExpression;
      Break;
    end;
  end;

  RedundantLoadsRemoved := 0;

  InGenList(Zex.Expression.Code,False);

  InCode([$18,$fe]); //infinite loop

  if RedundantLoadsRemoved>0 then
    Log.Write('RedundantLoadsRemoved: ' + IntToStr(RedundantLoadsRemoved) );

  //Resolving fixups
  for I := 0 to High(Fixups) do
  begin
    Z80Code.Position := Fixups[I].Offset;
    InCodeWord( CodeBase + NativeInt(InstructionOffsets[ Fixups[I].Target+1 ])  );
  end;

  //Components
  ResourceNames := TStringList.Create;
  for I := 0 to High(ResourceFixups) do
  begin
    if ResourceFixups[I].Resource.Name='' then
      ResourceFixups[I].Resource.SetString('Name','@tempresource' + AnsiString(IntToStr(I)));
    if ResourceNames.IndexOf( string(ResourceFixups[I].Resource.Name) )=-1 then
    begin
      Z80Code.Position := Z80Code.Size;

      ResourceNames.AddObject(string(ResourceFixups[I].Resource.Name),
        TObject(NativeInt(CodeBase + Z80Code.Position)) );

      if ResourceFixups[I].Resource is TZFile then
        Z80Code.Write(TZFile(ResourceFixups[I].Resource).FileEmbedded.Data^,
          TZFile(ResourceFixups[I].Resource).FileEmbedded.Size)
      else if ResourceFixups[I].Resource is TZBitmap then
      begin
        case Target of
          z80Spectrum : InWriteBitmap_Zx(Z80Code,TZBitmap(ResourceFixups[I].Resource));
          z80MasterSystem : InWriteBitmap_Sms(Z80Code,TZBitmap(ResourceFixups[I].Resource));
        else
          InFail('bitmap not supported for this target');
        end;
      end
      else if ResourceFixups[I].Resource is TExpStringConstant then
        Z80Code.Write(TExpStringConstant(ResourceFixups[I].Resource).Value^,Length(TExpStringConstant(ResourceFixups[I].Resource).Value))
      else if ResourceFixups[I].Resource is TDefineArray then
      begin
        Ar := ResourceFixups[I].Resource as TDefineArray;
        if Ar.Persistent then
          Z80Code.Write(Ar.Values.Data^,Ar.Values.Size)
        else
        begin
          //Memory array, allocate in var area
          ResourceNames.Objects[ResourceNames.Count-1] := TObject(NativeInt(VarBase + VarSize));
          if Ar._Type=zctByte then
            Inc(VarSize,Ar.SizeDim1)
          else if Ar._Type=zctInt then
            Inc(VarSize,Ar.SizeDim1*2)
          else
            InFail('wrong array type');
        end;
      end
      else
        InFail('Wrong type of resource: ' + ResourceFixups[I].Resource.ClassName);
    end;
    Z80Code.Position := ResourceFixups[I].Offset;
    InCodeWord( Word(ResourceNames.Objects[ ResourceNames.IndexOf(string(ResourceFixups[I].Resource.Name)) ]) );
  end;
  ResourceNames.Free;

  FillChar(RtlAddress,SizeOf(RtlAddress),0);
  for I := 0 to High(RtlFixups) do
  begin
    if RtlAddress[RtlFixups[I].Routine]=0 then
    begin
      Z80Code.Position := Z80Code.Size;
      RtlAddress[RtlFixups[I].Routine] := CodeBase + Z80Code.Position;
      case RtlFixups[I].Routine of
        rrtSwitch :
          InCodeString('d1 CB25 19 5e 23 56 626b e9');  //pop de, sla l, add hl,de,
            // 5E                     LD   e,(hl)
            // 23                     INC   hl
            // 56                     LD   d,(hl)
            // 62 6B                  LD   hl,de
            // E9                     JP   (hl)
      else
        InFail('rtl not handled');
      end;
    end;
    Z80Code.Position := RtlFixups[I].Offset;
    InCodeWord( RtlAddress[RtlFixups[I].Routine] );
  end;


  Z80Code.Position := 0;

  Z80File := TMemoryStream.Create;

  case Target of
    z80Spectrum:
      begin
        //http://www.worldofspectrum.org/faq/reference/z80format.htm
        //48kb + header
        Z80File.SetSize(48*1024+30);
        FillChar(Z80File.Memory^,Z80File.Size,0);

        InCode([$00,$5c,$ff,$ff,$a8,$10],Z80File);
        InCodeWord(CodeBase,Z80File); //PC
        InCode([$46,$ff,$ef,$9f,$2e-32,$91,$5c,$4b],Z80File);
        InCode([$17,$06,$00,$7f,$10,$00,$44,$3a],Z80File);
        InCode([$5c,$ff,$ff,$00,$00,$01],Z80File);

        //Screen attributes
        Z80File.Position := (16384+6144)-16384+30;
        for I := 0 to 767 do
          InCode([$38],Z80File);

        //Default BASIC system variables
        Z80File.Position := (23296+256)-16384+30;
        InCodeString(
          'FF000000FF0000000023050000000000'+
          '010006000B0001000100060010000000'+
          '00000000000000000000000000000000'+
          '000000000000003C400000002150FF00'+
          '0000000000000000380000CB5C0000B6'+
          '5CB65CCB5C0000CA5CCC5CCC5C000000'+
          '00CE5CCE5CCE5C00925C100200000000'+
          '0000000000000000FC110058FF000021'+
          '005B05170040FC502118051701380038'+
          '00000000000000000000000000000000'+
          '00000000000000000000000000000000'+
          '000057FFFFFFF409A8104BF409C41553'+
          '810FC41552F409C4155080800D80', Z80File);

        //Write code
        Z80File.Position := CodeBase-16384+30;
        Z80File.CopyFrom(Z80Code,Z80Code.Size);
      end;
    z80MasterSystem:
      begin
        OutFile := ChangeFileExt(OutFile,'.sms');
        //todo: add sms cart header
        Z80File.CopyFrom(Z80Code,Z80Code.Size);
      end;
  end;

  Z80File.SaveToFile(OutFile);
  Log.Write('File generated: ' + OutFile);

  Z80Code.Free;
  Z80File.Free;
  InstructionOffsets.Free;
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
  TemplatePath,OverridePath,ProjectPath,OutFile : string;
  Lookups : TDictionary<string,string>;

  procedure MPath(const P : string);
  begin
    if not TDirectory.Exists(P) then
      TDirectory.CreateDirectory(P);
  end;

  procedure MCopy(const Name : string; const DoReplaceStrings : boolean = False; const AlwaysOverwrite : boolean = False);
  var
    Src,Dst,Key,S,DstContent : string;
    I : integer;
    L : TStringList;
    NeedReplace : boolean;
  begin
    if (Length(OverridePath)>0) and TFile.Exists(OverridePath + Name) then
      Src := OverridePath + Name
    else
      Src := TemplatePath + Name;
    Dst := ProjectPath + Name;
    if not DoReplaceStrings then
    begin
      if TFile.Exists(Dst) and
        (not AlwaysOverwrite) and
        (TFile.GetLastWriteTime(Dst)>=TFile.GetLastWriteTime(Src)) then
        //no need to copy
      else
        TFile.Copy(Src,Dst,True);
    end
    else
    begin
      NeedReplace := False;
      if TFile.Exists(Dst) then
      begin
        L := TStringList.Create;
        L.LoadFromFile(Dst);
        DstContent := L.Text;
        L.Free;
      end
      else
        DstContent := '';
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
            begin
              S := StringReplace(S,Key,Lookups[Key],[rfReplaceAll]);
              if Pos(Lookups[Key],DstContent)<=0 then
                NeedReplace := True;
            end;
          end;
          L[I] := S;
        end;
        if NeedReplace or AlwaysOverwrite then
          //Only replace file if the replacement values did not already exists in destination
          L.SaveToFile(Dst);
      finally
        L.Free;
      end;
    end;
  end;

  function InGetApi(const s : string) : string;
  begin
    Result := Copy(S,1,Pos(' ',S)-1);
  end;

var
  Params,ApkFileName,S : string;
  F : TAndroidApkForm;
  SdkChanged : boolean;
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
    F.OrientationComboBox.ItemIndex := Self.ZApp.AndroidPortrait;

    for S in ZApp.GetProperties.GetByName('AndroidSdk').Options do
      F.AndroidVersionComboBox.Items.Add(S);
    F.AndroidVersionComboBox.ItemIndex := ZApp.AndroidSdk;

    if F.ShowModal=mrCancel then
      Exit;
    Self.ZApp.SetString('AndroidPackageName',AnsiString(F.PackageNameEdit.Text));
    Self.ZApp.SetString('Caption',AnsiString(F.AppNameEdit.Text));
    Self.ZApp.SetString('AndroidVersionName',AnsiString(F.VersionNameEdit.Text));
    Self.ZApp.AndroidVersionNumber := StrToInt(F.VersionNumberEdit.Text);
    Self.ZApp.AndroidPortrait := F.OrientationComboBox.ItemIndex;

    SdkChanged := Self.ZApp.AndroidSdk <> F.AndroidVersionComboBox.ItemIndex;
    Self.ZApp.AndroidSdk := F.AndroidVersionComboBox.ItemIndex;

    Self.SetFileChanged(True);
  finally
    F.Free;
  end;

  if String(Self.ZApp.AndroidPackageName)='' then
  begin
    ShowMessage('App.AndroidPackageName not set. Set the property and try again.');
    Exit;
  end;

  TemplatePath := Self.ExePath + 'Android\Template\base\';
  if ZApp.AndroidSdk=0 then
    OverridePath := ''
  else
    OverridePath := Self.ExePath + 'Android\Template\' +
      InGetApi(ZApp.GetProperties.GetByName('AndroidSdk').Options[ZApp.AndroidSdk]) +
      '\';

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

    case Self.ZApp.AndroidPortrait of
      0 : Lookups.Add('$orientation$', 'landscape');
      1 : Lookups.Add('$orientation$', 'portrait');
      2 : Lookups.Add('$orientation$', 'sensorLandscape');
    end;
    Lookups.Add('$antpath$', Self.AndroidAntPath);

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
    MPath(ProjectPath + 'src\org\zgameeditor');

    MCopy('libs\armeabi\libzgeandroid.so');
    MCopy('src\org\zgameeditor\Zge.java', False, SdkChanged);
    MCopy('src\org\zgameeditor\ZgeActivity.java');
    MCopy('res\drawable-ldpi\icon.png');

    MCopy('default.properties', False, SdkChanged);
    MCopy('local.properties', True);
    MCopy('AndroidManifest.xml', True, SdkChanged);
    MCopy('build.xml',True);
    if IsDebug then
    begin
      MCopy('run.bat',True);
      MCopy('m.bat',True);
    end;
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

{ TPropEditKey }

constructor TPropEditKey.Create(Comp: TZComponent; Prop: TZProperty);
begin
  Self.Comp := Comp;
  Self.Prop := Prop;
end;

end.
