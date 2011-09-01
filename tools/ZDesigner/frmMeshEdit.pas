unit frmMeshEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCompEditBase, ExtCtrls, ZClasses,DesignerGui, Contnrs, Meshes,
  Menus, StdCtrls,GlPanel;

type
  TMeshEditFrame = class(TCompEditFrameBase)
    Image: TImage;
    PopupMenu1: TPopupMenu;
    AddMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    PreviewPanel: TPanel;
    ScrollBox1: TScrollBox;
    LeftPanel: TGroupBox;
    RightPanel: TGroupBox;
    Splitter1: TSplitter;
    DisablePreviewCheckBox: TCheckBox;
    PreviewMenuItem: TMenuItem;
    WireframeCheckBox: TCheckBox;
    NormalsCheckBox: TCheckBox;
    Panel1: TPanel;
    SaveMeshToFileButton: TButton;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure DisablePreviewCheckBoxClick(Sender: TObject);
    procedure PreviewMenuItemClick(Sender: TObject);
    procedure MeshNormalsCheckBoxClick(Sender: TObject);
    procedure SaveMeshToFileButtonClick(Sender: TObject);
  private
    { Private declarations }
    Nodes : TObjectList;
    Mesh : TMesh;
    IsMeshConnected : boolean;
    SelectedNode : TObject;
    DragMode : (drmNone,drmMove,drmLink);
    DragPos,DragDst : TPoint;
    DragLinkIndex : integer;
    GraphSize : TPoint;
    Glp : TGLPanel;
    OldGlParent : TWinControl;
    procedure RepaintPage;
    procedure ReadFromComponent;
    procedure WriteToComponent;
    function FindNodeAt(X,Y : integer) : TObject;
    procedure InitPopupMenu;
    procedure OnAddClick(Sender: TObject);
    procedure Layout;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
    procedure SetComponent(C : TZComponent; TreeNode : TZComponentTreeNode); override;
    procedure OnPropChanged; override;
    procedure OnTreeChanged; override;
    procedure OnEditorClose; override;
  end;

var
  BitmapEditFrame: TMeshEditFrame;

implementation

uses Math, SugiyamaLayout, ZLog, frmEditor, Renderer, u3dsFile, System.Types;

{$R *.dfm}

const
  NodeWidth = 85;
  NodeHeight = 36;

type
  TMeshNode = class
  private
    Links : TObjectList;
    ParamCount : integer;
    Page : TBitmap;
    Pos : TPoint;
    Producer : TZComponent;
    Form : TMeshEditFrame;
    TempId : integer;
    constructor Create(Form : TMeshEditFrame; Producer : TZComponent; Page : TBitmap; X, Y, ParamCount: integer);
    destructor Destroy; override;
    procedure DrawLinks;
    procedure Draw;
    procedure AddLink(Node : TMeshNode);
    procedure ChangeLink(Node : TMeshNode; Index : integer);
    function GetTreeSize : integer;
    function GetParamPos(I: integer): TPoint;
    function GetParamRect(I: integer): TRect;
    function GetOutputRect: TRect;
  end;

  TMyLayout = class(TSugiyamaLayout)
  private
    MeshNodes : TObjectList;
    LayoutWidth,Layoutheight : integer;
  public
    constructor Create(MeshNodes : TObjectList);
    procedure ExtractNodes; override;
    procedure ApplyNodes; override;
  end;

{ TMeshNode }

procedure TMeshNode.AddLink(Node: TMeshNode);
begin
  if Links.Count>=ParamCount then
    raise Exception.Create('No more links allowed');
  Links.Add(Node);
end;

procedure TMeshNode.ChangeLink(Node: TMeshNode; Index: integer);
begin
  if Links.IndexOf(Node)>-1 then
    Exit;

  if (Node=nil) and (Index<Links.Count) then
    Links.Delete(Index);

  if Node<>nil then
  begin
    if Links.Count<ParamCount then
      Links.Add(Node)
    else
      Links[Index] := Node;
  end;
end;

constructor TMeshNode.Create(Form : TMeshEditFrame; Producer : TZComponent; Page : TBitmap; X, Y, ParamCount: integer);
begin
  Self.Page := Page;
  Self.Producer := Producer;
  Self.ParamCount := ParamCount;
  Pos.X := X;
  Pos.Y := Y;
  Links := TObjectList.Create(False);
  Self.Form := Form;
end;

const
  ParamRadius = 5;
  ParamStep = ParamRadius * 2 + 4;
  OutputRadius = 4;
  OutputStep = OutputRadius * 2 + 4;

function TMeshNode.GetOutputRect: TRect;
var
  P : TPoint;
begin
  P.X := Pos.X + NodeWidth div 2;;
  P.Y := Pos.Y + OutputStep div 2;
  Result.Left := P.X - OutputRadius;
  Result.Right := P.X + OutputRadius;
  Result.Top := P.Y - OutputRadius;
  Result.Bottom := P.Y + OutputRadius;
end;

function TMeshNode.GetParamPos(I : integer) : TPoint;
var
  Left : integer;
begin
  Left := Pos.X + NodeWidth div 2 - ((ParamCount * ParamStep) div 2);
  Result.X := Left + I * ParamStep + ParamStep div 2;
  Result.Y := Pos.Y + NodeHeight - ParamStep div 2;
end;

function TMeshNode.GetParamRect(I: integer): TRect;
var
  P : TPoint;
begin
  P := GetParamPos(I);
  Result.Left := P.X - ParamRadius;
  Result.Right := P.X + ParamRadius;
  Result.Top := P.Y - ParamRadius;
  Result.Bottom := P.Y + ParamRadius;
end;

destructor TMeshNode.Destroy;
begin
  Links.Free;
end;

procedure TMeshNode.Draw;
var
  Str : string;
  I : integer;
  C : TCanvas;
  Selected : boolean;
  R : TRect;
begin
  Selected := Form.SelectedNode = Self;

  Str := ComponentManager.GetInfo(Producer).ZClassName;
  Str := StringReplace(Str,'Mesh','',[]);

  C := Page.Canvas;
  //Back
  if Selected then
    C.Brush.Color := RGB(190, 190, 220)
  else
    C.Brush.Color := RGB(190, 190, 190);

  C.Pen.Color := clGray;
  if Selected then
    C.Brush.Color := RGB(170, 170, 230)
  else
    C.Brush.Color := RGB(190, 190, 190); //RGB(170, 170, 170);
  if DesignerPreviewProducer=Producer then
    C.Pen.Width := 2
  else
    C.Pen.Width := 1;
  C.Rectangle(Pos.X, Pos.Y, Pos.X + NodeWidth, Pos.Y + NodeHeight);

  C.Pen.Width := 1;

  //Text
  C.Brush.Style := bsClear;
  C.TextOut(Pos.X + (NodeWidth - C.TextWidth(Str)) div 2,
    Pos.Y + (NodeHeight - C.TextHeight(Str)) div 2,
    Str);
  C.Brush.Style := bsSolid;

  //Links
  for I := 0 to ParamCount-1 do
  begin
    if I<Links.Count then
      C.Brush.Color := clRed
    else
      C.Brush.Color := clLime;
    R := GetParamRect(I);
    C.Brush.Color := RGB(200, 200, 200);
    C.Ellipse(R.Left,R.Top,R.Right,R.Bottom);
  end;

  R := GetOutputRect;
  Inc(R.Left,OutputRadius);
//  C.Brush.Color := clDkGray;
  C.Brush.Color := RGB(200, 200, 200);
  C.Polygon([ Point(R.Left,R.Top),Point(R.Right,R.Bottom),Point(R.Left - OutputRadius,R.Bottom) ]);
end;

procedure TMeshNode.DrawLinks;
var
  I : integer;
  C : TCanvas;
  Link : TMeshNode;
  P : TPoint;
begin
  C := Page.Canvas;
  for I := 0 to Links.Count-1 do
  begin
    Link := TMeshNode(Links[I]);
    C.Pen.Color := clBlack;
    P := GetParamPos(I);
    C.MoveTo(P.X,P.Y);
    C.LineTo(Link.Pos.X + NodeWidth div 2, Link.Pos.Y + NodeHeight div 2);
  end;
end;

function TMeshNode.GetTreeSize: integer;

  function InCountChildren(Node : TMeshNode) : integer;
  var
    I : integer;
  begin
    Result := Node.Links.Count;
    for I := 0 to Node.Links.Count - 1 do
      Inc(Result, InCountChildren(TMeshNode(Node.Links[I])) );
  end;

begin
  Result := 0;
  Inc(Result, InCountChildren(Self) );
end;

{ TMyLayout }

constructor TMyLayout.Create(MeshNodes: TObjectList);
begin
  Self.MeshNodes := MeshNodes;
end;

procedure TMyLayout.ExtractNodes;
var
  I,J : integer;
  Node,FromNode,ToNode : TNode;
  MeshNode,Other : TMeshNode;
begin
  inherited;

  for I := 0 to MeshNodes.Count-1 do
  begin
    MeshNode := TMeshNode(MeshNodes[I]);
    Node := TNode.Create;
    Node.H := NodeHeight;
    Node.W := NodeWidth;
    Node.Control := MeshNode;
    Node.Id := Nodes.Count;
    MeshNode.TempId := Node.Id;
    Nodes.Add(Node);
  end;

  for I := 0 to MeshNodes.Count-1 do
  begin
    MeshNode := TMeshNode(MeshNodes[I]);
    for J := 0 to MeshNode.Links.Count-1 do
    begin
      Other := TMeshNode(MeshNode.Links[J]);
      FromNode := Nodes[MeshNode.TempId];
      ToNode := Nodes[Other.TempId];
      AddEdge(FromNode,ToNode);
    end;
  end;

end;

procedure TMyLayout.ApplyNodes;
var
  I : integer;
  Node : TNode;
  MeshNode : TMeshNode;
  MaxX,MaxY : integer;
begin
  inherited;

  MaxX := 0; MaxY := 0;
  for I := 0 to Nodes.Count-1 do
  begin
    Node := Nodes[I];
    if Node.IsDummy then
      Continue;
    MeshNode := Node.Control as TMeshNode;
    MeshNode.Pos.X := Node.X;
    MeshNode.Pos.Y := Node.Y;
    if MaxX<Node.X + NodeWidth then
      MaxX:=Node.X + NodeWidth;
    if MaxY<Node.Y + NodeHeight then
      MaxY:=Node.Y + NodeHeight;
  end;
  Self.LayoutWidth := MaxX;
  Self.LayoutHeight := MaxY;
end;

{ TMeshEditFrame }

constructor TMeshEditFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Nodes := TObjectList.Create(True);

  InitPopupMenu;

//  PreviewPanel.DoubleBuffered := True;
end;

procedure TMeshEditFrame.InitPopupMenu;
var
  Infos : PComponentInfoArray;
  Ci : TZComponentInfo;
  I : TZClassIds;
  M : TMenuItem;
begin
  Infos := ZClasses.ComponentManager.GetAllInfos;
  for I := Low(TComponentInfoArray) to High(TComponentInfoArray) do
  begin
    Ci := TZComponentInfo(Infos[I]);
    if Ci.NoUserCreate then
      Continue;
    if (not Ci.ZClass.InheritsFrom(TMeshProducer)) then
      Continue;

    M := TMenuItem.Create(AddMenuItem);
    M.Caption := Ci.ZClassName;
    M.OnClick := OnAddClick;
    M.Tag := Integer(Ci);
    AddMenuItem.Add(M);
  end;
end;

procedure TMeshEditFrame.Layout;
var
  L : TMyLayout;
begin
  if not IsMeshConnected then
    Exit;

  if Mesh.Producers.Count=0 then
    Exit;

  L := TMyLayout.Create(Nodes);
  try
    L.Execute;
    GraphSize.X := L.LayoutWidth;
    GraphSize.Y := L.LayoutHeight;
  finally
    L.Free;
  end;
end;


procedure TMeshEditFrame.MeshNormalsCheckBoxClick(Sender: TObject);
begin
  Renderer.NormalsVisible := (Sender as TCheckBox).Checked;
end;

destructor TMeshEditFrame.Destroy;
begin
  Nodes.Free;
  inherited;
end;

procedure TMeshEditFrame.DisablePreviewCheckBoxClick(Sender: TObject);
begin
  Glp.Invalidate;
  Glp.Visible := not (Sender as TCheckBox).Checked;
end;

function TMeshEditFrame.FindNodeAt(X, Y: integer): TObject;
var
  I : integer;
  Node : TMeshNode;
  P : TPoint;
  R : TRect;
begin
  Result := nil;
  P.X := X;
  P.Y := Y;
  for I := 0 to Nodes.Count - 1 do
  begin
    Node := TMeshNode(Nodes[I]);
    R.Left := Node.Pos.X;
    R.Top := Node.Pos.Y;
    R.Right := R.Left + NodeWidth;
    R.Bottom := R.Top + NodeHeight;
    if PtInRect(R,P) then
    begin
      Result := Node;
      Break;
    end;
  end;
end;

procedure TMeshEditFrame.FrameResize(Sender: TObject);
begin
  RepaintPage;
end;

procedure TMeshEditFrame.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node : TMeshNode;
  I : integer;
  P : TPoint;
begin
  Node := TMeshNode(FindNodeAt(X,Y));

  if (Node<>nil) and (SelectedNode=Node) then
  begin
    //Clicking a already selected node makes it the preview-node
    DesignerPreviewProducer := TMeshNode(SelectedNode).Producer;
    Mesh.Change;
  end;

  SelectedNode := Node;

  if Node<>nil then
  begin
    (Owner as TEditorForm).FindComponentAndFocusInTree(Node.Producer);

    DragLinkIndex := -1;
    P := Point(X,Y);
    for I := 0 to Node.ParamCount-1 do
      if PtInRect(Node.GetParamRect(I),P) then
      //Drag from parameter
      begin
        DragLinkIndex := I;
        DragMode := drmLink;
        Break;
      end;

    if (DragLinkIndex=-1) and PtInRect(Node.GetOutputRect,P) then
      //Drag from output
      DragMode := drmLink;

    DragPos := P;
    DragDst := P;

//    if DragLinkIndex >= 0 then
//    begin
{    end
    else
    begin
      DragMode := drmMove;
      DragPos := Point(X - Node.Pos.X,Y - Node.Pos.Y);
    end;}
  end else
    //Click on empty graph canvas selects whole Mesh-component in tree
    (Owner as TEditorForm).FindComponentAndFocusInTree(Self.Mesh);

  RepaintPage;
end;

procedure TMeshEditFrame.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  DoPaint : boolean;
begin
  DoPaint := False;

  if (DragMode=drmMove) and Assigned(SelectedNode) then
  begin
//Disable move for now, only use autolayout
//    TMeshNode(SelectedNode).Pos := Point((X - DragPos.X) div 10 * 10, (Y - DragPos.Y) div 10 * 10);
//    DoPaint := True;
  end;

  if DragMode=drmLink then
  begin
    DragDst := Point(X,Y);
    DoPaint := True;
  end;

  if DoPaint then
    RepaintPage;
end;

procedure TMeshEditFrame.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node,Other,FromNode,ToNode : TMeshNode;
  I : integer;
  P : TPoint;

  procedure InBreakCycle(Fn,Tn : TMeshNode);
  var
    I,J : integer;
  begin
    J := Tn.Links.IndexOf(Fn);
    if J>-1 then
      Tn.ChangeLink(nil,J)
    else for I := 0 to Tn.Links.Count - 1 do
      InBreakCycle(Fn,TMeshNode(Tn.Links[I]));
  end;

begin
  if DragMode=drmLink then
  begin
    Other := TMeshNode(FindNodeAt(X,Y));

    if (Other<>nil) and (DragLinkIndex=-1) then
    begin //Link from one nodes output to another nodes input argument
      FromNode := Other;
      ToNode := TMeshNode(SelectedNode);
      P := Point(X,Y);
      for I := 0 to FromNode.ParamCount-1 do
        if PtInRect(FromNode.GetParamRect(I),P) then
        begin
          DragLinkIndex := I;
          Break;
        end;
    end else
    begin //Link from input to output
      FromNode := TMeshNode(SelectedNode);
      ToNode := Other;
    end;

    if (FromNode<>ToNode) and (DragLinkIndex>-1) then
    begin
      FromNode.ChangeLink(ToNode,DragLinkIndex);
      if ToNode<>nil then
      begin
        //make sure that no other link has ToNode as target
        for I := 0 to Nodes.Count - 1 do
        begin
          Node := TMeshNode(Nodes[I]);
          if Node=FromNode then
            Continue;
          if Node.Links.IndexOf(ToNode)>-1 then
            Node.Links.Remove(ToNode);
        end;
        //make sure that tonode does not link back to fromnode
        InBreakCycle(FromNode,ToNode);
      end;
      WriteToComponent;
      ReadFromComponent;
      Glp.Invalidate;
    end;
  end;

  DragMode := drmNone;
  RepaintPage;
end;

procedure TMeshEditFrame.SetComponent(C: TZComponent; TreeNode: TZComponentTreeNode);
begin
  inherited;

  Self.Mesh := C as TMesh;
  ReadFromComponent;

  Glp := (Owner as TEditorForm).Glp;
  Glp.Visible := not DisablePreviewCheckBox.Checked;
  OldGlParent := Glp.Parent;
  Glp.Parent := PreviewPanel;

  RepaintPage;
  Glp.Invalidate;
end;

procedure TMeshEditFrame.OnEditorClose;
begin
  Glp.Visible := True;
  Glp.Parent:= OldGlParent;
  if DesignerPreviewProducer<>nil then
  begin
    DesignerPreviewProducer := nil;
    Mesh.Change;
  end;
  Nodes.Clear;
end;


procedure TMeshEditFrame.ReadFromComponent;
var
  I,ParamCount : integer;
  C : TZComponent;
  Stack : TObjectStack;
  Node : TMeshNode;
begin
  IsMeshConnected := False;

  Nodes.Clear;

{   hur länka ihop?
     läs en p
       om p inte är en producer fail "kan inte visa som graf"
     paramcount=p params
       läs paramcount från stack
       addera som children till p
     push p på stack}

  Stack := TObjectStack.Create;
  try

    for I := 0 to Mesh.Producers.Count - 1 do
    begin
      C := Mesh.Producers.GetComponent(I);

      if not (C is TContentProducer) then
      begin
        ZLog.GetLog(Self.ClassName).Write('Diagram can only handle Mesh-producer components.');
        Exit;
      end;

      ParamCount := ComponentManager.GetInfo(C).ParamCount;
      Node := TMeshNode.Create(Self,C,Image.Picture.Bitmap,I*100,10,ParamCount);
      Nodes.Add( Node );

      while (ParamCount>0) and (Stack.Count>0) do
      begin
        Node.AddLink( TMeshNode(Stack.Pop) );
        Dec(ParamCount);
      end;

      Stack.Push(Node);
    end;

    IsMeshConnected := True;
  finally
    Stack.Free;
  end;

  Layout;

  DragMode := drmNone;
end;


function TempIdSortProc(Item1, Item2: Pointer): Integer;
var
  I1,I2 : integer;
  N1,N2 : TMeshNode;
begin
  N1 := TMeshNode(Item1);
  N2 := TMeshNode(Item2);
  I1 := N1.TempId;
  I2 := N2.TempId;
  Result := I1-I2;
  if Result=0 then
    Result := N2.ParamCount - N1.ParamCount;
{  if I1 < I2 then
    Result := -1
  else if I1 = I2 then
    Result:=0
  else
    Result := 1;}
end;


procedure TMeshEditFrame.WriteToComponent;
var
  I,J : integer;
  Node : TMeshNode;
  InCounts : array of integer;
  Roots,Producers : TObjectList;
  C : TZComponent;

  procedure InGenNode(Node : TMeshNode);
  var
    I : integer;
  begin
    //Reverse order is needed here otherwise children inverts positions in ReadFromComponent
    for I := Node.Links.Count-1 downto 0 do
      InGenNode(TMeshNode(Node.Links[I]));
    Producers.Add(Node.Producer);
  end;

begin
{           clear b.producers
           hitta rot i träd (=den som inte är child till någon annan)
             om flera rötter generera dom i ordningen minst träd först
           depth first children
             först traversera barn
             sen generera sig själv}
  if (not IsMeshConnected) then
    Exit;

  SetLength(InCounts,Nodes.Count);
  FillChar(InCounts[0],SizeOf(Integer)*Nodes.Count,0);

  for I := 0 to Nodes.Count-1 do
  begin
    Node := TMeshNode(Nodes[I]);
    Node.TempId := I;
  end;

  for I := 0 to Nodes.Count-1 do
  begin
    Node := TMeshNode(Nodes[I]);
    for J := 0 to Node.Links.Count - 1 do
      Inc(InCounts[ TMeshNode(Node.Links[J]).TempId ]);
  end;

  Roots := TObjectList.Create(False);
  Producers := TObjectList.Create(False);
  try
    for I := 0 to High(InCounts) do
      if InCounts[I]=0 then
      begin
        Node := TMeshNode(Nodes[I]);
        Node.TempId := Node.GetTreeSize;
        Roots.Add( Node );
      end;

    Roots.Sort(TempIdSortProc);

    for I := 0 to Roots.Count-1 do
    begin
      Node := TMeshNode(Roots[I]);
      InGenNode(Node);
    end;

    for I := 0 to Producers.Count-1 do
    begin
      C := Producers[I] as TZComponent;
      J := Mesh.Producers.IndexOf(C);
      if J<>-1 then
        Mesh.Producers.RemoveAt(J);
      Mesh.Producers.InsertComponent(C,I)
    end;
    Mesh.Producers.Change;

  finally
    Roots.Free;
    Producers.Free;
  end;

  RefreshTreeNode;
end;


procedure TMeshEditFrame.RepaintPage;
var
  I,Min : integer;
  C : TCanvas;
begin
  C := Image.Picture.Bitmap.Canvas;

  Min := Image.Parent.ClientWidth;
  if GraphSize.X<Min then
    GraphSize.X := Min;
//  if not IsBitmapConnected then
//    GraphSize.X := 500;
  Min := Image.Parent.ClientHeight;
  if GraphSize.Y<Min then
    GraphSize.Y := Min;
  Image.Width := GraphSize.X;
  Image.Height := GraphSize.Y;
  Image.Picture.Bitmap.SetSize(Image.ClientRect.Right,Image.ClientRect.Bottom);

  C.Brush.Color := clWhite;
  C.FillRect(Image.ClientRect);

  if not IsMeshConnected then
  begin
    C.TextOut(10,10,'Graph can not be displayed for this Mesh');
  end
  else
  begin
    for I := 0 to Nodes.Count-1 do
      TMeshNode(Nodes[I]).DrawLinks;

    for I := 0 to Nodes.Count-1 do
      TMeshNode(Nodes[I]).Draw;

    if DragMode = drmLink then
    begin
      C.Pen.Color := clLime;
      C.MoveTo(DragPos.X, DragPos.Y);
      C.LineTo(DragDst.X, DragDst.Y);
    end;
  end;

end;

procedure TMeshEditFrame.OnAddClick(Sender: TObject);
var
  M : TMenuItem;
  Ci : TZComponentInfo;
  C : TZComponent;
begin
  if not IsMeshConnected then
    Exit;

  M := Sender as TMenuItem;
  Ci := TZComponentInfo(M.Tag);
  C := Ci.ZClass.Create(nil);

  Nodes.Add( TMeshNode.Create(Self,C,Image.Picture.Bitmap,0,0,0) );

  WriteToComponent;
  ReadFromComponent;

  RepaintPage;
  Glp.Invalidate;
end;

procedure TMeshEditFrame.DeleteMenuItemClick(Sender: TObject);
begin
  if (not IsMeshConnected) or (not Assigned(SelectedNode)) then
    Exit;

  (Owner as TEditorForm).DeleteComponentActionExecute(Self);

  ReadFromComponent;
  SetProjectChanged;

  RepaintPage;
  Glp.Invalidate;

  (Owner as TEditorForm).Tree.Selected := Self.TreeNode;
end;

procedure TMeshEditFrame.OnPropChanged;
begin
  Glp.Invalidate;
end;

procedure TMeshEditFrame.OnTreeChanged;
begin
  ReadFromComponent;
  RepaintPage;
  Glp.Invalidate;
end;

procedure TMeshEditFrame.PopupMenu1Popup(Sender: TObject);
begin
  DeleteMenuItem.Enabled := SelectedNode<>nil;
  AddMenuItem.Enabled := IsMeshConnected;
  PreviewMenuItem.Enabled := SelectedNode<>nil;
end;

procedure TMeshEditFrame.PreviewMenuItemClick(Sender: TObject);
begin
  DesignerPreviewProducer := TMeshNode(SelectedNode).Producer;
  Mesh.Change;
  RepaintPage;
  Glp.Invalidate;
end;


var
  SaveDialog : TSaveDialog;

procedure TMeshEditFrame.SaveMeshToFileButtonClick(Sender: TObject);
var
  Exp : T3dsExport;
begin
  if not IsMeshConnected then
    Exit;

  if SaveDialog=nil then
  begin
    SaveDialog := TSaveDialog.Create(Application.MainForm);
    SaveDialog.Filter := '3D-studio files (*.3ds)|*.3ds';
    SaveDialog.DefaultExt := '*.3ds';
  end;
  if not SaveDialog.Execute then
    Exit;

  Exp := T3dsExport.Create;
  try
    Exp.DoExport(Mesh,SaveDialog.FileName);
  finally
    Exp.Free;
  end;
end;


end.
