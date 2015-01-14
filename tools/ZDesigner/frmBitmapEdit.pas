unit frmBitmapEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCompEditBase, ExtCtrls, ZClasses,DesignerGui, Contnrs, ZBitmap,
  Menus, StdCtrls,GlPanel;

type
  TBitmapEditFrame = class(TCompEditFrameBase)
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
    SaveToFileButton: TButton;
    Panel1: TPanel;
    UseAlphaCheckBox: TCheckBox;
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
    procedure SaveToFileButtonClick(Sender: TObject);
  private
    { Private declarations }
    Nodes : TObjectList;
    Bitmap : TZBitmap;
    IsBitmapConnected : boolean;
    SelectedNode : TObject;
    DragMode : (drmNone,drmMove,drmLink);
    DragPos,DragDst : TPoint;
    DragLinkIndex : integer;
    GraphSize : TPoint;
    Glp : TGLPanel;
    procedure RepaintPage;
    procedure ReadFromComponent;
    procedure WriteToComponent;
    function FindNodeAt(X,Y : integer) : TObject;
    procedure InitPopupMenu;
    procedure OnAddClick(Sender: TObject);
    procedure Layout;
    procedure OnGlDraw(Sender: TObject);
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
  BitmapEditFrame: TBitmapEditFrame;

implementation

uses Meshes, Math, SugiyamaLayout, ZLog, frmEditor, BitmapProducers, ExtDlgs,
  System.Types, OpenGL12, Renderer, ZOpenGL;

{$R *.dfm}

const
  NodeWidth = 85;
  NodeHeight = 36;

type
  TBitmapNode = class
  private
    Links : TObjectList;
    ParamCount : integer;
    Page : TBitmap;
    Pos : TPoint;
    Producer : TZComponent;
    Form : TBitmapEditFrame;
    TempId : integer;
    constructor Create(Form : TBitmapEditFrame; Producer : TZComponent; Page : TBitmap; X, Y, ParamCount: integer);
    destructor Destroy; override;
    procedure DrawLinks;
    procedure Draw;
    procedure AddLink(Node : TBitmapNode);
    procedure ChangeLink(Node : TBitmapNode; Index : integer);
    function GetTreeSize : integer;
    function GetParamPos(I: integer): TPoint;
    function GetParamRect(I: integer): TRect;
    function GetOutputRect: TRect;
  end;

  TMyLayout = class(TSugiyamaLayout)
  private
    BitmapNodes : TObjectList;
    LayoutWidth,Layoutheight : integer;
  public
    constructor Create(BitmapNodes : TObjectList);
    procedure ExtractNodes; override;
    procedure ApplyNodes; override;
  end;

{ TBitmapNode }

procedure TBitmapNode.AddLink(Node: TBitmapNode);
begin
  if Links.Count>=ParamCount then
    raise Exception.Create('No more links allowed');
  Links.Add(Node);
end;

procedure TBitmapNode.ChangeLink(Node: TBitmapNode; Index: integer);
begin
  if (Producer is TBitmapProducerWithOptionalArgument) then
    (Producer as TBitmapProducerWithOptionalArgument).UseBlankSource := Node=nil;

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

constructor TBitmapNode.Create(Form : TBitmapEditFrame; Producer : TZComponent; Page : TBitmap; X, Y, ParamCount: integer);
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

function TBitmapNode.GetOutputRect: TRect;
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

function TBitmapNode.GetParamPos(I : integer) : TPoint;
var
  Left : integer;
begin
  Left := Pos.X + NodeWidth div 2 - ((ParamCount * ParamStep) div 2);
  Result.X := Left + I * ParamStep + ParamStep div 2;
  Result.Y := Pos.Y + NodeHeight - ParamStep div 2;
end;

function TBitmapNode.GetParamRect(I: integer): TRect;
var
  P : TPoint;
begin
  P := GetParamPos(I);
  Result.Left := P.X - ParamRadius;
  Result.Right := P.X + ParamRadius;
  Result.Top := P.Y - ParamRadius;
  Result.Bottom := P.Y + ParamRadius;
end;

destructor TBitmapNode.Destroy;
begin
  Links.Free;
end;

procedure TBitmapNode.Draw;
var
  Str : string;
  I : integer;
  C : TCanvas;
  Selected : boolean;
  R : TRect;
begin
  Selected := Form.SelectedNode = Self;

  Str := ComponentManager.GetInfo(Producer).ZClassName;
  Str := StringReplace(Str,'Bitmap','',[]);

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

procedure TBitmapNode.DrawLinks;
var
  I : integer;
  C : TCanvas;
  Link : TBitmapNode;
  P : TPoint;
begin
  C := Page.Canvas;
  for I := 0 to Links.Count-1 do
  begin
    Link := TBitmapNode(Links[I]);
    C.Pen.Color := clBlack;
    P := GetParamPos(I);
    C.MoveTo(P.X,P.Y);
    C.LineTo(Link.Pos.X + NodeWidth div 2, Link.Pos.Y + NodeHeight div 2);
  end;
end;

function TBitmapNode.GetTreeSize: integer;

  function InCountChildren(Node : TBitmapNode) : integer;
  var
    I : integer;
  begin
    Result := Node.Links.Count;
    for I := 0 to Node.Links.Count - 1 do
      Inc(Result, InCountChildren(TBitmapNode(Node.Links[I])) );
  end;

begin
  Result := 0;
  Inc(Result, InCountChildren(Self) );
end;

{ TMyLayout }

constructor TMyLayout.Create(BitmapNodes: TObjectList);
begin
  Self.BitmapNodes := BitmapNodes;
end;

procedure TMyLayout.ExtractNodes;
var
  I,J : integer;
  Node,FromNode,ToNode : TNode;
  BitmapNode,Other : TBitmapNode;
begin
  inherited;

  for I := 0 to BitmapNodes.Count-1 do
  begin
    BitmapNode := TBitmapNode(BitmapNodes[I]);
    Node := TNode.Create;
    Node.H := NodeHeight;
    Node.W := NodeWidth;
    Node.Control := BitmapNode;
    Node.Id := Nodes.Count;
    BitmapNode.TempId := Node.Id;
    Nodes.Add(Node);
  end;

  for I := 0 to BitmapNodes.Count-1 do
  begin
    BitmapNode := TBitmapNode(BitmapNodes[I]);
    for J := 0 to BitmapNode.Links.Count-1 do
    begin
      Other := TBitmapNode(BitmapNode.Links[J]);
      FromNode := Nodes[BitmapNode.TempId];
      ToNode := Nodes[Other.TempId];
      AddEdge(FromNode,ToNode);
    end;
  end;

end;

procedure TMyLayout.ApplyNodes;
var
  I : integer;
  Node : TNode;
  BitmapNode : TBitmapNode;
  MaxX,MaxY : integer;
begin
  inherited;

  MaxX := 0; MaxY := 0;
  for I := 0 to Nodes.Count-1 do
  begin
    Node := Nodes[I];
    if Node.IsDummy then
      Continue;
    BitmapNode := Node.Control as TBitmapNode;
    BitmapNode.Pos.X := Node.X;
    BitmapNode.Pos.Y := Node.Y;
    if MaxX<Node.X + NodeWidth then
      MaxX:=Node.X + NodeWidth;
    if MaxY<Node.Y + NodeHeight then
      MaxY:=Node.Y + NodeHeight;
  end;
  Self.LayoutWidth := MaxX;
  Self.LayoutHeight := MaxY;
end;

{ TBitmapEditFrame }

constructor TBitmapEditFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Nodes := TObjectList.Create(True);

  InitPopupMenu;

//  PreviewPanel.DoubleBuffered := True;
end;


destructor TBitmapEditFrame.Destroy;
begin
  Nodes.Free;
  inherited;
end;

procedure TBitmapEditFrame.InitPopupMenu;
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
    if not (Ci.ZClass.InheritsFrom(TContentProducer)) then
      Continue;
    if (Ci.ZClass.InheritsFrom(TMeshProducer)) then
      Continue; //TODO: test for inherits BitmapProducer instead
    if Copy(Ci.ZClassName,1,6)<>'Bitmap' then
      Continue;

    M := TMenuItem.Create(AddMenuItem);
    M.Caption := Ci.ZClassName;
    M.OnClick := OnAddClick;
    M.Tag := Integer(Ci);
    AddMenuItem.Add(M);
  end;
end;

procedure TBitmapEditFrame.Layout;
var
  L : TMyLayout;
begin
  if not IsBitmapConnected then
    Exit;

  if Bitmap.Producers.Count=0 then
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

procedure TBitmapEditFrame.DisablePreviewCheckBoxClick(Sender: TObject);
begin
  Glp.Invalidate;
  Glp.Visible := not DisablePreviewCheckBox.Checked;
end;

function TBitmapEditFrame.FindNodeAt(X, Y: integer): TObject;
var
  I : integer;
  Node : TBitmapNode;
  P : TPoint;
  R : TRect;
begin
  Result := nil;
  P.X := X;
  P.Y := Y;
  for I := 0 to Nodes.Count - 1 do
  begin
    Node := TBitmapNode(Nodes[I]);
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

procedure TBitmapEditFrame.FrameResize(Sender: TObject);
begin
  RepaintPage;
end;

procedure TBitmapEditFrame.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node : TBitmapNode;
  I : integer;
  P : TPoint;
begin
  Node := TBitmapNode(FindNodeAt(X,Y));

  if (Node<>nil) and (SelectedNode=Node) then
  begin
    //Clicking a already selected node makes it the preview-node
    DesignerPreviewProducer := TBitmapNode(SelectedNode).Producer;
    Bitmap.Change;
    Glp.Invalidate;
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
    //Click on empty graph canvas selects whole bitmap-component in tree
    (Owner as TEditorForm).FindComponentAndFocusInTree(Self.Bitmap);

  RepaintPage;
end;

procedure TBitmapEditFrame.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  DoPaint : boolean;
begin
  DoPaint := False;

  if (DragMode=drmMove) and Assigned(SelectedNode) then
  begin
//Disable move for now, only use autolayout
//    TBitmapNode(SelectedNode).Pos := Point((X - DragPos.X) div 10 * 10, (Y - DragPos.Y) div 10 * 10);
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

procedure TBitmapEditFrame.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node,Other,FromNode,ToNode : TBitmapNode;
  I : integer;
  P : TPoint;

  procedure InBreakCycle(Fn,Tn : TBitmapNode);
  var
    I,J : integer;
  begin
    J := Tn.Links.IndexOf(Fn);
    if J>-1 then
      Tn.ChangeLink(nil,J)
    else for I := 0 to Tn.Links.Count - 1 do
      InBreakCycle(Fn,TBitmapNode(Tn.Links[I]));
  end;

begin
  if DragMode=drmLink then
  begin
    Other := TBitmapNode(FindNodeAt(X,Y));

    if (Other<>nil) and (DragLinkIndex=-1) then
    begin //Link from one nodes output to another nodes input argument
      FromNode := Other;
      ToNode := TBitmapNode(SelectedNode);
      P := Point(X,Y);
      for I := 0 to FromNode.ParamCount-1 do
        if PtInRect(FromNode.GetParamRect(I),P) then
        begin
          DragLinkIndex := I;
          Break;
        end;
    end else
    begin //Link from input to output
      FromNode := TBitmapNode(SelectedNode);
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
          Node := TBitmapNode(Nodes[I]);
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

procedure TBitmapEditFrame.SetComponent(C: TZComponent; TreeNode: TZComponentTreeNode);
begin
  inherited;

  Self.Bitmap := C as TZBitmap;
  ReadFromComponent;

  Glp := TGLPanel.Create(Self);
  Glp.Align := alClient;
  Glp.SharedHrc := (Owner as TEditorForm).Glp.GetHrc;
  Glp.OnGLDraw := Self.OnGlDraw;
  Glp.Visible := not DisablePreviewCheckBox.Checked;
  Glp.Parent := PreviewPanel;

  RepaintPage;
  Glp.Invalidate;
end;

procedure TBitmapEditFrame.OnEditorClose;
begin
  if DesignerPreviewProducer<>nil then
  begin
    DesignerPreviewProducer := nil;
    Bitmap.Change;
  end;
  Nodes.Clear;
end;


procedure TBitmapEditFrame.ReadFromComponent;
var
  I,ParamCount : integer;
  C : TZComponent;
  Stack : TObjectStack;
  Node : TBitmapNode;
begin
  IsBitmapConnected := False;

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

    for I := 0 to Bitmap.Producers.Count - 1 do
    begin
      C := Bitmap.Producers.GetComponent(I);

      if not (C is TContentProducer) then
      begin
        ZLog.GetLog(Self.ClassName).Write('Diagram can only handle bitmap-producer components.');
        Exit;
      end;

      ParamCount := ComponentManager.GetInfo(C).ParamCount;
      Node := TBitmapNode.Create(Self,C,Image.Picture.Bitmap,I*100,10,ParamCount);
      Nodes.Add( Node );

      if (C is TBitmapProducerWithOptionalArgument) and
        (C as TBitmapProducerWithOptionalArgument).UseBlankSource then
        ParamCount := 0;

      while (ParamCount>0) and (Stack.Count>0) do
      begin
        Node.AddLink( TBitmapNode(Stack.Pop) );
        Dec(ParamCount);
      end;

      Stack.Push(Node);
    end;

    IsBitmapConnected := True;
  finally
    Stack.Free;
  end;

  Layout;

  DragMode := drmNone;
end;


function TempIdSortProc(Item1, Item2: Pointer): Integer;
var
  I1,I2 : integer;
  N1,N2 : TBitmapNode;
begin
  N1 := TBitmapNode(Item1);
  N2 := TBitmapNode(Item2);
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


procedure TBitmapEditFrame.WriteToComponent;
var
  I,J : integer;
  Node : TBitmapNode;
  InCounts : array of integer;
  Roots,Producers : TObjectList;
  C : TZComponent;

  procedure InGenNode(Node : TBitmapNode);
  var
    I : integer;
  begin
    //Reverse order is needed here otherwise children inverts positions in ReadFromComponent
    for I := Node.Links.Count-1 downto 0 do
      InGenNode(TBitmapNode(Node.Links[I]));
    Producers.Add(Node.Producer);
  end;

begin
{           clear b.producers
           hitta rot i träd (=den som inte är child till någon annan)
             om flera rötter generera dom i ordningen minst träd först
           depth first children
             först traversera barn
             sen generera sig själv}
  if (not IsBitmapConnected) then
    Exit;

  SetLength(InCounts,Nodes.Count);
  FillChar(InCounts[0],SizeOf(Integer)*Nodes.Count,0);

  for I := 0 to Nodes.Count-1 do
  begin
    Node := TBitmapNode(Nodes[I]);
    Node.TempId := I;
  end;

  for I := 0 to Nodes.Count-1 do
  begin
    Node := TBitmapNode(Nodes[I]);
    for J := 0 to Node.Links.Count - 1 do
      Inc(InCounts[ TBitmapNode(Node.Links[J]).TempId ]);
  end;

  Roots := TObjectList.Create(False);
  Producers := TObjectList.Create(False);
  try
    for I := 0 to High(InCounts) do
      if InCounts[I]=0 then
      begin
        Node := TBitmapNode(Nodes[I]);
        Node.TempId := Node.GetTreeSize;
        Roots.Add( Node );
      end;

    Roots.Sort(TempIdSortProc);

    for I := 0 to Roots.Count-1 do
    begin
      Node := TBitmapNode(Roots[I]);
      InGenNode(Node);
    end;

    for I := 0 to Producers.Count-1 do
    begin
      C := Producers[I] as TZComponent;
      J := Bitmap.Producers.IndexOf(C);
      if J<>-1 then
        Bitmap.Producers.RemoveAt(J);
      Bitmap.Producers.InsertComponent(C,I)
    end;
    Bitmap.Producers.Change;

  finally
    Roots.Free;
    Producers.Free;
  end;

  RefreshTreeNode;
end;


procedure TBitmapEditFrame.RepaintPage;
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

  if not IsBitmapConnected then
  begin
    C.TextOut(10,10,'Graph can not be displayed for this bitmap');
  end
  else
  begin
    for I := 0 to Nodes.Count-1 do
      TBitmapNode(Nodes[I]).DrawLinks;

    for I := 0 to Nodes.Count-1 do
      TBitmapNode(Nodes[I]).Draw;

    if DragMode = drmLink then
    begin
      C.Pen.Color := clLime;
      C.MoveTo(DragPos.X, DragPos.Y);
      C.LineTo(DragDst.X, DragDst.Y);
    end;
  end;

end;

procedure TBitmapEditFrame.OnAddClick(Sender: TObject);
var
  M : TMenuItem;
  Ci : TZComponentInfo;
  C : TZComponent;
begin
  if not IsBitmapConnected then
    Exit;

  M := Sender as TMenuItem;
  Ci := TZComponentInfo(M.Tag);
  C := Ci.ZClass.Create(nil);
  if C is TBitmapProducerWithOptionalArgument then
    (C as TBitmapProducerWithOptionalArgument).UseBlankSource := True;

  Nodes.Add( TBitmapNode.Create(Self,C,Image.Picture.Bitmap,0,0,0) );

  WriteToComponent;
  ReadFromComponent;

  RepaintPage;
  Glp.Invalidate;
end;

procedure TBitmapEditFrame.DeleteMenuItemClick(Sender: TObject);
begin
  if (not IsBitmapConnected) or (not Assigned(SelectedNode)) then
    Exit;

  (Owner as TEditorForm).DeleteComponentActionExecute(Self);

  ReadFromComponent;
  SetProjectChanged;

  RepaintPage;
  Glp.Invalidate;

  (Owner as TEditorForm).Tree.Selected := Self.TreeNode;
end;

procedure TBitmapEditFrame.OnPropChanged;
begin
  Glp.Invalidate;
end;

procedure TBitmapEditFrame.OnTreeChanged;
begin
  ReadFromComponent;
  RepaintPage;
  Glp.Invalidate;
end;

procedure TBitmapEditFrame.PopupMenu1Popup(Sender: TObject);
begin
  DeleteMenuItem.Enabled := SelectedNode<>nil;
  AddMenuItem.Enabled := IsBitmapConnected;
  PreviewMenuItem.Enabled := SelectedNode<>nil;
end;

procedure TBitmapEditFrame.PreviewMenuItemClick(Sender: TObject);
begin
  DesignerPreviewProducer := TBitmapNode(SelectedNode).Producer;
  Bitmap.Change;
  RepaintPage;
  Glp.Invalidate;
end;

function ZColorToColor(C : TZColorf) : TColor;
var
  R,G,B,A : longint;
begin
  R := Round(255 * C.R);
  G := Round(255 * C.G);
  B := Round(255 * C.B);
  A := Round(255 * C.A);
  Result:=(A shl 24) or (R shl 16) or (G shl 8) or B;
end;

var
  ImgDialog : TSavePictureDialog;

procedure TBitmapEditFrame.SaveToFileButtonClick(Sender: TObject);
var
  B : TBitmap;
  P : PColor;
  X,Y : integer;
  Ps,PSource : PZVector4f;
begin
  if not IsBitmapConnected then
    Exit;
  if ImgDialog=nil then
  begin
    ImgDialog := TSavePictureDialog.Create(EditorForm);
  end;
  if not ImgDialog.Execute then
    Exit;
  B := TBitmap.Create;
  PSource := Bitmap.GetCopyAsFloats;
  try
    B.Width := Bitmap.PixelWidth;
    B.Height := Bitmap.PixelHeight;
    B.PixelFormat := pf32bit;
    Ps := PSource;
    for Y := B.Height-1 downto 0 do
    begin
      P := B.ScanLine[Y];
      for X := 0 to B.Width-1 do
      begin
        P^ := ZColorToColor(TZColorf(PS^));
        Inc(P);
        Inc(Ps);
      end;
    end;
    B.SaveToFile(ImgDialog.FileName);
  finally
    B.Free;
    FreeMem(PSource);
  end;
end;

procedure TBitmapEditFrame.OnGlDraw(Sender : TObject);
var
  W,H : integer;
  B : TZBitmap;
  UseAlpha : boolean;
begin
  B := Self.Component as TZBitmap;
  B.Update;

  glPushAttrib(GL_ALL_ATTRIB_BITS);

  UseAlpha := Self.UseAlphaCheckBox.Checked;

  //Make sure texture matrix is reset
  (Owner as TEditorForm).ZApp.Driver.EnableMaterial(DefaultMaterial);

  if ShadersSupported then
    glUseProgram(0);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);

  glViewport(0, 0, Glp.Width, Glp.Height);

  glClearColor(0.5,0.5,0.5,0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if B=nil then
    Exit;

  glDisable( GL_LIGHTING );
  glDisable( GL_CULL_FACE );

  glScalef(2.0 / Glp.Width, -2.0 / Glp.Height, 1.0);

  W := (Min(Glp.Width,Glp.Height) div 2) - 8;
  H := W;

  W := Min(Round(W * B.PixelWidth/B.PixelHeight),W);
  H := Min(Round(H * B.PixelHeight/B.PixelWidth),H);

  //rita en quad
  glPushMatrix;

  glEnable(GL_TEXTURE_2D);
  if UseAlpha then
  begin
    //Draw gamut bitmap
    (Owner as TEditorForm).GamutZBitmap.UseTextureBegin;

    glBegin(GL_QUADS);
      //x.
      //..
      glTexCoord2f(0.0, H div 16);
      glVertex2f(-W,-H);

      //..
      //x.
      glTexCoord2f(0.0, 0.0);
      glVertex2f(-W,H);

      //..
      //.x
      glTexCoord2f(W div 16, 0.0);
      glVertex2f(W,H);

      //.x
      //..
      glTexCoord2f(W div 16, H div 16);
      glVertex2f(W,-H);
    glEnd();
  end;

  glDisable(GL_DEPTH_TEST);

  if UseAlpha then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end else
    glDisable(GL_BLEND);

  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);

  B.UseTextureBegin;
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
  glDisable(GL_BLEND);

  glPopMatrix;

  glPopAttrib();

  glFlush;
end;

end.
