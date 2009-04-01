unit frmBitmapEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCompEditBase, ExtCtrls, ZClasses,DesignerGui, Contnrs, ZBitmap,
  Menus, StdCtrls;

type
  TBitmapEditFrame = class(TCompEditFrameBase)
    Image: TImage;
    PopupMenu1: TPopupMenu;
    AddMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    Panel1: TPanel;
    PaintBox: TPaintBox;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
  private
    { Private declarations }
    Nodes : TObjectList;
    Bitmap : TZBitmap;
    IsBitmapConnected : boolean;
    SelectedNode : TObject;
    DragMode : (drmNone,drmMove,drmLink);
    DragPos,DragDst : TPoint;
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
  end;

var
  BitmapEditFrame: TBitmapEditFrame;

implementation

uses Meshes, Math, SugiyamaLayout, ZLog, frmEditor;

{$R *.dfm}

const
  NodeWidth = 85;
  NodeHeight = 40;

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
  end;

  TMyLayout = class(TSugiyamaLayout)
  private
    BitmapNodes : TObjectList;
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
  if Links.IndexOf(Node)>-1 then
    Exit;

  if (Node=nil) and (Index<Links.Count) then
    Links.Delete(Index);

  if Node<>nil then
    Links.Add(Node);
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
begin
  Selected := Form.SelectedNode = Self;

  Str := ComponentManager.GetInfo(Producer).ZClassName;
  Str := StringReplace(Str,'Bitmap','',[]);

  C := Page.Canvas;
  // back
  if Selected then
    C.Brush.Color := RGB(190, 190, 220)
  else
    C.Brush.Color := RGB(190, 190, 190);

  C.Pen.Color := clGray;
  C.Rectangle(Pos.X, Pos.Y, Pos.X + NodeWidth, Pos.Y + NodeHeight);
  C.Rectangle(Pos.X, Pos.Y, Pos.X + 10, Pos.Y + NodeHeight);

  if Selected then
    C.Brush.Color := RGB(170, 170, 230)
  else
    C.Brush.Color := RGB(170, 170, 170);
  C.Rectangle(Pos.X, Pos.Y, Pos.X + NodeWidth, Pos.Y +  20);
// text
  C.Brush.Style := bsClear;
  C.TextOut(Pos.X + (NodeWidth - C.TextWidth(Str)) div 2, Pos.Y + 4, Str);
  C.Brush.Style := bsSolid;
// links
  for I := 0 to ParamCount-1 do
  begin
    if I<Links.Count then
      C.Brush.Color := clRed
    else
      C.Brush.Color := clLime;
    C.Ellipse(Pos.X + 1, Pos.Y + 21 + i * 10, Pos.X + 9, Pos.Y + 29 + i * 10);
  end;
  // data
{    SetStretchBltMode(Handle, HALFTONE);
    StretchDIBits(Handle, Pos.X + 10, Pos.Y + 20 + 63, 64, -64,
                  0, 0, TEX_SIZE, TEX_SIZE, Data, bmi, DIB_RGB_COLORS, SRCCOPY);}
end;

procedure TBitmapNode.DrawLinks;
var
  I : integer;
  C : TCanvas;
  Link : TBitmapNode;
begin
  C := Page.Canvas;
  C.Pen.Style := psDash;
  for I := 0 to Links.Count-1 do
  begin
    Link := TBitmapNode(Links[I]);
    C.Pen.Color := clBlack;
    C.MoveTo(Pos.X + 5, Pos.Y + 20 + i * 10 + 5);
    C.LineTo(Link.Pos.X + NodeWidth div 2, Link.Pos.Y + NodeHeight div 2);
  end;
  C.Pen.Style := psSolid;
end;

function TBitmapNode.GetTreeSize: integer;

  function InCountChildren(Node : TBitmapNode) : integer;
  var
    I : integer;
  begin
    Result := Links.Count;
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
begin
  inherited;

  for I := 0 to Nodes.Count-1 do
  begin
    Node := Nodes[I];
    if Node.IsDummy then
      Continue;
    BitmapNode := Node.Control as TBitmapNode;
    BitmapNode.Pos.X := Node.X;
    BitmapNode.Pos.Y := Node.Y;
  end;
end;

{ TBitmapEditFrame }

constructor TBitmapEditFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Nodes := TObjectList.Create(True);

  InitPopupMenu;

  Panel1.DoubleBuffered := True;
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
  finally
    L.Free;
  end;
end;


destructor TBitmapEditFrame.Destroy;
begin
  Nodes.Free;
  inherited;
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

procedure TBitmapEditFrame.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node : TBitmapNode;
begin
  Node := TBitmapNode(FindNodeAt(X,Y));

  SelectedNode := Node;

  if Node<>nil then
  begin
    (Owner as TEditorForm).FindComponentAndFocusInTree(Node.Producer);

    if (X - Node.Pos.X < 10) and (Node.ParamCount>0) then
    begin
      DragMode := drmLink;
      DragPos := Point(X,Y);
      DragDst := Point(X,Y);
    end
    else
    begin
      DragMode := drmMove;
      DragPos := Point(X - Node.Pos.X,Y - Node.Pos.Y);
    end;
  end;

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
  Node,Other : TBitmapNode;
  I,J : integer;
begin
  if DragMode=drmLink then
  begin
    Other := TBitmapNode(FindNodeAt(X,Y));
    I := (DragPos.Y - TBitmapNode(SelectedNode).Pos.Y - 21) div 10;
    TBitmapNode(SelectedNode).ChangeLink(Other,I);
    //make sure that no other link has other as target
    for J := 0 to Nodes.Count - 1 do
    begin
      Node := TBitmapNode(Nodes[J]);
      if Node=SelectedNode then
        Continue;
      if Node.Links.IndexOf(Other)>-1 then
        Node.Links.Remove(Other);
    end;
    WriteToComponent;
    ReadFromComponent;
    PaintBox.Invalidate;
  end;

  DragMode := drmNone;
  RepaintPage;
end;

procedure TBitmapEditFrame.SetComponent(C: TZComponent; TreeNode: TZComponentTreeNode);
begin
  inherited;

  Self.Bitmap := C as TZBitmap;
  ReadFromComponent;

  RepaintPage;
  PaintBox.Invalidate;
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
begin
  I1 := TBitmapNode(Item1).TempId;
  I2 := TBitmapNode(Item2).TempId;
  if I1 < I2 then
    Result := -1
  else if I1 = I2 then
    Result:=0
  else
    Result := 1;
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
    for I := 0 to Node.Links.Count - 1 do
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
  I : integer;
  C : TCanvas;
begin
  C := Image.Picture.Bitmap.Canvas;

  Image.Picture.Bitmap.SetSize(Image.ClientRect.Right,Image.ClientRect.Bottom);
  C.Brush.Color := clGray;
  C.FillRect(Image.ClientRect);

  if not IsBitmapConnected then
  begin
    C.TextOut(10,10,'Diagram cannot be shown of bitmaps containing logical components');
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

  Nodes.Add( TBitmapNode.Create(Self,C,Image.Picture.Bitmap,0,0,0) );

  WriteToComponent;
  ReadFromComponent;

  RepaintPage;
  PaintBox.Invalidate;
end;

procedure TBitmapEditFrame.DeleteMenuItemClick(Sender: TObject);
begin
  if (not IsBitmapConnected) or (not Assigned(SelectedNode)) then
    Exit;

  (Owner as TEditorForm).DeleteComponentActionExecute(nil);

  ReadFromComponent;
  SetProjectChanged;

  RepaintPage;
  PaintBox.Invalidate;
end;

procedure TBitmapEditFrame.OnPropChanged;
begin
  PaintBox.Invalidate;
end;

procedure TBitmapEditFrame.OnTreeChanged;
begin
  ReadFromComponent;
  RepaintPage;
  PaintBox.Invalidate;
end;

procedure TBitmapEditFrame.PaintBoxPaint(Sender: TObject);
var
  Data : pointer;
  Bmi : TBitmapInfo;
  W,H : integer;
  C : TCanvas;
begin
  C := PaintBox.Canvas;

  Data := Bitmap.GetCopyAsBytes;

  W := Bitmap.PixelWidth;
  H := Bitmap.PixelHeight;

  ZeroMemory(@Bmi, SizeOf(Bmi));
  with Bmi.bmiHeader do
  begin
    biSize     := SizeOf(bmi.bmiHeader);
    biWidth    := W;
    biHeight   := H;
    biPlanes   := 1;
    biBitCount := 24;
  end;

  SetStretchBltMode(C.Handle, HALFTONE);

  StretchDIBits(C.Handle, 0, 0,
    PaintBox.ClientRect.Right, PaintBox.ClientRect.Bottom,
    0, 0, W, H, Data, Bmi, DIB_RGB_COLORS, SRCCOPY);

  FreeMem(Data);
end;



end.
