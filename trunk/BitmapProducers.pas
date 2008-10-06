unit BitmapProducers;

interface

uses ZOpenGL, ZClasses, ZExpressions;

type
  TBitmapRect = class(TContentProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    Color : TZColorf;
    Size : TZRectf;
  end;

  TBitmapZoomRotate = class(TContentProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    Zoom,Rotation,ScaleX,ScaleY : single;
  end;

  TBitmapExpression = class(TContentProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    Expression : TZExpressionPropValue;
    X,Y : single;
    Pixel : TZColorf;
  end;

  TBitmapFromFile = class(TContentProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    BitmapFile : TZBinaryPropValue;
    Transparency : (btNone,btBlackColor,btAlphaLayer);
    HasAlphaLayer : boolean;
  end;

  TBitmapBlur = class(TContentProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    Radius : integer;
  end;


implementation

uses ZBitmap {$ifdef zlog},ZLog{$endif}, ZMath;

{ TBitmapRect }

procedure TBitmapRect.ProduceOutput(Content : TContent; Stack : TZArrayList);
var
  B : TZBitmap;
  Arg : boolean;
begin
  //one optional argument
  Arg := Stack.Count>0;
  if Arg then
    B := TZBitmap(Stack.Pop())
  else
  begin
    B := TZBitmap.CreateFromBitmap( TZBitmap(Content) );
  end;

  B.RenderTargetBegin;

  if not Arg then
  begin
    //Clear if no source
    glClearColor(0.0,0,0,0.0);
    glClear(GL_COLOR_BUFFER_BIT);
  end;

//    glColor3f(0.0,1.0,0.0);
  glColor3fv(@Color);
  glBegin(GL_QUADS);
    glVertex2f(Size.Left,Size.Top);
    glVertex2f(Size.Left,Size.Bottom);
    glVertex2f(Size.Right,Size.Bottom);
    glVertex2f(Size.Right,Size.Top);
  glEnd();
  glColor3f(1.0,1.0,1.0);

  B.RenderTargetEnd;

  Stack.Push(B);
end;

procedure TBitmapRect.DefineProperties(List: TZPropertyList);
const DefSize = 0.2;
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Color',{$ENDIF}integer(@Color) - integer(Self), zptColorf);
    List.GetLast.DefaultValue.ColorfValue := MakeColorf(0.5,0.5,0.5,1.0);
  List.AddProperty({$IFNDEF MINIMAL}'Size',{$ENDIF}integer(@Size) - integer(Self), zptRectf);
    List.GetLast.DefaultValue.RectfValue := TZRectf(MakeColorf(-DefSize,-DefSize,DefSize,DefSize));
end;

{ TBitmapZoomRotate }

procedure TBitmapZoomRotate.ProduceOutput(Content : TContent; Stack : TZArrayList);
const
  Size = 9;
var
  B,SourceB : TZBitmap;
  TexLeft : single;
  TexTop : single;
  TexRight : single;
  TexBottom : single;
  Z : single;
begin
  //one argument required
  if Stack.Count=0 then
    Exit;
  SourceB := TZBitmap(Stack.Pop());

  B := TZBitmap.CreateFromBitmap( TZBitmap(Content) );

  B.RenderTargetBegin;

  SourceB.UseTextureBegin;

  Z := -Zoom;

  TexLeft := 0 - Z;
  TexRight := Size + Z;
  TexTop := 0 - Z;
  TexBottom := Size + Z;

  //1.0 = one full circle = 360 degrees in opengl
  if Rotation<>0 then
    glRotatef( (Rotation*360) , 0, 0, 1);

  glEnable(GL_TEXTURE_2D);
  glScalef(ScaleX,ScaleY,1);
  glBegin(GL_QUADS);
    glTexCoord2f(TexLeft, TexTop);
    glVertex2f(-Size,-Size);
    glTexCoord2f(TexLeft, TexBottom);
    glVertex2f(-Size,Size);
    glTexCoord2f(TexRight, TexBottom);
    glVertex2f(Size,Size);
    glTexCoord2f(TexRight, TexTop);
    glVertex2f(Size,-Size);
  glEnd();

//  SourceB.UseTextureEnd;
  SourceB.Free;

  B.RenderTargetEnd;

  Stack.Push(B);
end;

procedure TBitmapZoomRotate.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Zoom',{$ENDIF}integer(@Zoom) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Rotation',{$ENDIF}integer(@Rotation) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ScaleX',{$ENDIF}integer(@ScaleX) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'ScaleY',{$ENDIF}integer(@ScaleY) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
end;

{ TBitmapExpression }

procedure TBitmapExpression.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Expression',{$ENDIF}integer(@Expression) - integer(Self), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//X,Y : current coordinate (0..1)'#13#10 +
      '//Pixel : current color (rgb)';
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'X',{$ENDIF}integer(@X) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Y',{$ENDIF}integer(@Y) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Pixel',{$ENDIF}integer(@Pixel) - integer(Self), zptColorf);
    List.GetLast.NeverPersist := True;
end;

procedure TBitmapExpression.ProduceOutput(Content : TContent; Stack: TZArrayList);
var
  SourceB,B : TZBitmap;
  H,W,I,J : integer;
  Pixels,P : PColorf;
  XStep,YStep : single;
begin
  if Stack.Count>0 then
  begin
    SourceB := TZBitmap(Stack.Pop());
    B := TZBitmap.CreateFromBitmap( SourceB );
    Pixels := SourceB.GetCopyAsFloats;
    SourceB.Free;
  end
  else
  begin
    B := TZBitmap.CreateFromBitmap( TZBitmap(Content) );
    Pixels := nil;
  end;

  W := B.PixelWidth;
  H := B.PixelHeight;

  if Pixels=nil then
  begin
    GetMem(Pixels,W * H * Sizeof(Pixel) );
    FillChar(Pixels^,W * H * Sizeof(Pixel),0);
  end;

  B.SetMemory(Pixels,GL_RGBA,GL_FLOAT);

  P := Pixels;
  Y := 0;
  XStep := 1/W;
  YStep := 1/H;
  for I := 0 to H-1 do
  begin
    X := 0;
    for J := 0 to W-1 do
    begin
      Pixel := P^;
      ZExpressions.RunCode(Expression.Code);
      P^ := Pixel;
      Inc(P);
      X := X + XStep;
    end;
    Y := Y + YStep;
  end;

  //Needed to send the bitmap to opengl
  B.UseTextureBegin;
//  B.UseTextureEnd;

  FreeMem(Pixels);

  Stack.Push(B);
end;

{ TBitmapFromFile }

procedure TBitmapFromFile.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'BitmapFile',{$ENDIF}integer(@BitmapFile) - integer(Self), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}'Transparency',{$ENDIF}integer(@Transparency) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['None','BlackColor','AlphaLayer']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'HasAlphaLayer',{$ENDIF}integer(@HasAlphaLayer) - integer(Self), zptBoolean);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

procedure TBitmapFromFile.ProduceOutput(Content: TContent; Stack: TZArrayList);
var
  BM : TZBitmap;
  SP,DP,Mem : PByte;
  IsTransparent : boolean;
  PixelCount,Pixel : integer;
  I: Integer;
  B : byte;
begin
  BM := TZBitmap.CreateFromBitmap( TZBitmap(Content) );
  Mem := nil;

  PixelCount := BM.PixelWidth*BM.PixelHeight;

  {$ifndef minimal}
  if BitmapFile.Size<(PixelCount*3) then
  begin
    BM.Free;
    Exit;
  end;
  {$endif}

  //Ifall det behövs format och storleksdata om bitmappen
  //så kan man lägga till properties senare.

  IsTransparent := Transparency<>btNone;
  if IsTransparent or HasAlphaLayer then
  begin
    GetMem(Mem,PixelCount*4);
    SP := BitmapFile.Data;
    DP := Mem;
    Pixel := 0;
    for I := 0 to (PixelCount*3) - 1 do
    begin
      B := SP^;
      DP^ := B;
      Inc(Pixel,B);
      Inc(SP);
      Inc(DP);
      if (I mod 3)=2 then
      begin
        case Transparency of
          btBlackColor :
            if Pixel=0 then
              DP^ := 0
            else
              DP^ := High(Byte);
          btAlphaLayer :
            begin
              DP^ := SP^;
            end;
        end;
        if HasAlphaLayer then
          Inc(SP);
        Inc(DP);
        Pixel := 0;
      end;
    end;
    BM.SetMemory(Mem,GL_RGBA,GL_UNSIGNED_BYTE);
  end else
  begin
    BM.SetMemory(BitmapFile.Data,GL_RGB,GL_UNSIGNED_BYTE);
  end;

  //Needed to send the bitmap to opengl
  BM.UseTextureBegin;
//  BM.UseTextureEnd;

  if Mem<>nil then
    FreeMem(Mem);

  Stack.Push(BM);
end;

{ TBitmapBlur }

procedure TBitmapBlur.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Radius',{$ENDIF}integer(@Radius) - integer(Self), zptInteger);
end;

procedure TBitmapBlur.ProduceOutput(Content: TContent; Stack: TZArrayList);
var
  SourceB,B : TZBitmap;
  H,W,I,J,K,L : integer;
  SourceP,DestP : PColorf;
  Tot : TZVector3f;
  P : PZVector3f;

  procedure InAdd(X,Y:integer);
  var
    Tmp : PZVector4f;
  begin
    if (X<0) then
      X:=0
    else if (X>W-1) then
      X := W-1;
    if Y<0 then
      Y := 0
    else if (Y>H-1) then
      Y := H-1;
    Tmp := PZVector4f(SourceP);
    Inc(Tmp, Y*W + X);
    VecAdd3(PZVector3f(Tmp)^,Tot,Tot)
  end;

begin
  if Stack.Count=0 then
    Exit;

  SourceB := TZBitmap(Stack.Pop());
  B := TZBitmap.CreateFromBitmap( SourceB );
  SourceP := SourceB.GetCopyAsFloats;
  SourceB.Free;

  W := B.PixelWidth;
  H := B.PixelHeight;

  GetMem(DestP,SizeOf(TZVector3f)*W*H);
  B.SetMemory(DestP,GL_RGB,GL_FLOAT);

  //Reference: http://www.blackpawn.com/texts/blur/default.html
  P := PZVector3f(DestP);
  for I := 0 to H-1 do
  begin
    for J := 0 to W-1 do
    begin
      FillChar(Tot,SizeOf(Tot),0);
      for K := -Radius to Radius do
        for L := -Radius to Radius do
          InAdd(J+K,I + L);
      VecDiv3(Tot, Power(Radius*2+1,2),P^);
      Inc(P);
    end;
  end;

  //Needed to send the bitmap to opengl
  B.UseTextureBegin;

  FreeMem(SourceP);
  FreeMem(DestP);

  Stack.Push(B);
end;

initialization

  ZClasses.Register(TBitmapRect,BitmapRectClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'Bitmap';{$endif}
  ZClasses.Register(TBitmapZoomRotate,BitmapZoomRotateClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'Bitmap';{$endif}
  ZClasses.Register(TBitmapExpression,BitmapExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'Bitmap';{$endif}
  ZClasses.Register(TBitmapFromFile,BitmapFromFileClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'Bitmap';{$endif}
  ZClasses.Register(TBitmapBlur,BitmapBlurClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'Bitmap';{$endif}

end.
