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

unit ZBitmap;

interface

uses ZOpenGL, ZClasses;

type
  TBitmapSize = (bs16,bs32,bs64,bs128,bs256,bs512,bs1024,bs2048);

  //32 bits per pixel image
  TZBitmap = class(TContent)
  strict private
    IsInitialized : boolean;
    Memory : pointer;
    MemFormat,MemType : GLuint;
    procedure ReInit;
    procedure CleanUp;
  protected
    procedure CopyAndDestroy(Source : TContent); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Handle: GLuint;
    //Keep fields in sync with CopyAndDestroy + CreateFromBitmap
    PropWidth,PropHeight : TBitmapSize;
    Filter : (bmfLinear,bmfNearest,bmfMipmap);
    constructor CreateFromBitmap(B : TZBitmap);
    destructor Destroy; override;
    procedure Update; override;
    procedure UseTextureBegin;
    //procedure UseTextureEnd;
    procedure RenderTargetBegin;
    procedure RenderTargetEnd;
    procedure SetMemory(P : pointer; MemFormat, MemType : GLuint);
    function PixelWidth : integer;
    function PixelHeight : integer;
    function GetCopyAsFloats : pointer;
    function GetCopyAs3f: pointer;
    procedure ResetGpuResources; override;
  end;

implementation

{$ifndef minimal}
uses BitmapProducers,ZApplication,ZLog,SysUtils,Renderer,GLDrivers;
{$endif}
{$ifdef Android}
uses ZMath,Renderer,ZApplication,GLDrivers;
{$endif}

{ TZBitmap }

constructor TZBitmap.CreateFromBitmap(B: TZBitmap);
begin
  inherited Create(nil);
  PropHeight := B.PropHeight;
  PropWidth := B.PropWidth;
  Filter := B.Filter;
  {$ifdef android}
  Self._ZApp := B.ZApp;
  {$endif}
end;

destructor TZBitmap.Destroy;
begin
  CleanUp;
  inherited;
end;

{$ifdef android}
//GLES cannot do glGetTexImage so render to framebuffer and copy from there instead
procedure GLESPixelsFromTexture(B : TZBitmap; P : PFloat; ElementsPerPix : integer);
var
  Sp,Tp : PByte;
  Count,I : integer;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    glDisable(GL_LIGHTING);
    glDisable(GL_CULL_FACE);

    glViewport(0, 0, B.PixelWidth, B.PixelHeight);

    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ZERO);

    glDisable(GL_DEPTH_TEST);

    glEnable(GL_TEXTURE_2D);
    B.UseTextureBegin;
    glScalef(2,2,2);
    B.ZApp.Driver.RenderUnitQuad();

    Count := B.PixelWidth * B.PixelHeight;
    GetMem(Tp,Count*4);

    //Only rgba-format seems to work on GLES
    glReadPixels(0, 0, B.PixelWidth, B.PixelHeight, GL_RGBA, GL_UNSIGNED_BYTE, Tp);

    Sp := Tp;
    while Count>0 do
    begin
      for I := 0 to ElementsPerPix-1 do
      begin
        P^ := Sp^ / 255;
        Inc(P);
        Inc(Sp);
      end;
      if ElementsPerPix=3 then
        Inc(Sp);
      Dec(Count);
    end;
    FreeMem(Tp);

  glPopAttrib;
end;
{$endif}

function TZBitmap.GetCopyAsFloats: pointer;
var
  P : PFloat;
begin
  GetMem(P,PixelHeight * PixelWidth * 4 * SizeOf(single));
  {$ifdef android}
  GLESPixelsFromTexture(Self,P,4);
  {$else}
  UseTextureBegin;
  glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_FLOAT,P);
  {$endif}
  Result := P;
end;

function TZBitmap.GetCopyAs3f: pointer;
var
  P : PFloat;
begin
  GetMem(P,PixelHeight * PixelWidth * 3 * SizeOf(single));
  {$ifdef android}
  GLESPixelsFromTexture(Self,P,3);
  {$else}
  UseTextureBegin;
  glGetTexImage(GL_TEXTURE_2D,0,GL_RGB,GL_FLOAT,P);
  {$endif}
  Result := P;
end;

procedure TZBitmap.CleanUp;
begin
  if IsInitialized then
  begin
    IsInitialized := False;
    glDeleteTextures(1, @Handle);
  end;
end;

procedure TZBitmap.CopyAndDestroy(Source: TContent);
var
  B : TZBitmap;
begin
  CleanUp;
  B := TZBitmap(Source);
  Self.Handle := B.Handle;
  Self.IsInitialized := B.IsInitialized;
  Self.PropHeight := B.PropHeight;
  Self.PropWidth := B.PropWidth;
  Self.Memory := B.Memory;
  Self.MemFormat := B.MemFormat;
  Self.MemType := B.MemType;
  Self.Filter := B.Filter;
  B.Handle := 0;
  B.IsInitialized := False;
  B.Free;
end;

//Call this when bitmap size have changed
procedure TZBitmap.ReInit;
const
  FilterTypes : array[0..2] of integer = (GL_LINEAR,GL_NEAREST,GL_LINEAR_MIPMAP_LINEAR);
var
  Size,W,H,I : integer;
  P : pointer;

  {$ifdef android}
  NewMemory : pointer;
  procedure ConvertToByte;
  var
    P : PFloat;
    DP: PByte;
    I : integer;
  begin
    P := Self.Memory;
    I := W*H;
    if MemFormat=GL_RGB then
      I := I * 3
    else
      I := I * 4;
    GetMem(NewMemory,I);
    DP := NewMemory;
    while I>0 do
    begin
      DP^ := Trunc( Clamp(P^,0,1) * 255 );
      Inc(P);
      Inc(DP);
      Dec(I);
    end;
    Self.Memory := NewMemory;
    Self.MemType := GL_UNSIGNED_BYTE;
  end;
  {$endif}
begin
  CleanUp;

  if Producers.Count>0 then
  begin
    //Create from child content producers
    RefreshFromProducers;
  end
  else
  begin
    //Create as a texgen buffer
    glGenTextures(1, @Handle);
    glBindTexture(GL_TEXTURE_2D, Handle);

    W := PixelWidth;
    H := PixelHeight;

    //Generate mipmaps automatically, must be set before call to glTexImage2D
    if Filter=bmfMipmap then
      glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);

    if Memory<>nil then
    begin
      {$ifdef android}
      if Self.MemType=GL_FLOAT then
        ConvertToByte
      else
        NewMemory := nil;
      {$endif}
      glTexImage2D(GL_TEXTURE_2D, 0, Self.MemFormat, W, H, 0, Self.MemFormat, Self.MemType, Memory);
      Memory := nil;
      {$ifdef android}
      FreeMem(NewMemory);
      {$endif}
    end else
    begin
      //Must be set to something, otherwise copy from framebuffer won't work
      Size := H*W*4;
      GetMem(P, Size);
      FillChar(P^,Size,$ff);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, P);
      FreeMem(P);
    end;

    if Self.Filter=bmfMipmap then
      I := GL_LINEAR  //Mipmap is not a valid mag-filter
    else
      I := FilterTypes[Ord(Self.Filter)];
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, I );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, FilterTypes[Ord(Self.Filter)] ); //GL_NEAREST_MIPMAP_LINEAR);
    {$ifdef android}
    if (Self.Filter=bmfMipmap) and (Self.ZApp.Driver.Kind>glbFixed) then
      glGenerateMipmap(GL_TEXTURE_2D);  //Mipmaps must be explicitly generated in GLES2
    {$endif}
  end;
  IsChanged := False;
  Producers.IsChanged := False;
  IsInitialized := True;
end;


procedure TZBitmap.RenderTargetBegin;
begin
  if not IsInitialized then
    ReInit;

  glPushAttrib(GL_ALL_ATTRIB_BITS);
    //Skippa perspektiv och sätt upp -1.0 .. 1.0 upplösning
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    glDisable(GL_LIGHTING);
    glDisable(GL_CULL_FACE);
    //Clear
    {$ifndef minimal}
    glClearColor(0,0,0,0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    {$endif}
    glViewport(0, 0, PixelWidth, PixelHeight);
    {$ifdef zgeviz}
    //ZApp.ViewportChanged;
    {$endif}
end;

procedure TZBitmap.RenderTargetEnd;
var
  P : pointer;
begin
    glBindTexture(GL_TEXTURE_2D, Handle);

    {
      glCopyTexSubImage2D cannot be used here because the combination of calling
      glCopyTexSubImage2D and then later glGetTexImage on the same texture causes
      a crash in atioglxx.dll on ATI Radeon X1650 and X1550.
      Instead we copy to the main memory first and then update.
    }
    //    glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, PixelWidth, PixelHeight);  //**
    GetMem(P, PixelWidth * PixelHeight * 4);
    glReadPixels(0, 0, PixelWidth, PixelHeight, GL_RGBA, GL_UNSIGNED_BYTE, P);
    SetMemory(P,GL_RGBA,GL_UNSIGNED_BYTE);
    ReInit;
    FreeMem(P);

  glPopAttrib;
  {$ifdef zgeviz}
  //ZApp.ViewportChanged;
  {$endif}
end;

//Set this bitmap as current opengl texture
procedure TZBitmap.UseTextureBegin;
begin
  if (not IsInitialized)
   {$ifndef minimal}or Producers.IsChanged or IsChanged{$endif} then
    ReInit;

  glBindTexture(GL_TEXTURE_2D, Handle);
end;

procedure TZBitmap.Update;
begin
  //Dynamic bitmap must be updated before draw begins since backbuffer is being used
  if (not IsInitialized) or (Producers.IsChanged) or (IsChanged) then
    ReInit;
end;

procedure TZBitmap.SetMemory(P: pointer; MemFormat,MemType : GLUInt);
begin
  Self.Memory := P;
  Self.MemFormat := MemFormat;
  Self.MemType := MemType;
end;

function TZBitmap.PixelHeight: integer;
begin
  Result := 16 shl ord(PropHeight);
end;

function TZBitmap.PixelWidth: integer;
begin
  Result := 16 shl ord(PropWidth);
end;

procedure TZBitmap.ResetGpuResources;
begin
  CleanUp;
  inherited;
end;

procedure TZBitmap.DefineProperties(List: TZPropertyList);
begin
  inherited;
  {$ifndef minimal}
//  List.GetByName('Producers').SetChildClasses([TBitmapExpression,TBitmapRect,TBitmapZoomRotate]);
  {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Width',{$ENDIF}(@PropWidth), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['16','32','64','128','256','512','1024','2048']);{$endif}
    List.GetLast.DefaultValue.ByteValue := Ord(bs64);
  List.AddProperty({$IFNDEF MINIMAL}'Height',{$ENDIF}(@PropHeight), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['16','32','64','128','256','512','1024','2048']);{$endif}
    List.GetLast.DefaultValue.ByteValue := Ord(bs64);
  List.AddProperty({$IFNDEF MINIMAL}'Filter',{$ENDIF}(@Filter), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Linear','Nearest','Mipmap']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Handle',{$ENDIF}(@Handle), zptInteger);
    List.GetLast.NeverPersist:=True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

initialization

  ZClasses.Register(TZBitmap,ZBitmapClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Bitmap';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=9;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}

end.
