unit RetroCoding;

interface

uses System.Classes, ZApplication, ZClasses, ZBitmap;

type
  TRtlRoutine = (rrtSwitch);
  TRetroBuilder = class;
  TRetroTarget = (rtaSpectrum,rtaMasterSystem,rtaVectrex);
  TRetroCpu = class;

  TRetroBuilder = class
  strict private type
    TFixUp =
      record
        Offset,Target : integer;
        IsRelative : boolean;
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
  strict private
    InstructionOffsets : TList;
    Fixups : array of TFixUp;
    ResourceFixups : array of TResourceFixUp;
    RtlFixups : array of TRtlFixUp;
    CurrentInstructionIndex : integer;
  private
    Code : TMemoryStream;
    procedure WriteCode(const Code : array of byte);
    procedure WriteCodeString(S : string);
    procedure WriteCodeAddress(Ad : integer);
    procedure WriteFile(const Code : array of byte; Ms : TMemoryStream);
    procedure WriteFileString(S : string; Ms : TMemoryStream);
    procedure WriteFileAddress(Ad : integer; Ms : TMemoryStream);
    procedure GenList(Exp : TZComponentList; IsFunc : boolean);
    procedure Fail(const ErrorMessage : string);
    procedure AddFixup(const Destination : integer; IsRelative : boolean = False);
    procedure AddResourceFixup(const Resource : TZComponent);
    procedure AddRtlFixup(const Routine : TRtlRoutine);
  public
    ZApp : TZApplication;
    Cpu : TRetroCpu;
    FileName : string;
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
  end;

  TRetroCpu = class abstract
  private
    Builder: TRetroBuilder;
    CodeBase: integer;
    VarBase: integer;
    RedundantLoadsRemoved: integer;
    VarSize: integer;
    procedure GenBitmap(Bitmap: TZBitmap); virtual; abstract;
    procedure GenInit; virtual; abstract;
    procedure GenFinish; virtual; abstract;
    procedure Gen(C: TZComponent); virtual; abstract;
    procedure GenLoadResource(C: TZComponent); virtual; abstract;
    procedure GenLoadInteger(Value: integer); virtual; abstract;
    procedure GenArrayGetElement(C: TZComponent); virtual; abstract;
    procedure GenRtl(Routine: TRtlRoutine); virtual; abstract;
    procedure GenFile(Ms: TMemoryStream; var FileName: string); virtual; abstract;
    procedure GenAddress(const Address : integer; Ms : TMemoryStream); virtual; abstract;
    procedure PeepholeWrite(const Buf: array of byte); virtual; abstract;
  public
    Target : TRetroTarget;
    ShouldTerminateStrings : boolean;
    StringTerminator : byte;
  end;

  TCpuZ80 = class(TRetroCpu)
  strict private
    procedure GenBitmap_Zx(Bitmap: TZBitmap);
    procedure GenBitmap_Sms(Bitmap: TZBitmap);
  private
    procedure GenInit; override;
    procedure GenFinish; override;
    procedure Gen(C: TZComponent); override;
    procedure GenLoadResource(C: TZComponent); override;
    procedure GenLoadInteger(Value: integer); override;
    procedure GenArrayGetElement(C: TZComponent); override;
    procedure GenBitmap(Bitmap: TZBitmap); override;
    procedure GenRtl(Routine: TRtlRoutine); override;
    procedure GenFile(Ms: TMemoryStream; var FileName: string); override;
    procedure PeepholeWrite(const Buf: array of byte); override;
    procedure GenAddress(const Address : integer; Ms : TMemoryStream); override;
  end;

  TCpu6809 = class(TRetroCpu)
  strict private
  private
    procedure GenInit; override;
    procedure GenFinish; override;
    procedure Gen(C: TZComponent); override;
    procedure GenLoadResource(C: TZComponent); override;
    procedure GenLoadInteger(Value: integer); override;
    procedure GenArrayGetElement(C: TZComponent); override;
    procedure GenBitmap(Bitmap: TZBitmap); override;
    procedure GenRtl(Routine: TRtlRoutine); override;
    procedure GenFile(Ms: TMemoryStream; var FileName: string); override;
    procedure GenAddress(const Address : integer; Ms : TMemoryStream); override;
    procedure PeepholeWrite(const Buf: array of byte); override;
  end;

implementation

uses ZExpressions, SysUtils, ZLog, ZFile, ZOpenGL;

{ TRetroBuilder }

constructor TRetroBuilder.Create;
begin
  Code := TMemoryStream.Create;
  InstructionOffsets := TList.Create;
end;

destructor TRetroBuilder.Destroy;
begin
  Code.Free;
  InstructionOffsets.Free;
  inherited;
end;

procedure TRetroBuilder.AddFixup(const Destination: integer; IsRelative : boolean = False);
var
  I : integer;
begin
  I := Length(Fixups);
  SetLength(Fixups,I+1);
  Fixups[I].Offset := Code.Position;
  Fixups[I].Target := Self.CurrentInstructionIndex + Destination;
  Fixups[I].IsRelative := IsRelative;
  WriteCodeAddress(0);
end;

procedure TRetroBuilder.AddResourceFixup(const Resource: TZComponent);
var
  I : integer;
begin
  I := Length(ResourceFixups);
  SetLength(ResourceFixups,I+1);
  ResourceFixups[I].Offset := Code.Position;
  ResourceFixups[I].Resource := Resource;
  WriteCodeAddress(0);
end;

procedure TRetroBuilder.AddRtlFixup(const Routine: TRtlRoutine);
var
  I : integer;
begin
  I := Length(RtlFixups);
  SetLength(RtlFixups,I+1);
  RtlFixups[I].Offset := Code.Position;
  RtlFixups[I].Routine:= Routine;
  WriteCodeAddress(0);
end;

procedure TRetroBuilder.Execute;
var
  Zex : TZExpression;
  I : integer;
  ResourceNames : TStringList;
  Ar : TDefineArray;
  OutFile : TMemoryStream;
  RtlAddress : array[TRtlRoutine] of word;
begin
  Cpu.Builder := Self;
  Cpu.GenInit;

  //Find main ZExpression
  Zex := nil;
  for I := 0 to Self.ZApp.OnLoaded.ComponentCount-1 do
  begin
    if Self.ZApp.OnLoaded[I] is TZExpression then
    begin
      Zex := Self.ZApp.OnLoaded[I] as TZExpression;
      Break;
    end;
  end;

  GenList(Zex.Expression.Code,False);

  Cpu.GenFinish;


  //Resolving fixups
  for I := 0 to High(Fixups) do
  begin
    Code.Position := Fixups[I].Offset;
    if Fixups[I].IsRelative then
      //+2 here is pointer size
      WriteCodeAddress( NativeInt(InstructionOffsets[ Fixups[I].Target+1 ]) - (Code.Position+2) )
    else
      WriteCodeAddress( Cpu.CodeBase + NativeInt(InstructionOffsets[ Fixups[I].Target+1 ])  );
  end;

  //Components
  ResourceNames := TStringList.Create;
  for I := 0 to High(ResourceFixups) do
  begin
    if ResourceFixups[I].Resource.Name='' then
      ResourceFixups[I].Resource.SetString('Name','@tempresource' + AnsiString(IntToStr(I)));
    if ResourceNames.IndexOf( string(ResourceFixups[I].Resource.Name) )=-1 then
    begin
      Code.Position := Code.Size;

      ResourceNames.AddObject(string(ResourceFixups[I].Resource.Name),
        TObject(NativeInt(Cpu.CodeBase + Code.Position)) );

      if ResourceFixups[I].Resource is TZFile then
        Code.Write(TZFile(ResourceFixups[I].Resource).FileEmbedded.Data^,
          TZFile(ResourceFixups[I].Resource).FileEmbedded.Size)
      else if ResourceFixups[I].Resource is TZBitmap then
      begin
        Cpu.GenBitmap(TZBitmap(ResourceFixups[I].Resource));
      end
      else if ResourceFixups[I].Resource is TExpStringConstant then
      begin
        Code.Write(TExpStringConstant(ResourceFixups[I].Resource).Value^,Length(TExpStringConstant(ResourceFixups[I].Resource).Value));
        if Cpu.ShouldTerminateStrings then
          Code.Write(Cpu.StringTerminator,1);
      end
      else if ResourceFixups[I].Resource is TDefineArray then
      begin
        Ar := ResourceFixups[I].Resource as TDefineArray;
        if Ar.Persistent then
          Code.Write(Ar.Values.Data^,Ar.Values.Size)
        else
        begin
          //Memory array, allocate in var area
          ResourceNames.Objects[ResourceNames.Count-1] := TObject(NativeInt(Cpu.VarBase + Cpu.VarSize));
          if Ar._Type=zctByte then
            Inc(Cpu.VarSize,Ar.SizeDim1)
          else if Ar._Type=zctInt then
            Inc(Cpu.VarSize,Ar.SizeDim1*2)
          else
            Fail('wrong array type');
        end;
      end
      else
        Fail('Wrong type of resource: ' + ResourceFixups[I].Resource.ClassName);
    end;
    Code.Position := ResourceFixups[I].Offset;
    WriteCodeAddress( Integer(ResourceNames.Objects[ ResourceNames.IndexOf(string(ResourceFixups[I].Resource.Name)) ]) );
  end;
  ResourceNames.Free;


  FillChar(RtlAddress,SizeOf(RtlAddress),0);
  for I := 0 to High(RtlFixups) do
  begin
    if RtlAddress[RtlFixups[I].Routine]=0 then
    begin
      Code.Position := Code.Size;
      RtlAddress[RtlFixups[I].Routine] := Cpu.CodeBase + Code.Position;
      Cpu.GenRtl(RtlFixups[I].Routine);
    end;
    Code.Position := RtlFixups[I].Offset;
    WriteCodeAddress( RtlAddress[RtlFixups[I].Routine] );
  end;

  Code.Position := 0;

  OutFile := TMemoryStream.Create;
  try
    Cpu.GenFile(OutFile,FileName);
    OutFile.SaveToFile(FileName);
    ZLog.GetLog(Self.ClassName).Write('File generated: ' + FileName);
  finally
    OutFile.Free;
  end;
end;

procedure TRetroBuilder.Fail(const ErrorMessage: string);
begin
  raise Exception.Create(ErrorMessage);
end;

procedure TRetroBuilder.GenList(Exp: TZComponentList; IsFunc: boolean);
var
  I : integer;
  StringConstant : TExpStringConstant;
  IntConstant : TExpConstantInt;
  Ar : TDefineArray;
  C : TZComponent;
begin
  StringConstant := nil;
  IntConstant := nil;
  I := 0;
  while I<Exp.Count do
  begin
    while InstructionOffsets.Count<I do
      InstructionOffsets.Add(nil);
    InstructionOffsets.Add( pointer( Code.Position ) );

    Self.CurrentInstructionIndex := I;

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
          Cpu.GenLoadResource( TExpLoadComponent(C).Component );
        Continue;
      end;
    end else if (C is TExpExternalFuncCall) then
    begin
      if TExpExternalFuncCall(C).FuncName='emit' then
      begin
        WriteCodeString(String(StringConstant.Value));
      end else if TExpExternalFuncCall(C).FuncName='getResourceAddress' then
      begin
        Cpu.GenLoadResource( (Exp[I-1] as TExpLoadComponent).Component );
      end else if TExpExternalFuncCall(C).FuncName='push' then
      begin
        //do nothing
      end else if TExpExternalFuncCall(C).FuncName='pushString' then
      begin
        //do nothing
      end else if TExpExternalFuncCall(C).FuncName='emitByte' then
      begin
        WriteCode([ IntConstant.Constant ]);
      end
      else
        Fail('Invalid func call');
    end else if (C is TExpConstantInt) then
    begin
      //if next is TExternal 'emitByte' then hold in variable, else push integer value
      if (Exp[I+1] is TExpExternalFuncCall) and
        (TExpExternalFuncCall(Exp[I+1]).FuncName='emitByte') then
        IntConstant := C as TExpConstantInt
      else
      begin
        Cpu.GenLoadInteger( TExpConstantInt(C).Constant );
      end;
    end else if (C is TExpArrayGetElement) then
    begin
      Ar := (Exp[I-1] as TExpLoadComponent).Component as TDefineArray;
      Assert(Ar._Type=zctByte,'Wrong array type');
      Assert(Ar.Dimensions=dadOne,'Only 1D array supported');
      Cpu.GenArrayGetElement(Ar);
    end else
      Cpu.Gen(C);

    Inc(I);
  end;
end;

procedure TRetroBuilder.WriteCode(const Code: array of byte);
var
  B : integer;
begin
  for B in Code do
    Self.Code.Write(B,1);
end;

procedure TRetroBuilder.WriteCodeAddress(Ad: integer);
//Write nativeint for the platform
begin
  Cpu.GenAddress(Ad,Self.Code);
end;

procedure TRetroBuilder.WriteCodeString(S: string);
var
  Buf : array of byte;
begin
  S := StringReplace(S,' ','',[rfReplaceAll]);

  SetLength(Buf,Length(S) div 2);
  HexToBin(PWideChar(S),Buf[0],Length(Buf));

  Cpu.PeepholeWrite(Buf);
//  Code.Write(Buf[0],Length(Buf));
end;

procedure TRetroBuilder.WriteFile(const Code: array of byte; Ms: TMemoryStream);
var
  B : integer;
begin
  for B in Code do
    Ms.Write(B,1);
end;

procedure TRetroBuilder.WriteFileAddress(Ad: integer; Ms: TMemoryStream);
begin
  Cpu.GenAddress(Ad,Ms);
end;

procedure TRetroBuilder.WriteFileString(S: string; Ms: TMemoryStream);
var
  Buf : array of byte;
begin
  S := StringReplace(S,' ','',[rfReplaceAll]);

  SetLength(Buf,Length(S) div 2);
  HexToBin(PWideChar(S),Buf[0],Length(Buf));

  Ms.Write(Buf[0],Length(Buf));
end;

{ TCpuZ80 }

procedure TCpuZ80.GenInit;
begin
  case Target of
    rtaSpectrum:
      begin
        CodeBase := 30000;
        VarBase := 60000;
        Builder.WriteCode([$3e,$02,$cd,$01,$16]); //open print channel
      end;
    rtaMasterSystem:
      begin
        CodeBase := 0;
        VarBase := $c000;
        Builder.WriteCode([$31,$f0,$df,$f3,$ed,$56]); //ld sp, $dff0, di, im 1
      end;
  end;
end;

procedure TCpuZ80.GenFile(Ms: TMemoryStream; var FileName: string);
var
  I : integer;
begin
  case Target of
    rtaSpectrum:
      begin
        //http://www.worldofspectrum.org/faq/reference/z80format.htm
        //48kb + header
        Ms.SetSize(48*1024+30);
        FillChar(Ms.Memory^,Ms.Size,0);

        Builder.WriteFile([$00,$5c,$ff,$ff,$a8,$10],Ms);
        Builder.WriteFileAddress(CodeBase,Ms); //PC
        Builder.WriteFile([$46,$ff,$ef,$9f,$2e-32,$91,$5c,$4b],Ms);
        Builder.WriteFile([$17,$06,$00,$7f,$10,$00,$44,$3a],Ms);
        Builder.WriteFile([$5c,$ff,$ff,$00,$00,$01],Ms);

        //Screen attributes
        Ms.Position := (16384+6144)-16384+30;

        for I := 0 to 767 do
          Builder.WriteFile([$38],Ms);

        //Default BASIC system variables
        Ms.Position := (23296+256)-16384+30;
        Builder.WriteFileString(
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
          '810FC41552F409C4155080800D80', Ms);

        //Write code
        Ms.Position := CodeBase-16384+30;
        Ms.CopyFrom(Builder.Code,Builder.Code.Size);
        FileName := ChangeFileExt(FileName,'.z80');
      end;
    rtaMasterSystem:
      begin
        FileName := ChangeFileExt(FileName,'.sms');
        //todo: add sms cart header
        Ms.CopyFrom(Builder.Code,Builder.Code.Size);
      end;
  end;
end;

procedure TCpuZ80.GenFinish;
begin
  Builder.WriteCode([$18,$fe]); //infinite loop
  if RedundantLoadsRemoved>0 then
    ZLog.GetLog(Self.ClassName).Write('Redundant loads: ' + IntToStr(RedundantLoadsRemoved) );
end;

procedure TCpuZ80.GenAddress(const Address: integer; Ms: TMemoryStream);
var
  W : word;
begin
  W := Address;
  Ms.Write(W,2);
end;

procedure TCpuZ80.GenArrayGetElement(C: TZComponent);
begin
  Builder.WriteCode([$21]);  //ld hl,nn
  Builder.AddResourceFixup( C );
  Builder.WriteCodeString('d1 19 e5'); //pop de, add hl,de, push hl
end;

procedure TCpuZ80.GenBitmap(Bitmap: TZBitmap);
begin
  case Target of
    rtaSpectrum : GenBitmap_Zx(Bitmap);
    rtaMasterSystem : GenBitmap_Sms(Bitmap);
  end;
end;

procedure TCpuZ80.GenBitmap_Sms(Bitmap: TZBitmap);
var
  P,OriginalP : PLongWord;
  PixelCount,X,Y,TileX,TileY,Bit,I,J : integer;
  Color,OutByte : byte;
  //Map all possible colors to paletteindex, $ff=unused.
  AllColors : array[0..63] of byte;
  PixelRow : array[0..7] of byte;
  ColorsUsed,ColorIndex : integer;
  SavePosPal,SavePosEnd : integer;
  Stream : TMemoryStream;
begin
  Stream := Builder.Code;

  FillChar(AllColors,SizeOf(AllColors),$ff);
  PixelCount := Bitmap.PixelHeight * Bitmap.PixelWidth;
  GetMem(OriginalP,PixelCount * 4);
  Bitmap.UseTextureBegin;
  glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_UNSIGNED_BYTE,OriginalP);

  //Make room for palette first
  SavePosPal := Stream.Position;
  OutByte := 0;
  for I := 0 to 15 do
    Stream.Write(OutByte,1);

  ColorsUsed := 0;
  Y := 0;
  while Y<Bitmap.PixelHeight do
  begin
    X := 0;
    while X<Bitmap.PixelWidth do
    begin
      P := OriginalP;
      Inc(P,(Bitmap.PixelHeight-1-Y) * Bitmap.PixelWidth); //Image is upside down
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
        Dec(P,Bitmap.PixelWidth+8);
      end;
      Inc(X,8);
    end;
    Inc(Y,8);
  end;
  FreeMem(OriginalP);
  if ColorsUsed>16 then
    ZLog.GetLog(Self.ClassName).Write('Too many unique colors: ' + String(Bitmap.Name));

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

procedure TCpuZ80.GenBitmap_Zx(Bitmap: TZBitmap);
var
  P,OriginalP : PLongWord;
  PixelCount,X,Y : integer;
  OutByte : byte;
  Stream : TMemoryStream;
begin
  Stream := Builder.Code;
  PixelCount := Bitmap.PixelHeight * Bitmap.PixelWidth;
  GetMem(OriginalP,PixelCount * 4);
  Bitmap.UseTextureBegin;
  glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_UNSIGNED_BYTE,OriginalP);
  for Y := 0 to Bitmap.PixelHeight-1 do
  begin
    P := OriginalP;
    Inc(P,(Bitmap.PixelHeight-1-Y) * Bitmap.PixelWidth); //Image is upside down
    OutByte := 0;
    for X := 0 to Bitmap.PixelWidth-1 do
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

procedure TCpuZ80.GenLoadInteger(Value: integer);
begin
  Builder.WriteCode([$21]);  //ld hl,
  Builder.WriteCodeAddress(Value);
  Builder.WriteCodeString('e5');  //push hl
end;

procedure TCpuZ80.GenLoadResource(C: TZComponent);
begin
  Builder.WriteCode([$21]);  //ld hl,
  Builder.AddResourceFixup( C );
  Builder.WriteCodeString('e5');  //push hl
end;

procedure TCpuZ80.GenRtl(Routine: TRtlRoutine);
begin
  case Routine of
    rrtSwitch :
      Builder.WriteCodeString('d1 CB25 19 5e 23 56 626b e9');  //pop de, sla l, add hl,de,
            // 5E                     LD   e,(hl)
            // 23                     INC   hl
            // 56                     LD   d,(hl)
            // 62 6B                  LD   hl,de
            // E9                     JP   (hl)

  else
    Builder.Fail('rtl not handled');
  end;
end;

procedure TCpuZ80.PeepholeWrite(const Buf: array of byte);
var
  LastOpCode : byte;
  Ms : TMemoryStream;
begin
  Ms := Builder.Code;

  LastOpCode := 0;
  if Builder.Code.Position>0 then
    LastOpCode := (PByte(Ms.Memory)+Ms.Position-1)^;

  if (LastOpCode=$e5) and (Length(Buf)>0) then
  begin
    if Buf[0]=$e1 then
    begin //Remove pair: push hl, pop hl
      Inc(RedundantLoadsRemoved);
      Ms.Position := Ms.Position-1;
      Ms.Write(Buf[1],Length(Buf)-1);
      Exit;
    end;
    if Buf[0]=$d1 then
    begin //Replace "push hl, pop de" with "ex de, hl"
      Inc(RedundantLoadsRemoved);
      Ms.Position := Ms.Position-1;
      Builder.WriteCode([$eb]);
      Ms.Write(Buf[1],Length(Buf)-1);
      Exit;
    end;
  end;

  Ms.Write(Buf[0],Length(Buf));
end;

procedure TCpuZ80.Gen(C: TZComponent);
var
  I : integer;
  W : word;
  Switch : TExpSwitchTable;
begin
  if (C is TExpAccessLocal) then
  begin
    W := VarBase + TExpAccessLocal(C).Index*2;
    case TExpAccessLocal(C).Kind of
    loLoad :
      begin
        Builder.WriteCode([$2a]);  //ld hl,(nn)
        Builder.WriteCodeAddress(W);
        Builder.WriteCodeString('e5');  //push hl
      end;
    loStore :
      begin
        Builder.WriteCodeString('e1 22'); //pop hl, ld (nn),hl
        Builder.WriteCodeAddress(W);
      end;
    else
      Builder.Fail('Invalid TExpAccessLocal');
    end;
  end else if (C is TExpJump) then
  begin
    //Compared with codegen from z88dk
    case TExpJump(C).Kind of
      jsJumpEQ :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('e1 d1 a7 ed 52 ca');  //pop hl, pop de, and a (clear carry), sbc hl,de, jp z,nn
          Builder.AddFixup(TExpJump(C).Destination);
        end;
      jsJumpNE :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('e1 d1 a7 ed 52 c2');  //pop hl, pop de, and a, sbc hl,de, jp nz,nn
          Builder.AddFixup(TExpJump(C).Destination);
        end;
      jsJumpGE :
        begin //https://retrocomputing.stackexchange.com/questions/9163/comparing-signed-numbers-on-z80-8080-in-assembly
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('d1 e1 a7 ed 52 d2');  //pop de, pop hl, and a, sbc hl,de, jp nc,nn
          Builder.AddFixup(TExpJump(C).Destination);
        end;
      jsJumpGT :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('e1 d1 a7 ed 52 da');  //pop hl, pop de, and a, sbc hl,de, jp c,nn
          Builder.AddFixup(TExpJump(C).Destination);
        end;
      jsJumpLT :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('d1 e1 a7 ed 52 fa');  //pop de, pop hl, and a, sbc hl,de, jp m,nn
          Builder.AddFixup(TExpJump(C).Destination);
        end;
      jsJumpLE :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('e1 d1 a7 ed 52 d2');  //pop hl, pop de, and a, sbc hl,de, jp nc,nn
          Builder.AddFixup(TExpJump(C).Destination);
        end;
      jsJumpAlways :
        begin
          Builder.WriteCode([$c3]);  //jp
          Builder.AddFixup(TExpJump(C).Destination);
        end;
    else
      Builder.Fail('invalid TExpJump');
    end;
  end else if (C is TExpOpBinaryInt) then
  begin
    case TExpOpBinaryInt(C).Kind of
      vbkPlus :
        begin
          Builder.WriteCodeString('e1 d1 19 e5');  //pop hl, pop de, add hl,de, push hl
        end;
      vbkMinus :
        begin
          Builder.WriteCodeString('d1 e1 a7 ed 52 e5');  //pop hl, pop de, and a, sbc hl,de, push hl
        end;
      vbkBinaryAnd :
        begin
          Builder.WriteCodeString('e1d17ba56f2600e5'); //pop hl, pop de, ld a,e, and l, ld l,a, ld h,0, push hl
        end;
      vbkBinaryOr :
        begin
          Builder.WriteCodeString('e1d17bb56f2600e5'); //pop hl, pop de, ld a,e, or l, ld l,a, ld h,0, push hl
        end;
      vbkBinaryShiftLeft :
        begin
          Builder.WriteCodeString('d1e14378b72806cb25cb1410fae5'); //pop de, pop hl, ld b,e, jr z skip, sla l, rl h, djnz next, push hl
        end;
      vbkBinaryShiftRight :
        begin
          Builder.WriteCodeString('d1e14378b72806cb3ccb1d10fae5'); //pop de, pop hl, ld b,e, jr z skip, srl h, rr l, djnz next, push hl
        end;
    else
      Builder.Fail('wrong TExpOpBinaryInt');
    end;
  end else if (C is TExpAssign1) then
  begin
    Builder.WriteCodeString('d1 e1 73'); //pop de, pop hl, ld (hl),e
  end else if (C is TExpMisc) then
  begin
    case (C as TExpMisc).Kind of
      emNop : ;
      emPtrDeref1 :
        begin
          Builder.WriteCodeString('e1 6e 26 00 e5'); //pop hl, ld l,(hl), ld h,0, push hl
        end;
    else
      Builder.Fail('Unsupported misc instruction: ' + C.ClassName);
    end;
  end else if (C is TExpReturn) or (C is TExpStackFrame) then
  begin
    if (C is TExpStackFrame) then
      Inc(Self.VarSize,TExpStackFrame(C).Size*2);
    //ignore these
  end else if (C is TExpSwitchTable) then
  begin
    Switch := (C as TExpSwitchTable);
    if (Switch.LowBound<0) or (Switch.HighBound>128) then
      Builder.Fail('case statements must be in range 0-128');

    Builder.WriteCodeString('e1'); //pop hl

    if Switch.LowBound<>0 then
    begin
      Builder.WriteCode([$3e,Switch.LowBound,$bd,$da]); //ld a,lowbound, cp l, jp c,nn
      Builder.AddFixup(Switch.DefaultOrExit);
    end;

    Builder.WriteCode([$3e,Switch.HighBound,$bd,$fa]); //ld a,highbound, cp l, jp m,nn
    Builder.AddFixup(Switch.DefaultOrExit);

    Builder.WriteCode([$cd]); //call
    Builder.AddRtlFixup(rrtSwitch);

    //emit jumptable
    for I := 0 to Switch.HighBound-Switch.LowBound do
      Builder.AddFixup(PIntegerArray(Switch.Jumps.Data)^[I]);

  end else
    Builder.Fail('Unsupported instruction: ' + C.ClassName);
end;

{ TCpu6809 }

procedure TCpu6809.GenInit;
begin
  case Self.Target of
    rtaVectrex :
      begin
        VarBase := $c880;
        //Variable length header
        Builder.WriteCodeString(
          '67 20 47 43 45 20 32 30 30 34 80 FD 0D FC 30 72'+
          'A8 54 48 52 55 53 54 20 46 4F 52 20 56 45 43 54'+
          '52 45 58 20 31 2E 32 80 FC 30 60 A8 42 59 20 56'+
          '49 4C 4C 45 20 6A 80 00');

        //dp=d0
        Builder.WriteCodeString('86 D0 1F 8B'); //LDA   #0xD0, TFR   a,dp
        Self.ShouldTerminateStrings := True;
        Self.StringTerminator := $80;
      end;
  end;
end;

procedure TCpu6809.GenFinish;
begin

end;

procedure TCpu6809.Gen(C: TZComponent);
var
  I : integer;
  W : word;
  Switch : TExpSwitchTable;
begin
  if (C is TExpAccessLocal) then
  begin
    W := VarBase + TExpAccessLocal(C).Index*2;
    case TExpAccessLocal(C).Kind of
    loLoad :
      begin
        Builder.WriteCode([$fc]);  //ldd
        Builder.WriteCodeAddress(W);
        Builder.WriteCodeString('3406');  //pshs d
      end;
    loStore :
      begin
        Builder.WriteCodeString('3506 fd'); //puls d, std
        Builder.WriteCodeAddress(W);
      end;
    else
      Builder.Fail('Invalid TExpAccessLocal');
    end;
  end else if (C is TExpJump) then
  begin
    //Compared with codegen from z88dk
    case TExpJump(C).Kind of
      jsJumpEQ :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('ece1 10a3e1 1027');  //ldd ,s++  cmpd ,s++  lbeq
          Builder.AddFixup(TExpJump(C).Destination,True);
        end;
      jsJumpNE :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('ece1 10a3e1 1026');  //ldd ,s++  cmpd ,s++  lbne
          Builder.AddFixup(TExpJump(C).Destination,True);
        end;
      jsJumpGE :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('ece1 10a3e1 1023');  //ldd ,s++  cmpd ,s++  lbls
          Builder.AddFixup(TExpJump(C).Destination,True);
        end;
      jsJumpLE :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('ece1 10a3e1 102c');  //ldd ,s++  cmpd ,s++  lbge
          Builder.AddFixup(TExpJump(C).Destination,True);
        end;
      jsJumpGT :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('ece1 10a3e1 102d');  //ldd ,s++  cmpd ,s++  lblt
          Builder.AddFixup(TExpJump(C).Destination,True);
        end;
      jsJumpLT :
        begin
          Assert( TExpJump(C)._Type=jutInt );
          Builder.WriteCodeString('ece1 10a3e1 102e');  //ldd ,s++  cmpd ,s++  lbgt
          Builder.AddFixup(TExpJump(C).Destination,True);
        end;
      jsJumpAlways :
        begin
          Builder.WriteCode([$7e]);  //jmp
          Builder.AddFixup(TExpJump(C).Destination);
        end;
    else
      Builder.Fail('invalid TExpJump');
    end;
  end else if (C is TExpOpBinaryInt) then
  begin
    case TExpOpBinaryInt(C).Kind of
      vbkPlus :
        begin
          Builder.WriteCodeString('3506 e3e4 ede4');  //puls d, addd ,s  std ,s
        end;
      vbkMinus :
        begin
          Builder.WriteCodeString('ec62 a3e1 ede4');  //ld 2,s  subd ,s++  std ,s
        end;
      vbkBinaryAnd :
        begin
          Builder.WriteCodeString('todo'); //pop hl, pop de, ld a,e, and l, ld l,a, ld h,0, push hl
        end;
      vbkBinaryOr :
        begin
          Builder.WriteCodeString('todo'); //pop hl, pop de, ld a,e, or l, ld l,a, ld h,0, push hl
        end;
      vbkBinaryShiftLeft :
        begin
          Builder.WriteCodeString('todo'); //pop de, pop hl, ld b,e, jr z skip, sla l, rl h, djnz next, push hl
        end;
      vbkBinaryShiftRight :
        begin
          Builder.WriteCodeString('todo'); //pop de, pop hl, ld b,e, jr z skip, srl h, rr l, djnz next, push hl
        end;
    else
      Builder.Fail('wrong TExpOpBinaryInt');
    end;
  end else if (C is TExpAssign1) then
  begin
    Builder.WriteCodeString('3506 3510 a784'); //puls d  puls x  sta ,x
  end else if (C is TExpMisc) then
  begin
    case (C as TExpMisc).Kind of
      emNop : ;
      emPtrDeref1 :
        begin
          Builder.WriteCodeString('todo'); //pop hl, ld l,(hl), ld h,0, push hl
        end;
    else
      Builder.Fail('Unsupported misc instruction: ' + C.ClassName);
    end;
  end else if (C is TExpReturn) or (C is TExpStackFrame) then
  begin
    if (C is TExpStackFrame) then
      Inc(Self.VarSize,TExpStackFrame(C).Size*2);
    //ignore these
  end else if (C is TExpSwitchTable) then
  begin
    Assert(False);
//    Switch := (C as TExpSwitchTable);
//    if (Switch.LowBound<0) or (Switch.HighBound>128) then
//      Builder.Fail('case statements must be in range 0-128');
//
//    Builder.WriteCodeString('e1'); //pop hl
//
//    if Switch.LowBound<>0 then
//    begin
//      Builder.WriteCode([$3e,Switch.LowBound,$bd,$da]); //ld a,lowbound, cp l, jp c,nn
//      Builder.AddFixup(Switch.DefaultOrExit);
//    end;
//
//    Builder.WriteCode([$3e,Switch.HighBound,$bd,$fa]); //ld a,highbound, cp l, jp m,nn
//    Builder.AddFixup(Switch.DefaultOrExit);
//
//    Builder.WriteCode([$cd]); //call
//    Builder.AddRtlFixup(rrtSwitch);
//
//    //emit jumptable
//    for I := 0 to Switch.HighBound-Switch.LowBound do
//      Builder.AddFixup(PIntegerArray(Switch.Jumps.Data)^[I]);

  end else
    Builder.Fail('Unsupported instruction: ' + C.ClassName);
end;

procedure TCpu6809.GenBitmap(Bitmap: TZBitmap);
begin

end;

procedure TCpu6809.GenFile(Ms: TMemoryStream; var FileName: string);
begin
  case Target of
    rtaVectrex:
      begin
        FileName := ChangeFileExt(FileName,'.rom');
        Ms.CopyFrom(Builder.Code,Builder.Code.Size);
      end;
  end;
end;

procedure TCpu6809.GenLoadInteger(Value: integer);
begin
  Builder.WriteCode([$cc]);  //ldd #
  Builder.WriteCodeAddress(Value);
  Builder.WriteCodeString('3406');  //pshs d
end;

procedure TCpu6809.GenLoadResource(C: TZComponent);
begin
  Builder.WriteCode([$cc]);  //ldd #
  Builder.AddResourceFixup( C );
  Builder.WriteCodeString('3406');  //pshd d
end;

procedure TCpu6809.GenAddress(const Address: integer; Ms: TMemoryStream);
var
  W : word;
  B : byte;
begin
  W := Address;
  B := Hi(W);
  Ms.Write(B,1);
  B := Lo(W);
  Ms.Write(B,1);
end;

procedure TCpu6809.GenArrayGetElement(C: TZComponent);
begin
  Builder.WriteCode([$cc]);  //ldd #
  Builder.AddResourceFixup( C );
  Builder.WriteCodeString('e3e4 ede4'); //addd ,s  std ,s
end;

procedure TCpu6809.GenRtl(Routine: TRtlRoutine);
begin
  raise Exception.Create('Error Message');
end;

procedure TCpu6809.PeepholeWrite(const Buf: array of byte);
var
  LastOpCode1, LastOpCode2 : byte;
  Ms : TMemoryStream;
begin
  Ms := Builder.Code;

  LastOpCode1 := 0;
  LastOpCode2 := 0;
  if Builder.Code.Position>1 then
  begin
    LastOpCode1 := (PByte(Ms.Memory)+Ms.Position-2)^;
    LastOpCode2 := (PByte(Ms.Memory)+Ms.Position-1)^;
  end;

  if (LastOpCode1=$34) and (LastOpCode2=$06) and (Length(Buf)>1) then
  begin
    if ((Buf[0]=$35) and (Buf[1]=$06)) or
      ((Buf[0]=$ec) and (Buf[1]=$e1))  then
    begin //Remove pair: pshs d, puls d (ldd ,s++)
      Inc(RedundantLoadsRemoved);
      Ms.Position := Ms.Position-2;
      Ms.Write(Buf[2],Length(Buf)-2);
      Exit;
    end;
  end;

  Ms.Write(Buf[0],Length(Buf));
end;

end.
