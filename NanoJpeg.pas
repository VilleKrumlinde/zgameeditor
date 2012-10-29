{Copyright (c) 2012 Ville Krumlinde

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

unit NanoJpeg;

interface

{.$define USE_CHROMA_FILTER}


type
  //Original code by Martin Fiedler http://keyj.s2000.ws/?p=137
  //Delphi port by Ville Krumlinde
  TNjDecoder = class
  strict private
    type ErrorCodes = (
            OK = 0,        // decoding successful
            NotAJpeg,      // not a JPEG file
            Unsupported,   // unsupported format
            OutOfMemory,   // out of memory
            InternalError, // internal error
            SyntaxError,   // syntax error
            Internal_Finished); // used internally, will never be reported
    type VlcCode = packed record
      bits,code : byte;
    end;
    type PVlcCode = ^VlcCode;
    type Component = record
      cid,
      ssx, ssy,
      width, height,
      stride,
      qtsel,
      actabsel, dctabsel,
      dcpred : integer;
      pixels : PByte;
    end;
    type PComponent = ^Component;
    type Context = record
      error : ErrorCodes;
      pos : PByte;
      size,
      length,
      width, height,
      mbwidth, mbheight,
      mbsizex, mbsizey,
      ncomp : integer;
      comp : array[0..2] of Component;
      qtused, qtavail : integer;
      qtab : array[0..3,0..63] of byte;
      vlctab : array[0..3,0..65535] of VlcCode;
      buf, bufbits : integer;
      block : array[0..63] of integer;
      rstinterval : integer;
      rgb : PByte;
    end;
    var
      ctx : Context;
    const
      ZZ : array[0..63] of byte = (0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18,
        11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35,
        42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
        38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63);
      W1 = 2841;
      W2 = 2676;
      W3 = 2408;
      W5 = 1609;
      W6 = 1108;
      W7 = 565;
    procedure RowIDCT(blk : PInteger);
    procedure ColIDCT(blk : PInteger; pout : PByte; stride : integer);
    function ShowBits(bits : integer) : integer;
    procedure ByteAlign;
    function Decode16(pos: PByte): word;
    procedure DecodeLength;
    function GetBits(bits: integer): integer;
    procedure njThrow(const E: ErrorCodes);
    procedure Skip(count: integer);
    procedure SkipBits(bits: integer);
    procedure SkipMarker;
    procedure DecodeSOF;
    procedure DecodeDHT;
    procedure DecodeBlock(c: PComponent; pout: PByte);
    procedure DecodeDQT;
    procedure DecodeDRI;
    function GetVLC(vlc: PVlcCode; code: PByte): integer;
    procedure DecodeScan;
    {$ifdef USE_CHROMA_FILTER}
    procedure UpsampleV(c: PComponent);
    procedure UpsampleH(c: PComponent);
    {$else}
    procedure Upsample(c: PComponent);
    {$endif}
    procedure Convert;
  private
    class function Clip(const X : integer) : byte;
  public
    function Decode(jpeg: pointer; const size: integer): boolean;
    destructor Destroy; override;
    function GetWidth : integer;
    function GetHeight : integer;
    function GetImage : pointer;
  end;

implementation

{$POINTERMATH ON}

//Delphi shr does not work with negative values, use this instead
function cshr(const i : integer; const j : byte) : integer;
{$if defined(CPU386)}
asm
  mov cl,j
  sar eax,cl
end;
{$elseif defined(CPUX64)}
asm
  mov eax,i
  mov cl,j
  sar eax,cl
end;
{$elseif defined(Android)}
asm
  asr r0,r0,r1
end;
{$ifend}

{ TNjDecoder }

class function TNjDecoder.Clip(const X: integer): byte;
begin
  if X<0 then
    Result := 0
  else if X>$FF then
    Result := $FF
  else
    Result := X;
end;

procedure TNjDecoder.RowIDCT(blk: PInteger);
var
  x0, x1, x2, x3, x4, x5, x6, x7, x8 : integer;
begin
  x1 := blk[4] shl 11;
  x2 := blk[6];
  x3 := blk[2];
  x4 := blk[1];
  x5 := blk[7];
  x6 := blk[5];
  x7 := blk[3];

  if (x1 or x2 or x3 or x4 or x5 or x6 or x7) = 0 then
  begin
    blk[0] := blk[0] shl 3;
    blk[1] := blk[0];
    blk[2] := blk[0];
    blk[3] := blk[0];
    blk[4] := blk[0];
    blk[5] := blk[0];
    blk[6] := blk[0];
    blk[7] := blk[0];
    Exit;
  end;

  x0 := (blk[0] shl 11) + 128;
  x8 := W7 * (x4 + x5);
  x4 := x8 + (W1 - W7) * x4;
  x5 := x8 - (W1 + W7) * x5;
  x8 := W3 * (x6 + x7);
  x6 := x8 - (W3 - W5) * x6;
  x7 := x8 - (W3 + W5) * x7;
  x8 := x0 + x1;
  x0 := x0 - x1;
  x1 := W6 * (x3 + x2);
  x2 := x1 - (W2 + W6) * x2;
  x3 := x1 + (W2 - W6) * x3;
  x1 := x4 + x6;
  x4 := x4 - x6;
  x6 := x5 + x7;
  x5 := x5 - x7;
  x7 := x8 + x3;
  x8 := x8 - x3;
  x3 := x0 + x2;
  x0 := x0 - x2;
  x2 := cshr((181 * (x4 + x5) + 128), 8);
  x4 := cshr((181 * (x4 - x5) + 128), 8);
  blk[0] := cshr((x7 + x1), 8);
  blk[1] := cshr((x3 + x2), 8);
  blk[2] := cshr((x0 + x4), 8);
  blk[3] := cshr((x8 + x6), 8);
  blk[4] := cshr((x8 - x6), 8);
  blk[5] := cshr((x0 - x4), 8);
  blk[6] := cshr((x3 - x2), 8);
  blk[7] := cshr((x7 - x1), 8);
end;

procedure TNjDecoder.ColIDCT(blk : PInteger; pout : PByte; stride : integer);
var
  x0, x1, x2, x3, x4, x5, x6, x7, x8 : integer;
begin
  x1 := blk[8*4] shl 8;
  x2 := blk[8*6];
  x3 := blk[8*2];
  x4 := blk[8*1];
  x5 := blk[8*7];
  x6 := blk[8*5];
  x7 := blk[8*3];

  if (x1 or x2 or x3 or x4 or x5 or x6 or x7) = 0 then
  begin
    x1 := Clip( cshr((blk[0] + 32), 6) + 128);
    for x0 := 8 downto 1 do
    begin
      pout^ := x1;
      Inc(pout,stride);
    end;
    Exit;
  end;

  x0 := (blk[0] shl 8) + 8192;
  x8 := W7 * (x4 + x5) + 4;
  x4 := cshr((x8 + (W1 - W7) * x4), 3);
  x5 := cshr((x8 - (W1 + W7) * x5), 3);
  x8 := W3 * (x6 + x7) + 4;
  x6 := cshr((x8 - (W3 - W5) * x6), 3);
  x7 := cshr((x8 - (W3 + W5) * x7), 3);
  x8 := x0 + x1;
  x0 := x0 - x1;
  x1 := W6 * (x3 + x2) + 4;
  x2 := cshr((x1 - (W2 + W6) * x2), 3);
  x3 := cshr((x1 + (W2 - W6) * x3), 3);
  x1 := x4 + x6;
  x4 := x4 - x6;
  x6 := x5 + x7;
  x5 := x5 - x7;
  x7 := x8 + x3;
  x8 := x8 - x3;
  x3 := x0 + x2;
  x0 := x0 - x2;
  x2 := cshr((181 * (x4 + x5) + 128), 8);
  x4 := cshr((181 * (x4 - x5) + 128), 8);
  pout^ := Clip((cshr((x7 + x1), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x3 + x2), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x0 + x4), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x8 + x6), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x8 - x6), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x0 - x4), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x3 - x2), 14)) + 128); Inc(pout,stride);
  pout^ := Clip((cshr((x7 - x1), 14)) + 128);
end;

function TNjDecoder.ShowBits(bits : integer) : integer;
var
  newbyte,marker : byte;
begin
  if Bits=0 then
    Exit(0);

  while (ctx.bufbits < bits) do
  begin
    if (ctx.size <= 0) then
    begin
      ctx.buf := (ctx.buf shl 8) or $FF;
      ctx.bufbits := ctx.bufbits + 8;
      continue;
    end;
    newbyte := ctx.pos^; Inc(ctx.pos);
    Dec(ctx.size);
    ctx.bufbits := ctx.bufbits + 8;
    ctx.buf := (ctx.buf shl 8) or newbyte;
    if (newbyte = $FF) then
    begin
      if (ctx.size<>0) then
      begin
        marker := ctx.pos^; Inc(ctx.pos);
        Dec(ctx.size);
        case marker of
          0 : ;
          $D9 : ctx.Size:=0;
        else
          begin
            if (marker and $f8)<>$d0 then
              ctx.error := SyntaxError
            else
            begin
              ctx.buf := (ctx.buf shl 8) or marker;
              ctx.bufbits := ctx.bufbits + 8;
            end;
          end;
        end;
      end else
        ctx.error := SyntaxError;
    end;
  end;

  Result := (ctx.buf shr (ctx.bufbits - bits)) and ((1 shl bits) - 1);
end;

procedure TNjDecoder.SkipBits(bits : integer);
begin
  if (ctx.bufbits < bits) then
    ShowBits(bits);
  ctx.bufbits := ctx.bufbits-bits;
end;

function TNjDecoder.GetBits(bits : integer) : integer;
begin
  Result := ShowBits(bits);
  SkipBits(bits);
end;

function TNjDecoder.GetHeight: integer;
begin
  Result := Ctx.height;
end;

function TNjDecoder.GetImage: pointer;
begin
  if ctx.ncomp = 1 then
    Result := ctx.comp[0].pixels
  else
    Result := ctx.rgb;
end;

procedure TNjDecoder.ByteAlign;
begin
  ctx.bufbits := ctx.bufbits and $F8;
end;

procedure TNjDecoder.Skip(count : integer);
begin
  ctx.pos := ctx.pos + count;
  ctx.size := ctx.size - count;
  ctx.length := ctx.length - count;
  if (ctx.size < 0) then
    ctx.error := SyntaxError;
end;

function TNjDecoder.Decode16(pos : PByte) : word;
begin
  Result := (pos[0] shl 8) or pos[1];
end;

procedure TNjDecoder.njThrow(const E : ErrorCodes);
begin
  ctx.Error := E;
end;

procedure TNjDecoder.DecodeLength;
begin
  if (ctx.size < 2) then
    njThrow(SyntaxError);
  ctx.length := Decode16(ctx.pos);
  if (ctx.length > ctx.size) then
    njThrow(SyntaxError);
  Skip(2);
end;

procedure TNjDecoder.SkipMarker;
begin
  DecodeLength();
  Skip(ctx.length);
end;

procedure TNjDecoder.DecodeSOF;
var
  i, ssxmax, ssymax : integer;
  c : ^Component;
begin
  ssxmax := 0; ssymax :=0;
  DecodeLength();
  if (ctx.length < 9) then
    njThrow(SyntaxError);
  if (ctx.pos[0] <> 8) then
    njThrow(Unsupported);
  ctx.height := Decode16(ctx.pos+1);
  ctx.width := Decode16(ctx.pos+3);
  ctx.ncomp := ctx.pos[5];
  Skip(6);
  case (ctx.ncomp) of
    1,3 : ;
  else
    njThrow(Unsupported);
  end;

  if (ctx.length < (ctx.ncomp * 3)) then
    njThrow(SyntaxError);

  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    c.cid := ctx.pos[0];

    c.ssx := ctx.pos[1] shr 4;
    if c.ssx=0 then
      njThrow(SyntaxError);

    if (c.ssx and (c.ssx - 1))<>0 then
      njThrow(Unsupported);  // non-power of two

    c.ssy := ctx.pos[1] and 15;
    if c.ssy=0 then
      njThrow(SyntaxError);

    if (c.ssy and (c.ssy - 1))<>0 then
      njThrow(Unsupported);  // non-power of two

    c.qtsel := ctx.pos[2];
    if ((c.qtsel) and $FC)<>0 then
      njThrow(SyntaxError);

    Skip(3);
    ctx.qtused := ctx.qtused or (1 shl c.qtsel);
    if (c.ssx > ssxmax) then ssxmax := c.ssx;
    if (c.ssy > ssymax) then ssymax := c.ssy;
    Inc(C);
  end;
  ctx.mbsizex := ssxmax shl 3;
  ctx.mbsizey := ssymax shl 3;
  ctx.mbwidth := (ctx.width + ctx.mbsizex - 1) div ctx.mbsizex;
  ctx.mbheight := (ctx.height + ctx.mbsizey - 1) div ctx.mbsizey;

  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    c.width := (ctx.width * c.ssx + ssxmax - 1) div ssxmax;
    c.stride := (c.width + 7) and $7FFFFFF8;
    c.height := (ctx.height * c.ssy + ssymax - 1) div ssymax;
    c.stride := ctx.mbwidth * ctx.mbsizex * c.ssx div ssxmax;

    if (((c.width < 3) and (c.ssx <> ssxmax)) or ((c.height < 3) and (c.ssy <> ssymax))) then
      njThrow(Unsupported);

    GetMem(c.pixels,c.stride * (ctx.mbheight * ctx.mbsizey * c.ssy div ssymax));
    if c.pixels=nil then
      njThrow(OutOfMemory);

    Inc(C);
  end;
  if (ctx.ncomp = 3) then
  begin
    GetMem(ctx.rgb, ctx.width * ctx.height * ctx.ncomp);
    if ctx.rgb=nil then
      njThrow(OutOfMemory);
  end;
  Skip(ctx.length);
end;

destructor TNjDecoder.Destroy;
var
  i : integer;
begin
  for i := 0 to 3-1 do
    if (ctx.comp[i].pixels<>nil) then
      FreeMem(ctx.comp[i].pixels);
  if (ctx.rgb<>nil) then
    FreeMem(ctx.rgb);
  inherited;
end;

procedure TNjDecoder.DecodeDHT;
var
  codelen, currcnt, remain, spread, i, j : integer;
  vlc : PVlcCode;
  counts : array[0..15] of byte;
  code : byte;
begin
  DecodeLength();
  while (ctx.length >= 17) do
  begin
    i := ctx.pos[0];
    if (i and $EC)<>0 then njThrow(SyntaxError);
    if (i and $02)<>0 then njThrow(Unsupported);
    i := (i or (i shr 3)) and 3;  // combined DC/AC + tableid value

    for codelen := 1 to 16 do
      counts[codelen - 1] := ctx.pos[codelen];
    Skip(17);
    vlc := @ctx.vlctab[i][0];
    spread := 65536;
    remain := spread;
    for codelen := 1 to 16 do
    begin
      spread := spread div (1 shl 1);
      currcnt := counts[codelen - 1];
      if (currcnt=0) then
        Continue;
      if (ctx.length < currcnt) then
        njThrow(SyntaxError);
      remain := remain - (currcnt shl (16 - codelen));
      if (remain < 0) then
        njThrow(SyntaxError);
      for i := 0 to currcnt-1 do
      begin
        code := ctx.pos[i];
        for j := spread downto 1 do
        begin
          vlc.bits := codelen;
          vlc.code := code;
          Inc(vlc);
        end;
      end;
      Skip(currcnt);
    end;
    while (remain>0) do
    begin
      vlc.bits := 0;
      Inc(vlc);
      Dec(Remain);
    end;
  end;
  if (ctx.length<>0) then
    njThrow(SyntaxError);
end;

procedure TNjDecoder.DecodeDQT;
var
  i : integer;
  t : PByte;
begin
  DecodeLength();
  while (ctx.length >= 65) do
  begin
    i := ctx.pos[0];
    if (i and $FC)<>0 then
      njThrow(SyntaxError);
    ctx.qtavail := ctx.qtavail or (1 shl i);
    t := @ctx.qtab[i][0];
    for i := 0 to 64-1 do
      t[i] := ctx.pos[i + 1];
    Skip(65);
  end;
  if (ctx.length<>0) then
    njThrow(SyntaxError);
end;

procedure TNjDecoder.DecodeDRI;
begin
  DecodeLength();
  if (ctx.length < 2) then
    njThrow(SyntaxError);
  ctx.rstinterval := Decode16(ctx.pos);
  Skip(ctx.length);
end;

function TNjDecoder.GetVLC(vlc : PVlcCode; code : PByte) : integer;
var
  value,bits : integer;
begin
  value := ShowBits(16);
  bits := vlc[value].bits;
  if bits=0 then
  begin
    ctx.error := SyntaxError;
    Exit(0);
  end;
  SkipBits(bits);
  value := vlc[value].code;
  if (code<>nil) then
    code^ := value;
  bits := value and 15;
  if (bits=0) then
    Exit(0);
  value := GetBits(bits);
  if (value < (1 shl (bits - 1))) then
    value := value + (((-1) shl bits) + 1);
  Result := value;
end;

function TNjDecoder.GetWidth: integer;
begin
  Result := Ctx.width;
end;

procedure TNjDecoder.DecodeBlock(c : PComponent; pout : PByte);
var
  Code :  byte;
  Value, Coef : integer;
begin
  code := 0;
  coef := 0;
  FillChar(ctx.block, SizeOf(Ctx.Block), 0);
  c.dcpred := c.dcpred + GetVLC(@ctx.vlctab[c.dctabsel][0], nil);
  ctx.block[0] := (c.dcpred) * ctx.qtab[c.qtsel][0];
  repeat
    value := GetVLC(@ctx.vlctab[c.actabsel][0], @code);
    if (code=0) then
      break;  // EOB
    if ( ((code and $0F)=0) and (code <> $F0)) then
      njThrow(SyntaxError);
    coef := coef + (code shr 4) + 1;
    if (coef > 63) then
      njThrow(SyntaxError);
    ctx.block[ ZZ[coef] ] := value * ctx.qtab[c.qtsel][coef];
  until Coef>=63;
  coef := 0;
  while Coef<64 do
  begin
    RowIDCT(@ctx.block[coef]);
    Inc(Coef,8);
  end;
  for coef := 0 to 8-1 do
  begin
    ColIDCT(@ctx.block[coef], @pout[coef], c.stride);
  end;
end;

procedure TNjDecoder.DecodeScan;
var
  i, mbx, mby, sbx, sby, rstcount, nextrst : integer;
  c : PComponent;
begin
  rstcount := ctx.rstinterval;
  nextrst := 0;
  DecodeLength();
  if (ctx.length < (4 + 2 * ctx.ncomp)) then
    njThrow(SyntaxError);

  if (ctx.pos[0] <> ctx.ncomp) then
    njThrow(Unsupported);

  Skip(1);
  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    if (ctx.pos[0] <> c.cid) then
      njThrow(SyntaxError);
    if (ctx.pos[1] and $EE)<>0 then
      njThrow(SyntaxError);
    c.dctabsel := (ctx.pos[1] shr 4);
    c.actabsel := (ctx.pos[1] and 1) or 2;
    Skip(2);
    Inc(C);
  end;

  if ((ctx.pos[0]<>0) or (ctx.pos[1] <> 63) or (ctx.pos[2]<>0)) then
    njThrow(Unsupported);

  Skip(ctx.length);
  mbx := 0; mby := 0;
  while True do
  begin
    c := @ctx.comp;
    for i := 0 to ctx.ncomp-1 do
    begin
      for sby := 0 to c.ssy-1 do
        for sbx := 0 to c.ssx-1 do
        begin
          DecodeBlock(c, @c.pixels[((mby * c.ssy + sby) * c.stride + mbx * c.ssx + sbx) shl 3]);
          if ctx.error>OK then
            Exit;
        end;
      Inc(C);
    end;
    Inc(mbx);
    if (mbx >= ctx.mbwidth) then
    begin
      mbx := 0;
      Inc(mby);
      if mby >= ctx.mbheight then
        Break;
    end;
    Dec(rstcount);
    if (ctx.rstinterval<>0) and (rstcount=0) then
    begin
      ByteAlign();
      i := GetBits(16);
      if (((i and $FFF8) <> $FFD0) or ((i and 7) <> nextrst)) then
        njThrow(SyntaxError);
      nextrst := (nextrst + 1) and 7;
      rstcount := ctx.rstinterval;
      for i := 0 to 3-1 do
        ctx.comp[i].dcpred := 0;
    end;
  end;
  ctx.error := Internal_Finished;
end;

{$ifdef USE_CHROMA_FILTER}

const
  CF4A = (-9);
  CF4B = (111);
  CF4C = (29);
  CF4D = (-3);
  CF3A = (28);
  CF3B = (109);
  CF3C = (-9);
  CF3X = (104);
  CF3Y = (27);
  CF3Z = (-3);
  CF2A = (139);
  CF2B = (-11);

function CF(const X : integer) : integer;
begin
  Result := TNjDecoder.clip( cshr((x + 64), 7) );
end;

procedure TNjDecoder.UpsampleH(c : PComponent);
var
  xmax,x,y : integer;
  pout,lin,lout : PByte;
begin
  xmax := c.width - 3;
  GetMem(pout, (c.width * c.height) shl 1);
  if (pout=nil) then
    njThrow(OutOfMemory);
  lin := c.pixels;
  lout := pout;
  for y := c.height downto 1 do
  begin
    lout[0] := CF(CF2A * lin[0] + CF2B * lin[1]);
    lout[1] := CF(CF3X * lin[0] + CF3Y * lin[1] + CF3Z * lin[2]);
    lout[2] := CF(CF3A * lin[0] + CF3B * lin[1] + CF3C * lin[2]);
    for x := 0 to xmax-1 do
    begin
      lout[(x shl 1) + 3] := CF(CF4A * lin[x] + CF4B * lin[x + 1] + CF4C * lin[x + 2] + CF4D * lin[x + 3]);
      lout[(x shl 1) + 4] := CF(CF4D * lin[x] + CF4C * lin[x + 1] + CF4B * lin[x + 2] + CF4A * lin[x + 3]);
    end;
    lin := lin + c.stride;
    lout := lout + (c.width shl 1);
    lout[-3] := CF(CF3A * lin[-1] + CF3B * lin[-2] + CF3C * lin[-3]);
    lout[-2] := CF(CF3X * lin[-1] + CF3Y * lin[-2] + CF3Z * lin[-3]);
    lout[-1] := CF(CF2A * lin[-1] + CF2B * lin[-2]);
  end;
  c.width := c.width shl 1;
  c.stride := c.width;
  FreeMem(c.pixels);
  c.pixels := pout;
end;

procedure TNjDecoder.UpsampleV(c : PComponent);
var
  w,s1,s2,x,y : integer;
  pout,cin,cout : PByte;
begin
  w := c.width;
  s1 := c.stride;
  s2 := s1 + s1;
  GetMem(pout,(c.width * c.height) shl 1);
  if (pout=nil) then
    njThrow(OutOfMemory);
  for x := 0 to w-1 do
  begin
    cin := @c.pixels[x];
    cout := @pout[x];
    cout^ := CF(CF2A * cin[0] + CF2B * cin[s1]); cout := cout + w;
    cout^ := CF(CF3X * cin[0] + CF3Y * cin[s1] + CF3Z * cin[s2]); cout := cout + w;
    cout^ := CF(CF3A * cin[0] + CF3B * cin[s1] + CF3C * cin[s2]); cout := cout + w;
    cin := cin + s1;
    for y := c.height - 3 downto 1 do
    begin
      cout^ := CF(CF4A * cin[-s1] + CF4B * cin[0] + CF4C * cin[s1] + CF4D * cin[s2]); cout := cout + w;
      cout^ := CF(CF4D * cin[-s1] + CF4C * cin[0] + CF4B * cin[s1] + CF4A * cin[s2]); cout := cout + w;
      cin := cin + s1;
    end;
    cin := cin + s1;
    cout^ := CF(CF3A * cin[0] + CF3B * cin[-s1] + CF3C * cin[-s2]); cout := cout + w;
    cout^ := CF(CF3X * cin[0] + CF3Y * cin[-s1] + CF3Z * cin[-s2]); cout := cout + w;
    cout^ := CF(CF2A * cin[0] + CF2B * cin[-s1]);
  end;
  c.height := c.height shl 1;
  c.stride := c.width;
  FreeMem(c.pixels);
  c.pixels := pout;
end;

{$else}

procedure TNjDecoder.Upsample(c : PComponent);
var
  x, y, xshift, yshift : integer;
  pout, lin, lout : PByte;
begin
  xshift := 0; yshift := 0;

  while (c.width < ctx.width) do
  begin
    c.width := c.width shl 1;
    Inc(xshift);
  end;

  while (c.height < ctx.height) do
  begin
    c.height := c.height shl 1;
    Inc(yshift);
  end;

  GetMem(pout, c.width * c.height);
  if pout=nil then
    njThrow(OutOfMemory);
  lout := pout;
  for y := 0 to c.height-1 do
  begin
    lin := @c.pixels[(y shr yshift) * c.stride];
    for x := 0 to c.width-1 do
      lout[x] := lin[(x shr xshift)];
    lout := lout + c.width;
  end;
  c.stride := c.width;
  FreeMem(c.pixels);
  c.pixels := pout;
end;

{$endif}

procedure TNjDecoder.Convert;
var
  i : integer;
  c : PComponent;
  x, yy : integer;
  prgb,py,pcb,pcr,pin,pout : PByte;
  y,cb,cr : integer;
begin
  c := @ctx.comp;
  for i := 0 to ctx.ncomp-1 do
  begin
    {$ifdef USE_CHROMA_FILTER}
    while (c.width < ctx.width) or (c.height < ctx.height) do
    begin
      if (c.width < ctx.width) then UpsampleH(c);
//      if (ctx.error>OK) then Exit;
      if (c.height < ctx.height) then UpsampleV(c);
//      if (ctx.error>OK) then Exit;
    end;
    {$else}
    if ((c.width < ctx.width) or (c.height < ctx.height)) then
      Upsample(c);
    {$endif}

    if ((c.width < ctx.width) or (c.height < ctx.height)) then
      njThrow(InternalError);
    Inc(c);
  end;
  if (ctx.ncomp = 3) then
  begin
    // convert to RGB (and flip vertical)
    prgb := ctx.rgb;
    py  := ctx.comp[0].pixels + (ctx.height-1)*ctx.comp[0].stride;
    pcb := ctx.comp[1].pixels + (ctx.height-1)*ctx.comp[1].stride;
    pcr := ctx.comp[2].pixels + (ctx.height-1)*ctx.comp[2].stride;
    for yy := ctx.height downto 1 do
    begin
      for x := 0 to ctx.width-1 do
      begin
        y := py[x] shl 8;
        cb := pcb[x] - 128;
        cr := pcr[x] - 128;
        prgb^ := Clip(cshr((y            + 359 * cr + 128), 8)); Inc(prgb);
        prgb^ := Clip(cshr((y -  88 * cb - 183 * cr + 128), 8)); Inc(prgb);
        prgb^ := Clip(cshr((y + 454 * cb            + 128), 8)); Inc(prgb);
      end;
      py := py - ctx.comp[0].stride;
      pcb := pcb - ctx.comp[1].stride;
      pcr := pcr - ctx.comp[2].stride;
    end;
  end else if (ctx.comp[0].width <> ctx.comp[0].stride) then
  begin
    // grayscale -> only remove stride
    pin := @ctx.comp[0].pixels[ctx.comp[0].stride];
    pout := @ctx.comp[0].pixels[ctx.comp[0].width];
    for y := ctx.comp[0].height - 1 downto 1 do
    begin
      Move(pout^, pin^, ctx.comp[0].width);
      Inc(pin,ctx.comp[0].stride);
      Inc(pout,ctx.comp[0].width);
    end;
    ctx.comp[0].stride := ctx.comp[0].width;
  end;
end;

function TNjDecoder.Decode(jpeg : pointer; const size : integer) : boolean;
begin
  ctx.pos := jpeg;
  ctx.size := size and $7FFFFFFF;

  if (ctx.size < 2) then
    Exit(False);//return NJ_NO_JPEG;

  if (((ctx.pos[0] xor $FF)<>0) or ((ctx.pos[1] xor $D8)<>0)) then
    Exit(False);//return NJ_NO_JPEG;

  Skip(2);
  while ctx.error=OK do
  begin
    if ((ctx.size < 2) or (ctx.pos[0] <> $FF)) then
      Exit(False);//return NJ_SYNTAX_ERROR;
    Skip(2);
    case (ctx.pos[-1]) of
      $C0: DecodeSOF();
      $C4: DecodeDHT();
      $DB: DecodeDQT();
      $DD: DecodeDRI();
      $DA: DecodeScan();
      $FE: SkipMarker();
    else
      begin
        if ((ctx.pos[-1] and $F0) = $E0) then
          SkipMarker()
        else
          Exit(False);//return NJ_UNSUPPORTED;
      end;
    end;
  end;
  if (ctx.error<>Internal_Finished) then
    Exit(False);
  Convert();
  Result := True;
end;

end.
