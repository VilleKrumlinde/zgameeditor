//http://coding.derkeiler.com/Archive/Delphi/borland.public.delphi.non-technical/2004-10/4402.html
//Anpassningar från: http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=41
{
  Något är knas:
    PNG fungerar ej, ger oleerror
    GIF och JPG läser in filer fast i fel storlek (256x256 blir 271x271)
}

unit uTinyGif;

interface

uses windows, classes, sysutils, graphics, axctrls;

type
  TTinyGifImage = class (TOleGraphic)
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
  public
    procedure LoadFromFile(const Filename: string); override;
  end;

implementation

resourcestring
  sGIFImageFile	= 'GIF Image';

{ TTinyGifImage }
function TTinyGifImage.GetWidth: Integer;
var
  hdcTemp : HDC;
  lWidthPixels,lWidth : integer;
begin
  Self.Picture.get_Width(lWidth);
  hdcTemp := CreateCompatibleDC(GetDC(0));
  lWidthPixels := MulDiv(lWidth, GetDeviceCaps(hdcTemp, LOGPIXELSX), 2540);
  Result := lWidthPixels;
  ReleaseDC(0,hdcTemp);
end;

function TTinyGifImage.GetHeight: Integer;
var
  hdcTemp : HDC;
  lHeightPixels,lHeight : integer;
begin
  Self.Picture.get_Height(lHeight);
  hdcTemp := CreateCompatibleDC(GetDC(0));
  lHeightPixels	:= MulDiv(lHeight, GetDeviceCaps(hdcTemp, LOGPIXELSY), 2540);
  Result := lHeightPixels;
  ReleaseDC(0,hdcTemp);
end;

procedure TTinyGifImage.LoadFromFile(const Filename: string);
var
  s : TFileStream;
begin
  s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(s)
  finally
    s.Free
  end
end;
initialization
  TPicture.RegisterFileFormat('GIF', sGIFImageFile, TTinyGIFImage);
//  TPicture.RegisterFileFormat('JPG', 'JPEG Image', TTinyGIFImage);
//  TPicture.RegisterFileFormat('PNG', 'PNG Image', TTinyGIFImage);
finalization
  TPicture.UnregisterGraphicClass(TTinyGIFImage);
end.
