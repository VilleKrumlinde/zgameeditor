{
  Convert opengl-header definitions to ZGE scripting based syntax.

  To use: Download xml-files from https://bitbucket.org/alfonse/gl-xml-specs/overview
  and save them in Input-folder.
}

unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses LibXmlParser, Generics.Collections;

{$R *.dfm}

type
  TGLEnum = record
    Name,Value : ansistring;
  end;

  TGLParam = record
    Name,Typ : ansistring;
  end;

  TGLFunc = class
    Name,Return : ansistring;
    Params : TList<TGLParam>;
    constructor Create;
    destructor Destroy; override;
  end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Xml : TXmlParser;
  Dst,BadTypes : TList<ansistring>;
  Enums : TList<TGLEnum>;
  Funcs : TObjectList<TGLFunc>;
  TypeMap : TDictionary<ansistring,ansistring>;
  ZgeTypeMap : TDictionary<ansistring,ansistring>;

  procedure ReadTypeMap;
  begin
    while Xml.Scan do
      case Xml.CurPartType of
        ptStartTag,ptEmptyTag :
          if Xml.CurName='type-def' then
            TypeMap.Add(Xml.CurAttr.Value('typename'),Xml.CurAttr.Value('C-lang'));
        ptEndTag :
          if Xml.CurName='typemap' then
            Break;
      end;
  end;

  procedure ReadEnums;
  var
    Enum : TGLEnum;
  begin
    while Xml.Scan do
      case Xml.CurPartType of
        ptStartTag,ptEmptyTag :
          if Xml.CurName='enum' then
          begin
            Enum.Name := Xml.CurAttr.Value('name');
            if Xml.CurAttr.Node('value')=nil then
              Enum.Value := 'GL_' + Xml.CurAttr.Value('ref')
            else
              Enum.Value := Xml.CurAttr.Value('value');
            Enums.Add(Enum);
          end;
        ptEndTag :
          if Xml.CurName='enumerations' then
            Break;
      end;
  end;

  function ToZgeType(var S : ansistring) : boolean;
  var
    Value : ansistring;
  begin
    Result := ZgeTypeMap.TryGetValue(S,Value);
    if not Result then
    begin
      if not BadTypes.Contains(S) then
        BadTypes.Add(S);
    end
    else
      S := Value;
  end;

  procedure ReadFuncs;
  var
    F : TGLFunc;
    P : TGLParam;
    BadType : boolean;
  begin
    while Xml.Scan do
      case Xml.CurPartType of
        ptStartTag,ptEmptyTag :
          if Xml.CurName='function' then
          begin
            F := TGLFunc.Create;
            F.Name := Xml.CurAttr.Value('name');
            F.Return := Xml.CurAttr.Value('return');
            BadType := False;
            if F.Return<>'void' then
            begin
              F.Return := TypeMap[F.Return];
              if not ToZgeType(F.Return) then
                BadType := True;
            end;

            if Xml.CurPartType=ptStartTag then
              while Xml.Scan do
              begin
                if (Xml.CurPartType=ptEmptyTag) and (Xml.CurName='param') then
                begin
                  P.Name := Xml.CurAttr.Value('name');
                  if (P.Name='string') or (P.Name='ref') then
                    P.Name := '_' + P.Name;
                  P.Typ := TypeMap[ Xml.CurAttr.Value('type') ];
                  if Xml.CurAttr.Value('kind')='array' then
                  begin
                    ToZgeType(P.Typ);
                    {if P.Typ='int' then
                      P.Typ := 'ref int'
                    else }if (P.Typ='string') or (P.Typ='ref string') then
                    else
                      P.Typ := 'xptr';
                  end
                  else if not ToZgeType(P.Typ) then
                    BadType := True;
                  F.Params.Add(P);
                end;
                if (Xml.CurPartType=ptEndTag) and (Xml.CurName='function') then
                  Break;
              end;

            if not BadType then
              Funcs.Add(F)
            else
              F.Free;
        end;
        ptEndTag :
          if Xml.CurName='function-defs' then
            Break;
      end;
  end;

var
  Enum : TGLEnum;
  Func : TGLFunc;
  Param : TGLParam;
  S : ansistring;
begin
  TypeMap := TDictionary<ansistring,ansistring>.Create;
  Enums := TList<TGLEnum>.Create;
  Funcs := TObjectList<TGLFunc>.Create;
  BadTypes := TList<ansistring>.Create;

  ZgeTypeMap := TDictionary<ansistring,ansistring>.Create;
  ZgeTypeMap.Add('GLenum','int');
  ZgeTypeMap.Add('GLuint','int');
  ZgeTypeMap.Add('GLboolean','int');
  ZgeTypeMap.Add('GLbyte','int');
  ZgeTypeMap.Add('GLint','int');
  ZgeTypeMap.Add('GLsizei','int');
  ZgeTypeMap.Add('GLsizeiptr','int');
  ZgeTypeMap.Add('GLintptr','int');
  ZgeTypeMap.Add('GLfloat','float');
  ZgeTypeMap.Add('GLchar','string');
  ZgeTypeMap.Add('GLchar*','ref string');
  ZgeTypeMap.Add('GLcharARB','string');
  ZgeTypeMap.Add('GLcharARB*','ref string');
  ZgeTypeMap.Add('GLbitfield','int');


  Xml := TXmlParser.Create;
  try
    Xml.LoadFromFile( AnsiString(ExtractFilePath(Application.ExeName) + 'input\opengl.xml') );
    while Xml.Scan do
      case Xml.CurPartType of
        ptStartTag :
          if Xml.CurName='typemap' then
            ReadTypemap
          else if Xml.CurName='enumerations' then
            ReadEnums
          else if Xml.CurName='function-defs' then
            ReadFuncs;
      end;
  finally
    Xml.Free;
  end;

  Dst := TList<ansistring>.Create;

  Dst.Add('const int');
  for Enum in Enums do
    if Length(Enum.Value)<11 then
      Dst.Add( '  GL_' + Enum.Name + ' = ' + Enum.Value + ',' );
  S := Dst[Dst.Count-1];
  S := Copy(S,1,Length(S)-1) + ';';
  Dst[Dst.Count-1] := S;
  Dst.Add('');

  for Func in Funcs do
  begin
    S := Func.Return + ' gl' + Func.Name + '(';
    for Param in Func.Params do
      S := S + Param.Typ + ' ' + Param.Name + ',';
    if Func.Params.Count>0 then
      Delete(S,Length(S),1);
    S := S + ') { }';
    Dst.Add(S);
  end;

  Memo1.Lines.BeginUpdate;
  Memo1.Lines.Clear;
  for S in Dst do
    Memo1.Lines.Add(string(S));
  Memo1.Lines.EndUpdate;

  Dst.Free;

  for S in BadTypes do
    OutputDebugString(PChar(string(S)));

  BadTypes.Free;
  ZgeTypeMap.Free;
  TypeMap.Free;
  Enums.Free;
  Funcs.Free;
end;




{ TGLFunc }

constructor TGLFunc.Create;
begin
  Params := TList<TGLParam>.Create;
end;

destructor TGLFunc.Destroy;
begin
  Params.Free;
  inherited;
end;

end.
