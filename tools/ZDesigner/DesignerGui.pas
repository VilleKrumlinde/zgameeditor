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

unit DesignerGui;

interface

uses Controls,Classes,ExtCtrls,ZClasses,ComCtrls,Contnrs,Forms,Menus,Graphics,Windows;

type
  TPropValueChangedEvent = procedure of object;

  TZPropertyEditor =  class(TScrollBox)
  private
    C : TZComponent;
    LastWidth : integer;
    procedure RebuildGui;
    procedure OnPropEditValueChanged;
    procedure MyOnResize(Sender: TObject);
  public
    RootComponent : TZComponent;
    OnPropValueChanged : TPropValueChangedEvent;
    WantsFocus : TWinControl;
    procedure SetComponent(C : TZComponent);
    constructor Create(Owner : TComponent); override;
  end;

  TZComponentTreeNode = class(TTreeNode)
  public
    //En av dessa är satt beroende på typ av trädnod
    Prop : TZProperty;
    Component : TZComponent;
    ComponentList : TZComponentList;
    procedure RefreshNodeName;
  end;

  TZComponentTreeView = class(TTreeView)
  strict private
    RootComponent : TZComponent;
    procedure RebuildGui;
    procedure MyExpanded(Sender: TObject; Node: TTreeNode);
    procedure MyCustomAdvancedDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
       State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
  protected
    function CreateNode: TTreeNode; override;
    function GetPopupMenu: TPopupMenu; override;
    procedure CreateWnd; override;
    procedure ChangeScale(M: Integer; D: Integer); override;
  public
    ShowOpCodes : boolean;
    LockShowNode : TTreeNode;
    OnRecreate : TNotifyEvent;
    function AddNode(C : TZComponent; Parent : TTreenode; Index : integer = -1) : TTreeNode;
    procedure SetRootComponent(C : TZComponent);
    function FindNodeForComponentList(L: TZComponentList): TZComponentTreeNode;
    function FindNodeForComponent(C: TZComponent): TZComponentTreeNode;
    procedure RefreshNode(Node : TTreeNode; C : TZComponent);
    function ZSelected : TZComponentTreeNode;
    function SortSelections : TObjectList;
    constructor Create(AOwner: TComponent); override;
    property OnChange;
    property Items;
    property PopupMenu;
    property Images;
    property OnChanging;
  end;

procedure GetAllObjects(C : TZComponent; List : TObjectList);
procedure GetObjectNames(C : TZComponent; List : TStringList);
function HasReferers(Root, Target : TZComponent; Deep : boolean = True) : boolean;
procedure GetReferersTo(Root, Target : TZComponent; List : TObjectList);
function FindInstanceOf(C : TZComponent; Zc : TZComponentClass) : TZComponent;
function DesignerFormatFloat(V : single) : string;
function ZColorToColor(C : TZColorf) : TColor;
function ColorToZColor(C : TColor) : TZColorf;

implementation

uses StdCtrls,System.SysUtils,Math,Dialogs,frmEditor,Compiler,ZLog,ZBitmap,
  ExtDlgs,frmMemoEdit,uMidiFile,AudioComponents,AxCtrls,CommCtrl,
  frmRawAudioImportOptions,ZFile,BitmapProducers,
  frmArrayEdit, ZExpressions, Vcl.Imaging.Pngimage, ZApplication, u3dsFile, Meshes,
  Vcl.Imaging.Jpeg, Vcl.Themes, Vcl.Styles,ZMath;

type
  TZPropertyEditBase = class(TCustomPanel)
  private
    Component : TZComponent;
    Prop : TZProperty;
    ValuePanel : TPanel;
    OldValue,Value : TZPropertyValue;
    OnPropValueChanged : TPropValueChangedEvent;
    NamePanel : TPanel;
    PropEditor : TZPropertyEditor;
    IsReadOnlyProp : boolean;
    procedure SetProp(C : TZComponent; Prop : TZProperty); virtual;
    procedure UpdateProp;
    procedure OnFocusControl(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
  end;

  TZPropertyStringEdit = class(TZPropertyEditBase)
  strict private
    Edit : TEdit;
    procedure OnStoreValue(Sender : TObject);
    procedure OnMemoEdit(Sender: TObject);
  private
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
  end;

  TZPropertyFloatEdit = class(TZPropertyEditBase)
  private
    Edit : TEdit;
    procedure OnUDClick(Sender: TObject; Button: TUDBtnType);
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnEditChange(Sender : TObject);
    procedure OnEditExit(Sender: TObject);
    procedure StoreFloat(const S: string);
  end;

  TZPropertyColorEdit = class(TZPropertyEditBase)
  private
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnClick(Sender : TObject);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnPaint(Sender : TObject);
  end;

  TZPropertyComponentEdit = class(TZPropertyEditBase)
  private
    Cb : TComboBox;
    FocusButton : TButton;
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnDropDown(Sender : TObject);
    procedure OnChange(Sender : TObject);
    procedure DoFindComponent(Sender: TObject);
  end;

  TZPropertyIntegerEdit = class(TZPropertyEditBase)
  private
    Edit : TEdit;
    procedure OnUDClick(Sender: TObject; Button: TUDBtnType);
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnEditChange(Sender : TObject);
  end;

  TZPropertyFloatsEdit = class(TZPropertyEditBase)
  private
    FloatP : PFloatArray;
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnEditChange(Sender : TObject);
    procedure OnEditExit(Sender: TObject);
    procedure StoreFloat(Edit: TEdit);
  end;

  TZPropertyBooleanEdit = class(TZPropertyEditBase)
  private
    Check : TCheckBox;
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnEditChange(Sender : TObject);
  end;

  TZPropertyByteEdit = class(TZPropertyEditBase)
  private
    Cb : TComboBox;
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnCbDropDown(Sender : TObject);
    procedure OnChange(Sender : TObject);
  end;

  TZBinaryPropEdit = class(TZPropertyEditBase)
  private
    ClearButton : TButton;
    procedure DataChanged;
    procedure SetProp(C : TZComponent; Prop : TZProperty); override;
    procedure OnStoreValue(Sender : TObject);
    procedure GetPictureStream(const Filename: string; Stream: TMemoryStream);
    procedure OnClearValue(Sender: TObject);
  end;

//Utility, kanske flytta till zclasses
function ZColorToColor(C : TZColorf) : TColor;
var
  R,G,B : longint;
begin
  R := Round(255 * C.R);
  G := Round(255 * C.G);
  B := Round(255 * C.B);
  Result:=(B shl 16) or (G shl 8) or R;
end;

function ColorToZColor(C : TColor) : TZColorf;
begin
  Result.B := ((C shr 16) and 255) * (1.0/255);
  Result.G := ((C shr 8) and 255) * (1.0/255);
  Result.R := (C and 255) * (1.0/255);
  Result.A := 1;
end;


function DesignerFormatFloat(V : single) : string;
const F : array[1..FloatTextDecimals+2] of char = ('0','.','0','#','#','#');
begin
  Result := FormatFloat(F, V);
end;


{ TZPropertyEditor }

procedure TZPropertyEditor.SetComponent(C: TZComponent);
begin
  Self.C := C;
  RebuildGui;
end;

constructor TZPropertyEditor.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  Self.OnResize := MyOnResize;
  ParentBackground := True;
  Self.BorderStyle := bsNone;
end;

procedure TZPropertyEditor.MyOnResize(Sender : TObject);
begin
  if Visible and (Self.Width<>LastWidth) then
    RebuildGUI;
  LastWidth := Self.Width;
end;

procedure TZPropertyEditor.RebuildGui;
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  I : integer;
  PEditor : TZPropertyEditBase;
begin
  Hide;

  DestroyComponents;
  (Owner as TEditorForm).HideEditor;

  if C = nil then
    Exit;

  //Important: otherwise vertscrollbars appears that crash if clicked upon
  Self.DisableAutoRange;

  PEditor := nil;
  WantsFocus := nil;
  PropList := C.GetProperties;
  for I:=0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    if {Prop.NeverPersist or }Prop.ExcludeFromXml or Prop.HideInGui then
      Continue;
    case Prop.PropertyType of
      zptString,zptExpression : PEditor := TZPropertyStringEdit.Create(Self);
      zptFloat,zptScalar : PEditor := TZPropertyFloatEdit.Create(Self);
      zptColorf : PEditor := TZPropertyColorEdit.Create(Self);
      zptComponentRef : PEditor := TZPropertyComponentEdit.Create(Self);
      zptInteger : PEditor := TZPropertyIntegerEdit.Create(Self);
      zptRectf : PEditor := TZPropertyFloatsEdit.Create(Self);
      zptVector3f : PEditor := TZPropertyFloatsEdit.Create(Self);
      zptBoolean : PEditor := TZPropertyBooleanEdit.Create(Self);
      zptByte : PEditor := TZPropertyByteEdit.Create(Self);
      zptBinary : PEditor := TZBinaryPropEdit.Create(Self);
    else
      Continue;
    end;
    PEditor.IsReadOnlyProp := Prop.NeverPersist;// or Prop.IsReadOnly;
    PEditor.Height := 20;
    PEditor.Parent := Self;
    PEditor.SetProp(C,Prop);
    PEditor.Top := 10000;
    PEditor.Align := alTop;
    PEditor.OnPropValueChanged := Self.OnPropEditValueChanged;
    //Autofocus viktiga propertys
    //'Name' får focus
    //NeedRefreshNodeName causes focus-error if adding new rendermesh-component, skip for now
    if {(Prop.NeedRefreshNodeName) or }(Prop.PropertyType=zptExpression) then
      WantsFocus := PEditor;
  end;

  Self.EnableAutoRange;

  Show;
end;


procedure TZPropertyEditor.OnPropEditValueChanged;
begin
  //Skicka vidare event från propeditor till owner
  if Assigned(OnPropValueChanged) then
    OnPropValueChanged;
end;


{ TZPropertyEditBase }

constructor TZPropertyEditBase.Create(AOwner: TComponent);
begin
  inherited;
  Self.PropEditor := AOwner as TZPropertyEditor;
  Self.BorderStyle := bsNone;
  Self.BevelOuter := bvNone;
end;

destructor TZPropertyEditBase.Destroy;
begin
  inherited;
end;

procedure TZPropertyEditBase.OnFocusControl(Sender: TObject);
begin
  if (Sender is TEdit) and
   (TEdit(Sender).Owner is TZPropertyFloatsEdit) or (TEdit(Sender).Owner is TZPropertyFloatEdit) then
  begin
    (PropEditor.Owner as TEditorForm).ShowFloatEditor(Sender as TEdit,TZPropertyFloatEdit(TEdit(Sender).Owner).Prop.PropertyType=zptScalar);
  end
  else if (Sender is TEdit) and (TEdit(Sender).Tag=100) then
    (PropEditor.Owner as TEditorForm).ShowExprEditor(Sender as TEdit)
  else if (Sender is TEdit) and (TEdit(Sender).Tag=101) then
    (PropEditor.Owner as TEditorForm).ShowShaderEditor(Sender as TEdit)
  else
    (PropEditor.Owner as TEditorForm).HideEditor;
end;

procedure TZPropertyEditBase.SetFocus;
begin
  //Shifta fokus till den första kontrollen i värdepanelen
  if ValuePanel.ControlCount>0 then
    (ValuePanel.Controls[0] as TWinControl).SetFocus;
end;

procedure TZPropertyEditBase.SetProp(C: TZComponent; Prop: TZProperty);
begin
  Self.Component := C;
  Self.Prop := Prop;
//  Self.BevelOuter := bvNone;

  //Läs aktuellt värde
  C.GetProperty(Prop,Value);
  OldValue := Value;

  NamePanel := TPanel.Create(Self);
  NamePanel.Align := alLeft;
  NamePanel.BevelOuter := bvNone;
  NamePanel.Width := Math.Max(120, (Owner as TWinControl).Width div 3);
  NamePanel.Parent := Self;
  NamePanel.Alignment := taLeftJustify;

  ValuePanel := TPanel.Create(Self);
  ValuePanel.Align := alClient;
  ValuePanel.BevelOuter := bvNone;
  ValuePanel.Parent := Self;

{  NameLabel := TLabel.Create(Self);
  NameLabel.Align := alClient;
  NameLabel.Caption := Prop.Name;
  NameLabel.Parent := NamePanel;}
  NamePanel.Caption := ' ' + Prop.Name;
  NamePanel.Hint := Prop.Name;

  if IsReadOnlyProp then
    NamePanel.Font.Color := clGrayText
  else
  begin
    //Visa namn med bold ifall ej defaultvalue
    if not Prop.IsDefaultValue(Value) then
      NamePanel.Font.Style:=[fsBold];
  end;
end;

procedure TZPropertyEditBase.UpdateProp;
var
  IsName : boolean;
begin
  IsName := Prop.Name='Name';
  if IsName then
    (PropEditor.Owner as TEditorForm).ValidateNewName(String(OldValue.StringValue),String(Value.StringValue));
  //Skriver nytt värde till prop och notifierar event
  Component.SetProperty(Prop,Value);
  OnPropValueChanged;
  if Prop.NeedRefreshNodeName and
    ((PropEditor.Owner as TEditorForm).Tree.ZSelected<>nil) then
    (PropEditor.Owner as TEditorForm).Tree.ZSelected.RefreshNodeName;
  OldValue := Value;
  if not Prop.IsDefaultValue(Value) then
    NamePanel.Font.Style:=[fsBold]
  else
    NamePanel.Font.Style:=[];
end;

{ TZPropertyStringEdit }

procedure TZPropertyStringEdit.SetProp(C: TZComponent; Prop: TZProperty);
var
  B : TButton;
begin
  inherited;
  Edit := TEdit.Create(Self);
  Edit.Align := alClient;

  if Prop.PropertyType=zptExpression then
  begin
    Edit.Text := Value.ExpressionValue.Source;
    Edit.Tag:=100;
  end
  else
    Edit.Text := String( Value.StringValue );

  if  (Prop.Name='VertexShaderSource') or
    (Prop.Name='FragmentShaderSource') then
    Edit.Tag := 101;

  Edit.OnExit := OnStoreValue;
  Edit.OnEnter := OnFocusControl;
  Edit.Enabled := not IsReadOnlyProp;
  Edit.Parent := ValuePanel;

  if (Prop.Name='Text') or (Prop.Name='Comment') or (Prop.Name='StringValue') then
  begin
    B := TButton.Create(Self);
    B.Align := alRight;
    B.Left := 1000;
    B.Width := 20;
    B.Caption := '...';
    B.Hint := 'Edit multi-line text';
    B.OnClick := OnMemoEdit;
    B.Parent := ValuePanel;
    B.Enabled := not IsReadOnlyProp;
  end;

//  NamePanel.FocusControl := Edit;
  //Sätt hint-text till propname så att main kan få tag på rätt prop
  Edit.Hint := Prop.Name;
end;


procedure TZPropertyStringEdit.OnMemoEdit(Sender: TObject);
begin
  if MemoEditForm=nil then
    MemoEditForm := TMemoEditForm.Create(Application);
  MemoEditForm.Caption := 'Editing ' + Self.Prop.Name;
  MemoEditForm.Memo1.Text := String( Value.StringValue );
  if MemoEditForm.ShowModal=mrOK then
  begin
    Edit.Text := MemoEditForm.Memo1.Text;
    OnStoreValue(Edit);
  end;
end;

procedure TZPropertyStringEdit.OnStoreValue(Sender: TObject);
var
  S : string;
  IsChanged : boolean;
begin
  IsChanged := False;
  S := TEdit(Sender).Text;
  if Prop.PropertyType=zptString then
  begin
    IsChanged := Value.StringValue<>AnsiString(S);
    Value.StringValue := AnsiString(S);
  end
  else if Prop.PropertyType=zptExpression then
  begin
    IsChanged := Value.ExpressionValue.Source<>S;
    Value.ExpressionValue.Source := S;
  end;
  if IsChanged then
  begin
    try
      UpdateProp;
    except
      TEdit(Sender).Undo;
      TEdit(Sender).SetFocus;
    end;
  end;
end;

{ TZPropertyFloatEdit }

procedure TZPropertyFloatEdit.SetProp(C: TZComponent; Prop: TZProperty);
var
  Up : TUpDown;
begin
  inherited;
  Edit := TEdit.Create(Self);
  Edit.Align := alClient;
  Edit.Text := DesignerFormatFloat(Value.FloatValue);
  Edit.OnChange := OnEditChange;
  Edit.OnEnter := OnFocusControl;
  Edit.OnExit := OnEditExit;
  if (Self.Prop.NeverPersist and (not Self.Prop.IsReadOnly)) then
  begin
    //Tillåt ändra nevepersist floats: detta gör att man kan ändra
    //DefineVariable.Value i editor vilket är bra vid felsök.
    Edit.Font.Color := clGrayText;
  end
  else
    Edit.Enabled := not IsReadOnlyProp;
  Edit.Parent := ValuePanel;
//  NameLabel.FocusControl := Edit;

  if not IsReadOnlyProp then
  begin
    Up := TUpDown.Create(Self);
    Up.OnClick := OnUDClick;
    Up.Align := alRight;
    Up.Max := 1000;
    Up.Position := 500;
    Up.Orientation := udHorizontal;
    Up.Parent := ValuePanel;
  end;
end;

procedure TZPropertyFloatEdit.OnEditChange(Sender: TObject);
begin
  if Edit.Focused then
    //If focused then user is typing the value
    //Wait with updating prop until exit
    Edit.Color := clYellow
  else
  begin
    //Not focused, value from up/down or trackbar
    //Write change directly
    StoreFloat(TEdit(Sender).Text);
  end;
end;

procedure TZPropertyFloatEdit.StoreFloat(const S : string);
begin
  if TryStrToFloat(S,Value.FloatValue) then
    UpdateProp
  else
  begin
    ZLog.GetLog(Self.ClassName).Write('Not a float value: ' + S);
    //Edit.Undo;
  end;
  Edit.Color := clWhite;
end;

procedure TZPropertyFloatEdit.OnEditExit(Sender: TObject);
begin
  StoreFloat(TEdit(Sender).Text);
end;

procedure TZPropertyFloatEdit.OnUDClick(Sender: TObject;  Button: TUDBtnType);
var
  Add : single;
begin
  Add := 0.01 * (1-(ord(Button=btPrev)*2));
  Edit.Text := DesignerFormatFloat(StrToFloatDef(Edit.Text,0) + Add);
end;

{ TZPropertyColorEdit }
var
  ColorDialog : TColorDialog;

procedure TZPropertyColorEdit.OnClick(Sender: TObject);
var
  AlphaTemp : single;
begin
  if ColorDialog=nil then
    ColorDialog := TColorDialog.Create(Application);
  ColorDialog.Color := ZColorToColor( Value.ColorfValue );
  ColorDialog.Options := [cdFullOpen];
  if ColorDialog.Execute then
  begin
    AlphaTemp := Value.ColorfValue.V[3];
    Value.ColorfValue := ColorToZColor(ColorDialog.Color);
    if AlphaTemp>0 then
      Value.ColorfValue.V[3] := AlphaTemp;
    UpdateProp;
    (ValuePanel.Controls[0] as TPaintBox).Color := ColorDialog.Color;
  end;
end;

procedure TZPropertyColorEdit.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  S : string;
begin
  if Button=mbRight then
  begin
    S := InputBox('Color editor','Enter alpha value (range 0-1):',FloatToStr(Value.ColorfValue.V[3]) );
    Value.ColorfValue.V[3] := StrToFloatDef(S,1.0);
    UpdateProp;
  end;
end;

procedure TZPropertyColorEdit.OnPaint(Sender: TObject);
var
  P : TPaintBox;
begin
  P := Sender as TPaintBox;
  P.Canvas.Brush.Color := P.Color;
  P.Canvas.FillRect(P.BoundsRect);
end;

procedure TZPropertyColorEdit.SetProp(C: TZComponent; Prop: TZProperty);
var
  P : TPaintBox;

begin
  inherited;

  //Use a paintbox to make it work with vcl-styles
  P := TPaintBox.Create(Self);
  P.Parent := ValuePanel;
  P.Align := alClient;
  P.OnPaint := Self.OnPaint;

  if not IsReadOnlyProp then
  begin
    P.OnClick := Self.OnClick;
    P.OnMouseDown := Self.OnMouseDown;
  end;
  P.Color := ZColorToColor( Value.ColorfValue );
  P.Hint := 'Left click to select color, right click to select alpha';
end;

{ TZPropertyComponentEdit }

procedure TZPropertyComponentEdit.SetProp(C: TZComponent;
  Prop: TZProperty);
var
  B : TButton;
begin
  inherited;
  B := TButton.Create(Self);
  B.OnClick := DoFindComponent;
  B.Caption:='>';
  B.Hint := 'Click to focus selected component in project tree';
  B.Width := 16;
  B.Enabled := Assigned(Value.ComponentValue);
  B.Align := alRight;
  B.Parent := ValuePanel;
  Self.FocusButton := B;

  Cb := TComboBox.Create(Self);
  if Value.ComponentValue<>nil then
    Cb.Text := String(Value.ComponentValue.Name);
  Cb.Style := csDropDown;
  Cb.Sorted := True;
  Cb.OnDropDown := Self.OnDropDown;
  Cb.OnChange := Self.OnChange;
  Cb.Enabled := not IsReadOnlyProp;
  Cb.Align := alClient;
  Cb.OnEnter := OnFocusControl;
  Cb.Parent := ValuePanel;
end;

procedure TZPropertyComponentEdit.DoFindComponent(Sender: TObject);
begin
  (PropEditor.Owner as TEditorForm).FindComponentAndFocusInTree(Cb.Text);
end;

procedure TZPropertyComponentEdit.OnDropDown(Sender: TObject);
var
  I,J : integer;
  List : TStringList;
  O : TZComponent;
  Filtered : boolean;
begin
  List := TStringList.Create;
  try
    //todo: använd symboltabell
    GetObjectNames(PropEditor.RootComponent,List);

    if Cb.Items.Count>0 then
      Exit;
    Cb.Items.Add('(none)');

    //filtrera så att endast objekt av giltiga klasser kan väljas
    for I := 0 to List.Count-1 do
    begin
      O := TZComponent(List.Objects[I]);
      if High(Prop.ChildClasses)>-1 then
      begin
        Filtered := True;
        for J := 0 to High(Prop.ChildClasses) do
          if O is Prop.ChildClasses[J] then
          begin
            Filtered := False;
            Break;
          end;
        if Filtered then
          Continue;
      end;
      Cb.Items.AddObject( List[I], O )
    end;
  finally
    List.Free;
  end;
end;

procedure TZPropertyComponentEdit.OnChange(Sender: TObject);
var
  C : TZComponent;
begin
  C := nil;
  if Cb.ItemIndex>0 then
    C := Cb.Items.Objects[ Cb.ItemIndex ] as TZComponent;
  //Hindra sätta till self
  if C<>Component then
  begin
    Value.ComponentValue := C;
    UpdateProp;
    //Componentref kan påverka nodnamnet, gör refresh
    (PropEditor.Owner as TEditorForm).Tree.ZSelected.RefreshNodeName;
  end;
  FocusButton.Enabled := Cb.ItemIndex>0;
end;

{ TZPropertyIntegerEdit }

procedure TZPropertyIntegerEdit.SetProp(C: TZComponent; Prop: TZProperty);
var
  Up : TUpDown;
begin
  inherited;
  Edit := TEdit.Create(Self);
  Edit.Align := alClient;

  Edit.Text := IntToStr(Value.IntegerValue);

  Edit.OnChange := OnEditChange;
  Edit.OnEnter := OnFocusControl;
  Edit.Enabled := not IsReadOnlyProp;
  Edit.Parent := ValuePanel;
//  NameLabel.FocusControl := Edit;

  if not IsReadOnlyProp then
  begin
    Up := TUpDown.Create(Self);
    Up.OnClick := OnUDClick;
    Up.Align := alRight;
    Up.Max := 1000;
    Up.Position := 500;
    Up.Orientation := udHorizontal;
    Up.Parent := ValuePanel;
  end;
end;

procedure TZPropertyIntegerEdit.OnEditChange(Sender: TObject);
begin
  if TryStrToInt(TEdit(Sender).Text,Value.IntegerValue) then
    UpdateProp;
end;

procedure TZPropertyIntegerEdit.OnUDClick(Sender: TObject;
  Button: TUDBtnType);
var
  Add : integer;
begin
  Add := (1-(ord(Button=btPrev)*2));
  Edit.Text := IntToStr( StrToIntDef(Edit.Text,0) + Add );
end;

{ TZComponentTreeView }

procedure TZComponentTreeView.RebuildGui;
begin
  Items.BeginUpdate;
  Items.Clear;
  AddNode(RootComponent,nil);
  Items.EndUpdate;
//  FullExpand;
  if Items.Count>0 then
  begin
    Items[0].MakeVisible;
    Items[0].Expand(False);
  end;
end;

function TZComponentTreeView.AddNode(C : TZComponent; Parent : TTreenode; Index : integer = -1) : TTreeNode;
var
  Node : TZComponentTreeNode;
begin
  Assert(C<>nil);

  if Index=-1 then
    Node := Items.AddChild(Parent,'') as TZComponentTreeNode
  else
    Node := Items.Insert(Parent.Item[Index],'') as TZComponentTreeNode;

  Node.Component := C;
  Node.ImageIndex := ComponentManager.GetInfo(C).ImageIndex;
  Node.SelectedIndex := Node.ImageIndex;
  Node.RefreshNodeName;
  RefreshNode(Node,C);
  Result := Node;
end;

procedure TZComponentTreeView.RefreshNode(Node: TTreeNode; C: TZComponent);
var
  I,J : integer;
  NestedNode : TZComponentTreeNode;
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
begin
  Self.Items.BeginUpdate;

  if Node.HasChildren then
    Node.DeleteChildren;
  PropList := C.GetProperties;

  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    if Prop.HideInGui then
      Continue;
    case Prop.PropertyType of
      zptExpression :
        begin
          //Expression-nodes visas ska bara visas för debug, tillåt ej ändringar
          C.GetProperty(Prop,Value);
          if false then //(displaying opcodes is too slow) if ShowOpCodes then
          begin
            if Value.ExpressionValue.Code.Count=0 then
              Continue;
            NestedNode := Items.AddChild(Node, Prop.Name ) as TZComponentTreeNode;
            //NestedNode.ComponentList := Value.ExpressionValue.Code;
            NestedNode.ImageIndex := 3;
            NestedNode.SelectedIndex := NestedNode.ImageIndex;
            NestedNode.Prop := Prop;
            for J := 0 to Value.ExpressionValue.Code.ComponentCount-1 do
              AddNode(Value.ExpressionValue.Code.GetComponent(J),NestedNode);
          end;
        end;
      zptComponentList :
        begin
          //Ifall endast en child-nod finns så skippa prop-namnet
          //Detta gör trädet smalare
          C.GetProperty(Prop,Value);
{          if NestedCount=1 then
            NestedNode := Node
          else
          begin}
            NestedNode := Items.AddChild(Node, Prop.Name ) as TZComponentTreeNode;
            NestedNode.ComponentList := Value.ComponentListValue;
            NestedNode.ImageIndex := 3;
            NestedNode.SelectedIndex := NestedNode.ImageIndex;
//          end;
          NestedNode.Prop := Prop;
          for J := 0 to Value.ComponentListValue.ComponentCount-1 do
            AddNode(Value.ComponentListValue.GetComponent(J),NestedNode);
        end;
    end;
  end;
  Self.Items.EndUpdate;
end;

procedure TZComponentTreeView.SetRootComponent(C: TZComponent);
begin
  RootComponent := C;
  RebuildGui;
end;

function SortNodes(P1,P2 : pointer) : integer;
var
  N1,N2 : TTreeNode;
  I1,I2 : integer;
begin
  N1 := TTreeNode(P1); N2 := TTreeNode(P2);
  I1 := N1.Parent.IndexOf(N1);
  I2 := N2.Parent.IndexOf(N2);
  if I1=I2 then
    Result := 0
  else if I1<I2 then
    Result := -1
  else
    Result := 1;
end;

function TZComponentTreeView.SortSelections : TObjectList;
var
  I : integer;
begin
  //Sort selected sibling-nodes in their child-order to the parent
  Result := TObjectList.Create(False);
  for I := 0 to Self.SelectionCount-1 do
    Result.Add(Self.Selections[I]);
  Result.Sort(SortNodes);
end;

procedure TZComponentTreeView.ChangeScale(M, D: Integer);
begin
  inherited;

end;

constructor TZComponentTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HideSelection := False;
  ShowRoot := False;

//  OnAdvancedCustomDrawItem := MyCustomAdvancedDrawItem;

  ReadOnly := True;
  OnExpanded := MyExpanded;
end;

//** The code below is currently not used **
procedure TZComponentTreeView.MyCustomAdvancedDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
    DefaultDraw: Boolean);
var
  NodeRect: TRect;
  I: Integer;
  MyCaption,Comment : string;
  Col1,Col2 : TZColorf;
begin
  if Node=LockShowNode then
    Canvas.Font.Style := [fsBold]
  else if Assigned((Node as TZComponentTreeNode).Component) and (Node as TZComponentTreeNode).Component.DesignDisable then
    Canvas.Font.Style := [fsStrikeOut]
  else
    Canvas.Font.Style := [];

  if (Stage=cdPostPaint) and (not (cdsSelected in State)) then
  begin
    NodeRect := Node.DisplayRect(True);

    // TStyleManager.ActiveStyle.DrawElement()
    MyCaption := Node.Text;
    I := Pos('"', MyCaption);
    if I > 0 then
    begin
      Comment := Copy(MyCaption, I - 1, Length(MyCaption) - I + 2);
      MyCaption := Copy(MyCaption, 1, I - 2);
    end
    else
      Comment := '';

    if Length(Comment) > 0 then
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := TStyleManager.ActiveStyle.GetStyleColor(scTreeView);

      Col1 := ColorBtoF( ColorToRGB(TStyleManager.ActiveStyle.GetStyleColor(scTreeView)) );
      Col2 := ColorBtoF( ColorToRGB(TStyleManager.ActiveStyle.GetSystemColor(clWindowText)) );

      //Use a mixture of back and foreground color
      for I := 0 to 3 do
        Col1.V[I] := Col1.V[I] + (Col2.V[I]-Col1.V[I])*0.75;
      Col1.V[3]:=0;
      Canvas.Font.Color := ColorFtoB(Col1);

      Canvas.TextOut(NodeRect.Left + Canvas.TextWidth(MyCaption) + Canvas.TextWidth(' '),
        NodeRect.Top + 1, Comment);
    end;
  end;

  DefaultDraw := True;
end;

function TZComponentTreeView.CreateNode: TTreeNode;
begin
  Result := TZComponentTreeNode.Create(Items);
end;

procedure TZComponentTreeView.CreateWnd;
begin
  inherited;
  if Assigned(OnRecreate) then
    OnRecreate(Self);
end;

function TZComponentTreeView.FindNodeForComponentList(L: TZComponentList): TZComponentTreeNode;
var
  I : integer;
  Item : TZComponentTreeNode;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    Item := Items[I] as TZComponentTreeNode;
    if Item.ComponentList=L then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TZComponentTreeView.FindNodeForComponent(C: TZComponent): TZComponentTreeNode;
var
  I : integer;
  Item : TZComponentTreeNode;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    Item := Items[I] as TZComponentTreeNode;
    if Item.Component=C then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TZComponentTreeView.GetPopupMenu: TPopupMenu;
begin
   Result := nil;

//Hack som behövs för att man ska kunna högerklicka i trädet

//************* force the selected node to really be selected **************//
// if the user right-clicks on something in the tree it temporarily makes
// it selected when the RightClickSelect is true this code forces that to
// really be permanently selected.
//**************************************************************************

   if Selected = nil then
      Exit;

   TreeView_SelectItem( Selected.Handle, Selected.ItemId );

   Result := inherited GetPopupMenu;
end;

function TZComponentTreeView.ZSelected: TZComponentTreeNode;
begin
  Result := Selected as TZComponentTreeNode;
end;

procedure TZComponentTreeView.MyExpanded(Sender: TObject; Node: TTreeNode);
begin
  //Automatiskt öppna children när man klickar på en parentcomponent
  if (Node.Count=1) and Assigned((Node.getFirstChild as TZComponentTreeNode).ComponentList) then
    Node.getFirstChild.Expand(False);
end;

{ TZPropertyFloatsEdit }

procedure TZPropertyFloatsEdit.OnEditChange(Sender: TObject);
var
  Edit : TEdit;
begin
  Edit := Sender as TEdit;
  if Edit.Focused then
    //If focused then user is typing the value
    //Wait with updating prop until exit
    Edit.Color := clYellow
  else
  begin
    //Not focused, value from trackbar
    //Write change directly
    StoreFloat(TEdit(Sender));
  end;
end;

procedure TZPropertyFloatsEdit.StoreFloat(Edit : TEdit);
begin
  if TryStrToFloat( Edit.Text, FloatP^[ Edit.Tag ] ) then
    UpdateProp
  else
  begin
    ZLog.GetLog(Self.ClassName).Write('Not a float value: ' + Edit.Text);
    //Edit.Undo;
  end;
  Edit.Color := clWhite;
end;

procedure TZPropertyFloatsEdit.OnEditExit(Sender: TObject);
begin
  StoreFloat(TEdit(Sender));
end;

procedure TZPropertyFloatsEdit.SetProp(C: TZComponent; Prop: TZProperty);
var
  Edit : TEdit;
  I,Count : integer;
begin
  inherited;

  case Prop.PropertyType of
    zptRectf :
      begin
        Count := 4;
        FloatP := @Value.RectfValue;
      end;
    zptVector3f :
      begin
        Count := 3;
        FloatP := @Value.Vector3fValue;
      end;
    else
      raise Exception.Create('Fel propertytype');
  end;

  for I := 0 to Count-1 do
  begin
    Edit := TEdit.Create(Self);
    Edit.Tag := I;
    Edit.Width := 40;
    Edit.Align := alLeft;
    Edit.Text := DesignerFormatFloat(FloatP^[I]);
    Edit.OnChange := OnEditChange;
    Edit.OnEnter := OnFocusControl;
    Edit.OnExit := OnEditExit;
    Edit.Enabled := not IsReadOnlyProp;
    Edit.Parent := ValuePanel;
//    if I=0 then
  //    NameLabel.FocusControl := Edit;
  end;
end;

procedure GetAllObjects(C : TZComponent; List : TObjectList);
//Returns all objects
begin
  C.GetAllChildren(List,True);
end;

procedure GetReferersTo(Root, Target : TZComponent; List : TObjectList);
//Returns all objects that refers to component Target
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I,J : integer;
  All : TObjectList;
  C : TZComponent;
begin
  All := TObjectList.Create(False);
  try
    GetAllObjects(Root,All);
    for I := 0 to All.Count-1 do
    begin
      C := TZComponent(All[I]);
      if C.DesignDisable then
        Continue;
      PropList := C.GetProperties;
      for J := 0 to PropList.Count-1 do
      begin
        Prop := TZProperty(PropList[J]);
        case Prop.PropertyType of
          zptComponentRef :
            begin
              C.GetProperty(Prop,Value);
              if Value.ComponentValue=Target then
                List.Add(C);
            end;
        end;
      end;
    end;
  finally
    All.Free;
  end;
end;

function HasReferers(Root, Target : TZComponent; Deep : boolean = True) : boolean;
var
  TargetList,List : TObjectList;
  I,J : integer;
  C : TZComponent;
begin
  TargetList:= TObjectList.Create(False);
  List:=TObjectList.Create(False);
  try
    //Hämta alla barnen till Target
    if Deep then
      GetAllObjects(Target,TargetList)
    else
      TargetList.Add(Target);
    for I := 0 to TargetList.Count-1 do
    begin
      //Kolla alla barnen om det finns referens som är utanför targetlist
      C := TargetList[I] as TZComponent;
      List.Clear;
      GetReferersTo(Root,C,List);
      for J := 0 to List.Count-1 do
      begin
        if TargetList.IndexOf(List[J])=-1 then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Result := False;
  finally
    List.Free;
    TargetList.Free;
  end;
end;

procedure GetObjectNames(C : TZComponent; List : TStringList);
//Returns all objectnames
//Hoppar över objekt som ej har namn tilldelat
var
  I : integer;
  ObList : TObjectList;
  S : string;
begin
  ObList := TObjectList.Create(False);
  try
    GetAllObjects(C,ObList);
    for I := 0 to ObList.Count-1 do
    begin
      S := String(TZComponent(ObList[I]).Name);
      if Length(S)>0 then
        List.AddObject(S , TZComponent(ObList[I]) );
    end;
  finally
    ObList.Free;
  end;
end;

function FindInstanceOf(C : TZComponent; Zc : TZComponentClass) : TZComponent;
var
  I : integer;
  ObList : TObjectList;
begin
  Result := nil;
  ObList := TObjectList.Create(False);
  try
    GetAllObjects(C,ObList);
    for I := 0 to ObList.Count-1 do
    begin
      if TZComponent(ObList[I]) is Zc then
      begin
        Result := TZComponent(ObList[I]);
        Break;
      end;
    end;
  finally
    ObList.Free;
  end;
end;


{ TZPropertyBooleanEdit }

procedure TZPropertyBooleanEdit.OnEditChange(Sender: TObject);
begin
  Value.BooleanValue := Check.Checked;
  UpdateProp;
end;

procedure TZPropertyBooleanEdit.SetProp(C: TZComponent; Prop: TZProperty);
begin
  inherited;

  Check := TCheckBox.Create(Self);
  Check.Checked := Value.BooleanValue;
  Check.Parent := ValuePanel;
  Check.Enabled := not Self.IsReadOnlyProp;
  Check.OnClick := Self.OnEditChange;
end;

{ TZPropertyByteEdit }

procedure TZPropertyByteEdit.OnChange(Sender: TObject);
begin
  if Cb.Style=csDropDownList then
    Value.ByteValue := Cb.ItemIndex
  else
    Value.ByteValue := StrToIntDef(Cb.Text,Value.ByteValue);
  UpdateProp;
end;

procedure TZPropertyByteEdit.OnCbDropDown(Sender: TObject);
var
  I : integer;
begin
  if Cb.Items.Count=0 then
  begin
    Cb.Items.BeginUpdate;
    for I := 0 to 255 do
      Cb.Items.Add( IntToStr(I) );
    Cb.Items.EndUpdate;
  end;
end;

procedure TZPropertyByteEdit.SetProp(C: TZComponent; Prop: TZProperty);
var
  I : integer;
begin
  inherited;
  Cb := TComboBox.Create(Self);

  Cb.Align := alClient;
  Cb.OnChange := Self.OnChange;
  Cb.OnEnter := OnFocusControl;
  Cb.Enabled := not IsReadOnlyProp;
  Cb.Parent := ValuePanel;

  if Length(Prop.Options)>0 then
  begin
    Cb.Style := csDropDownList;
    Cb.Items.BeginUpdate;
    for I := 0 to High(Prop.Options) do
      Cb.Items.Add( Prop.Options[I] );
    Cb.Items.EndUpdate;
    Cb.ItemIndex := Value.ByteValue;
  end
  else
  begin
    Cb.OnDropDown := Self.OnCbDropDown;
    Cb.Style := csDropDown;
    Cb.Text := IntToStr(Value.ByteValue);
  end;
end;

{ TZComponentTreeNode }

procedure TZComponentTreeNode.RefreshNodeName;
begin
  Text := String(Component.GetDisplayName);
  if Component.Comment<>'' then
    Text := Text + ' "' + String(Component.Comment) + '"';
  if Component.DesignDisable then
    Text := Text + ' (disabled)';
end;

{ TZBinaryPropEdit }

function GraphicToBitmap(Pic :TPicture) : Graphics.TBitmap;
var
  Bmp: Graphics.TBitmap;
begin
  Bmp := Graphics.TBitmap.Create;
  if Pic.Graphic is TPngImage then
    (Pic.Graphic as TPngImage).AssignTo(Bmp)
  else
  begin
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := Pic.Graphic.Width;
    Bmp.Height := Pic.Graphic.Height;
    Bmp.Canvas.Draw(0,0,Pic.Graphic);
  end;
  //Pic.Assign(Bmp);
  Result := Bmp;
end;

procedure TZBinaryPropEdit.GetPictureStream(const Filename : string; Stream : TMemoryStream);
var
  Pic : TPicture;
  OwnBm : boolean;
  Bm : Graphics.TBitmap;
  X,Y : integer;
  R,G,B,A : byte;
  ZBm : TZBitmap;
  PPixel : PRGBQuad;
  UseAlpha : boolean;
begin
  Pic:=TPicture.Create;
  OwnBm := False;
  Bm := nil;
  try
    Pic.LoadFromFile(FileName);

    //Om bild laddats via TOleGraphic (gif/jpg) så måste den konverteras
    if not (Pic.Graphic is Graphics.TBitmap) then
    begin
      Bm := GraphicToBitmap(Pic);
      OwnBm := True;
    end
    else
    begin
      Bm := Pic.Bitmap;
    end;

    if (Self.Component.GetOwner is TZBitmap) then
    begin
      ZBm := (Self.Component.GetOwner as TZBitmap);
      if (ZBm.PixelWidth<>Bm.Width) or  (ZBm.PixelHeight<>Bm.Height) then
      begin
        ShowMessage(
          Format('Bitmap must have the same dimensions as the bitmap that owns the producer-list.'#13#13 +
           'The bitmap you try to import is %d x %d.'#13 +
           'The bitmap-component is %d x %d.',
              [Bm.Width,Bm.Height,ZBm.PixelWidth,ZBm.PixelHeight])
           );
        Abort;
      end;
    end;

    Self.Component.SetString('Comment','Imported from ' + AnsiString(ExtractFileName(FileName)) );

    if Pic.Graphic is TJpegImage then
    begin
      (Self.Component as TBitmapFromFile).FileFormat := bffJpeg;
      Stream.LoadFromFile(FileName);
      Exit;
    end;

    UseAlpha := Bm.PixelFormat=pf32bit;

    if not (Bm.PixelFormat in [pf24Bit,pf32Bit]) then
      Bm.PixelFormat := pf24Bit;

    if UseAlpha then
      ZLog.GetLog(Self.ClassName).Write('Alpha-channel present');

    //Store image upside down, this is how GL wants it
    for Y := Bm.Height - 1 downto 0 do
    begin
      PPixel := Bm.ScanLine[Y];
      for X := 0 to Bm.Width - 1 do
      begin
        if UseAlpha then
          A := PPixel.rgbReserved;

        B := PPixel.rgbBlue;
        G := PPixel.rgbGreen;
        R := PPixel.rgbRed;

        if Bm.PixelFormat=pf32bit then
          Inc(PPixel)
        else
          Inc(NativeUInt(PPixel),3);

        Stream.Write(R,1);
        Stream.Write(G,1);
        Stream.Write(B,1);
        if UseAlpha then
          Stream.Write(A,1);
      end;
    end;

    (Self.Component as TBitmapFromFile).HasAlphaLayer := UseAlpha;
    if UseAlpha then
      (Self.Component as TBitmapFromFile).Transparency := btAlphaLayer
    else
      (Self.Component as TBitmapFromFile).Transparency := btNone;

  finally
    if OwnBm then
      Bm.Free;
    Pic.Free;
  end;
end;

procedure TZBinaryPropEdit.OnClearValue(Sender: TObject);
begin
  if Application.MessageBox('Clear the current value?', PChar(Application.Title), MB_YESNO)<>IDYES then
    Exit;
  Value.BinaryValue.Size := 0;
  Value.BinaryValue.Data := nil;
  UpdateProp;
  DataChanged;
end;

procedure TZBinaryPropEdit.OnStoreValue(Sender: TObject);
var
  M : TMemoryStream;

  function InGetPicture : boolean;
  var
    D : TOpenPictureDialog;
  begin
    Result := False;
    D := TOpenPictureDialog.Create(Self);
    try
//      D.Filter := 'Bitmaps (*.bmp)|*.bmp';
//      D.DefaultExt := '*.bmp';
      if not D.Execute then
        Exit;
      GetPictureStream(D.FileName,M);
      Result := True;
    finally
      D.Free;
    end;
  end;

  function InGetMusic : boolean;
  var
    D : TOpenDialog;
    Midi : TMidiFile;
  begin
    Result := False;
    D := TOpenDialog.Create(Self);
    try
      D.Filter := 'Midi-files (*.mid)|*.mid';
      D.DefaultExt := '*.mid';
      if not D.Execute then
        Exit;
      Midi := TMidiFile.Create(D.FileName);
      try
        Midi.WriteZzdcResource(Self.Component as TMusic,M);
        Self.Component.SetString('Comment','Imported from ' + AnsiString(ExtractFileName(D.FileName)));
        //Music-instruments har ändrats, refresh treenode
        (PropEditor.Owner as TEditorForm).Tree.RefreshNode((PropEditor.Owner as TEditorForm).Tree.ZSelected,Self.Component);
        (PropEditor.Owner as TEditorForm).Tree.ZSelected.Expand(True);
      finally
        Midi.Free;
      end;
      Result := True;
    finally
      D.Free;
    end;
  end;

  function InGetSound : boolean;
  var
    D : TOpenDialog;
  begin
    Result := False;
    D := TOpenDialog.Create(Self);
    try
      D.Title := 'Pick audio file to import';
      D.Filter := 'OGG-files (*.ogg)|*.ogg|Raw-files (*.raw)|*.raw';
      D.DefaultExt := '*.ogg';
      if not D.Execute then
        Exit;

      if SameText(ExtractFileExt(D.FileName),'.raw') then
      begin
        if ImportRawAudioForm=nil then
          ImportRawAudioForm := TImportRawAudioForm.Create(Application);

        if ImportRawAudioForm.ShowModal=mrCancel then
          Exit;

        Self.Component.SetString('Comment','Imported from ' + AnsiString(ExtractFileName(D.FileName)));
        (Self.Component as TSampleImport).SampleRate := StrToFloatDef(ImportRawAudioForm.sampleRateComboBox.Text,8000);
        PByte(@(Self.Component as TSampleImport).SampleFormat)^ := ImportRawAudioForm.sampleFormatListBox.ItemIndex;
        (Self.Component as TSampleImport).SampleFileFormat := sffRAW;
        M.LoadFromFile(D.FileName);
        Result := True;
      end;

      if SameText(ExtractFileExt(D.FileName),'.ogg') then
      begin
        Self.Component.SetString('Comment','Imported from ' + AnsiString(ExtractFileName(D.FileName)));
        (Self.Component as TSampleImport).SampleFileFormat := sffOGG;
        M.LoadFromFile(D.FileName);
        Result := True;
      end;
    finally
      D.Free;
    end;
  end;

  function InGetAnyFile : boolean;
  var
    D : TOpenDialog;
  begin
    Result := False;
    D := TOpenDialog.Create(Self);
    try
      D.Filter := 'All files (*.*)|*.*';
      D.DefaultExt := '*.*';
      if not D.Execute then
        Exit;
      M.LoadFromFile(D.FileName);
      Self.Component.SetString('Comment','Imported from ' + AnsiString(ExtractFileName(D.FileName)));
      Result := True;
    finally
      D.Free;
    end;
  end;

  function InGetIcon : boolean;
  var
    D : TOpenDialog;
  begin
    Result := False;
    D := TOpenDialog.Create(Self);
    try
      D.Filter := 'Icons (*.ico)|*.ico';
      D.DefaultExt := '*.ico';
      if not D.Execute then
        Exit;
      M.LoadFromFile(D.FileName);
      //Self.Component.Comment := 'Imported from ' + ExtractFileName(D.FileName);
      Result := True;
    finally
      D.Free;
    end;
  end;

  procedure InEditArray;
  var
    A : TDefineArray;
  begin
    A := Component as TDefineArray;
    if not A.Persistent then
    begin
      ShowMessage('Set "Persistent" before editing arrays');
      Exit;
    end;
    if not (A._Type in [zctInt,zctFloat,zctByte]) then
    begin
      ShowMessage('Persistent arrays only supported for numeric data types');
      Exit;
    end;
    if ArrayEditForm=nil then
      Application.CreateForm(TArrayEditForm,ArrayEditForm);
    ArrayEditForm.SetArray(A);
    ArrayEditForm.Show;
  end;

  procedure InGetMeshImport;
  var
    D : TOpenDialog;
    Imp : T3dsImport;
  begin
    D := TOpenDialog.Create(Self);
    try
      D.Filter := '3D-studio files (*.3ds)|*.3ds';
      D.DefaultExt := '*.3ds';
      if not D.Execute then
        Exit;
      Imp := T3dsImport.Create(D.FileName);
      try
        Imp.MeshImpToUpdate := Component as TMeshImport;
        Imp.Import;
        Component.Change;
      finally
        Imp.Free;
      end;
    finally
      D.Free;
    end;
  end;

begin
  M := TMemoryStream.Create;
  try
    case ComponentManager.GetInfo(Component).ClassId of
      BitmapFromFileClassId :
        if not InGetPicture then
          Exit;
      MusicClassId :
        if not InGetMusic then
          Exit;
      SampleImportClassId :
        if not InGetSound then
          Exit;
      ZFileClassId :
        if not InGetAnyFile then
          Exit;
      ApplicationClassId :
        if not InGetIcon then
          Exit;
      MeshImportClassId :
        InGetMeshImport;
      DefineArrayClassId :
        InEditArray;
    else
      raise Exception.Create('Unknown binary property');
    end;

    if M.Size>0 then
    begin
      Value.BinaryValue.Size := M.Size;
      GetMem(Value.BinaryValue.Data,M.Size);
      Move(M.Memory^,Value.BinaryValue.Data^,M.Size);
      UpdateProp;
    end;
    DataChanged;

  finally
    M.Free;
  end;
end;

procedure TZBinaryPropEdit.DataChanged;
begin
  ValuePanel.Caption := 'Size: ' + IntToStr(Value.BinaryValue.Size);
  ClearButton.Enabled := (not Self.IsReadOnlyProp) and (Value.BinaryValue.Size>0);
end;

procedure TZBinaryPropEdit.SetProp(C: TZComponent; Prop: TZProperty);
var
  B : TButton;
  IsEnabled : boolean;
begin
  inherited;

//  if not IsReadOnlyProp then
//    IsReadOnlyProp := (ComponentManager.GetInfo(C).ClassId=MeshImportClassId);
  IsEnabled := (not Self.IsReadOnlyProp);

  ValuePanel.Alignment := taLeftJustify;

  ClearButton := TButton.Create(Self);
  ClearButton.Align := alRight;
  ClearButton.Width := 40;
  ClearButton.Caption := 'Clear';
  ClearButton.Hint := 'Clear the current content';
  ClearButton.OnClick := OnClearValue;
  ClearButton.Parent := ValuePanel;
  ClearButton.Enabled := IsEnabled;

  B := TButton.Create(Self);
  B.Align := alRight;
  B.Left := 1000;
  B.Width := 60;
  if (ComponentManager.GetInfo(C).ClassId=DefineArrayClassId) then
  begin
    B.Caption := 'Edit...';
  end
  else
  begin
    B.Caption := 'Import...';
    B.Hint := 'Select a file to import';
  end;
  B.OnClick := OnStoreValue;
  B.Parent := ValuePanel;
  B.Enabled := IsEnabled;

  DataChanged;
end;

end.
