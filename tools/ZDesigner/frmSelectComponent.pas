unit frmSelectComponent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, ZClasses, Vcl.StdCtrls;

type
  TSelectComponentForm = class(TForm)
    CompListView: TListView;
    OkButton: TButton;
    Button2: TButton;
    HelpButton: TButton;
    procedure CompListViewDblClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CompListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure CompListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure CompListViewInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
  private
    { Private declarations }
    EnabledList : array of boolean;
  public
    { Public declarations }
    function GetSelectedClass : TZComponentInfo;
    procedure FilterBy(ParentComps, ParentLists : TStringList; const Prop : TZProperty);
  end;

var
  SelectComponentForm: TSelectComponentForm;

implementation

uses dmCommon,uHelp,CommCtrl, Vcl.Styles, Vcl.Themes, Math;

{$R *.dfm}

procedure TSelectComponentForm.FormCreate(Sender: TObject);
var
  Infos : PComponentInfoArray;
  Ci : TZComponentInfo;
  I : TZClassIds;
  Item : TListItem;
begin
  Infos := ZClasses.ComponentManager.GetAllInfos;
  for I := Low(TComponentInfoArray) to High(TComponentInfoArray) do
  begin
    if I=AnyComponentClassId then
      Continue;
    Ci := TZComponentInfo(Infos[I]);
    Assert(Ci<>nil, 'Component info=nil. Component class removed?');
    if Ci.NoUserCreate then
      Continue;
    Item := CompListView.Items.Add;
    Item.Caption := Ci.ZClassName;
//    Item.SubItems.Add(Ci.HelpText);
    Item.Data := Ci;
    Item.ImageIndex := Ci.ImageIndex;
  end;
  SetLength(EnabledList,Length(Infos^));

  ListView_SetColumnWidth(CompListView.Handle, 0, 200);
end;

function TSelectComponentForm.GetSelectedClass: TZComponentInfo;
begin
  Result := TZComponentInfo(CompListView.Selected.Data);
end;

//Reference: http://theroadtodelphi.wordpress.com/2012/03/14/vcl-styles-and-owner-draw/
procedure TSelectComponentForm.CompListViewCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  LStyles   : TCustomStyleServices;
  LColor    : TColor;
  Fs : TThemedTreeView;
begin
  LStyles:=StyleServices;

  if EnabledList[Item.Index] then
    Fs := ttItemNormal
  else
    Fs := ttItemDisabled;
  if not LStyles.GetElementColor(LStyles.GetElementDetails(Fs), ecTextColor, LColor) or (LColor = clNone) then
  begin
    if EnabledList[Item.Index] then
	    LColor := LStyles.GetSystemColor(clWindowText)
    else
	    LColor := LStyles.GetSystemColor(clGrayText);
  end;

  Sender.Canvas.Font.Color  := LColor;
  Sender.Canvas.Brush.Color := LStyles.GetStyleColor(scListView);
end;

procedure TSelectComponentForm.CompListViewDblClick(Sender: TObject);
begin
  if OKButton.Enabled  then
    ModalResult := mrOK;
end;

procedure TSelectComponentForm.CompListViewInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
begin
  InfoTip := TZComponentInfo(Item.Data).HelpText;
end;

procedure TSelectComponentForm.CompListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    OkButton.Enabled := EnabledList[Item.Index]
  else
    OkButton.Enabled := False;
  HelpButton.Enabled := Selected;
end;

procedure TSelectComponentForm.FilterBy(ParentComps, ParentLists : TStringList; const Prop : TZProperty);
var
  Ci : TZComponentInfo;
  I,J : integer;
  Item : TListItem;
  PassedFilter,Enabled : boolean;
begin
  for I := 0 to CompListView.Items.Count - 1 do
  begin
    Item := CompListView.Items[I];
    Ci := TZComponentInfo(Item.Data);

    Enabled := True;
    if (Ci.NeedParentComp<>'') and (ParentComps.IndexOf(Ci.NeedParentComp)=-1) then
      Enabled := False;

//    if (Ci.NeedParentList<>'') and (ParentLists.IndexOf(Ci.NeedParentList)=-1) then
//      Enabled := False;

    if High(Prop.ChildClasses)>-1 then
    begin
      PassedFilter :=False;
      for J := 0 to High(Prop.ChildClasses) do
        if (Ci.ZClass = Prop.ChildClasses[J]) or
          //(Ci.ZClass.ClassParent=FilterC[J]) then
          (Ci.ZClass.InheritsFrom(Prop.ChildClasses[J])) then
        begin
          PassedFilter := True;
          Break;
        end;
      if not PassedFilter then
        Enabled := False;
    end else
    begin
      //If no filter, then it is toplevel.
 {     if Ci.NoTopLevelCreate then
        Continue;}
    end;

    EnabledList[I] := Enabled;
  end;
end;


procedure TSelectComponentForm.HelpButtonClick(Sender: TObject);
begin
  HtmlHelp(0,Application.HelpFile + '::/' + GetSelectedClass.ZClassName + '.html', HH_DISPLAY_TOPIC, 0);
end;


end.
