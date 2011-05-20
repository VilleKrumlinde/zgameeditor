unit frmSelectComponent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ZClasses, StdCtrls;

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

uses dmCommon,uHelp,CommCtrl;

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

procedure TSelectComponentForm.CompListViewCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if EnabledList[Item.Index] then
    CompListView.Canvas.Font.Color := clWindowText
  else
    CompListView.Canvas.Font.Color := clGrayText;
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
  uHelp.ShowHelp( 'ComponentRef/' + GetSelectedClass.ZClassName );
end;


end.
