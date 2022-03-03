unit frmXmlEdit;

interface

uses
  {$ifndef ZgeLazarus}
  SynEdit, Windows, Messages,
  {$endif}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TXmlEditForm = class(TForm)
    Panel1 : TPanel;
    OkButton: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    {$ifndef ZgeLazarus}
    SynEdit : TSynEdit;
    {$endif}
  end;

var
  XmlEditForm: TXmlEditForm;

implementation

{$R *.dfm}

uses
  {$ifndef ZgeLazarus}
  SynHighlighterXML, SynEditSearch,
  {$endif}
  dmCommon;

procedure TXmlEditForm.FormCreate(Sender: TObject);
begin
  {$ifndef ZgeLazarus}
  SynEdit := TSynEdit.Create(Self);
  SynEdit.Align := alClient;
  SynEdit.Gutter.Visible := True;
  SynEdit.Gutter.ShowLineNumbers := True;
  SynEdit.Parent := Panel1;
  SynEdit.Highlighter := TSynXMLSyn.Create(Self);
  SynEdit.WantTabs := True;
  SynEdit.TabWidth := 2;
  SynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  SynEdit.SearchEngine := TSynEditSearch.Create(Self);
  SynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;
  SynEdit.MaxScrollWidth := 4096;
  {$endif}
end;

end.
