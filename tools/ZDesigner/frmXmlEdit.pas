unit frmXmlEdit;

interface

uses
  {$ifndef ZgeLazarus}
  Windows, Messages,
  {$endif}
  SynEdit, 
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
    SynEdit : TSynEdit;
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
  SynEdit := TSynEdit.Create(Self);
  SynEdit.Align := alClient;
  {$ifndef ZgeLazarus}
  SynEdit.Gutter.Visible := True;
  SynEdit.Gutter.ShowLineNumbers := True;
  SynEdit.Highlighter := TSynXMLSyn.Create(Self);
  SynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  SynEdit.SearchEngine := TSynEditSearch.Create(Self);
  SynEdit.MaxScrollWidth := 4096;
  {$endif}
  SynEdit.Parent := Panel1;
  SynEdit.WantTabs := True;
  SynEdit.TabWidth := 2;
  SynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;
end;

end.
