unit frmXmlEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEdit, ExtCtrls;

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

uses SynHighlighterXML, SynEditSearch, dmCommon;

procedure TXmlEditForm.FormCreate(Sender: TObject);
begin
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
end;

end.
