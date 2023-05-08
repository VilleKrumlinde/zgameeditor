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
  {$ifdef ZgeLazarus}
  SynEditTypes,
  {$else}
  SynHighlighterXML, SynEditSearch,
  {$endif}
  dmCommon;

procedure TXmlEditForm.FormCreate(Sender: TObject);
begin
  SynEdit := TSynEdit.Create(Self);
  SynEdit.Align := alClient;
  {$ifdef ZgeLazarus}
  SynEdit.Options := [eoAutoIndent, eoDragDropEditing,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  SynEdit.Options2 := [eoEnhanceEndKey, eoOverwriteBlock];
  {$else}
  SynEdit.Gutter.Visible := True;
  SynEdit.Gutter.ShowLineNumbers := True;
  SynEdit.Highlighter := TSynXMLSyn.Create(Self);
  SynEdit.MaxScrollWidth := 4096;
  SynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  SynEdit.SearchEngine := TSynEditSearch.Create(Self);
  {$endif}
  SynEdit.Parent := Panel1;
  SynEdit.WantTabs := True;
  SynEdit.TabWidth := 2;
  SynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;
end;

end.
