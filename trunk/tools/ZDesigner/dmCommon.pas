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

unit dmCommon;

interface

uses
  SysUtils, Classes, ImgList, Controls, Menus, ActnList, XPStyleActnCtrls,
  ActnMan, StdActns, Dialogs, System.Actions;

type
  TCommonModule = class(TDataModule)
    CompIconsImageList: TImageList;
    SynEditPopupMenu: TPopupMenu;
    ActionManager1: TActionManager;
    Selectall1: TMenuItem;
    EditCopy1: TEditCopy;
    Copy1: TMenuItem;
    EditSelectAll1: TEditSelectAll;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    EditDelete1: TEditDelete;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    EditUndo1: TEditUndo;
    N2: TMenuItem;
    Undo1: TMenuItem;
    FindDialog1: TFindDialog;
    FindAction: TAction;
    Find1: TMenuItem;
    N3: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    ReplaceAction: TAction;
    Replace1: TMenuItem;
    procedure FindActionExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure ReplaceActionExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CommonModule: TCommonModule;

implementation

{$R *.dfm}

uses Forms, SynEdit, SynEditTypes;

procedure TCommonModule.FindActionExecute(Sender: TObject);
var
  Syn : TSynEdit;
begin
  Syn := (((Sender as TAction).ActionComponent as TMenuItem).GetParentMenu as TPopUpMenu).PopupComponent as TSynEdit;
  if Syn=nil then
    Syn := Screen.ActiveControl as TSynEdit;
  FindDialog1.Tag := Integer(Syn);
  FindDialog1.Execute(Syn.Handle);
end;

procedure TCommonModule.ReplaceActionExecute(Sender: TObject);
var
  Syn : TSynEdit;
begin
  Syn := (((Sender as TAction).ActionComponent as TMenuItem).GetParentMenu as TPopUpMenu).PopupComponent as TSynEdit;
  if Syn=nil then
    Syn := Screen.ActiveControl as TSynEdit;
  ReplaceDialog1.Tag := Integer(Syn);
  ReplaceDialog1.Execute(Syn.Handle);
end;

procedure TCommonModule.FindDialog1Find(Sender: TObject);
var
  Syn : TSynEdit;
  Opt : TSynSearchOptions;
begin
  Syn := TSynEdit(FindDialog1.Tag);
  if Screen.ActiveControl<>Syn then
  begin
    FindDialog1.CloseDialog;
    Exit;
  end;
  Opt := [];
  if frWholeWord in FindDialog1.Options then
    Include(Opt,ssoWholeWord);
  if frMatchCase in FindDialog1.Options then
    Include(Opt,ssoMatchCase);
  if not (frDown in FindDialog1.Options) then
    Include(Opt,ssoBackwards);
//  Opt := [ssoMatchCase, ssoWholeWord, ssoBackwards,
//    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt];
  Syn.SearchReplace(FindDialog1.FindText,'',Opt);
end;

procedure TCommonModule.ReplaceDialog1Replace(Sender: TObject);
var
  Syn : TSynEdit;
  Opt : TSynSearchOptions;
begin
  Syn := TSynEdit(ReplaceDialog1.Tag);
  if Screen.ActiveControl<>Syn then
  begin
    ReplaceDialog1.CloseDialog;
    Exit;
  end;
  Opt := [];
  if frWholeWord in ReplaceDialog1.Options then
    Include(Opt,ssoWholeWord);
  if frMatchCase in ReplaceDialog1.Options then
    Include(Opt,ssoMatchCase);
  if not (frDown in ReplaceDialog1.Options) then
    Include(Opt,ssoBackwards);
  if (frReplace in ReplaceDialog1.Options) then
    Include(Opt,ssoReplace);
  if (frReplaceAll in ReplaceDialog1.Options) then
    Include(Opt,ssoReplaceAll);
//  Opt := [ssoMatchCase, ssoWholeWord, ssoBackwards,
//    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt];
  Syn.SearchReplace(ReplaceDialog1.FindText,ReplaceDialog1.ReplaceText,Opt);
end;

end.
