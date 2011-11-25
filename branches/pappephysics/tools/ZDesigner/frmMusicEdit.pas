unit frmMusicEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCompEditBase, StdCtrls,ZClasses,AudioComponents, ExtCtrls, DesignerGUI,
  ComCtrls, CheckLst;

type
  TMusicEditFrame = class(TCompEditFrameBase)
    PlayButton: TButton;
    Memo1: TMemo;
    procedure PlayButtonClick(Sender: TObject);
  private
    { Private declarations }
    Music : TMusic;
    IsPlaying : boolean;
    Thread : TThread;
  public
    { Public declarations }
    procedure SetComponent(C : TZComponent; TreeNode : TZComponentTreeNode); override;
    procedure OnEditorClose; override;
  end;

var
  MusicEditFrame: TMusicEditFrame;

implementation

{$R *.dfm}

uses AudioPlayer,ZPlatform;

type
  TPlaybackThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Music : TMusic;
  end;

{ TPlaybackThread }

procedure TPlaybackThread.Execute;
var
  LastTime,TimeStep,Now : single;
begin
  LastTime := Platform_GetTime;
  Music.Start;
  while not Terminated do
  begin
    Now := Platform_GetTime;
    TimeStep := (Now - LastTime);
    LastTime := Now;
    Music.AdvanceMusic(TimeStep);
    AudioPlayer.EmitSoundsInEmitList;
    Sleep(10);
  end;
end;

procedure TMusicEditFrame.SetComponent(C: TZComponent; TreeNode : TZComponentTreeNode);
begin
  inherited;
  Music := (C as TMusic);
end;

procedure TMusicEditFrame.PlayButtonClick(Sender: TObject);
var
  T :TPlaybackThread;
begin
  if IsPlaying then
  begin
    PlayButton.Caption := '&Play music';
//    PlayTimer.Enabled := False;
    Music.Stop;
    if Thread<>nil then
    begin
      Thread.Terminate;
      Thread := nil;
    end;
  end
  else
  begin
    Platform_InitGlobals; //reset timer
    PlayButton.Caption := '&Stop music';
//    Music.Start;
{    if ThreadCheckBox.Checked then
    begin }
      T := TPlaybackThread.Create(True);
      T.Priority := tpTimeCritical;
      T.Music:=Music;
      T.Start;
      T.FreeOnTerminate := True;
      Self.Thread := T;
{    end
    else
    begin}
//      LastTimerTime := Platform_GetTime;
//      PlayTimer.Enabled := True;
//    end;
  end;
  IsPlaying := not IsPlaying;
end;

procedure TMusicEditFrame.OnEditorClose;
begin
  if IsPlaying then
    PlayButton.Click;
  Music := nil;
end;


end.
