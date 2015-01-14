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

program ZDesigner;

{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

{$if defined(CPUX64)}
  {$EXCESSPRECISION OFF} //Needed for fast single-precision math
{$ifend}

uses
  Forms,
  HTMLHelpViewer,
  ZLog in '..\..\ZLog.pas',
  DesignerGui in 'DesignerGui.pas',
  ZClasses in '..\..\ZClasses.pas',
  ZBitmap in '..\..\ZBitmap.pas',
  BitmapProducers in '..\..\BitmapProducers.pas',
  GLPanel in 'GLPanel.pas',
  OpenGL12 in 'OpenGL12.pas',
  frmEditor in 'frmEditor.pas' {EditorForm},
  Animators in '..\..\Animators.pas',
  Meshes in '..\..\Meshes.pas',
  ZApplication in '..\..\ZApplication.pas',
  Commands in '..\..\Commands.pas',
  ZExpressions in '..\..\ZExpressions.pas',
  Compiler in 'Compiler\Compiler.pas',
  Collision in '..\..\Collision.pas',
  Zc in 'Compiler\Zc.PAS',
  frmSelectComponent in 'frmSelectComponent.pas' {SelectComponentForm},
  Steering in '..\..\Steering.pas',
  frmCompEditBase in 'frmCompEditBase.pas' {CompEditFrameBase: TFrame},
  uSymTab in '..\..\uSymTab.pas',
  dmCommon in 'dmCommon.pas' {CommonModule: TDataModule},
  AudioPlayer in '..\..\AudioPlayer.pas',
  AudioComponents in '..\..\AudioComponents.pas',
  frmChannelFrame in 'frmChannelFrame.pas' {ChannelFrame: TFrame},
  frmEnvelopeFrame in 'frmEnvelopeFrame.pas' {EnvelopeFrame: TFrame},
  frmLfoFrame in 'frmLfoFrame.pas' {LfoFrame: TFrame},
  frmModulationFrame in 'frmModulationFrame.pas' {ModulationFrame: TFrame},
  frmSoundEdit in 'frmSoundEdit.pas' {SoundEditFrame: TFrame},
  frmMusicEdit in 'frmMusicEdit.pas' {MusicEditFrame: TFrame},
  ImplicitMeshes in '..\..\ImplicitMeshes.pas',
  frmAbout in 'frmAbout.pas' {AboutForm},
  uHelp in 'uHelp.pas',
  frmToolMissing in 'frmToolMissing.pas' {ToolMissingForm},
  ZFile in '..\..\ZFile.pas',
  unitPEFile in '3rdparty\unitPEFile.pas',
  unitResourceDetails in '3rdparty\unitResourceDetails.pas',
  unitResourceRCData in '3rdparty\unitResourceRCData.pas',
  frmMemoEdit in 'frmMemoEdit.pas' {MemoEditForm},
  uMidiFile in 'uMidiFile.pas',
  uTinyGif in '3rdparty\uTinyGif.pas',
  u3dsFile in 'u3dsFile.pas',
  frm3dsImportOptions in 'frm3dsImportOptions.pas' {Import3dsForm},
  frmRawAudioImportOptions in 'frmRawAudioImportOptions.pas' {ImportRawAudioForm},
  frmSettings in 'frmSettings.pas' {SettingsForm},
  unitResourceGraphics in '3rdparty\unitResourceGraphics.pas',
  unitEXIcon in '3rdparty\unitEXIcon.pas',
  Zc_Ops in 'Compiler\Zc_Ops.pas',
  frmBitmapEdit in 'frmBitmapEdit.pas' {BitmapEditFrame: TFrame},
  SugiyamaLayout in '3rdparty\SugiyamaLayout.pas',
  frmMeshEdit in 'frmMeshEdit.pas' {MeshEditFrame: TFrame},
  frmArrayEdit in 'frmArrayEdit.pas' {ArrayEditForm},
  ZPlatform in '..\..\ZPlatform.pas',
  frmXmlEdit in 'frmXmlEdit.pas' {XmlEditForm},
  Vcl.Themes,
  Vcl.Styles,
  CocoAncestor in 'Compiler\CocoAncestor.pas',
  CocoSets in 'Compiler\CocoSets.pas',
  frmAndroidApk in 'frmAndroidApk.pas' {AndroidApkForm},
  GLDrivers in '..\..\GLDrivers.pas',
  frmScriptedCompEditBase in 'frmScriptedCompEditBase.pas' {ScriptedCompEditFrameBase: TFrame},
  frmSpriteSheetEdit in 'frmSpriteSheetEdit.pas' {SpriteSheetEditFrame: TFrame};

{$R *.res}

{.$SETPEFLAGS 1} // IMAGE_FILE_RELOCS_STRIPPED

//+>2gb memory in 64-bitwindows
{$SETPEFLAGS $21}

begin
  //Report memleaks when run inside delphi debugger
  ReportMemoryLeaksOnShutdown := DebugHook<>0;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := frmEditor.AppName;
  Application.CreateForm(TCommonModule, CommonModule);
  Application.CreateForm(TEditorForm, EditorForm);
  Application.Run;
end.
