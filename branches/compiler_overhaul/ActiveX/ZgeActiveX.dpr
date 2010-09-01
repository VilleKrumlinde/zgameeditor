library ZgeActiveX;

uses
  ComServ,
  ZgeActiveX_TLB in 'ZgeActiveX_TLB.pas',
  ZgeActiveXControlImpl in 'ZgeActiveXControlImpl.pas' {ZgeActiveXControl: TActiveForm} {ZgeActiveXControl: CoClass},
  ZClasses in '..\ZClasses.pas',
  ZOpenGL in '..\ZOpenGL.pas',
  ZBitmap in '..\ZBitmap.pas',
  BitmapProducers in '..\BitmapProducers.pas',
  ZPlatform in '..\ZPlatform.pas',
  ZApplication in '..\ZApplication.pas',
  ZLog in '..\ZLog.pas',
  Animators in '..\Animators.pas',
  Meshes in '..\Meshes.pas',
  Renderer in '..\Renderer.pas',
  ZMath in '..\ZMath.pas',
  Commands in '..\Commands.pas',
  ZExpressions in '..\ZExpressions.pas',
  Collision in '..\Collision.pas',
  Steering in '..\Steering.pas',
  AudioPlayer in '..\AudioPlayer.pas',
  AudioComponents in '..\AudioComponents.pas',
  ImplicitMeshes in '..\ImplicitMeshes.pas',
  DLLFuncs in '..\DLLFuncs.pas',
  ZFile in '..\ZFile.pas';

{$ifndef fpc}
  {$R ..\Data.res ..\Data.rc}
{$endif}

{$E ocx}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
