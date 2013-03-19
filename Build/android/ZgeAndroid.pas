{Copyright (c) 2012 Ville Krumlinde

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

{
  ZGE-port of Android

  Thanks to Andrey Kemka and his ZenGL project for help with Android/Fpc techniques:
    http://zengl.org/
}

library zgeandroid;

{$MODE DELPHI}

uses jni, ZOpenGL,
  ZClasses,
  ZApplication,
  ZPlatform,
  ZBitmap,
  BitmapProducers,
  ZLog,
  Animators,
  Meshes,
  Renderer,
  ZMath,
  Commands,
  ZExpressions,
  Collision,
  Steering,
  AudioPlayer,
  AudioComponents,
  ImplicitMeshes,
  ZFile,
  NanoJpeg;

const
  AppInited : boolean = false;
  ZApp : TZApplication = nil;

procedure Java_org_zgameeditor_Zge_NativeDestroy( env : PJNIEnv; thiz : jobject ); cdecl;
begin
  ZApp.Free;
  AppInited := False;
end;

procedure Java_org_zgameeditor_Zge_NativeSurfaceCreated( env : PJNIEnv; thiz : jobject);cdecl;
begin

end;

procedure Java_org_zgameeditor_Zge_NativeInit( env : PJNIEnv; thiz : jobject; ExtPath : jstring; DataPath : jstring; LibraryPath : jstring);cdecl;
var
  P : PAnsiChar;
begin
  P := env^.GetStringUTFChars(Env,ExtPath,nil);
    GetMem(AndroidPath,ZStrLength(P)+1);
    ZStrCopy(AndroidPath,P);
    AndroidLog(AndroidPath);
  env^.ReleaseStringUTFChars(Env,ExtPath,P);

  P := env^.GetStringUTFChars(Env,DataPath,nil);
    GetMem(AndroidDataPath,ZStrLength(P)+1);
    ZStrCopy(AndroidDataPath,P);
    AndroidLog(AndroidDataPath);
  env^.ReleaseStringUTFChars(Env,DataPath,P);

  P := env^.GetStringUTFChars(Env,LibraryPath,nil);
    GetMem(AndroidLibraryPath,ZStrLength(P)+1);
    ZStrCopy(AndroidLibraryPath,P);
    AndroidLog(AndroidLibraryPath);
  env^.ReleaseStringUTFChars(Env,LibraryPath,P);
  
  AndroidThis := Env^.NewGlobalRef(Env,thiz);
end;

procedure InitApp;
begin
  if not AppInited then
  begin
    ZApp := ZApplication.LoadApplicationComponent;
    ZApp.ScreenWidth := 100;
    ZApp.ScreenHeight := 100;
    ZApp.Run;
    AppInited := True;
  end;
end;

procedure Java_org_zgameeditor_Zge_NativeSetDataContent( env : PJNIEnv; thiz : jobject; Content : jarray); cdecl;
var
  Size :  integer;
  IsCopy : jboolean;
  P : pointer;
begin
  Size := env^.GetArrayLength(env,Content);
  P := env^.GetPrimitiveArrayCritical(env, Content, IsCopy);
  
  GetMem(AndroidContentPtr,Size);
  Move(P^,AndroidContentPtr^,Size);
  AndroidContentSize := Size;
  
  env^.ReleasePrimitiveArrayCritical(env, Content, P, 0);
  
  InitApp;
end;

procedure Java_org_zgameeditor_Zge_NativeInitAppFromSDCard( env : PJNIEnv; thiz : jobject);cdecl;
begin
  AndroidLog('init from sdcard');
  InitApp;
end;

procedure Java_org_zgameeditor_Zge_NativeSurfaceChanged( env : PJNIEnv; thiz : jobject; Width, Height : jint );cdecl;
begin
  if AppInited then
  begin
    ZApp.ScreenWidth := Width;
    ZApp.ScreenHeight := Height;
    ZApp.ResetGpuResources;
    ZApp.Driver.InitGL
  end;
end;

procedure DrawTestTriangle;
const
  verts : array[0..8] of single = (-0.5,-0.5,0 ,0.5,-0.5,0, 0,0.5,0);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glColor4f(0.5,1,0,1);
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3,GL_FLOAT,0,@Verts);
  glDrawArrays(GL_TRIANGLES,0,3);
  glDisableClientState(GL_VERTEX_ARRAY);
end;

function Java_org_zgameeditor_Zge_NativeDrawFrame( env : PJNIEnv; thiz : jobject ) : boolean; cdecl;
begin
  if AppInited then
  begin
    ZApp.Main;
    Result := ZApp.Terminating;
  end
  else
    Result := False;
end;

procedure Java_org_zgameeditor_Zge_NativeActivate( env : PJNIEnv; thiz : jobject; Activate : jboolean );cdecl;
begin
 if Activate<>0 then
   AndroidKeys[ KeyResumed ] := True
 else
   AndroidKeys[ KeyPaused ] := True;
end;

function Java_org_zgameeditor_Zge_NativeCloseQuery( env : PJNIEnv; thiz : jobject ) : boolean; cdecl;
begin
  if AppInited then
    Result := ZApp.EscapeToQuit
  else
    Result := True;
end;

procedure Java_org_zgameeditor_Zge_NativeTouch( env : PJNIEnv; thiz : jobject; ID : jint; X, Y, Pressure : jfloat );cdecl;
var
  I,Ix,Iy : integer;
  IsUp,Found : boolean;
  
begin
  Ix := Trunc(X);
  Iy := Trunc(Y);
  AndroidCurrentMouse.X := Ix;
  AndroidCurrentMouse.Y := Iy;
  
  IsUp := Pressure<0.0001;
  if IsUp then
    AndroidKeys[ '{' ] := True;  //Mouse click
  
  Found := False;
  for I := 0 to AndroidTouchCount-1 do
  begin
    if AndroidTouch[I].Id=ID then
    begin
      if IsUp then
      begin
        if I<>AndroidTouchCount-1 then
          Move(AndroidTouch[I+1],AndroidTouch[I],SizeOf(AndroidTouch[0])*(AndroidTouchCount-I-1));
        Dec(AndroidTouchCount);
      end
      else
      begin
        AndroidTouch[I].X := Ix;
        AndroidTouch[I].Y := Iy;
      end;
      Found := True;
      Break;
    end;
  end;
  if (not Found) and (not IsUp) and (AndroidTouchCount<High(AndroidTouch)) then
  begin
    Inc(AndroidTouchCount);
    AndroidTouch[AndroidTouchCount-1].Id := Id;
    AndroidTouch[AndroidTouchCount-1].X := Ix;
    AndroidTouch[AndroidTouchCount-1].Y := Iy;
  end;
end;

procedure Java_org_zgameeditor_Zge_NativeKeydown( env : PJNIEnv; thiz : jobject; Keycode : jint);cdecl;
begin
  AndroidKeys[ Ansichar(Keycode and 255) ] := True;
end;

procedure Java_org_zgameeditor_Zge_NativeKeyup( env : PJNIEnv; thiz : jobject; Keycode : jint);cdecl;
begin
  AndroidKeys[ Ansichar(Keycode and 255) ] := False;
end;

function Java_org_zgameeditor_Zge_NativeGetGLBase( env : PJNIEnv; thiz : jobject ) : integer; cdecl;
begin
  Result := Ord(ZApp.GLBase);
  if Result=0 then
    AndroidLog('GLBase: 1.1')
  else
    AndroidLog('GLBase: 2');
end;


exports
  Java_org_zgameeditor_Zge_NativeInit,
  Java_org_zgameeditor_Zge_NativeDestroy,
  Java_org_zgameeditor_Zge_NativeSurfaceCreated,
  Java_org_zgameeditor_Zge_NativeSurfaceChanged,
  Java_org_zgameeditor_Zge_NativeDrawFrame,
  Java_org_zgameeditor_Zge_NativeActivate,
  Java_org_zgameeditor_Zge_NativeTouch,
  Java_org_zgameeditor_Zge_NativeCloseQuery,
  Java_org_zgameeditor_Zge_NativeKeydown,
  Java_org_zgameeditor_Zge_NativeKeyup,
  Java_org_zgameeditor_Zge_NativeSetDataContent,
  Java_org_zgameeditor_Zge_NativeGetGLBase,
  Java_org_zgameeditor_Zge_NativeInitAppFromSDCard,

  JNI_OnLoad;


begin
end.

