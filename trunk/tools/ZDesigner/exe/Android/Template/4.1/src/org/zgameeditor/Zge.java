/* Copyright (c) 2012 Ville Krumlinde

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
THE SOFTWARE. */

/*
  Thanks to Andrey Kemka and his ZenGL project for help with Android/Fpc techniques:
    http://zengl.org/

	Support for Android game pads implemented by Radovan Cervenka.
*/

package org.zgameeditor;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;
import java.io.InputStream;
import java.util.*;

import android.os.Environment;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.res.AssetManager;
import android.opengl.GLSurfaceView;
import android.net.Uri;
import android.view.*;
import android.view.inputmethod.InputMethodManager;
import android.view.GestureDetector;
import android.util.Log;

public class Zge extends GLSurfaceView
{
    private native void NativeInit(String ExtPath, String DataPath, String LibraryPath);
    private native void NativeDestroy();
    private native void NativeSurfaceCreated();
    private native void NativeSurfaceChanged( int width, int height );
    private native boolean NativeDrawFrame();
    private native void NativeActivate( boolean Activate );
    private native boolean NativeCloseQuery();
    private native void NativeTouch( int ID, float X, float Y, float Pressure );
    private native void NativeKeyup(int keycode);
    private native void NativeKeydown(int keycode);
    private native void NativeSetDataContent(byte[] content);
    private native int NativeGetGLBase();
    private native void NativeInitAppFromSDCard();
    private native void NativeSetJoyButton(int joyId, int buttonNr, boolean value);
    private native void NativeSetJoyAxisValue(int joyId, int axisNr, float value);

    private boolean IsDestroy;
    private CRenderer Renderer;
    private InputMethodManager InputManager;
    private GestureDetector gestureDetector;

    private String DataPath;
    private String LibraryPath;

    public Zge(Context context)
    {
        super( context );

        System.loadLibrary("zgeandroid");

        String dataPath = context.getFilesDir().getAbsolutePath() + "/";
        String libraryPath = getContext().getApplicationInfo().dataDir + "/lib/";
        String extPath = Environment.getExternalStorageDirectory().getAbsolutePath();
        NativeInit(extPath, dataPath, libraryPath);

        initZApp();

        if(NativeGetGLBase()==1) {
          Log.i("ZgeAndroid", "GLES 2");
          setEGLContextClientVersion(2);
        }
        Renderer = new CRenderer();
        setRenderer( Renderer );

        setFocusableInTouchMode( true );
    }

    private void initZApp() {
        //Tries to load embedded data. If none present, then it will look for zzdc.dat on external path instead
        byte[] b = openAssetFile("/assets/zzdc.dat");
        if(b!=null)
            NativeSetDataContent(b);
        else
            NativeInitAppFromSDCard();
    }

    //Called from native
    public void openURL( String url ) {
        Log.i("ZgeAndroid", "About to open: " + url);
        Uri uriUrl = Uri.parse( url );
        Intent intent = new Intent(Intent.ACTION_VIEW, uriUrl);
        intent.setFlags( Intent.FLAG_ACTIVITY_NEW_TASK );
        getContext().startActivity( intent );
    }

    //Called from native
    public byte[] openAssetFile(String name) {
        name=name.substring(8); //strip "/assets/" from name
        Log.i("ZgeAndroid", "About to open: " + name);
        AssetManager assets = getContext().getAssets();
        byte[] data=new byte[0]; //Return zero length to native if file not available
        try {
            InputStream stream = assets.open(name,AssetManager.ACCESS_BUFFER);
            int size = stream.available();
            Log.i("ZgeAndroid", "Open ok, available: " + size);
            data = new byte[size];
            stream.read(data);
            stream.close();
        } catch (Exception e) {
            Log.e("ZgeAndroid",e.getMessage());
        }
        return data;
    }

    public Boolean onCloseQuery()
    {
        return NativeCloseQuery();
    }

    public void onStop() {
        Log.i("ZgeAndroid", "stop");
        NativeActivate( false );
        //Call drawframe to update zge and give expressions a chance to catch stop event
        NativeDrawFrame();
    }

    public void onStart() {
        Log.i("ZgeAndroid", "start");
        NativeActivate( true );
    }

    @Override
    public void onPause()
    {
        super.onPause();
    }

    @Override
    public void onResume()
    {
        super.onResume();
    }

    @Override
    public boolean onTouchEvent( MotionEvent event )
    {
        int action = event.getAction();
        int actionType = action & MotionEvent.ACTION_MASK;
        //Log.i("ZgeAndroid", "Action: " + action + " actiontype: " + actionType);

        switch ( actionType )
        {
            case MotionEvent.ACTION_DOWN:
            {
                int count = event.getPointerCount();
                for ( int i = 0; i < count; i++ )
                {
                    int pointerID = event.getPointerId( i );
                    NativeTouch( pointerID, event.getX( i ), event.getY( i ), event.getPressure( i ) );
                }
                break;
            }

            case MotionEvent.ACTION_UP:
            {
                int count = event.getPointerCount();
                for ( int i = 0; i < count; i++ )
                {
                    int pointerID = event.getPointerId( i );
                    NativeTouch( pointerID, event.getX( i ), event.getY( i ), 0 );
                }
                break;
            }

            case MotionEvent.ACTION_MOVE:
            {
                int count = event.getPointerCount();
                for ( int i = 0; i < count; i++ )
                {
                    int pointerID = event.getPointerId( i );
                    NativeTouch( pointerID, event.getX( i ), event.getY( i ), event.getPressure( i ) );
                }
                break;
            }

            case MotionEvent.ACTION_POINTER_DOWN:
            {
                int pointerIndex = ( action & MotionEvent.ACTION_POINTER_INDEX_MASK  ) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT;
                int pointerId = event.getPointerId( pointerIndex );
                NativeTouch( pointerId, event.getX( pointerIndex ), event.getY( pointerIndex ), event.getPressure( pointerIndex ) );
                break;
            }

            case MotionEvent.ACTION_POINTER_UP:
            {
                int pointerIndex = ( action & MotionEvent.ACTION_POINTER_INDEX_MASK  ) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT;
                int pointerId = event.getPointerId( pointerIndex );
                NativeTouch( pointerId, event.getX( pointerIndex ), event.getY( pointerIndex ), 0 );
                break;
            }
        }

        return true;
    }

    public void Finish()
    {
        NativeDestroy();
        ((Activity)getContext()).finish();
        System.exit( 0 );
    }

    // this can be removed for Android API level 19,
    // because InputDevice.getControllerNumber() can be used instead
    private static final int MAX_NUMBER_OF_DEVICES = 4;
    private static int[] devices = new int[MAX_NUMBER_OF_DEVICES];
    private static int lastDevice = 0;
    private static int getControllerNumber(int deviceId) {
      for ( int i = 0; i < lastDevice; ++i )
        if ( devices[i] == deviceId ) return i;

      if ( lastDevice == MAX_NUMBER_OF_DEVICES) return -1;

      devices[lastDevice++] = deviceId;
      return lastDevice - 1;
    }

    // mapping of Android API button codes to ZGE button codes
    @SuppressWarnings("serial")
    static final Map<Integer, Integer> BUTTON_CODES =
      Collections.unmodifiableMap(new HashMap<Integer, Integer>() {{
        put(KeyEvent.KEYCODE_BUTTON_A, 0); // OuyaController.BUTTON_O
        put(KeyEvent.KEYCODE_BUTTON_X, 1); // OuyaController.BUTTON_U
        put(KeyEvent.KEYCODE_BUTTON_Y, 2); // OuyaController.BUTTON_Y
        put(KeyEvent.KEYCODE_BUTTON_B, 3); // OuyaController.BUTTON_A
        put(KeyEvent.KEYCODE_BUTTON_L1, 4); // OuyaController.BUTTON_L1
        put(KeyEvent.KEYCODE_BUTTON_L2, 5); // OuyaController.BUTTON_L2
        put(KeyEvent.KEYCODE_BUTTON_THUMBL, 6); // OuyaController.BUTTON_L3
        put(KeyEvent.KEYCODE_BUTTON_R1, 7); // OuyaController.BUTTON_R1
        put(KeyEvent.KEYCODE_BUTTON_R2, 8); // OuyaController.BUTTON_R2
        put(KeyEvent.KEYCODE_BUTTON_THUMBR, 9); // OuyaController.BUTTON_R3
        put(KeyEvent.KEYCODE_DPAD_UP, 10); // OuyaController.BUTTON_DPAD_UP
        put(KeyEvent.KEYCODE_DPAD_DOWN, 11); // OuyaController.BUTTON_DPAD_DOWN
        put(KeyEvent.KEYCODE_DPAD_LEFT, 12); // OuyaController.BUTTON_DPAD_LEFT
        put(KeyEvent.KEYCODE_DPAD_RIGHT, 13); // OuyaController.BUTTON_DPAD_RIGHT
        put(KeyEvent.KEYCODE_MENU, 14); // OuyaController.BUTTON_MENU

        put(KeyEvent.KEYCODE_BUTTON_C, 15);
        put(KeyEvent.KEYCODE_BUTTON_Z, 16);
        put(KeyEvent.KEYCODE_BUTTON_MODE, 17);
        put(KeyEvent.KEYCODE_BUTTON_SELECT, 18);
        put(KeyEvent.KEYCODE_BUTTON_START, 19);
        put(KeyEvent.KEYCODE_BUTTON_1, 20);
        put(KeyEvent.KEYCODE_BUTTON_2, 21);
        put(KeyEvent.KEYCODE_BUTTON_3, 22);
        put(KeyEvent.KEYCODE_BUTTON_4, 23);
        put(KeyEvent.KEYCODE_BUTTON_5, 24);
        put(KeyEvent.KEYCODE_BUTTON_6, 25);
        put(KeyEvent.KEYCODE_BUTTON_7, 26);
        put(KeyEvent.KEYCODE_BUTTON_8, 27);
        put(KeyEvent.KEYCODE_BUTTON_9, 28);
        put(KeyEvent.KEYCODE_BUTTON_10, 29);
        put(KeyEvent.KEYCODE_BUTTON_11, 30);
        put(KeyEvent.KEYCODE_BUTTON_12, 31);
      }});

    private static int getJoyButtonCode(int keyCode) {
      Integer result = BUTTON_CODES.get(keyCode);
      return result != null ? result.intValue() : -1;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {

      if ( event.getRepeatCount() == 0) {
        // handle KeyBack in ZGE
        if (keyCode == KeyEvent.KEYCODE_BACK) {
          NativeKeydown(253); // KeyBack code in ZGE
          if (NativeCloseQuery()) IsDestroy = true;
          return true;
        }

        if ( (event.getSource() & InputDevice.SOURCE_CLASS_BUTTON) != 0 )
          // button down
          NativeSetJoyButton(getControllerNumber(event.getDeviceId()), getJoyButtonCode(keyCode), true);
        else
          // key down
          NativeKeydown(event.getUnicodeChar());
      }

      return true;//super.onKeyDown(keyCode, event);
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {

      // handle KeyBack in ZGE
      if (keyCode == KeyEvent.KEYCODE_BACK) {
        NativeKeydown(253); // KeyBack code in ZGE
        if (NativeCloseQuery()) IsDestroy = true;
        return true;
      }

      if ( (event.getSource() & InputDevice.SOURCE_CLASS_BUTTON) != 0 )
        // button up
        NativeSetJoyButton(getControllerNumber(event.getDeviceId()), getJoyButtonCode(keyCode), false);
      else
        // key up
        NativeKeyup(event.getUnicodeChar());

      return true;//super.onKeyUp(keyCode, event);
    }

    @Override
    public boolean onGenericMotionEvent(final MotionEvent event) {

      // joystick
      if ( (event.getSource() & InputDevice.SOURCE_CLASS_JOYSTICK ) != 0) {

        int controllerNb = getControllerNumber(event.getDeviceId());

        NativeSetJoyAxisValue(controllerNb,
            0, // MotionEvent.AXIS_X or OuyaController.AXIS_LS_X
            event.getAxisValue(MotionEvent.AXIS_X));

        NativeSetJoyAxisValue(controllerNb,
            1, // MotionEvent.AXIS_Y or OuyaController.AXIS_LS_Y
            event.getAxisValue(MotionEvent.AXIS_Y));

        NativeSetJoyAxisValue(controllerNb,
            2, // MotionEvent.AXIS_Z or OuyaController.AXIS_RS_X
            event.getAxisValue(MotionEvent.AXIS_Z));

        NativeSetJoyAxisValue(controllerNb,
            3, // MotionEvent.AXIS_RZ or OuyaController.AXIS_RS_Y
            event.getAxisValue(MotionEvent.AXIS_RZ));

        NativeSetJoyAxisValue(controllerNb,
            4, // MotionEvent.AXIS_LTRIGGER or OuyaController.AXIS_L2
            event.getAxisValue(MotionEvent.AXIS_LTRIGGER));

        NativeSetJoyAxisValue(controllerNb,
            5, // MotionEvent.AXIS_RTRIGGER or OuyaController.AXIS_R2
            event.getAxisValue(MotionEvent.AXIS_RTRIGGER));

        NativeSetJoyAxisValue(controllerNb,
            6, // MotionEvent.AXIS_LTRIGGER or OuyaController.AXIS_L2
            event.getAxisValue(MotionEvent.AXIS_RX));

        NativeSetJoyAxisValue(controllerNb,
            7, // MotionEvent.AXIS_RTRIGGER or OuyaController.AXIS_R2
            event.getAxisValue(MotionEvent.AXIS_RY));

        return true;
      }

      return false;
    }

    class CRenderer implements Renderer
    {
        public void onSurfaceCreated( GL10 gl, EGLConfig config )
        {
            Log.i("ZgeAndroid", "SurfaceCreated: " + gl.glGetString( GL10.GL_VERSION ));
            NativeSurfaceCreated();
        }

        public void onSurfaceChanged( GL10 gl, int width, int height )
        {
            Log.i("ZgeAndroid", "SurfaceChanged");
            NativeSurfaceChanged( width, height );
        }

        public void onDrawFrame( GL10 gl )
        {
            if(NativeDrawFrame())
              IsDestroy=true;

            if ( IsDestroy )
                Finish();
        }
    }

}
