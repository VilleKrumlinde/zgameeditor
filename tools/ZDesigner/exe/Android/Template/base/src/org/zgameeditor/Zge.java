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
*/

package org.zgameeditor;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;
import java.io.InputStream;

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

        //The lines below are needed to get alpha channel
        //Source: https://github.com/Wizcorp/Ejecta-X/issues/13
        setZOrderOnTop(true);
        setEGLConfigChooser(8, 8, 8, 8, 16, 0);
        getHolder().setFormat(android.graphics.PixelFormat.RGBA_8888);

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
        if(b.length!=0)
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

    @Override
    public boolean onKeyDown( int keyCode, KeyEvent event ) {
        if ((keyCode == KeyEvent.KEYCODE_BACK) && (event.getRepeatCount()== 0)) {
            NativeKeydown(253); //Trigger KeyBack in ZGE
            if ( NativeCloseQuery() )
                IsDestroy = true;
            return true;
        } else
            NativeKeydown(event.getUnicodeChar());

        return super.onKeyDown( keyCode, event );
    }

    @Override
    public boolean onKeyUp( int keyCode, KeyEvent event )
    {
        NativeKeyup(event.getUnicodeChar());
        return super.onKeyUp( keyCode, event );
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
