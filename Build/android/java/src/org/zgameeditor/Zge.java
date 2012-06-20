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
  ZGE for Android is inspired from work by Andrey Kemka and his ZenGL project:
    http://zengl.org/
*/

package org.zgameeditor;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

import android.os.Environment;
import android.app.Activity;
import android.content.Context;
import android.opengl.GLSurfaceView;
import android.text.InputType;
import android.view.*;
import android.view.inputmethod.BaseInputConnection;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputMethodManager;
import android.view.GestureDetector.SimpleOnGestureListener;
import android.view.GestureDetector;
import android.util.Log;

public class Zge extends GLSurfaceView
{
    private native void zglNativeDestroy();
    private native void zglNativeSurfaceCreated( String HomeDirectory );
    private native void zglNativeSurfaceChanged( int width, int height );
    private native void zglNativeDrawFrame();
    private native void zglNativeActivate( boolean Activate );
    private native boolean zglNativeCloseQuery();
    private native void zglNativeTouch( int ID, float X, float Y, float Pressure );
    private native void zglNativeKeyup(int keycode);
    private native void zglNativeKeydown(int keycode);
    private native void zglSetDataContent(byte[] content);

    private boolean IsDestroy;
    private zglCRenderer Renderer;
    private InputMethodManager InputManager;
    private GestureDetector gestureDetector;

    public Zge(Context context)
    {
        super( context );

        System.loadLibrary("zgeandroid");

        //DataDir = context.getFilesDir().getAbsolutePath();
        Renderer = new zglCRenderer();
        setRenderer( Renderer );

        setFocusableInTouchMode( true );
    }

    public void loadEmbeddedData(int id) {
        try {
            java.io.InputStream is = getResources().openRawResource(id);
            byte[] b = new byte[is.available()];
            is.read(b);
            zglSetDataContent(b);
        } catch (Exception e) {
            Log.i("ZgeAndroid", "Failed to load embedded: " + e.toString());
        }
    }

    public Boolean onCloseQuery()
    {
        return zglNativeCloseQuery();
    }

    @Override
    public void onPause()
    {
        super.onPause();
        zglNativeActivate( false );
    }

    @Override
    public void onResume()
    {
        super.onResume();
        zglNativeActivate( true );
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
                    zglNativeTouch( pointerID, event.getX( i ), event.getY( i ), event.getPressure( i ) );
                }
                break;
            }

            case MotionEvent.ACTION_UP:
            {
                int count = event.getPointerCount();
                for ( int i = 0; i < count; i++ )
                {
                    int pointerID = event.getPointerId( i );
                    zglNativeTouch( pointerID, event.getX( i ), event.getY( i ), 0 );
                }
                break;
            }

            case MotionEvent.ACTION_MOVE:
            {
                int count = event.getPointerCount();
                for ( int i = 0; i < count; i++ )
                {
                    int pointerID = event.getPointerId( i );
                    zglNativeTouch( pointerID, event.getX( i ), event.getY( i ), event.getPressure( i ) );
                }
                break;
            }

            case MotionEvent.ACTION_POINTER_DOWN:
            {
                int pointerIndex = ( action & MotionEvent.ACTION_POINTER_INDEX_MASK  ) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT;
                int pointerId = event.getPointerId( pointerIndex );
                zglNativeTouch( pointerId, event.getX( pointerIndex ), event.getY( pointerIndex ), event.getPressure( pointerIndex ) );
                break;
            }

            case MotionEvent.ACTION_POINTER_UP:
            {
                int pointerIndex = ( action & MotionEvent.ACTION_POINTER_INDEX_MASK  ) >> MotionEvent.ACTION_POINTER_INDEX_SHIFT;
                int pointerId = event.getPointerId( pointerIndex );
                zglNativeTouch( pointerId, event.getX( pointerIndex ), event.getY( pointerIndex ), 0 );
                break;
            }
        }

        return true;
    }

    public void Finish()
    {
        zglNativeDestroy();
        ((Activity)getContext()).finish();
        System.exit( 0 );
    }

    @Override
    public boolean onKeyDown( int keyCode, KeyEvent event )
    {
        if ( keyCode == KeyEvent.KEYCODE_BACK )
            if ( zglNativeCloseQuery() )
            {
                IsDestroy = true;
                return true;
            }

        zglNativeKeydown(event.getUnicodeChar());
        return super.onKeyDown( keyCode, event );
    }

    @Override
    public boolean onKeyUp( int keyCode, KeyEvent event )
    {
        zglNativeKeyup(event.getUnicodeChar());
        return super.onKeyUp( keyCode, event );
    }

    class zglCRenderer implements Renderer
    {
        public void onSurfaceCreated( GL10 gl, EGLConfig config )
        {
            String extpath = Environment.getExternalStorageDirectory().getAbsolutePath();
            Log.i("ZgeAndroid", "SurfaceCreated, root: " + extpath);
            zglNativeSurfaceCreated( extpath );
        }

        public void onSurfaceChanged( GL10 gl, int width, int height )
        {
            Log.i("ZgeAndroid", "SurfaceChanged");
            zglNativeSurfaceChanged( width, height );
        }

        public void onDrawFrame( GL10 gl )
        {
            if ( IsDestroy )
                Finish();

            zglNativeDrawFrame();
        }
    }

}
