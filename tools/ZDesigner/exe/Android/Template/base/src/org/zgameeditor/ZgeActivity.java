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

import android.app.Activity;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;

public class ZgeActivity extends Activity
{
    public static ZgeActivity zgeActivity;
    public Zge zge;

    public void onCreate( Bundle savedInstanceState )
    {
        //Set static member so that jni methods in external libraries can get hold of activity
        zgeActivity = this;

        super.onCreate( savedInstanceState );

        this.requestWindowFeature( Window.FEATURE_NO_TITLE );
        getWindow().setFlags( WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN );

        zge = new Zge(this);
        setContentView( zge );
    }

    @Override
    public void onDestroy()
    {
        //Remove global handle so that we do not get in the way of garbage collection
        zgeActivity = null;
    }

    @Override
    protected void onPause()
    {
        zge.onPause();
        super.onPause();
    }

    @Override
    protected void onResume()
    {
        super.onResume();
        zge.onResume();
    }

    @Override
    protected void onStop() {
        zge.onStop();
        super.onStop();
    }

    @Override
    protected void onStart() {
        zge.onStart();
        super.onStart();
    }

}
