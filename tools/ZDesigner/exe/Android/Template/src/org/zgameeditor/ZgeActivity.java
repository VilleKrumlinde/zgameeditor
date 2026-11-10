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

import android.app.ActionBar;
import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Build;
import android.view.View;
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

        if (Build.VERSION.SDK_INT < 16) {
            this.requestWindowFeature( Window.FEATURE_NO_TITLE );
            getWindow().setFlags( WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN );
        } else { // Jellybean and up
            View decorView = getWindow().getDecorView();
            decorView.setSystemUiVisibility(
                View.SYSTEM_UI_FLAG_IMMERSIVE
                // Set the content to appear under the system bars so that the
                // content doesn't resize when the system bars hide and show.
                | View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                | View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                | View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
                // Hide the nav bar and status bar
                | View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                | View.SYSTEM_UI_FLAG_FULLSCREEN);

            // Remember that you should never show the action bar if the
            // status bar is hidden, so hide that too if necessary.
            ActionBar actionBar = getActionBar();
            if(actionBar != null) {
                actionBar.hide();
            }
        }

        zge = new Zge(this);
        setContentView( zge );

        getWindow().getDecorView().setBackgroundColor(Color.BLACK);
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
        super.onPause();
        zge.onPause();
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
