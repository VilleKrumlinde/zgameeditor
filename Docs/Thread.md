# Thread {#Thread}

Allows to execute an expression in a separate thread, independently on other execution. Thread is launched by calling @ref startThread "startThread() function" and is stopped when its expression finishes execution. A thread can be paused by calling the @ref sleep "sleep() function".

The following rules you should take into account when working with threads:

* Do not run hundreds of threads. at the same time.
* Do not call sleep() with a very high number.
* Do not access data that is no longer available from thread.
* You can make no OpenGL calls in a thread, so don't use any render components. This is a limit of the OpenGL API. 
* Any threads you created will keep running until finished, even if you stop your project in the ZGameEditor designer. Check the log window to see when a thread is started and finished. 

See also: @ref startThread "startThread() function", @ref sleep "sleep() function"

## Properties

@dl

@dt Expression
@dd Expression running in its own thread. The special integer variable "param" holds the value passed in the @ref startThread "startThread()" call.

@dlx
