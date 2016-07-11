
A Tour of Yo
=============

Welcome to the world of Yo! Here we are going to produce a movie clip with only a few lines of codes.

Example for impatients
----------------------

Although video editting requires visualization in user interface to be more precise and easy to operate for movie makers, there are some tasks that are more suitable for a programming solution rather than doing it manually. For example, there may be a video editting task that requires a lot of repeatable but trivial operations such as concatenate many photo frames to get a time-elapse video. Or there is an analytical task where we want to cut off all black frames and silent part from a very long video. The logic is pretty straightforward but no video editting software could provide an easy and quick solution for these kind of tasks. That is why we proposed **Yo**.


A time elapse video
~~~~~~~~~~~~~~~~~~~
The following program creates a time-elapse video with thousands of photographs with only 5 lines of code:

::

    photos = Clip[]("photo/")       # read all pics in photos/ and create a array of clips 
    mymovie = Clip()                # create mymovie for final rendering
    for p in photos:                # set the playing time of every pic as 1 frame   
        mymovie = mymovie & p[1:2]  # and concatenate it to the end of the main clip
    mymovie.save("timelapse.webm")  # render and save the file 


Features
---------
Features in **video editting level**:

* Video and Audio cropping and concatenating
* Video and Audio Effects (Chroma Key, Color Adjustment, Grayscale, etc…)
* Multi­Layer Compositing
* Animation Curves (Bézier, Linear, Constant)
* Time Mapping (Curve­based Slow Down, Speed Up, Reverse)
* Audio Mixing & Resampling
* Frame Rate Conversions
* Multi­Processor Support (Performance)
* Unit Tests (Stability)
* All FFmpeg Formats and Codecs Supported (Images, Videos, and Audio files)


Features in **language level**:

* Basic arithemtic, boolean, string operations and control flow (condition / loop)
* Use indent as blocks
* Type inference 
* Object-oriented
* User-defined type and function supported
* Lambda calculation
* Built-in functions for file system, video rendering etc.
* Competiable with C++ libraries imported 
