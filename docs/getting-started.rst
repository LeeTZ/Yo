Getting Started
===============

Installation
------------
The latest version of **Yo** can be download by cloning the git repository:
::

    git clone https://github.com/LeeTZ/Yo.git

The current distributions are available for the Linux, Mac OS X (10.7 and above) on the 32-bit (386) and 64-bit (amd64) x86 processor architectures.


Build Tools
-----------

The latest version of ``g++`` and ``OCaml`` is needed to run **Yo** compiler. 

`Install g\+\+ <http://askubuntu.com/questions/271388/how-to-install-gcc-4-8>`__,

`Install OCaml <https://ocaml.org/docs/install.html>`__.


To provide various manipulation on videos, audio and images, **Yo** depends on the library ``libopenshot`` and ``libopenshot-audio``, which is not available in a Debian package. please follow the `Installation Guide <http://openshot.org/files/libopenshot/InstallationGuide.pdf>`__ to get them installed.


Go to the folder `src/` and run
::

    make

Then the executables needed are built up.

Complie & Run
--------------

Once the dependencies are installed correctly, to complie and run a program, go to the ``bin/`` directory under the **Yo** home folder and run
::

     ./yo.sh yourprogram.yo

A valid **Yo** program ended with extension ``.yo``.
A config file named ``config.ini`` can be put in the same directory with ``yo.sh`` to define the output details of the videos including the width, height, fps(frame per second) and bit rate.

For **Yo** program examples, see next chapter `A Tour with Yo <a-tour-of-yo>`__.

Getting Helps
--------------

For real-time help, ask the helpful yophers or mail to the author `Tiezheng Li <mailto:litiezheng513@gmail.com>`__.

Report bugs using `Github issue tracker <https://github.com/LeeTZ/Yo/issues>`__.
