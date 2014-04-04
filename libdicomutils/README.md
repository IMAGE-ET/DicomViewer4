libDicomUtils
=============

libDicomUtils is a shared library used by DicomViewer4 to open compressed
images, extract tags and open DICOMDIR files. It's based on the great Open
Source, and multiplatform C++ DCMTK library.

Prerequisites
--------------

1) CMake and CMake Gui. On Debian based systems:

    sudo apt-get install cmake cmake-gui

Compiling libDicomUtils
-----------------------

1) Download dcmtk's latest snapshot

    http://dicom.offis.de/download/dcmtk/snapshot/dcmtk-3.6.1_20131114.tar.gz

2) Uncompress:
    
    tar xvfz dcmtk-3.6.1_20131114.tar.gz

3) cd dcmtk-3.6.1_20131114

3) cmake-gui

Select BUILD_SHARED_LIBS, then configure and generate. Then close the app.

![Image](https://raw.githubusercontent.com/leonardorame/DicomViewer4/master/libdicomutils/cmake.png)

    make

4) The last step will create the "lib" directory containing dcmtk's shared
libraries. On Linux systems you must copy or create a symbolic link to that directory:

    sudo ln -s /home/martin/devel/dcmtk-3.6.1_20131114/lib/*.so.3 /usr/local/lib/dcmtk/    

5) Add the /usr/local/lib/dcmtk directory to ld.conf (as root):

    touch /etc/ld.so.conf.d/dcmtk.conf
    echo "/usr/local/lib/dcmtk" > /etc/ld.so.conf.d/dcmtk.conf

6) Rebuild the ldconfig cache:

    sudo ldconfig

7) Now it's time to compile libDicomUtils.so

You must be at the libdicomutils directory (DicomViewer4/libdicomutils), then just do:

    make dicomutils-2.0
    
This will create the shared library libdicomutils-2.0.so
    
5) Done!.
