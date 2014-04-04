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

3) cmake-gui

Select BUILD_SHARED_LIBS, then configure and generate. Then close the app.

!https://github.com/leonardorame/DicomViewer4/blob/master/libdicomutils/cmake.png!

    make

4) Now it's time to compile libDicomUtils.so

You must be at the libdicomutils directory (DicomViewer4/libdicomutils), then just do:

    make dicomutils-2.0.so
    
This will create the shared library libdicomutils-2.0.so
    
5) Done!.
