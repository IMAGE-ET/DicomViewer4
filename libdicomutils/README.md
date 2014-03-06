libDicomUtils
=============

libDicomUtils is a shared library used by DicomViewer4 to open compressed
images, extract tags and open DICOMDIR files. It's based on the great Open
Source, and multiplatform C++ DCMTK library.

Compiling libDicomUtils
=======================

1) Download dcmtk-3.6.0:

ftp://dicom.offis.de/pub/dicom/offis/software/dcmtk/dcmtk360/dcmtk-3.6.0.tar.gz

2) Uncompress:

tar xvfz dcmtk-3.6.0.tar.gz

3) As explained in DCMTK FAQ #13, edit the config/Makefile.def file:

FAQ #13: http://forum.dcmtk.org/viewtopic.php?f=4&t=19&sid=3fe4f140b949978e074a598e232c5b85

cd dcmtk-3.6.0
config/rootconf
./configure --without-libxml --without-openssl --without-libpng --without-libtiff
vim config/Makefile.def 

# Replace the default values with this:

CFLAGS= -fPIC -O2
CXXFLAGS= -fPIC -O2
AR= gcc
ARFLAGS= -shared -o
LIBEXT= so
RANLIB= :

# Save and close

make

# Note: on Ubuntu 12.04 I've got some pthread related errors and I had to do
# this trick:
# After the first make (the one that display the errors), edit the file
# config/Makefile.def again, this time replacing the original LIBS with this:

LIBS = -lpthread -lrt -lnsl 

# switching -lrt with -lpthread

then "make" again.

# Note: if anything went wrong, please refer to http://forum.dcmtk.org

4) Compile with builtindict:

cd dcmdata/libsrc
make builtindict
cd ../..

5) I'm not really sure this step is required, but anyway, let's
do a "make" again.

6) Now it's time to compile libDicomUtils.so
