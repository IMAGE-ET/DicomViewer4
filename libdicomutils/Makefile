#
#	Makefile for dicomutils
#

SHELL = /bin/sh
DEFINES = -DHAVE_CONFIG_H 
CXXFLAGS = -O2 -static -Wall -shared -pedantic $(DEFINES)
#CXXFLAGS = $(DEFINES)

# Windows con MingW/MSYS
#ROOTPATH = dcmtk

# Linux
ROOTPATH = ~/devel/dcmtk-3.6.1_20131114


INCLUDES = -I$(ROOTPATH)/config/include \
    -I$(ROOTPATH)/ofstd/include \
    -I$(ROOTPATH)/oflog/include \
    -I$(ROOTPATH)/dcmimgle/include \
    -I$(ROOTPATH)/dcmimage/include \
    -I$(ROOTPATH)/dcmjpeg/include \
    -I$(ROOTPATH)/dcmtls/include \
    -I$(ROOTPATH)/dcmsign/include \
    -I$(ROOTPATH)/dcmqrdb/include \
    -I$(ROOTPATH)/dcmdata/include \
    -I$(ROOTPATH)/dcmnet/include \

LIBRERIAS = -L$(ROOTPATH)/config \
    -L$(ROOTPATH)/ofstd/libsrc \
    -L$(ROOTPATH)/oflog/libsrc \
    -L$(ROOTPATH)/dcmjpeg/libsrc \
    -L$(ROOTPATH)/dcmjpeg/libijg8 \
    -L$(ROOTPATH)/dcmjpeg/libijg12 \
    -L$(ROOTPATH)/dcmjpeg/libijg16 \
    -L$(ROOTPATH)/dcmimgle/libsrc \
    -L$(ROOTPATH)/dcmtls/libsrc \
    -L$(ROOTPATH)/dcmsign/libsrc \
    -L$(ROOTPATH)/dcmimage/libsrc \
    -L$(ROOTPATH)/dcmqrdb/libsrc \
    -L$(ROOTPATH)/dcmdata/libsrc \
    -L$(ROOTPATH)/dcmnet/libsrc 

LOCALLIBS =  -ldcmjpeg \
    -lijg8 \
    -lijg12 \
    -lijg16 \
    -ldcmimage \
    -ldcmimgle \
    -ldcmnet \
    -ldcmdata \
    -loflog \
    -lofstd \
    -lpthread \
		-lz # -lwsock32 -lnetapi32 

OBJECTS = $(ROOTPATH)/oflog/libsrc/*.o \
        $(ROOTPATH)/ofstd/libsrc/*.o \
        $(ROOTPATH)/dcmjpeg/libsrc/*.o \
        $(ROOTPATH)/dcmjpeg/libijg8/*.o \
        $(ROOTPATH)/dcmjpeg/libijg12/*.o \
        $(ROOTPATH)/dcmjpeg/libijg16/*.o \
        $(ROOTPATH)/dcmimage/libsrc/*.o \
        $(ROOTPATH)/dcmimgle/libsrc/*.o \
        $(ROOTPATH)/dcmdata/libsrc/*.o \
        $(ROOTPATH)/dcmnet/libsrc/*.o
	
dicomutils-2.0-so: dicomutils-2.0.o
	g++ -fPIC -O3 -shared -o libdicomutils-2.0.so dicomutils-2.0.o $(LIBRERIAS) $(OBJECTS) $(LOCALLIBS) 

dicomutils-2.0.o: dicomutils-2.0.cpp
	g++ $(DEFINES) -fPIC $(INCLUDES) $(LOCALLIBS) $(LIBRERIAS) -c dicomutils-2.0.cpp -g

clean:
	-rm *.o


