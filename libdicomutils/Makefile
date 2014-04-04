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
ROOTPATH = ../../dcmtk-3.6.1_20131114


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

LIBRERIAS = -L$(ROOTPATH)/lib

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

dicomutils-2.0-so: dicomutils-2.0.o
	g++ -fPIC -O3 -shared -o libdicomutils-2.0.so dicomutils-2.0.o $(LIBRERIAS) $(LOCALLIBS) 

dicomutils-2.0.o: dicomutils-2.0.cpp
	g++ $(DEFINES) -fPIC $(INCLUDES) $(LIBRERIAS) $(LOCALLIBS) -c dicomutils-2.0.cpp -g

clean:
	-rm *.o


