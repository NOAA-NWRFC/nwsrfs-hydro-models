.SUFFIXES : .a .o .cxx .c .f

#SRC_C =
#SRC_CC =
#SRC_F = 
#TOBUILD =
# type lssrc_c=`ls *.c 2> /dev/null`; src_c=`echo $$lssrc_c`
# type lssrc_cc=`ls *.cxx 2> /dev/null`; src_cc=`echo $$lssrc_cc`
# type lssrc_f=`ls *.f 2> /dev/null`; src_f=`echo $$lssrc_f`

# type make SRC_C="$$src_c" SRC_CC="$$src_cc" SRC_F="$$src_f" TOBUILD=libs
# or
# type make SRC_C="$$src_c" SRC_CC="$$src_cc" SRC_F="$$src_f" TOBUILD=objs

OBJ_CC = ${SRC_CC:.cxx=.o}
OBJ_C = ${SRC_C:.c=.o}
OBJ_F = ${SRC_F:.f=.o}

LIBDIR = ../../lib
LIB = $(LIBDIR)/libresj.a
LIBOBJ = $(LIB)($(OBJ_CC)) \
$(LIB)($(OBJ_C)) \
$(LIB)($(OBJ_F))

INCLUDE = ../../include
COMMON_SRC = ../../../common/src
COMMON = ../../../common

FC       = /fs/shared/NEWopt/Linux/pgi724/linux86/7.2/bin/pgf90
FDEBUG   = -g -fpic -g77libs
FFLAGS   = $(FDEBUG) -Mbackslash -Mpreprocess

CC       = /usr/bin/gcc
CDEBUG   = -gstabs -fpic
CFLAGS   = $(CDEBUG) -fno-strict-aliasing -DUNIX -DGRTYPE_PS -DGRTYPE_XWIN -DLINX -DX1D -DTIMER -D__USE_GNU -DUSE_XM_MWMUTIL_H -DUSE_XGPM -Wall -Wno-unused-variable -Wno-comment


CXX      = /usr/bin/g++ -Wno-deprecated
CXXDEBUG = -gstabs -fpic
CXXFLAGS = $(CXXDEBUG) -fno-strict-aliasing -DUNIX -DGRTYPE_PS -DGRTYPE_XWIN -DLINX -DX1D -DTIMER -D__USE_GNU

INSERTS      = -I$(COMMON)/include/commonblocks \
               -I$(INCLUDE)/common \
               -I$(INCLUDE)

INSERTS_FC   = -I/fs/shared/NEWopt/Linux/pgi724/linux86/7.2/include \
               -I$(INCLUDE)/common \
               -I$(COMMON)/include/commonblocks

INSERTS_CC   = -I/usr/include/informix/esql  \
               -I/usr/X11R6/include  \
               -I/usr/include  \
               -I/usr/lib/gcc/i386-redhat-linux/3.4.6/include  \
               -I/usr/local/netcdf/include  \
               -I/usr/local/jdk-1.5.0_04/include  \
               -I/usr/local/jdk-1.5.0_04/include/linux  \
               -I$(COMMON)/include  \
               -I$(INCLUDE)  \
               -I$(INCLUDE)/common \
               -I$(INCLUDE)/resj

	       
INSERTS_CXX  = -I/usr/X11R6/include  \
               -I/usr/include  \
               -I/usr/include/informix/esql  \
               -I/usr/lib/gcc/i386-redhat-linux/3.4.6/include  \
               -I/usr/local/netcdf/include  \
               -I$(INCLUDE) \
               -I$(INCLUDE)/resj \
               -I$(COMMON)/include

.f.a :
	 $(FC) $(FFLAGS) $(INSERTS_FC) $(INSERTS) -c $<
	ar -rv $(LIB) $*.o
	rm -f $*.o

.f.o :
	 $(FC) $(FFLAGS) $(INSERTS_FC) $(INSERTS) -c $<

.cxx.a :
	$(CXX) $(CXXFLAGS) $(INSERTS_CXX) $(INSERTS) -c $<
	ar -rv $(LIB) $*.o
	rm -f $*.o

.cxx.o :
	$(CXX) $(CXXFLAGS) $(INSERTS_CXX) $(INSERTS) -c $<

.c.a :
	$(CC) $(CFLAGS) $(INSERTS_CC) $(INSERTS) -c $<
	ar -rv $(LIB) $*.o
	rm -f $*.o

.c.o :
	$(CC) $(CFLAGS) $(INSERTS_CC) $(INSERTS) -c $<

all: ${TOBUILD}

objs: ${OBJ_C} ${OBJ_CC} ${OBJ_F}

libs: ${LIBOBJ} 

clean:
	rm -f ${OBJ_C} ${OBJ_CC} ${OBJ_F} 
