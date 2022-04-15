INCLUDEFILE = ../../MakefileTemplate
include ${INCLUDEFILE}

LIBDIR = ../lib
LIB = $(LIBDIR)/libcommon.a
LIBOBJ = $(LIB)($(OBJ_F)) \
$(LIB)($(OBJ_C))

all: ${TOBUILD}

objs: ${OBJ_C} ${OBJ_CC} ${OBJ_F}

libs: ${LIBOBJ}
	cd nwsrfslib; \
	lssrc_c=`ls *.c 2> /dev/null`; src_c=`echo $$lssrc_c`; \
	lssrc_f=`ls *.f 2> /dev/null`; src_f=`echo $$lssrc_f`; \
	make SRC_C="$$src_c" SRC_F="$$src_f" TOBUILD=libs; \
	cd ..;

clean:
	rm -f ${OBJ_C} ${OBJ_CC} ${OBJ_F} 

cleanlib:
	rm -f $(LIB)
	cd nwsrfslib; make cleanlib 
