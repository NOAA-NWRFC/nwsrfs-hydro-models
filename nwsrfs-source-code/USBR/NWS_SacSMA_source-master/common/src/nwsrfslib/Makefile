INCLUDEFILE = ../../../MakefileTemplate
include ${INCLUDEFILE}

INSERTS = -I../../include  \
           -I../../include/commonblocks

LIBDIR = ../../lib
LIB = $(LIBDIR)/libnwsrfs.a
LIBOBJ = $(LIB)($(OBJ_F)) \
$(LIB)($(OBJ_C))

all: ${TOBUILD}

objs: ${OBJ_C} ${OBJ_CC} ${OBJ_F}

libs: ${LIBOBJ}

clean:
	rm -f ${OBJ_C} ${OBJ_CC} ${OBJ_F}

cleanlib:
	rm -f $(LIB)
