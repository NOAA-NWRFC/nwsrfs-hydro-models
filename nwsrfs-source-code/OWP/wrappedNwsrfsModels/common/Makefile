#gcc_version=`/usr/bin/gcc --version | head -1 | cut -d ' ' -f3`; \
 set -vx
PGM = common

all: libs

libs:
	mkdir -p lib
	gcc_version=`/usr/bin/gcc --version | head -1 | cut -d ' ' -f3`; \
	top_roof=`pwd | sed s/$(PGM)//`; \
	cd src; \
	lssrc_c=`ls *.c 2> /dev/null`; src_c=`echo $$lssrc_c`; \
	lssrc_f=`ls *.f 2> /dev/null`; src_f=`echo $$lssrc_f`; \
	make SRC_C="$$src_c" SRC_F="$$src_f" TOBUILD=libs TOP_ROOF=$$top_roof GCC_VERSION="$$gcc_version"; \
	cd ..;

cleanlib:
	rm -f lib/libcommon.a lib/libnwsrfs.a 

clean:
	rm -f lib/libcommon.a lib/libnwsrfs.a
cleanall :
	true
