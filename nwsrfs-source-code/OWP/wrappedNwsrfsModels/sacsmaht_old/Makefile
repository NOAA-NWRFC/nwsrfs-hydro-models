#  ============================================================================
#	Makefile to make objs and binary frz_sce_sls
#  ============================================================================
 
PGM =sacsmaht

make:  
	make -f Makefile_obj_lx
	make -f Fastmake_for_sacsmaht
	# copy the executable to the Modules bin directory for testing through FEWS
	-mkdir -p ../../ohdfewsadapter/Modules/bin 
	-cp -fp bin/$(PGM) ../../ohdfewsadapter/Modules/bin

clean: 
	make -f Makefile_obj_lx clean
	make -f Fastmake_for_sacsmaht clean
	# rm the executable from the Modules directory used for testing through FEWS
	-rm -rf ../../ohdfewsadapter/Modules/bin/$(PGM)
