# To build all the NWS RFS models
#
# File Name: Makefile
# Author: Raymond.Chui@noaa.gov
# Created: August, 2009

NWSRFS_MODELS = common \
lagk \
apicont \
baseflow \
chanloss \
consuse \
resj \
genStatesResj \
ressngl \
genStatesRessngl \
sarroute \
ssarresv \
ffgGribProduct

#sacsmaht \
# Note: common must be in the 1st place, and ressngl must after lagk
# snow17, sacsma and unithg are re-written in Java, no need here
# chanloss and lagk also re-written in Java, now.
all :
	for model in $(NWSRFS_MODELS); do \
		cd $$model; make clean; make; \
		cd ..; \
	done

clean :
	for model in $(NWSRFS_MODELS); do \
		cd $$model; make clean; \
		cd ..; \
	done

# this target should clean libs for those programs that have libs
# and do same as clean as well
cleanall :
	for model in $(NWSRFS_MODELS); do \
		cd $$model; make cleanlib; \
		cd ..; \
	done

# make model MODEL=model_name
model :
	cd $(MODEL); make clean; make

# make modelclean MODEL=model_name
modelclean :
	cd $(MODEL); make clean

# make modelcleanall MODEL=model_name
modelcleanall :
	cd $(MODEL); make cleanall
