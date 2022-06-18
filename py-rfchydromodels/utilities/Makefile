.ONESHELL:

name	=model_src
ffile	=types.f90 sorting.f90 stats.f90 utilities.f90 uh_optim.f90 uh2p_get_scale.f90 FA.f90 lagk_run.f90 sac_snow.f90 consuse_run.f90 chanloss.f90 rsnwelev_run.f90 initCommonBlocksPin.f fintp7.f fka7.f flag7.f fop7.f fserc7.f fslag7.f pin7.f pina7.f umemov.f umemst.f PACK19.f exsnow19.f aesc19.f rout19.f zero19.f duamel.f sac1.f ex_sac1.f melt19.f ex57.f ex42.f
options	=--quiet

all:
	find . -name "$(name)*" -delete
	cd ../model-source
	f2py $(options) -m $(name) -c $(ffile)
	mv $(name)* ../py-rfchydromodels/utilities