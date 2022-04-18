rm model-src.cpython*
cd ../model-source
f2py -m model_src -c types.f90 sorting.f90 stats.f90 utilities.f90 FA.f90 lagk_run.f90 sac_snow_only_tci.f90 sac_snow_states.f90 consuse_run.f90 chanloss.f90 fintp7.f fka7.f flag7.f fop7.f fserc7.f fslag7.f pin7.f pina7.f umemov.f umemst.f PACK19.f exsnow19.f aesc19.f rout19.f zero19.f duamel.f sac1.f ex_sac1.f melt19.f ex57.f
mv model_src.cpython* ../py-rfchydromodels/utilities