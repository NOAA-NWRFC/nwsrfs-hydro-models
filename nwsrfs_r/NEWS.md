# nwsrfsr 1.0.2

* Fix installation failure under parallel make (`make -j`) by declaring the
  missing Fortran module dependencies (`sorting.o: types.o` and
  `sac_snow.o: utilities.o`) in `src/Makevars` and `src/Makevars.win`.

# nwsrfsr 1.0.0

* Initial CRAN release
* Low-level Fortran wrappers: sac_snow(), uh(), lagk(), chanloss(), consuse(), fa_nwrfc()
* High-level orchestration: nwsrfs_run(), load_example(), update_pars()
* AdjustQ preprocessing: adjustq(), adjustq_load_example()
* Bundled example data for NRKW1 and SFLN2 stations
