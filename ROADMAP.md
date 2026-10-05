# nwsrfs-hydro-models Development Roadmap


## 1.1 
- Repo mantinance 
- Consoladate branches and bug fixes 

## 1.2 (November/December 2026)
- Modularity of the R/python interface
  - R pipe support %>%, |> 
- Split fortran subroutines
  - Sac only, sac/snow coupled
  - Snow is coupled but called modularly to support other models
- BMI compliant config file
  - Only existing features: time_step, run_type, models, etc.

## 1.3 
- Flexible time step
  - Support (15min, 30 min), 1, 3, 6, 12, 24

## 1.4
- Hindcast support
  - Warm start 

# 1.5 
- Autocalb in nwsrfsr/py
  - Finessing 
  - autocalb becomes template/workflow/example repo 

## 2.0
- Reorganization/refactor/rethink
  - Requires touching autocalb as well
- Support compiled callable external subroutines 
