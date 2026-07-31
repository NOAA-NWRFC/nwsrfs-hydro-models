project('nwsrfspy', 'c', 'fortran',
  version : '1.0.2',
  meson_version: '>= 1.1.0',
  default_options : [
    'warning_level=1',
    'optimization=2',
    'b_lto=false'
  ],
)

# Disable floating-point expression contraction so model results agree across
# platforms. Compilers may fuse a*b+c into a fused multiply-add with a single
# rounding; whether they do varies by compiler version and CPU, and in this
# iterated single-precision model chain the one-ulp differences are amplified
# into visibly different simulations. The R package applies the same flag (see
# its configure script), so both wrappers produce matching results everywhere.
# get_supported_arguments drops the flag on compilers that reject it.
fc = meson.get_compiler('fortran')
add_project_arguments(fc.get_supported_arguments('-ffp-contract=off'), language: 'fortran')

py = import('python').find_installation(pure: false)
module_name = 'nwsrfs_src'

incdir_numpy = run_command(py,
  ['-c', 'import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
  ['-c', 'import numpy.f2py; print(numpy.f2py.get_include())'],
  check : true
).stdout().strip()

# -------------------------
# 1. Fortran sources
# -------------------------

fortran_sources = files(
  'src/types.f90', 'src/sorting.f90', 'src/stats.f90', 'src/utilities.f90',
  'src/zero_brent.f90', 'src/uh_optim.f90', 'src/uh2p_get_scale.f90', 'src/FA.f90',
  'src/lagk_run.f90', 'src/sac_snow.f90', 'src/consuse_run.f90', 'src/chanloss.f90',
  'src/rsnwelev_run.f90','src/initCommonBlocksPin.f', 'src/fintp7.f', 'src/fka7.f',
  'src/flag7.f', 'src/fop7.f', 'src/fserc7.f', 'src/fslag7.f', 'src/pin7.f',
  'src/pina7.f', 'src/umemov.f', 'src/umemst.f', 'src/PACK19.f', 'src/exsnow19.f',
  'src/aesc19.f', 'src/rout19.f', 'src/zero19.f', 'src/duamel.f', 'src/sac1.f',
  'src/ex_sac1.f', 'src/melt19.f', 'src/ex57.f', 'src/ex42.f', 'src/blockdata_fatlgk.f'
)

generated_f2py_sources = [
  module_name + 'module.c',
  module_name + '-f2pywrappers.f',
  module_name + '-f2pywrappers2.f90',
]

# -----------------------------------------
# 2. Generate F2PY wrapper source files
# -----------------------------------------

f2py_wrapper = custom_target(
  module_name + '_f2py',
  input: fortran_sources,
  output: generated_f2py_sources,
  command: [
    py, '-m', 'numpy.f2py',
    '@INPUT@',
    '-m', module_name,
    '--quiet',
    '--lower',
  ]
)

inc_np = include_directories(incdir_numpy, incdir_f2py)
fortranobject_c = incdir_f2py / 'fortranobject.c'

# -----------------------------------------
# 3. Build the Python extension module
# -----------------------------------------
py_dep = py.dependency()


ext_link_args = []
if host_machine.system() == 'darwin'
  ext_link_args += [
    '-Wl,-undefined,dynamic_lookup',
    '-static-libgfortran',
  ]
endif

py.extension_module(
  module_name,
  sources: [
    f2py_wrapper[0], # generated C API wrapper
    f2py_wrapper[1], # f2py wrappers for legacy Fortran procedures
    f2py_wrapper[2], # f2py wrappers for Fortran 90 module procedures
    fortran_sources,
    fortranobject_c,
  ],
  include_directories: inc_np,
  dependencies: py_dep,
  link_args: ext_link_args,
  subdir: 'nwsrfs_py/wrapper',
  install: true
)

# -----------------------------------------
# 4. Install Python package files
# -----------------------------------------

# 1. ALL Python files in one list
all_package_files = files(
  
  # Legal/attribution record
  'LICENSE',
  'COPYRIGHTS',
  'NOTICE',

  # Base package files
  'nwsrfs_py/__init__.py',
  'nwsrfs_py/simulation.py',
  'nwsrfs_py/adjustq.py',
  'nwsrfs_py/utils.py',
  
  # Wrapper files
  'nwsrfs_py/wrapper/__init__.py',
  'nwsrfs_py/wrapper/nwsrfs_f2py_wrapper.py',
  
  # Data root
  'nwsrfs_py/data/__init__.py',
  'nwsrfs_py/data/README.md',
  
  # SFLN2 Data
  'nwsrfs_py/data/SFLN2/__init__.py',
  'nwsrfs_py/data/SFLN2/flow_instantaneous_SFLN2.csv',
  'nwsrfs_py/data/SFLN2/forcing_por_SFLN2-1.csv',
  'nwsrfs_py/data/SFLN2/forcing_por_SFLN2-2.csv',
  'nwsrfs_py/data/SFLN2/forcing_por_SFLN2-CU.csv',
  'nwsrfs_py/data/SFLN2/flow_daily_SFLN2.csv',
  
  # SFLN2 Results
  'nwsrfs_py/data/SFLN2/results_por_01/__init__.py',
  'nwsrfs_py/data/SFLN2/results_por_01/pars_optimal.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/adc_SFLN2-1.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/fa_factors.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/optimal_6hr_inst.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/optimal_consuse_states.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/rsnwelev_pars.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/uh_1hr_SFLN2-1.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/uh_1hr_SFLN2-2.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/uh_1hr_SFLN2-CU.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/uh_6hr_SFLN2-1.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/uh_6hr_SFLN2-2.csv',
  'nwsrfs_py/data/SFLN2/results_por_01/uh_6hr_SFLN2-CU.csv',

  # NRKW1 Data
  'nwsrfs_py/data/NRKW1/__init__.py',
  'nwsrfs_py/data/NRKW1/flow_instantaneous_NRKW1.csv',
  'nwsrfs_py/data/NRKW1/upflow_NSSW1.csv',
  'nwsrfs_py/data/NRKW1/upflow_MFNW1.csv',
  'nwsrfs_py/data/NRKW1/forcing_por_NRKW1-2.csv',
  'nwsrfs_py/data/NRKW1/forcing_por_NRKW1-1.csv',
  'nwsrfs_py/data/NRKW1/upflow_NFNW1.csv',
  'nwsrfs_py/data/NRKW1/flow_daily_NRKW1.csv',

  # NRKW1 Results
  'nwsrfs_py/data/NRKW1/results_por_02/__init__.py',
  'nwsrfs_py/data/NRKW1/results_por_02/pars_optimal.csv',
  'nwsrfs_py/data/NRKW1/results_por_02/optimal_routed_flow.csv',
  'nwsrfs_py/data/NRKW1/results_por_02/optimal_routed_states.csv',
  'nwsrfs_py/data/NRKW1/results_por_02/optimal_6hr_inst.csv',
  'nwsrfs_py/data/NRKW1/results_por_02/optimal_states_6hr.csv',
  'nwsrfs_py/data/NRKW1/results_por_02/lagk_tables.csv',

  # Adjustq check
  'nwsrfs_py/data/Adjustq_check/__init__.py',
  'nwsrfs_py/data/Adjustq_check/NRKW1_w_sim.csv',
  'nwsrfs_py/data/Adjustq_check/NRKW1_wout_sim.csv'
)

# 2. Install everything in one command while maintaining the directory tree
py.install_sources(
  all_package_files,
  preserve_path: true
)

# -----------------------------------------
# 5. Unit Testing with Pytest
# -----------------------------------------

# Define the test execution
test('pytest_suite', 
  py, 
  args: [
    '-m', 'pytest', 
    meson.current_source_dir() / 'tests',
    '-v', 
    '-s'
  ],
  workdir: meson.current_source_dir(),
  timeout: 100 # Adjust if your simulations take a long time to run
)
