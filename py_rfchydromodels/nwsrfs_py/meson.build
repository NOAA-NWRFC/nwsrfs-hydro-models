project('nwsrfspy', 'c', 'fortran',
  version : '0.1.0',
  meson_version: '>= 1.1.0',
  default_options : ['warning_level=2'],
)

py = import('python').find_installation(pure: false)
module_name = 'nwsrfs_src'
src_dir = meson.current_source_dir() / 'src'

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

fortran_sources = [
  'src/types.f90', 'src/sorting.f90', 'src/stats.f90', 'src/utilities.f90',
  'src/uh_optim.f90', 'src/uh2p_get_scale.f90', 'src/FA.f90', 'src/lagk_run.f90',
  'src/sac_snow.f90', 'src/consuse_run.f90', 'src/chanloss.f90', 'src/rsnwelev_run.f90',
  'src/initCommonBlocksPin.f', 'src/fintp7.f', 'src/fka7.f', 'src/flag7.f',
  'src/fop7.f', 'src/fserc7.f', 'src/fslag7.f', 'src/pin7.f', 'src/pina7.f',
  'src/umemov.f', 'src/umemst.f', 'src/PACK19.f', 'src/exsnow19.f', 'src/aesc19.f',
  'src/rout19.f', 'src/zero19.f', 'src/duamel.f', 'src/sac1.f', 'src/ex_sac1.f',
  'src/melt19.f', 'src/ex57.f', 'src/ex42.f'
]

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
  link_args: ['-Wl,-undefined,dynamic_lookup'],
  subdir: 'nwsrfs_py/wrapper',
  install: true
)

# -----------------------------------------
# 4. Install Python package files
# -----------------------------------------

py.install_sources(
  'nwsrfs_py/__init__.py',
  'nwsrfs_py/simulation.py',
  'nwsrfs_py/adjustq.py',
  'nwsrfs_py/utils.py',
  subdir: 'nwsrfs_py'
)

py.install_sources(
  'nwsrfs_py/wrapper/__init__.py',
  'nwsrfs_py/wrapper/nwsrfs_f2py_wrapper.py',
  subdir: 'nwsrfs_py/wrapper'
)
