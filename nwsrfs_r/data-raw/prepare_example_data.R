# %%
# Prepare NRKW1 and SFLN2 example data from Python package CSVs
# Run from repo root: Rscript nwsrfs_r/data-raw/prepare_example_data.R

py_data = file.path("nwsrfs_py", "nwsrfs_py", "data")
r_data = file.path("nwsrfs_r", "data")

# %%
# --- NRKW1 ---
nrkw1_dir = file.path(py_data, "NRKW1")

# Parameters
nrkw1_pars = read.csv(file.path(nrkw1_dir, "results_por_02", "pars_optimal.csv"),
  stringsAsFactors = FALSE
)
save(nrkw1_pars, file = file.path(r_data, "nrkw1_pars.rda"), compress = "xz")

# Forcing (list of data.frames, one per zone)
nrkw1_forcing = list(
  `NRKW1-1` = read.csv(file.path(nrkw1_dir, "forcing_por_NRKW1-1.csv")),
  `NRKW1-2` = read.csv(file.path(nrkw1_dir, "forcing_por_NRKW1-2.csv"))
)
save(nrkw1_forcing, file = file.path(r_data, "nrkw1_forcing.rda"), compress = "xz")

# Upflow (list of data.frames, one per upstream point)
nrkw1_upflow = list(
  MFNW1 = read.csv(file.path(nrkw1_dir, "upflow_MFNW1.csv")),
  NFNW1 = read.csv(file.path(nrkw1_dir, "upflow_NFNW1.csv")),
  NSSW1 = read.csv(file.path(nrkw1_dir, "upflow_NSSW1.csv"))
)
save(nrkw1_upflow, file = file.path(r_data, "nrkw1_upflow.rda"), compress = "xz")

# Daily flow
nrkw1_daily_flow = read.csv(file.path(nrkw1_dir, "flow_daily_NRKW1.csv"),
  stringsAsFactors = FALSE
)
save(nrkw1_daily_flow, file = file.path(r_data, "nrkw1_daily_flow.rda"), compress = "xz")

# Instantaneous flow
nrkw1_inst_flow = read.csv(file.path(nrkw1_dir, "flow_instantaneous_NRKW1.csv"),
  stringsAsFactors = FALSE
)
save(nrkw1_inst_flow, file = file.path(r_data, "nrkw1_inst_flow.rda"), compress = "xz")

cat("NRKW1 data saved\n")

# %%
# --- SFLN2 ---
sfln2_dir = file.path(py_data, "SFLN2")

# Parameters
sfln2_pars = read.csv(file.path(sfln2_dir, "results_por_01", "pars_optimal.csv"),
  stringsAsFactors = FALSE
)
save(sfln2_pars, file = file.path(r_data, "sfln2_pars.rda"), compress = "xz")

# Forcing (list of data.frames, one per zone plus CU zone)
sfln2_forcing = list(
  `SFLN2-1` = read.csv(file.path(sfln2_dir, "forcing_por_SFLN2-1.csv")),
  `SFLN2-2` = read.csv(file.path(sfln2_dir, "forcing_por_SFLN2-2.csv")),
  `SFLN2-CU` = read.csv(file.path(sfln2_dir, "forcing_por_SFLN2-CU.csv"))
)
save(sfln2_forcing, file = file.path(r_data, "sfln2_forcing.rda"), compress = "xz")

# Daily flow
sfln2_daily_flow = read.csv(file.path(sfln2_dir, "flow_daily_SFLN2.csv"),
  stringsAsFactors = FALSE
)
save(sfln2_daily_flow, file = file.path(r_data, "sfln2_daily_flow.rda"), compress = "xz")

# Instantaneous flow
sfln2_inst_flow = read.csv(file.path(sfln2_dir, "flow_instantaneous_SFLN2.csv"),
  stringsAsFactors = FALSE
)
save(sfln2_inst_flow, file = file.path(r_data, "sfln2_inst_flow.rda"), compress = "xz")

cat("SFLN2 data saved\n")
cat("Done.\n")
