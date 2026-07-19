test_that("load_example NRKW1 runs and returns expected structure", {
  run = load_example("NRKW1")

  expect_s3_class(run, "nwsrfs_run")
  expect_true(run$localflow_logic)
  expect_true(run$upflow_logic)
  expect_false(run$chanloss_logic)
  expect_false(run$consuse_logic)
  expect_equal(run$zone_names, c("NRKW1-1", "NRKW1-2"))
  expect_equal(run$upflow_names, c("MFNW1", "NFNW1", "NSSW1"))
  expect_true(length(run$sim) > 0)
  expect_true(all(run$sim > 0))
})


test_that("load_example SFLN2 runs and returns expected structure", {
  run = load_example("SFLN2")

  expect_s3_class(run, "nwsrfs_run")
  expect_true(run$localflow_logic)
  expect_false(run$upflow_logic)
  expect_true(run$chanloss_logic)
  expect_true(run$consuse_logic)
  expect_true("SFLN2-CU" %in% run$zone_names)
  expect_true(length(run$sim) > 0)
})


test_that("update_pars modifies parameters and re-runs", {
  run = load_example("NRKW1")
  orig_sim = run$sim

  # Increase a SAC parameter slightly
  new_pars = data.frame(
    p_name = "uztwm_NRKW1-1",
    value = run$pars$value[run$pars$p_name == "uztwm_NRKW1-1"] * 1.01
  )
  run2 = update_pars(run, new_pars)

  expect_s3_class(run2, "nwsrfs_run")
  # Sim should be different after parameter change
  expect_false(identical(run2$sim, orig_sim))
})
