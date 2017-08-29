context("alfresco")

test_that("rvtable class and attributes preserved", {
  x <- alf_defaults()

  expect_is(x, "list")
  expect_equal(x$atlas_shiny_dir, "/big_scratch/shiny")
  expect_equal(x$atlas_run_owner_dir, "mfleonawicz@alaska.edu")
  expect_equal(x$age_spinups, "/big_scratch/mfleonawicz/Alf_Files_20121129/Spinup300Year_32Reps/Age_0_1900.tif")
})
