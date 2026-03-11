test_that("check_yaml() works", {
  testing_path <- file.path(tempdir(), "sr-check-yaml")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  csasdown::draft("sr", testing = TRUE, testing_affirm_ovr = TRUE)

  # ---------------------------------------------------------------------------
  expect_message(csasdown::check_yaml(type = "sr", verbose = TRUE),
                 "Your \\S+ file contains all necessary YAML options")

  # ---------------------------------------------------------------------------
  expect_error(csasdown::check_yaml(type = "badtype", verbose = TRUE),
               "must be one of")

  # ---------------------------------------------------------------------------
  file.copy("index.Rmd", "x.Rmd")
  rmd <- readLines("index.Rmd")
  ind <- grep("french_region:", rmd)
  rmd <- rmd[-ind]
  writeLines(rmd, "index.Rmd")
  suppressMessages(
    expect_error(csasdown::check_yaml(type = "sr"),
                 "Your \\S+ file is missing the YAML tag")
  )

  # ---------------------------------------------------------------------------
  file.copy("x.Rmd", "index.Rmd", overwrite = TRUE)
  suppressMessages(
    expect_error(csasdown::check_yaml(type = "resdoc"),
                 paste0("Your \\S+ file is missing the YAML tag\\(s\\)"))
  )

  # ---------------------------------------------------------------------------
  suppressMessages(
    expect_error(csasdown::check_yaml(type = "techreport"),
                 paste0("Your \\S+ file is missing the YAML tag\\(s\\):"))
  )

})
