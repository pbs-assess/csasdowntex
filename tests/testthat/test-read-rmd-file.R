test_that("read_rmd_file() works", {
  testing_path <- file.path(tempdir(), "sr-read-rmd-file")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)

  rmd_dir <- testthat::test_path("preprocess-chunks-files")
  file.copy(file.path(rmd_dir, "empty-file.Rmd"),
            file.path(testing_path, "empty-file.Rmd"))

  setwd(testing_path)
  # suppressMessages(csasdown::draft(
  #   system.file("rmarkdown", "templates", "sr", package = "csasdown"),
  #   create_dir = FALSE,
  #   edit = FALSE
  # ))

  # -----------------------------------------------------------------------------
  expect_error(csasdown:::read_rmd_file("nonexistent-file.Rmd"),
               "The file 'nonexistent-file.Rmd' does not exist.")

  # -----------------------------------------------------------------------------
  # expect_identical(csasdown:::read_rmd_file("empty-file.Rmd"), "")

  # -----------------------------------------------------------------------------
  rmd <- c("Test", "")
  writeLines(rmd, "two-line.Rmd")
  expect_identical(csasdown:::read_rmd_file("two-line.Rmd"),
                   c("cat(\"Test", "", "\")"))

  rmd <- c("Test", "    ", "                ", "# Header", "  ")
  writeLines(rmd, "blanks.Rmd")
  expect_identical(csasdown:::read_rmd_file("blanks.Rmd"),
                   c("cat(\"Test", "", "", "# Header", "", "\")"))

})
