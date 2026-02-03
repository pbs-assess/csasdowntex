test_that("set_french() and get_french() throws errors", {
  testing_path <- file.path(tempdir(), "sr-set-french-errors")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_error(get_french("nonexistent-file.Rmd"),
               "File \\S+ does not exist")
  expect_error(set_french("nonexistent-file.Rmd"),
               "File \\S+ does not exist")

  fn <- "index.Rmd"
  rmd <- readLines(fn)
  back_rmd <- rmd

  ind <- grep("french:", rmd)
  tmp <- rmd[ind]
  rmd[ind] <- ""
  writeLines(rmd, fn)
  expect_error(get_french(),
               "YAML tag of incorrect format or not found in file")
  expect_error(set_french(),
               "The YAML tag \\S+ was not found in the file")

  rmd <- back_rmd
  rmd[ind] <- tmp
  rmd[ind + 1] <- tmp
  writeLines(rmd, fn)
  expect_error(get_french(),
               "YAML tag has more than one entry in file")

  expect_error(set_french(),
               "The YAML tag \\S+ was found more than once")
})

test_that("set_french() and get_french() works", {
  testing_path <- file.path(tempdir(), "sr-set-french")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_false(get_french())
  set_french(val = TRUE)
  expect_true(get_french())
  set_french(val = FALSE)
  expect_false(get_french())

  fn <- 'index.Rmd'
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  french_pat <- "^\\s*french:\\s*(false|true)\\s*$"
  french_ind <- grep(french_pat, trim_rmd)
  leading_spaces <- gsub("^(\\s*)french:\\s(true|false)\\s*$", "\\1", rmd[french_ind])
  all_spaces <- grep("^\\s*$", leading_spaces)
  if(!length(all_spaces)){
    leading_spaces <- ""
  }
  rmd[french_ind] <- paste0(leading_spaces, "french: truee")
  writeLines(rmd, fn)
  expect_error(get_french())

  set_french(val = TRUE)
  expect_true(get_french())

})
