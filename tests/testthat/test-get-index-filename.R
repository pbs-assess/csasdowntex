test_that("get_index_filename() works for the resdoc", {
  testing_path <- file.path(tempdir(), "resdoc-get-index-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # -----------------------------------------------------------------------------
  expect_error(get_index_filename(NULL),
               "The \\S+ argument \\(filename\\) cannot be")
  expect_error(get_index_filename(""),
               "The \\S+ argument \\(filename\\) cannot be an empty string")
  expect_error(get_index_filename("nonexistent-file.yml"),
               "File \\S+ does not exist")
  expect_identical(get_index_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(get_index_filename("tmp-bookdown.yml"), "index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(get_index_filename(), "tmp-index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(get_index_filename(),
               paste0("Index filename not found in \\S+. This is ",
                      "typically index.Rmd and should be the first entry after"))

})

test_that("get_index_filename() works for the SR", {
  testing_path <- file.path(tempdir(), "sr-get-index-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # -----------------------------------------------------------------------------
  expect_error(get_index_filename(NULL))
  expect_error(get_index_filename(""))
  expect_identical(get_index_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(get_index_filename("tmp-bookdown.yml"), "index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(get_index_filename(), "tmp-index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(get_index_filename())
})

test_that("get_index_filename() works for the techreport", {
  testing_path <- file.path(tempdir(), "techreport-get-index-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # -----------------------------------------------------------------------------
  expect_error(get_index_filename(NULL))
  expect_error(get_index_filename(""))
  expect_identical(get_index_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(get_index_filename("tmp-bookdown.yml"), "index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(get_index_filename(), "tmp-index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(get_index_filename())
})
