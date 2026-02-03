test_that("draft() works", {
  testing_path <- file.path(tempdir(), "resdoc-draft-test")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)

  # ---------------------------------------------------------------------------
  expect_message(draft("sr",
                  verbose = TRUE,
                  testing = TRUE,
                  testing_affirm = TRUE),
  paste0("Created a new RStudio project file"))

  # ---------------------------------------------------------------------------
  expect_error(draft("resdoc-b",
                               verbose = TRUE,
                               testing = TRUE,
                               testing_affirm = FALSE),
         paste0("New document not drafted"))

  # ---------------------------------------------------------------------------
  # Bad directory
  expect_error(draft("resdoc-b",
                               directory = "baddir",
                               verbose = TRUE,
                               testing = TRUE,
                               testing_affirm = FALSE),
    paste0("does not exist so the csasdowntex project cannot be created there"))


  expect_error(create_rstudio_project_file(directory = "baddir",
                                                      verbose = TRUE),
               paste0("does not exist so an RStudio project file cannot ",
                      "be created there"))

  # ---------------------------------------------------------------------------
  dir.create("test-folder")
  setwd("test-folder")
  unlink("make-report-here", recursive = TRUE, force = TRUE)
  file.create("do-not-delete.txt")
  dir.create("make-report-here", showWarnings = FALSE)
  draft(type = "resdoc", directory = "make-report-here", verbose = TRUE)
  expect_true(file.exists("make-report-here/_bookdown.yml"))
  expect_true(file.exists("make-report-here/make-report-here.Rproj"))
  expect_true(file.exists("do-not-delete.txt"))
  unlink("make-report-here", recursive = TRUE, force = TRUE)
  unlink("do-not-delete.txt", recursive = TRUE, force = TRUE)
  setwd("..")
  unlink("test-folder", recursive = TRUE, force = TRUE)
})
