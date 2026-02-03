test_that("run_pdflatex() works", {

  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  render()

  # -----------------------------------------------------------------------------
  expect_error(run_pdflatex(),
               paste0("The file \\S+ exists. Delete it ",
                      "before running this function."))

  # -----------------------------------------------------------------------------
  unlink("_book/*.pdf", force = TRUE)

  suppressWarnings(run_pdflatex(extra_pdflatex = 2))
  setwd(wd)
})
