test_that("get_render_type() and set_render_type() throw errors", {
  testing_path <- file.path(tempdir(), "resdoc-get-set-render-error")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_error(get_render_type("nonexistent-file.Rmd"), "does not exist")
  # ---------------------------------------------------------------------------
  rmd <- readLines("index.Rmd")
  ind <- grep("resdoc_pdf:", rmd)
  tmp <- rmd[ind]
  rmd[ind] <- " resdoc_pdf:"
  writeLines(rmd, "index.Rmd")
  rmd <- readLines("index.Rmd")
  ind <- grep("resdoc_pdf:", rmd)
  expect_identical(ind, 50L)
  # set_render_type("index.Rmd", "asis")
  # rmd <- readLines("index.Rmd")
  # ind <- grep("resdoc_pdf:", rmd)
  # expect_identical(ind, 50L)

  # ---------------------------------------------------------------------------
  rmd[ind] <- ""
  writeLines(rmd, "index.Rmd")
  expect_error(get_render_type(), "Document type not found")

  # ---------------------------------------------------------------------------
  rmd[ind] <- tmp
  rmd[ind + 1] <- tmp
  writeLines(rmd, "index.Rmd")
  expect_warning(get_render_type(),
                 "Document type defined more than once")
})

test_that("get_render_type() and set_render_type() works for resdoc", {
  testing_path <- file.path(tempdir(), "resdoc-get-set-render")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_identical(get_render_type(), "resdoc_pdf")
  # ---------------------------------------------------------------------------
  set_render_type(doc_type = "word")
  expect_identical(get_render_type(), "resdoc_word")
  # ---------------------------------------------------------------------------
  set_render_type(doc_type = "pdf")
  expect_identical(get_render_type(), "resdoc_pdf")

  # ---------------------------------------------------------------------------
  expect_error(set_render_type(doc_type = ""))
  # ---------------------------------------------------------------------------
  expect_error(set_render_type(doc_type = "oops"))
  # ---------------------------------------------------------------------------
  expect_invisible(set_render_type(doc_type = NULL))
})

test_that("get_render_type() and set_render_type() works for SR", {
  testing_path <- file.path(tempdir(), "sr-get-set-render")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_identical(get_render_type(), "sr_pdf")
  # ---------------------------------------------------------------------------
  set_render_type(doc_type = "word")
  expect_identical(get_render_type(), "sr_word")
  # ---------------------------------------------------------------------------
  set_render_type(doc_type = "pdf")
  expect_identical(get_render_type(), "sr_pdf")

  # ---------------------------------------------------------------------------
  expect_error(set_render_type(doc_type = ""))
  # ---------------------------------------------------------------------------
  expect_error(set_render_type(doc_type = "oops"))
  # ---------------------------------------------------------------------------
  expect_invisible(set_render_type(doc_type = NULL))
})

test_that("get_render_type() and set_render_type() works for techreport", {
  testing_path <- file.path(tempdir(), "techreport-get-set-render")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "techreport", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_identical(get_render_type(), "techreport_pdf")
  # ---------------------------------------------------------------------------
  set_render_type(doc_type = "word")
  expect_identical(get_render_type(), "techreport_word")
  # ---------------------------------------------------------------------------
  set_render_type(doc_type = "pdf")
  expect_identical(get_render_type(), "techreport_pdf")

  # ---------------------------------------------------------------------------
  expect_error(set_render_type(doc_type = ""))
  # ---------------------------------------------------------------------------
  expect_error(set_render_type(doc_type = "oops"))
  # ---------------------------------------------------------------------------
  expect_invisible(set_render_type(doc_type = NULL))
})

