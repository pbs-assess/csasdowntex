test_that("create_tempdir_for_latex() works", {

  testing_path <- file.path(tempdir(), "sr-latex-test-1")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # ---------------------------------------------------------------------------
  expect_error(create_tempdir_for_latex("badtype",
                                                  "b",
                                                  root_dir = getwd()),
               "must be one of")

  # ---------------------------------------------------------------------------
  expect_error(create_tempdir_for_latex("sr",
                                                  "x",
                                                  root_dir = getwd()),
               "must be one of")

  # ---------------------------------------------------------------------------
  expect_error(create_tempdir_for_latex("sr",
                                                  "b",
                                                  root_dir = getwd()),
               "sr.tex does not exist in the '_book' directory")

  # ---------------------------------------------------------------------------
  # Render the SR
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  suppressWarnings(render())
  unlink("_book/sr-english.tex")
  # Try for case where copying from the '_book' directory
  expect_error(create_tempdir_for_latex("sr",
                                        "b",
                                        tmp_dir = file.path(testing_path,
                                                            "test"),
                                        root_dir = getwd()),
               "sr.tex does not exist in the '_book' directory")

  # ---------------------------------------------------------------------------
  testing_path <- file.path(tempdir(), "sr-latex-test-2")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))
  # Render the SR
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  suppressWarnings(render())

  # Try for case where copying from the '_book' directory
  tmp_dir <- create_tempdir_for_latex("sr",
                                      "b",
                                      tmp_dir = file.path(testing_path,
                                                          "test"),
                                      root_dir = getwd())
  tmp_csas_dir <- file.path(tmp_dir, "csas-style")

  expect_true(file.exists(file.path(tmp_csas_dir, "res-doc.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "res-doc-french.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "sr.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "sr-french.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "tech-report.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "tech-report-french.sty")))
  expect_true(dir.exists(file.path(tmp_csas_dir, "images")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-cache-pdf")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-cache-word")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-figs-pdf")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-figs-word")))
  expect_true(file.exists(file.path(tmp_dir, "sr-english.tex")))

  # ---------------------------------------------------------------------------
  # Test copying of the tex file from the root directory instead of the _book
  # directory
  testing_path <- file.path(tempdir(), "sr-latex-test-3")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # Render the SR
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  suppressWarnings(render())

  file.copy(file.path("_book", "sr-english.tex"), "sr-english.tex")
  tmp_dir <- create_tempdir_for_latex("sr",
                                      "r",
                                      tmp_dir = file.path(testing_path,
                                                          "test"),
                                      root_dir = getwd()
  )
  tmp_csas_dir <- file.path(tmp_dir, "csas-style")

  expect_true(file.exists(file.path(tmp_csas_dir, "res-doc.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "res-doc-french.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "sr.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "sr-french.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "tech-report.sty")))
  expect_true(file.exists(file.path(tmp_csas_dir, "tech-report-french.sty")))
  expect_true(dir.exists(file.path(tmp_csas_dir, "images")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-cache-pdf")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-cache-word")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-figs-pdf")))
  expect_true(dir.exists(file.path(tmp_dir, "knitr-figs-word")))
  expect_true(file.exists(file.path(tmp_dir, "sr-english.tex")))

  unlink(testing_path, recursive = TRUE, force = TRUE)
})
