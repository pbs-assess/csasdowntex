testing_path <- file.path(tempdir(), "sr")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(draft(
  system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
  create_dir = FALSE,
  edit = FALSE
))

# -----------------------------------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(check_yaml(type = "sr", verbose = TRUE),
               "contains all necessary YAML options")

# -----------------------------------------------------------------------------
# Render the PDF sr
test_that("render generates the PDF of the sr", {
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  render()
  expect_true(file.exists(file.path(testing_path, "_book", "sr-english.pdf")))
})

# ----------------------------------------------------
# Render the Word sr
test_that("render generates the .docx of the sr", {
  set_french(val = FALSE)
  set_render_type(doc_type = "word")
  render()
  expect_true(file.exists(file.path(testing_path, "_book", "sr-english.docx")))
})

# -----------------------------------------------------------------------------
# Render the French PDF sr
test_that("render generates the French PDF of the sr", {
  set_french(val = TRUE)
  set_render_type(doc_type = "pdf")
  suppressWarnings(
    bookdown::render_book("index.Rmd") # TODO fails with render()!?
  )
  expect_true(file.exists(file.path(testing_path, "_book", "sr.pdf")))
})

# ----------------------------------------------------
# Render the French Word sr
test_that("render generates the French .docx of the sr", {
  set_french(val = TRUE)
  set_render_type(doc_type = "word")
  # render()
  suppressWarnings(
    bookdown::render_book("index.Rmd") # TODO fails with render()!?
  )
  expect_true(file.exists(file.path(testing_path, "_book", "sr.docx")))
})

# -----------------------------------------------------------------------------
# Render the PDF sr, with `NULL` highlight
test_that("render generates monochrome code PDF of the sr", {
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: "
  writeLines(rmd, "index.Rmd")
  render()
  expect_true(file.exists(file.path(testing_path, "_book", "sr-english.pdf")))
  # Checked manually that the code chunks are monochrome
})

# -----------------------------------------------------------------------------
# Render the PDF sr, with bogus highlight
test_that("render detects bogus highlight", {
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: bogus"
  writeLines(rmd, "index.Rmd")
  expect_error(render(),
               paste0("must be one of"))
})

# -----------------------------------------------------------------------------
# Render the PDF sr, with character line number mod
test_that("render detects character line number mod value", {
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: tango"
  ind <- grep("line_nums_mod:", rmd)
  rmd[ind] <- "   line_nums_mod: A"
  writeLines(rmd, "index.Rmd")
  expect_error(render(), paste0("must be a numeric ",
                                          "or integer value."))
})

# -----------------------------------------------------------------------------
# Try to render a doc with no Rmd file input
test_that("render with no Rmd input", {
  testing_path <- file.path(tempdir(), "sr")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_warning(render(suppress_warnings = FALSE))
})
