wd <- getwd()
rnd_num <- as.integer(abs(rnorm(1) * 1e6))
testing_path <- file.path(tempdir(), paste0("manureport_", rnd_num))
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(draft(
  system.file("rmarkdown", "templates", "manureport", package = "csasdowntex"),
  create_dir = FALSE,
  edit = FALSE
))

# -----------------------------------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(check_yaml(type = "manureport", verbose = TRUE),
               "contains all necessary YAML options")

# ----------------------------------------------------
# Render the PDF manureport
test_that("render generates the PDF of the manureport", {
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  render(suppress_warnings = TRUE)
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "manureport-english.pdf")))
})

# -----------------------------------------------------------------------------
# Render the Word manureport
test_that("render generates the .docx of the manureport", {
  set_french(val = FALSE)
  set_render_type(doc_type = "word")
  render(suppress_warnings = TRUE)
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "manureport-english.docx")))
})

# ----------------------------------------------------
# Render the PDF manureport in French
test_that("render generates the PDF of the manureport in French", {
  set_french(val = TRUE)
  set_render_type(doc_type = "pdf")
  suppressWarnings(
    render(suppress_warnings = TRUE)
  )
  expect_true(file.exists(file.path(testing_path, "_book",
    "manureport-french.pdf")))
})


# -----------------------------------------------------------------------------
# Render the PDF manureport, with `NULL` highlight
# test_that("render generates monochrome code PDF of the manureport",
#  {
#   set_french(val = FALSE)
#   set_render_type(doc_type = "pdf")
#   rmd <- readLines("index.Rmd")
#   ind <- grep("highlight:", rmd)
#   rmd[ind] <- "   highlight: "
#   writeLines(rmd, "index.Rmd")
#   render()
#   expect_true(file.exists(file.path(testing_path, "_book", "manureport-english.pdf")))
#   # Checked manually that the code chunks are monochrome
# })

# -----------------------------------------------------------------------------
# Render the PDF resdoc, with bogus highlight
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
# Render the PDF resdoc, with character line number mod
test_that("render detects character line number mod value", {
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: tango"
  ind <- grep("line_nums_mod:", rmd)
  rmd[ind] <- "   line_nums_mod: A"
  writeLines(rmd, "index.Rmd")
  expect_error(render(),
               paste0("must be a numeric ",
                      "or integer value."))
})

# -----------------------------------------------------------------------------
# Detect that PDF cover page is missing
test_that("render detects cover page missing", {
  unlink("manu-report-cover*.pdf", force = TRUE)
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("line_nums_mod:", rmd)
  rmd[ind] <- "   line_nums_mod: 1"
  writeLines(rmd, "index.Rmd")
  render(suppress_warnings = TRUE)
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "manureport-english.pdf")))
  expect_true(file.exists(file.path(testing_path,
                                    "manu-report-cover.pdf")))
})

setwd(wd)
