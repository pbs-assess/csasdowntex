wd <- getwd()
rnd_num <- as.integer(abs(rnorm(1) * 1e6))
testing_path <- file.path(tempdir(), paste0("techreport_", rnd_num))
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "techreport", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))

# -----------------------------------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(csasdown::check_yaml(type = "techreport", verbose = TRUE),
               "contains all necessary YAML options")

# ----------------------------------------------------
# Render the PDF techreport
test_that("csasdown::render generates the PDF of the techreport", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render(suppress_warnings = TRUE)
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "techreport-english.pdf")))
})

# -----------------------------------------------------------------------------
# Render the Word techreport
test_that("csasdown::render generates the .docx of the techreport", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "word")
  csasdown::render(suppress_warnings = TRUE)
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "techreport-english.docx")))
})

# ----------------------------------------------------
# Render the PDF techreport in French
test_that("csasdown::render generates the PDF of the techreport in French", {
  csasdown::set_french(val = TRUE)
  csasdown:::set_render_type(doc_type = "pdf")
  suppressWarnings(
    csasdown::render(suppress_warnings = TRUE)
  )
  expect_true(file.exists(file.path(testing_path, "_book",
    "techreport-french.pdf")))
})


# -----------------------------------------------------------------------------
# Render the PDF techreport, with `NULL` highlight
# test_that("csasdown::render generates monochrome code PDF of the techreport",
#  {
#   csasdown::set_french(val = FALSE)
#   csasdown:::set_render_type(doc_type = "pdf")
#   rmd <- readLines("index.Rmd")
#   ind <- grep("highlight:", rmd)
#   rmd[ind] <- "   highlight: "
#   writeLines(rmd, "index.Rmd")
#   csasdown::render()
#   expect_true(file.exists(file.path(testing_path, "_book", "techreport-english.pdf")))
#   # Checked manually that the code chunks are monochrome
# })

# -----------------------------------------------------------------------------
# Render the PDF resdoc, with bogus highlight
test_that("csasdown::render detects bogus highlight", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: bogus"
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown::render(),
               paste0("must be one of"))
})

# -----------------------------------------------------------------------------
# Render the PDF resdoc, with character line number mod
test_that("csasdown::render detects character line number mod value", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: tango"
  ind <- grep("line_nums_mod:", rmd)
  rmd[ind] <- "   line_nums_mod: A"
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown::render(),
               paste0("must be a numeric ",
                      "or integer value."))
})

# -----------------------------------------------------------------------------
# Detect that PDF cover page is missing
test_that("csasdown::render detects cover page missing", {
  unlink("tech-report-cover*.pdf", force = TRUE)
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("line_nums_mod:", rmd)
  rmd[ind] <- "   line_nums_mod: 1"
  writeLines(rmd, "index.Rmd")
  csasdown::render(suppress_warnings = TRUE)
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "techreport-english.pdf")))
  expect_true(file.exists(file.path(testing_path,
                                    "tech-report-cover.pdf")))
})

setwd(wd)
