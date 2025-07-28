testing_path <- file.path(tempdir(), "resdoc-b")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(draft(
  system.file("rmarkdown", "templates", "resdoc-b", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))

# -----------------------------------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(check_yaml(verbose = TRUE),
               "contains all necessary YAML options")

# -----------------------------------------------------------------------------
# Render the PDF resdoc
test_that("render generates the PDF of the resdoc", {
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-english.pdf")))
})

# -----------------------------------------------------------------------------
# Render the PDF resdoc with verbose = TRUE (Add codecov coverage)
test_that("render generates the PDF of the resdoc", {
  unlink(file.path(testing_path, "_book", "resdoc-english.pdf"), force = TRUE)
  set_french(val = FALSE)
  set_render_type(doc_type = "pdf")
  render(verbose = TRUE)
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-english.pdf")))
})

# -----------------------------------------------------------------------------
# Render the Word resdoc
test_that("render generates the .docx of the resdoc", {
  set_french(val = FALSE)
  set_render_type(doc_type = "word")
  render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-english.docx")))
})

# -----------------------------------------------------------------------------
# Add the title page to the Word resdoc
# Fails on macos on GitHub
#Add_resdoc_docx_titlepage()

# test_that("add_resdoc_docx_titlepage() generates the .docx of the resdoc", {
#   expect_true(file.exists(file.path(testing_path, "_book", "resdoc.docx")))
# })

# -----------------------------------------------------------------------------
# Render the French PDF resdoc
test_that("render generates the French PDF of the resdoc", {
  set_french(val = TRUE)
  set_render_type(doc_type = "pdf")
  render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-french.pdf")))
})

# -----------------------------------------------------------------------------
# Render the French Word resdoc
test_that("render generates the French .docx of the resdoc", {
  set_french(val = TRUE)
  set_render_type(doc_type = "word")
  render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-french.docx")))
})
