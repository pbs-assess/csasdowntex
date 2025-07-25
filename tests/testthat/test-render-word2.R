testing_path <- file.path(tempdir(), "fsar")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(csasdown::draft(
  "fsar",
  create_dir = FALSE,
  edit = FALSE
))

# Render the Word2 FSAR
test_that("csasdown::render generates the .docx of the FSAR with fsar_word2", {
  suppressWarnings(csasdown::render_sar())
  expect_true(file.exists(file.path(testing_path, "_book", "fsar.docx")))
})


testing_path <- file.path(tempdir(), "resdoc")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(csasdown::draft(
  "resdoc",
  create_dir = FALSE,
  edit = FALSE
))

# Render the Word2 resdoc
test_that("csasdown::render generates the .docx of the resdoc with resdoc_word2", {
  f <- readLines("index.Rmd")
  i <- grep("csasdown::resdoc_", f)
  f[i] <- gsub("pdf", "word2", f[i])
  writeLines(f, "index.Rmd")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book", "resdoc.docx")))
})
