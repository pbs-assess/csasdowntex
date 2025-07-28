# Render the officedown FSAR
test_that("render generates the .docx of the FSAR with fsar_word", {
  testing_path <- file.path(tempdir(), "fsar")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    "fsar",
    create_dir = FALSE,
    edit = FALSE
  ))
  expect_error(render(), "render_fsar()")
  suppressWarnings(render_sar())
  expect_true(file.exists(file.path(testing_path, "_book", "fsar.docx")))
  file.remove(file.path(testing_path, "_book", "fsar.docx"))

  suppressWarnings(render_fsar())
  expect_true(file.exists(file.path(testing_path, "_book", "fsar.docx")))
  file.remove(file.path(testing_path, "_book", "fsar.docx"))

  # custom yaml file:
  file.rename("_bookdown.yml", "_my_test_yml.yml")
  expect_error(suppressWarnings(render_sar()), regexp = "YAML")
  suppressWarnings(render_sar(config_file = "_my_test_yml.yml"))
  expect_true(file.exists(file.path(testing_path, "_book", "fsar.docx")))
})

# Render the Word2 resdoc
test_that("render generates the .docx of the resdoc with resdoc_word2", {
  testing_path <- file.path(tempdir(), "resdoc")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    "resdoc",
    create_dir = FALSE,
    edit = FALSE
  ))
  f <- readLines("index.Rmd")
  i <- grep("resdoc_", f)
  f[i] <- gsub("pdf", "word2", f[i])
  writeLines(f, "index.Rmd")
  render()
  expect_true(file.exists(file.path(testing_path, "_book", "resdoc-english.docx")))
})
