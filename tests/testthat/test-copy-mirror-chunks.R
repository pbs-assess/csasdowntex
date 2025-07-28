test_that("copy_mirror_chunks() works", {

  # ---------------------------------------------------------------------------
  # Empty file
  fn <- file.path(test_path(), "preprocess-chunks-files", "empty-file.Rmd")
  actual <- copy_mirror_chunks(fn, nowrite = TRUE)[[1]]
  expect_null(actual)

  # ---------------------------------------------------------------------------
  # Zero mirror chunks
  rmd <- c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
           "cat('Test')",
           "```",
           "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
           "cat('Test 2')",
           "```")
  fn <- "zero-mirror-chunks.Rmd"
  writeLines(rmd, fn)
  actual <- copy_mirror_chunks(fn, nowrite = TRUE)[[1]]
  expect_null(actual, NULL)

  # ---------------------------------------------------------------------------
  # One mirror chunk
  fn <- file.path(test_path(), "preprocess-chunks-files", "single-mirror.Rmd")
  actual <- copy_mirror_chunks(fn, nowrite = TRUE)[[1]]
  expected <- c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                "cat(\"\")",
                "```",
                "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                "cat(\"\")",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  # Two mirror chunks, one file
  fn <- file.path(test_path(), "preprocess-chunks-files", "double-mirror.Rmd")
  actual <- copy_mirror_chunks(fn, nowrite = TRUE)[[1]]
  expected <- c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                "cat(\"\")",
                "```",
                "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                "cat(\"\")",
                "```",
                "```{r chunk-2-en, eval = !fr(), results = 'asis'}",
                "cat(\"\")",
                "```" )
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  # Two mirror chunks across two files
  fn1 <- file.path(test_path(), "preprocess-chunks-files", "single-mirror.Rmd")
  fn2 <- file.path(test_path(), "preprocess-chunks-files", "single-mirror-refother.Rmd")
  actual <- copy_mirror_chunks(c(fn1, fn2), nowrite = TRUE)
  expected <- list(c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                     "cat(\"\")",
                     "```",
                     "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                     "cat(\"\")",
                     "```"),
                   c("```{r chunk-2-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                     "cat(\"\")",
                     "```" ))
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  # Two mirror chunks across three files, one with a `cat()`
  fn1 <- file.path(test_path(), "preprocess-chunks-files", "single-mirror.Rmd")
  fn2 <- file.path(test_path(), "preprocess-chunks-files", "single-mirror-refother.Rmd")
  fn3 <- file.path(test_path(), "preprocess-chunks-files", "single-mirror-refother-withcat.Rmd")
  actual <- copy_mirror_chunks(c(fn1, fn2, fn3), nowrite = TRUE)
  expected <- list(c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                     "cat(\"\")",
                     "```",
                     "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                     "cat(\"\")",
                     "```"),
                   c("```{r chunk-2-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                     "cat(\"\")",
                     "```"),
                   c("```{r chunk-3-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                     "cat(\"\")",
                     "```",
                     "```{r chunk-4-en, eval = !fr(), results = 'asis'}",
                     "cat(\"## Header for testing\")",
                     "```"))
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  # Two mirror chunks, two files, chained mirrors
  fn1 <- file.path(test_path(), "preprocess-chunks-files", "single-mirror.Rmd")
  fn2 <- file.path(test_path(), "preprocess-chunks-files", "single-chained.Rmd")
  expect_error(copy_mirror_chunks(c(fn1, fn2), nowrite = TRUE),
               "Chained mirror reference")

  # ---------------------------------------------------------------------------
  # One mirror chunk, one file, no reference chunk for mirror
  fn <- file.path(test_path(), "preprocess-chunks-files", "single-chained.Rmd")
  expect_error(copy_mirror_chunks(fn, nowrite = TRUE),
               "does not appear to have a source chunk")

  # ---------------------------------------------------------------------------
  # One mirror chunk, one file, more than one reference chunk
  fn <- file.path(test_path(), "preprocess-chunks-files",
                  "single-mirror-multiple-chunks.Rmd")
  expect_error(copy_mirror_chunks(fn, nowrite = TRUE),
               "appears to have multiple source chunks")

  # ---------------------------------------------------------------------------
  # Verbose, One mirror chunk, one file, more than one reference chunk
  fn <- file.path(test_path(), "preprocess-chunks-files",
                  "single-mirror.Rmd")
  expect_message(copy_mirror_chunks(fn, nowrite = TRUE,
                                               verbose = TRUE),
               "Copying Rmarkdown code into")

})
