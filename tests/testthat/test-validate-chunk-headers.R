test_that("validate_chunk_headers() works", {
  wd <- getwd()
  rmd_dir <- testthat::test_path("validate-chunk-headers-files")

  testing_path <- file.path(tempdir(), "test-validate-chunk-headers")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  file.copy(file.path(rmd_dir, "regex-tests-1.Rmd"),
            file.path(testing_path, "regex-tests-1.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-2.Rmd"),
            file.path(testing_path, "regex-tests-2.Rmd"))
  setwd(testing_path)

  expect_invisible(validate_chunk_headers(NULL))
  # ---------------------------------------------------------------------------

  fn <- "test.Rmd"
  writeLines(c("```{r x-1-en, eval = !fr(), results = 'asis'}",
               "```",
               "```{r x-2-english, eval = !fr(), results = 'asis'}",
               "```"),
             fn)
  expect_error(validate_chunk_headers(fn),
               paste0("is not of correct format for English chunks"))
  # ---------------------------------------------------------------------------

  writeLines(c("```{r fr-y-1, eval = fr(), needs_trans = FALSE, results = \"asis\"}",
               "```"),
             fn)
  expect_error(validate_chunk_headers(fn),
               paste0("is not of correct format for French chunks"))
  # ---------------------------------------------------------------------------

  writeLines(c("```{r x-1-en, eval = !fr(), results = 'asis'}",
               "```",
               "```{r x-1-fr, eval = fr(), results = 'asis'}",
               "```"),
             fn)
  expect_warning(validate_chunk_headers(fn),
                 paste0("is missing for this French chunk and has been added"))
  # ---------------------------------------------------------------------------

  rmd_files <-  c("regex-tests-1.Rmd", "regex-tests-2.Rmd")
  w <- capture_warnings(validate_chunk_headers(
    rmd_files,
    en_chunk_regex = NULL,
    fr_chunk_regex = NULL))
  expect_match(w[1], "is missing for this French chunk and has been added")
  expect_match(w[2], "is missing for this French chunk and has been added")
  expect_match(w[3], "is missing for this French chunk and has been added")
  expect_match(w[4], "is not allowed for English chunks and has been removed")
  expect_match(w[5], "is not allowed for English chunks and has been removed")
  expect_match(w[6], "is not allowed for English chunks and has been removed")
  expect_match(w[7], "is not allowed for English chunks and has been removed")
  expect_match(w[8], "is missing for this French chunk and has been added")
  expect_match(w[9], "is missing for this French chunk and has been added")
  expect_match(w[10], "is missing for this French chunk and has been added")
  expect_match(w[11], "is not allowed for English chunks and has been removed")
  expect_match(w[12], "is not allowed for English chunks and has been removed")
  expect_match(w[13], "is not allowed for English chunks and has been removed")
  expect_match(w[14], "is not allowed for English chunks and has been removed")
  # ---------------------------------------------------------------------------

  writeLines(c("```{r x-1-en, eval = !fr()}",
               "```"),
             fn)
  expect_warning(validate_chunk_headers(fn),
                 "is missing and has been added automatically")
  # ---------------------------------------------------------------------------

  writeLines(c("```{r x-1, needs_trans = FALSE}",
               "```"),
             fn)
  expect_warning(validate_chunk_headers(fn),
                 "is not of correct format for neutral chunks")
  # ---------------------------------------------------------------------------
  writeLines(c("```{r y-1-fr, eval = fr(), needs_trans = FALSE}",
               "```"),
             fn)
  expect_warning(validate_chunk_headers(fn),
                 "is missing and has been added automatically")
  # ---------------------------------------------------------------------------

  writeLines(c("```{r x-1-en}", "```"), fn)
  expect_warning(validate_chunk_headers(fn),
                 "with a name that follows the format for an English chunk")

  writeLines(c("```{r x-1-fr}", "```"), fn)
  expect_warning(validate_chunk_headers(fn),
                 "with a name that follows the format for a French chunk")

  setwd(wd)
  })
