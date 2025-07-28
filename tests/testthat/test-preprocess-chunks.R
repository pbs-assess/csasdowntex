test_that("preprocess_chunks() works", {

  rmd_dir <- testthat::test_path("preprocess-chunks-files")
  testing_path <- file.path(tempdir(), "resdoc-preprocess-chunks")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  file.copy(file.path(rmd_dir, "empty-file.Rmd"),
            file.path(testing_path, "empty-file.Rmd"))
  file.copy(file.path(rmd_dir, "double-chunk-cat.Rmd"),
            file.path(testing_path, "double-chunk-cat.Rmd"))
  file.copy(file.path(rmd_dir, "double-chunk-cat-extra-blanks.Rmd"),
            file.path(testing_path, "double-chunk-cat-extra-blanks.Rmd"))
  file.copy(file.path(rmd_dir, "double-chunk-cat-single-quotes.Rmd"),
            file.path(testing_path, "double-chunk-cat-single-quotes.Rmd"))
  setwd(testing_path)
  expect_error(draft("resdoc", testing = TRUE, testing_affirm_ovr = FALSE))

  # ---------------------------------------------------------------------------
  expect_invisible(preprocess_chunks(NULL))

  # ---------------------------------------------------------------------------
  expect_error(preprocess_chunks("unknown-file.Rmd"),
               "The file \\S+ does not exist. Check the YAML file entry")


  # ---------------------------------------------------------------------------
  preprocess_chunks("double-chunk-cat.Rmd")
  expect_identical(readLines("double-chunk-cat.Rmd"),
                   c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                     "cat(\"# Header",
                     "",
                     "Test text \", print('Hello World!'), \"\")",
                     "```",
                     "",
                     "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                     "cat(\"## Header 2",
                     "Test text \", print('Hello World 2!'), \"\")",
                      "```"))

  # ---------------------------------------------------------------------------
  expect_error(expect_message(
    preprocess_chunks("double-chunk-cat-extra-blanks.Rmd"),
    paste0("One or more chunks in the file double-chunk-cat-extra-blanks.Rmd ",
           "with knitr option")))

  # ---------------------------------------------------------------------------
  preprocess_chunks("double-chunk-cat-single-quotes.Rmd")
  actual <- readLines("double-chunk-cat-single-quotes.Rmd")
  expect_identical(actual, c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                             "cat(\"# Header",
                             "",
                             "Test text \", print(\"Hello World!\"), \"\")",
                             "```",
                             "",
                             "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                             "cat(\"## Header 2",
                              "Test text \", print('Hello World 2!'), \"\")",
                              "```"))

  # ---------------------------------------------------------------------------
  preprocess_chunks("empty-file.Rmd")
  expect_identical(readLines("empty-file.Rmd"), character(0))

  # ---------------------------------------------------------------------------
  rmd <- c("```{r test, eval = !fr(), needs_trans = TRUE}",
           "cat('Text in cat surrounded by single quotes')",
           "```")
  fn <- "single-quote-only-test.Rmd"
  writeLines(rmd, fn)

  preprocess_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r test, eval = !fr(), needs_trans = TRUE}",
                "cat(\"Text in cat surrounded by single quotes\")",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  rmd <- c("```{r test}",
           "cat('X')",
           "```")
  fn <- "single-quote-only-test.Rmd"
  writeLines(rmd, fn)

  preprocess_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r test}",
                "cat(\"X\")",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  rmd <- c("```{r test}",
           "# comment",
           "cat('X')",
           "```")
  fn <- "single-quote-only-test.Rmd"
  writeLines(rmd, fn)

  preprocess_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r test}",
                "# comment",
                "cat(\"X\")",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  rmd <- c("```{r test}",
           "# comment",
           "cat('X')",
           "```",
           "",
           "",
           "```{r test2, needs_trans = TRUE}",
           "cat(\"Y\")",
           "```")
  fn <- "single-quote-only-test.Rmd"
  writeLines(rmd, fn)

  preprocess_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r test}",
                "# comment",
                "cat(\"X\")",
                "```",
                "",
                "",
                "```{r test2, needs_trans = TRUE}",
                "cat(\"Y\")",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  rmd <- c("```{r test}",
           "# comment",
           "cat('`r unique(mtcars$cyl)`')",
           "```")
  fn <- "single-quote-only-test.Rmd"
  writeLines(rmd, fn)

  preprocess_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r test}",
                "# comment",
                "cat(\"\", unique(mtcars$cyl), \"\")",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  rmd <- c("```{r test, needs_trans = TRUE}",
           "```",
           "```{r test2, needs_trans = TRUE}",
           "print('Hello')",
           "#",
           "```")

  fn <- "empty-chunks-test.Rmd"
  writeLines(rmd, fn)
  offsets <- remove_comments_from_chunks(fn, verbose = TRUE)

  expect_error(preprocess_chunks(fn,
                                            line_offsets = offsets,
                                            verbose = TRUE),
               paste0("check the source chunks for those by searching ",
                      "the project for the chunk name"))


})
