test_that("Removing all comments from knitr chunks works", {

  # dr <- file.path(testthat::test_path(), "remove-comments-from-chunk")
  dr <- tempdir()
  fn <- file.path(dr, "tests.Rmd")
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "```",
                "# BBBBBBBBBB",
                "",
                "BBBBBBBBBB",
                "")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
                "",
                "AAAAAAAAAA",
                "",
                "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
           "",
           "AAAAAAAAAA",
           "",
           "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "```",
           "# BBBBBBBBBB",
           "",
           "BBBBBBBBBB",
           "")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "```",
                "# BBBBBBBBBB",
                "",
                "BBBBBBBBBB",
                "",
                "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
                "cat('More text!')",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
           "",
           "AAAAAAAAAA",
           "",
           "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "```",
           "# BBBBBBBBBB",
           "",
           "BBBBBBBBBB",
           "",
           "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
           "cat('More text!')",
           "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```",
             "# CCCCCCCCCC",
             "",
             "CCCCCCCCCC",
             "")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
           "",
           "AAAAAAAAAA",
           "",
           "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "```",
           "# BBBBBBBBBB",
           "",
           "BBBBBBBBBB",
           "",
           "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
           "cat('More text!')",
           "```",
           "# CCCCCCCCCC",
           "",
           "CCCCCCCCCC",
           "")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "```",
                "# AAAAAAAAAA",
                "",
                "AAAAAAAAAA",
                "",
                "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
                "cat('More text!')",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3")
  writeLines(chunk, fn)
  expect_error(remove_comments_from_chunks(fn),
               paste0("The number of knitr starting code chunk header ",
                      "lines does not equal the number of ending code chunk ",
                      "lines (triple backtick-lines)"),
               fixed = TRUE)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.",
             "')",
             "",
             "cat('# Test comment 3')",
             "```",
             "# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  expect_error(remove_comments_from_chunks(fn),
               paste0("inside each code chunk:"))

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.",
             "')",
             "```",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "cat('More text!')",
             "```")
  writeLines(chunk, fn)
  expect_message(remove_comments_from_chunks(fn, verbose = TRUE),
               paste0("There were no comments in chunks found to remove"))

})
