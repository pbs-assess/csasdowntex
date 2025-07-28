test_that("Conversion of blank lines in Rmd works correctly", {

  chunk <- NULL
  tmp <- conv_blank_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c(" ")
  tmp <- conv_blank_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], " ")
  # ---------------------------------------------------------------------------

  chunk <- c(" ", "")
  tmp <- conv_blank_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], c(" ", ""))
  # ---------------------------------------------------------------------------

  chunk <- c("")
  tmp <- conv_blank_lines(chunk)
  expect_identical(tmp[[1]], c("", "\\n", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("", "")
  tmp <- conv_blank_lines(chunk)
  expect_identical(tmp[[1]], c("\\n", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("", "", "")
  tmp <- conv_blank_lines(chunk)
  expect_identical(tmp[[1]], c("\\n", "", "\\n", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("", "", "", "", "")
  tmp <- conv_blank_lines(chunk)
  expect_identical(tmp[[1]], c("\\n", "", "\\n", "", "\\n", "", "\\n", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("asd", "", "", "")
  tmp <- conv_blank_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)
  # ---------------------------------------------------------------------------

  chunk <- c("", "Some text..", "More Rmd text")
  tmp <- conv_blank_lines(chunk)
  expect_identical(tmp[[1]], c("\\n", ""))
  expect_identical(tmp[[2]], c("Some text..", "More Rmd text"))
  # ---------------------------------------------------------------------------

  chunk <- c("", "", "", "Some text..", "More Rmd text")
  tmp <- conv_blank_lines(chunk)
  expect_identical(tmp[[1]], c("\\n", "", "\\n", ""))
  expect_identical(tmp[[2]], c("Some text..", "More Rmd text"))
  # ---------------------------------------------------------------------------

})
