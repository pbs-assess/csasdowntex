test_that("Conversion of header lines in Rmd works correctly", {

  chunk <- NULL
  tmp <- conv_header_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- "# Header 1"
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], "# Header 1")
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("# Header 1", "## Subheader 1", "## Subheader 2",
             "### Subsubheader 1")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c(chunk, ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("# Header 1", "", "",
             "## Subheader 1", "", "",
             "## Subheader 2", "### Subsubheader 1")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# Header 1", "## Subheader 1",
                               "## Subheader 2", "### Subsubheader 1", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("# Header 1", "", "",
             "## Subheader 1",
             "## Subheader 2",
             "### Subsubheader 1", "", "")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# Header 1", "## Subheader 1",
                               "## Subheader 2", "### Subsubheader 1", ""))
  expect_identical(tmp[[2]], c("", ""))
  # ---------------------------------------------------------------------------

  chunk <- c("", "", "# Header 1", "", "",
             "## Subheader 1",
             "## Subheader 2", "### Subsubheader 1", "", "")
  tmp <- conv_header_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)
  # ---------------------------------------------------------------------------

  chunk <- c("# H1", "")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", ""))
  expect_identical(tmp[[2]], "")
  # ---------------------------------------------------------------------------

  chunk <- c("# H1", "", "", "", "", "Normal text", "", "", "", "", "", "# H2")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", ""))
  expect_identical(tmp[[2]], c("", "", "", "", "Normal text", "", "", "", "",
                               "", "# H2"))
  # ---------------------------------------------------------------------------

  chunk <- c("# H1",
             "# H2",
             "# H3")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", "# H2", "# H3", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("# H1", "",
             "# H2",
             "# H3")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", "# H2", "# H3", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("# H1",
             "# H2", "",
             "# H3")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", "# H2", "# H3", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("# H1", "",
             "# H2", "",
             "# H3")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", "# H2", "# H3", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("# H1", "",
             "# H2", "",
             "# H3", "")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", "# H2", "# H3", ""))
  expect_identical(tmp[[2]], "")
  # ---------------------------------------------------------------------------

  chunk <- c("# H1",
             "# H2",
             "# H3", "")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", "# H2", "# H3", ""))
  expect_identical(tmp[[2]], "")
  # ---------------------------------------------------------------------------

  chunk <- c("# H1", "-----", "a", "-----", "b", "-----")
  tmp <- conv_header_lines(chunk)
  expect_identical(tmp[[1]], c("# H1", ""))
  expect_identical(tmp[[2]], c("-----", "a", "-----", "b", "-----"))
  # ---------------------------------------------------------------------------

})
