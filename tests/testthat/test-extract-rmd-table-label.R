test_that("extract_rmd_table_label() works", {

  ret <- extract_rmd_table_label(NULL)
  expect_identical(ret, list(NULL, NULL))

  # ---------------------------------------------------------------------------
  j <- c("")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "", "")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "", "# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Generic text")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "   Table: Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("   Table: Caption", NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "    Table: Caption")
  expect_warning(ret <- extract_rmd_table_label(j))
  expect_null(ret[[1]])
  expect_identical(ret[[2]], j)

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "1. Item 1")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", ""))
  expect_identical(ret[[2]], "1. Item 1")

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "1. Item 1", "2. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", ""))
  expect_identical(ret[[2]], c("1. Item 1", "2. Item 2"))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "# Header 1")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", ""))
  expect_identical(ret[[2]], "# Header 1")

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "# Header 1", "# Header 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", ""))
  expect_identical(ret[[2]], c("# Header 1", "# Header 2"))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "-----", "Table header")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table: Caption", "-----",
                               "Table header"),
                             NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "-----", "Table header", "-----")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", "-----", "Table header",
                               "-----"))
  expect_null(ret[[2]])
  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "Table header", "-----", "Content")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", ""))
  expect_identical(ret[[2]], c("Table header", "-----", "Content"))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", ""))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "Another caption line",
         "Yet another caption line")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table: Caption", "Another caption line",
                               "Yet another caption line"), NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "Another caption line",
         "Yet another caption line", "", "Generic text")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table: Caption", "Another caption line",
                               "Yet another caption line"),
                             c("", "Generic text")))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table:", "")
  expect_warning(ret <- extract_rmd_table_label(j))
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table:", "Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table:", "Caption"), NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "   Table:", "Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("   Table:", "Caption"), NULL))
  # ---------------------------------------------------------------------------

  j <- c("", "", "    Table:", "Caption")
  expect_warning(ret <- extract_rmd_table_label(j))
  expect_identical(ret, list(NULL, j))
  # ---------------------------------------------------------------------------

  j <- c("", "", "Table:", "Caption", "Another caption line",
         "Yet another caption line")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table:", "Caption", "Another caption line",
                               "Yet another caption line"), NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table:", "Caption", "Another caption line",
         "Yet another caption line", "", "Generic text")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table:", "Caption", "Another caption line",
                               "Yet another caption line"),
                             c("", "Generic text")))

  # ---------------------------------------------------------------------------
  j <- c("Table: Caption", "Another caption line", "",
         "1. Item", "2. Item")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", "Another caption line"))
  expect_identical(ret[[2]], c("", "1. Item", "2. Item"))

  # ---------------------------------------------------------------------------
  j <- c("Table: Caption", "Another caption line",
         "1. Item", "2. Item")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table: Caption", "Another caption line", ""))
  expect_identical(ret[[2]], c("1. Item", "2. Item"))

  # ---------------------------------------------------------------------------
  j <- c("Table:", "Caption", "Another caption line", "",
         "1. Item", "2. Item")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table:", "Caption", "Another caption line"))
  expect_identical(ret[[2]], c("", "1. Item", "2. Item"))

  # ---------------------------------------------------------------------------
  j <- c("Table:", "Caption", "Another caption line",
         "1. Item", "2. Item")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret[[1]], c("Table:", "Caption", "Another caption line", ""))
  expect_identical(ret[[2]], c("1. Item", "2. Item"))
})
