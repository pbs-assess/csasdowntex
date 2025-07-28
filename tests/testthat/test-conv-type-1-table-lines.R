test_that("Conversion of type 1 table lines in Rmd works correctly", {

  # ---------------------------------------------------------------------------
  chunk <- NULL
  tmp <- conv_type_1_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])

  # ---------------------------------------------------------------------------
  chunk <- "----"
  expect_error(tmp <- conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "", "-----")
  expect_error(tmp <- conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----")
  expect_error(conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "-----")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----"))
  expect_null(tmp[[2]])

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "-----", "", "", "")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----"))
  expect_identical(tmp[[2]], c("", "", ""))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "-----", "", "",
             "Table: This is a table caption")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----",
                               "Table: This is a table caption"))
  expect_null(tmp[[2]])

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "", "", "" ,"-----")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----"))
  expect_null(tmp[[2]])

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----")
  expect_error(tmp <- conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "xyz", "-----", "abc", "", "efg", "-----", "", "",
             "Non-caption text")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "xyz", "-----", "abc", "", "efg",
                               "-----"))
  expect_identical(tmp[[2]], c("", "", "Non-caption text"))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----",
             "Table: (\\#tab:text) Test label")
  expect_error(tmp <- conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "-----", "", "",
             "No caption text", "", "Table: (\\#tab:text) Test label ")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----"))
  expect_identical(tmp[[2]], c("", "", "No caption text", "",
                               "Table: (\\#tab:text) Test label "))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----",
             "Table: (\\#tab:text) Test label.", "Two lines.")
  expect_error(tmp <- conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "-----", "", "Table:",
             "- 2nd line ", "         - third line", "", "")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----","asd", "-----", "xyz", "-----",
                               "Table:", "- 2nd line ", ""))
  expect_identical(tmp[[2]], c("         - third line", "", ""))

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             " hgnhhnhnf    oiuoiuo  ",
             "----------- -----------",
             "", "", "", "", "", "",
             "Table: (\\#tab:text) Test label.", "Two lines.")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: (\\#tab:text) Test label.",
                               "Two lines."))
  expect_null(tmp[[2]])

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "", "", "", "", "", "",
             "Table: (\\#tab:text) Test label.", "", "", "Two lines.")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: (\\#tab:text) Test label."))
  expect_identical(tmp[[2]], c("", "", "Two lines."))

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------"))
  expect_null(tmp[[2]])

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------"))
  expect_identical(tmp[[2]], "")

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "", "", "", "Table: Caption")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: Caption"))
  expect_null(tmp[[2]])

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "", "", "", "Table: Caption", "1. Item 1")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: Caption", ""))
  expect_identical(tmp[[2]], "1. Item 1")

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "Table: Caption", "1. Item 1")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: Caption", ""))
  expect_identical(tmp[[2]], "1. Item 1")

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "Table: Caption", "", "", "1. Item 1")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: Caption"))
  expect_identical(tmp[[2]], c("", "", "1. Item 1"))

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "Table: Caption", "", "", "", "", "", "1. Item 1")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: Caption"))
  expect_identical(tmp[[2]], c("", "", "", "", "", "1. Item 1"))

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             "", "", "", "",
             " # hgnhhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "Table: Caption", "", "", "", "", "", "1. Item 1")
  expect_error(conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " -gfbbggfbf   ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "Table: Caption", "", "", "", "", "", "1. Item 1")
  tmp <- conv_type_1_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " -gfbbggfbf   ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: Caption"))
  expect_identical(tmp[[2]], c("", "", "", "", "", "1. Item 1"))

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " - gfbbggfbf   ffvvfvf  ",
             "", "", "", "",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "Table: Caption", "", "", "", "", "", "1. Item 1")
  expect_error(conv_type_1_table_lines(chunk))

  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf   ffvvfvf  ",
             "", "", "", "",
             " 1. hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "Table: Caption", "", "", "", "", "", "1. Item 1")
  expect_error(conv_type_1_table_lines(chunk))
})
