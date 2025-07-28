df <- data.frame(a = LETTERS[1:26], b = letters[1:26])

test_that("csas_table works with an extra_header", {
  x <- csas_table(
    df,
    caption = "Example of long table with header above column names",
    format = "latex",
    bold_header = FALSE,
    escape = FALSE,
    repeat_header = TRUE,
    font_size = 8,
    extra_header = toupper(letters[1:ncol(df)]),
    ex_line_sep = 0,
    ex_align = "r")

  expect_true(grepl("endfirsthead", x))

  # ---------------------------------------------------------------------------
  # Column names with newlines
  col_names <- c("Capital\nLetters", "Small\nLetters")
  x <- csas_table(
    df,
    caption = "Newline col names",
    format = "latex",
    bold_header = FALSE,
    escape = FALSE,
    repeat_header = TRUE,
    font_size = 8,
    extra_header = toupper(letters[1:ncol(df)]),
    ex_line_sep = 0,
    ex_align = "r",
    col_names = col_names)

  nl <- grep("\\{Capital\\\\\\\\Letters\\}", x)
  expect_identical(nl, 1L)
  nl <- grep("\\{Small\\\\\\\\Letters\\}", x)
  expect_identical(nl, 1L)

  # ---------------------------------------------------------------------------
  # Add extra header for non-latex should throw error
  expect_error(csas_table(df,
                                    extra_header = toupper(letters[1:ncol(df)])),
               paste0("Adding an extra header is only supported for latex"))

  # ---------------------------------------------------------------------------
  # Try bold header for latex should throw error
  expect_warning(csas_table(df,
                                      format = "latex",
                                      bold_header = TRUE),
                 paste0("Bold headers not supported for the \\S+ format"))

  # ---------------------------------------------------------------------------
  df <- data.frame(a = LETTERS[1:26], b = c(letters[1:25], "endhead"))
  expect_warning(csas_table(df,
                                      format = "latex",
                                      bold_header = FALSE),
                 paste0("found more than once in the table latex"))

  # ---------------------------------------------------------------------------
  df <- data.frame(a = LETTERS[1:26], b = c(letters[1:25], "endfirsthead"))
  expect_warning(csas_table(df,
                                      format = "latex",
                                      bold_header = FALSE),
                 paste0("found more than once in the table"))

  # ---------------------------------------------------------------------------
  expect_error(add_extra_header(align = "x"),
               "must be one of")

})
