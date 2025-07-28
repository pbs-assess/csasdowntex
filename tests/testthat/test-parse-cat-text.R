test_that("cat parser works", {

  # -----------------------------------------------------------------------------
  # Not starting with cat
  strs <- c("'This is an example",
            "of not starting with cat()")
  expect_error(parse_cat_text(strs),
               "The first line of \\S+ must start with")

  # -----------------------------------------------------------------------------
  # Wrong quote type
  strs <- c("cat(`'This is an example",
            "of not starting with cat()`)")
  expect_error(parse_cat_text(strs),
               paste0("Quote type not allowed for \\S+ text. You must use ",
                      "either single or double quotes. You used `"))

  # -----------------------------------------------------------------------------
  # `verbose`
  strs <- c("cat('This is an example",
            "of not starting with cat()')")
  x <- capture_log(parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  mess <- purrr::map_chr(j, ~{.x$message})
  expect_match(mess[2], "Pushed '\\(' to stack on line 2, char 25")
  expect_match(mess[3], "Matched '\\)' on line 2, char 26")
  expect_match(mess[4], "Matched closing '\\)' for `cat\\(\\)` on line 2, char 28")

  # -----------------------------------------------------------------------------
  # `verbose` error
  strs <- c("cat('This is an )')")
  x <- capture_log(parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  w <- purrr::map_chr(j, ~{.x$message})
  expect_match(w[1], "Pushed '\\(' from \\S+ to stack. Stack size is now 1")
  expect_match(w[2], "No matching '\\(' for this '\\)' on line  1, char 13")

  x <- capture_log(parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  mess <- purrr::map_chr(j, ~{.x$message})
  types <- purrr::map_chr(j, ~{.x$type})
  expect_identical(types[1], "message")
  expect_match(mess[1], "Pushed '\\(' from \\S+ to stack. Stack size is now 1")
  expect_identical(types[2], "error")
  expect_match(mess[2], "No matching '\\(' for this '\\)' on line  1, char 13")

  # -----------------------------------------------------------------------------
  # Single quotes
  strs <- c("cat('This is an example",
            "of matching cat parens",
            "')")
  strs_mod <- c("'This is an example",
                "of matching cat parens",
                "'")
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Double quotes
  strs <- c('cat("This is an example',
            'of matching cat parens',
            '")')
  strs_mod <- c('"This is an example',
                'of matching cat parens',
                '"')
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Correct complex paren matching
  strs <- c('cat("This is (an) example',
            'of matching (cat) parens()',
            '")')
  strs_mod <- c('"This is (an) example',
                'of matching (cat) parens()',
                '"')
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Correct complex paren matching
  strs <- c('cat("This is an example',
            'of matching cat(((()))) parens()',
            '()()")')
  strs_mod <- c('"This is an example',
                'of matching cat(((()))) parens()',
                '()()"')
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Catch extra trailing characters after ')'
  strs <- c('cat("This is an example',
            'of matching cat parens',
            '") extra')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("This is an )example',
            'of matching cat parens',
            '")')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This is an example',
            'of matching cat parens',
            '")')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This( is an example',
            'of matching cat))) parens',
            '")')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("This is an example',
            'of ((())))matching cat parens',
            '")')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("This is an example',
            'of matching cat parens',
            ')")')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This is an example',
            'of (matching( cat) parens)))',
            '")')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This) is an example',
            'of matching (cat) (parens',
            '")')
  expect_error(parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Longer than needed text, multiple lines (ok)
  strs <- c('cat("This is an example',
            'of longer than needed text',
            '")',
            'to find the correct text')
  strs_mod <- c('"This is an example',
                'of longer than needed text',
                '"')
  expect_identical(parse_cat_text(strs), strs_mod)

  # -----------------------------------------------------------------------------
  # Longer than needed text, same lines
  strs <- c('cat("This is an example',
            'of longer than needed text',
            '") to find the correct text')
  expect_error(parse_cat_text(strs))

})
