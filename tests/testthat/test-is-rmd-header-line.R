test_that("is_rmd_header_line() works", {

  expect_null(is_rmd_header_line(NULL))
  # ---------------------------------------------------------------------------

  expect_error(is_rmd_header_line(NA),
               "An NA is present in the vector of strings:")
  # ---------------------------------------------------------------------------

  expect_error(is_rmd_header_line(c("1. List Item", NA)),
               "An NA is present in the vector of strings:")
  # ---------------------------------------------------------------------------

  expect_false(is_rmd_header_line(""))
  # ---------------------------------------------------------------------------

  expect_false(is_rmd_header_line("1. List Item"))
  # ---------------------------------------------------------------------------

  expect_false(is_rmd_header_line("plaintext"))
  # ---------------------------------------------------------------------------

  expect_true(is_rmd_header_line("# No indentation"))
  # ---------------------------------------------------------------------------

  expect_true(is_rmd_header_line(" # One indentation"))
  # ---------------------------------------------------------------------------

  expect_true(is_rmd_header_line("  # Two indentation"))
  # ---------------------------------------------------------------------------

  expect_true(is_rmd_header_line("   # Three indentation (max"))
  # ---------------------------------------------------------------------------

  expect_false(is_rmd_header_line("    # Four indentation (too much)"))
  # ---------------------------------------------------------------------------

  expect_false(is_rmd_header_line("#No space between hash and text"))
  # ---------------------------------------------------------------------------

  expect_true(is_rmd_header_line("## Second header"))
  # ---------------------------------------------------------------------------

  expect_true(is_rmd_header_line("### Third header"))
  # ---------------------------------------------------------------------------

  expect_true(is_rmd_header_line("   ### Third header indented 3"))
  # ---------------------------------------------------------------------------

  expect_false(is_rmd_header_line("###Third header, no space"))
  # ---------------------------------------------------------------------------

  j <- c("# Header 1", "#   Header 2", " ## Secondary header 1",
         "## Secondary header 2", "#Incorrect header")
  ret <- is_rmd_header_line(j)
  expect_identical(ret, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  # ---------------------------------------------------------------------------
})
