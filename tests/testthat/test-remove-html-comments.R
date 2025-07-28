test_that("remove_html_comments() works", {

  j <- NULL
  ret <- remove_html_comments(j)
  expect_null(ret)
  # ---------------------------------------------------------------------------

  j <- c("<!---->")
  ret <- remove_html_comments(j)
  expect_identical(ret, NA_character_)
  # ---------------------------------------------------------------------------

  j <- c("<!---------------------------->")
  ret <- remove_html_comments(j)
  expect_identical(ret, NA_character_)
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, NA_character_)
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  -->world!")
  ret <- remove_html_comments(j)
  expect_identical(ret, "Hello world!")
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  -->world!<!-- csasdown is great       -->!")
  ret <- remove_html_comments(j)
  expect_identical(ret, "Hello world!!")
  # ---------------------------------------------------------------------------

  j <- c("Hi <!--ello--><!-- Anything  -->world!<!-- csasdown is great   -->!")
  ret <- remove_html_comments(j)
  expect_identical(ret, "Hi world!!")
  # ---------------------------------------------------------------------------

  j <- c("Multiple <!-- Anything  ", "Lines -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, c("Multiple ", NA_character_))
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  ", "-->Lines", "<!--", "xxx", "--> Hello!")
  ret <- remove_html_comments(j)
  expect_identical(ret, c(NA_character_, "Lines", NA_character_,
                          NA_character_, " Hello!"))
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  ", "-->Lines", "<!--", "xxx", "--> Hello!<!--",
         "XX", "--> World!")
  ret <- remove_html_comments(j)
  expect_identical(ret, c(NA_character_, "Lines", NA_character_, NA_character_,
                          " Hello!", NA_character_, " World!"))
  # ---------------------------------------------------------------------------

  j <- c("<!-- A -->",
         "<-- B ->",
         "<!-- C -->",
         "<!-- D -->",
         "E",
         "<!- F ->",
         "<!-- G -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, c(NA_character_, "<-- B ->", NA_character_,
                          NA_character_, "E", "<!- F ->", NA_character_))
  # ---------------------------------------------------------------------------

  j <- c("<!-- A ",
         "<-- B ->",
         " C ->",
         "<-- D >",
         "E",
         " F ->",
         "< G -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, rep(NA_character_, 7))
  # ---------------------------------------------------------------------------

  j <- c("<!--Hello <!-- ", "Anything --> -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, rep(NA_character_, 2))
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  ")
  expect_error(remove_html_comments(j))
  # ---------------------------------------------------------------------------

  j <- c("Hello --> Anything  ")
  expect_error(remove_html_comments(j))
  # ---------------------------------------------------------------------------

  j <- c("Hello --> Anything  <!--")
  expect_error(remove_html_comments(j))
  # ---------------------------------------------------------------------------

})
