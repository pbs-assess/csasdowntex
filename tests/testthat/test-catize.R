str0 <- "Test without r code"
str1 <- "`r 1 + 1` Test with r code first only"
str2 <- "Test with r code last only `r 2 * 3`"
str3 <- "`r 1 + 1` Test with r code first and last only `r 2 * 3`"
str4 <- "`r 1`"
str5 <- "complex `r map(x, ~{.x / (.x ^ 2) })` r code example from directory: `r here()`."
str6 <- "`r code_1``r code_2`"

test_that("catize() function works", {
  i0 <- catize(str0)
  i1 <- catize(str1)
  i2 <- catize(str2)
  i3 <- catize(str3)
  i4 <- catize(str4)
  i5 <- catize(str5)
  i6 <- catize(str6)
  i7 <- catize("")

  expect_equal(i0, str0)
  expect_equal(as.character(i1), "\", 1 + 1, \" Test with r code first only")
  expect_equal(as.character(i2), "Test with r code last only \", 2 * 3, \"")
  expect_equal(as.character(i3), "\", 1 + 1, \" Test with r code first and last only \", 2 * 3, \"")
  expect_equal(as.character(i4), "\",  1 ,\"")
  expect_equal(as.character(i5), "complex \", map(x, ~{.x / (.x ^ 2) }), \" r code example from directory: \", here(), \".")
  expect_equal(as.character(i6), "\", code_1, \"\", code_2, \"")
  expect_equal(i7, "")

})

