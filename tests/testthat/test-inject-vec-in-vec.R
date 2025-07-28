test_that("inject_vec_in_vec() works", {

  expect_error(inject_vec_in_vec(1, 1, 1:2))

  expect_error(inject_vec_in_vec(NULL, 1, 1))
  expect_error(inject_vec_in_vec(1, NULL, 1))
  expect_error(inject_vec_in_vec(1, 1, NULL))

  expect_error(inject_vec_in_vec(NA, 1, 1))
  expect_error(inject_vec_in_vec(1, NA, 1))
  expect_error(inject_vec_in_vec(1, 1, NA))

  expect_error(inject_vec_in_vec(c("a", NA), 1, 1))
  expect_error(inject_vec_in_vec(1, c("a", NA), 1))
  expect_error(inject_vec_in_vec(1, 1, c("a", NA)))

  expect_identical(inject_vec_in_vec(c("a", NULL), 1, 1), 1)
  expect_identical(inject_vec_in_vec(1, c("a", NULL), 1), "a")
  expect_error(inject_vec_in_vec(1, 1, c("a", NULL)))

  ab <- c("a", "b")
  expect_identical(inject_vec_in_vec("", ab, 1), ab)
  expect_identical(inject_vec_in_vec("b", ab, 1), ab)
  expect_identical(inject_vec_in_vec(1:10, "a", 1:10), rep("a", 10))
  expect_identical(inject_vec_in_vec(1:10, ab, 1), c(ab, 2:10))
  expect_identical(inject_vec_in_vec(1:10, ab, 5), c(1:4, ab, 6:10))
  expect_identical(inject_vec_in_vec(1:10, ab, 10), c(1:9, ab))
  expect_identical(inject_vec_in_vec(1:10, ab, c(1, 3, 6, 10)),
                   c(ab, 2, ab, 4, 5, ab, 7, 8, 9, ab))


  a <- letters[1:10]
  b <- LETTERS[25:26]
  actual <- inject_vec_in_vec(a, b, c(2, 5, 8))
  expected <- c("a", "Y", "Z", "c", "d", "Y", "Z", "f", "g", "Y", "Z", "i", "j")
  expect_identical(actual, expected)

})
