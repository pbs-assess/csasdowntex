test_that("replace_vecs_with_vecs() works", {

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list("xx")
  s_ind <- 1
  e_ind <- 1
  expect_error(replace_vecs_with_vecs(NULL, r_vecs, s_ind, e_ind))
  expect_error(replace_vecs_with_vecs(vec, NULL, s_ind, e_ind))
  expect_error(replace_vecs_with_vecs(vec, r_vecs, NULL, e_ind))
  expect_error(replace_vecs_with_vecs(vec, r_vecs, s_ind, NULL))
  expect_error(replace_vecs_with_vecs(NA, r_vecs, s_ind, e_ind))
  expect_error(replace_vecs_with_vecs(vec, NA, s_ind, e_ind))
  expect_error(replace_vecs_with_vecs(vec, r_vecs, NA, e_ind))
  expect_error(replace_vecs_with_vecs(vec, r_vecs, s_ind, NA))

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list("xx")
  s_ind <- 1
  e_ind <- 1
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("xx", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                "U", "V", "W", "X", "Y", "Z")

  expect_identical(j, expected)

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list("xx")
  s_ind <- c(26)
  e_ind <- c(26)
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                "U", "V", "W", "X", "Y", "xx")

  expect_identical(j, expected)

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list("xx", c("aa", "yy"))
  s_ind <- c(1, 26)
  e_ind <- c(1, 26)
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("xx", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                "U", "V", "W", "X", "Y", "aa", "yy")

  expect_identical(j, expected)

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list("xx", "yy", c("bb", "cc", "dd"))
  s_ind <- c(1, 12, 26)
  e_ind <- c(10, 13, 26)
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("xx", "K", "yy", "N", "O", "P", "Q", "R", "S", "T",
                "U", "V", "W", "X", "Y", "bb", "cc", "dd")

  expect_identical(j, expected)

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list(c("aa", "bb", "cc"), c("zz", "yy", "xx"))
  s_ind <- c(3, 22)
  e_ind <- c(5, 24)
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("A", "B", "aa", "bb", "cc", "F", "G", "H", "I", "J",
                "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                "U", "zz", "yy", "xx", "Y", "Z")

  expect_identical(j, expected)

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list(c("aa", "bb", "cc"), c("zz", "yy", "xx"))
  s_ind <- c(1, 16)
  e_ind <- c(14, 26)
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("aa", "bb", "cc", "O", "zz", "yy", "xx")

  expect_identical(j, expected)

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list(c("aa", "bb", "cc"), c("zz", "yy", "xx"))
  s_ind <- c(1, 16)
  e_ind <- c(15, 26)
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("aa", "bb", "cc", "zz", "yy", "xx")

  expect_identical(j, expected)

  # ---------------------------------------------------------------------------
  vec <- LETTERS[1:26]
  r_vecs <- list(1, 2)
  s_ind <- c(1, 18)
  e_ind <- c(15, 25)
  j <- replace_vecs_with_vecs(vec, r_vecs, s_ind, e_ind)
  expected <- c("1", "P", "Q", "2", "Z")

  expect_identical(j, expected)

})
