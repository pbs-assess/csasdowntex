test_that("csasdown::post_process_extract_chunks() function works", {

  expect_error(post_process_extract_chunks(letters, NULL, NULL),
               "`beg_inds` is zero length. You must have at least one set of indices to extract")

  expect_error(post_process_extract_chunks(letters, 1:2, NULL),
               "`beg_inds` and `end_inds` must be the same length")

  expect_error(post_process_extract_chunks(letters, 1:2, 1),
               "`beg_inds` and `end_inds` must be the same length")

  a <- post_process_extract_chunks(letters, c(1, 11), c(10, 12))

  expect_equal(length(a), 3)
  expect_equal(a$between[[1]], letters[1:10])
  expect_equal(a$between[[2]], letters[11:12])
  expect_equal(a$inbetween[[1]], "")
  expect_equal(a$inbetween[[2]], letters[13:26])
  expect_equal(a$first, TRUE)

  b <- post_process_extract_chunks(letters, c(2, 8), c(6, 10))

  expect_equal(length(b), 3)
  expect_equal(b$between[[1]], letters[2:6])
  expect_equal(b$between[[2]], letters[8:10])
  expect_equal(b$inbetween[[1]], "a")
  expect_equal(b$inbetween[[2]], "g")
  expect_equal(b$inbetween[[3]], letters[11:26])
  expect_equal(b$first, FALSE)

  d <- post_process_extract_chunks(letters, c(2, 8, 15), c(6, 10, 25))

  expect_equal(length(d), 3)
  expect_equal(d$between[[1]], letters[2:6])
  expect_equal(d$between[[2]], letters[8:10])
  expect_equal(d$between[[3]], letters[15:25])
  expect_equal(d$inbetween[[1]], "a")
  expect_equal(d$inbetween[[2]], "g")
  expect_equal(d$inbetween[[3]], letters[11:14])
  expect_equal(d$inbetween[[4]], "z")
  expect_equal(d$first, FALSE)

  expect_error(post_process_extract_chunks(letters, c(2, 8, 15), c(8, 10, 25)),
               "overlapping chunks have been defined.")

  expect_error(post_process_extract_chunks(letters, c(2, 8, 15), c(9, 10, 25)),
               "overlapping chunks have been defined.")

  expect_error(post_process_extract_chunks(letters, c(2, 8, 10), c(7, 10, 25)),
               "overlapping chunks have been defined.")

  expect_error(post_process_extract_chunks(letters, c(22, 8, 15), c(7, 10, 25)),
               "The first beginning chunk index is greater than the first end chunk index")

})
