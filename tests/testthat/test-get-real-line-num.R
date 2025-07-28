test_that("get_real_line_num works", {

  # ---------------------------------------------------------------------------
  expect_error(get_real_line_num(NULL),
               "cannot be")

  # ---------------------------------------------------------------------------
  expect_error(get_real_line_num("header", NULL),
               "cannot be")

  # ---------------------------------------------------------------------------
  expect_error(get_real_line_num("header", 1:4),
               "must be a")

  # ---------------------------------------------------------------------------
  x <- tibble::tibble(fn = rep("test.Rmd", 4),
                      chunk_header = c("```{r test-1-en}",
                                       "```{r test-1-en}",
                                       "```{r test-2-en}",
                                       "```{r test-2-fr}"),
                      chunk_ind = c(1, 5, 9, 12),
                      pre_num = c(0, 3, 5, 2),
                      post_num = c(1, 1, 0, 4),
                      rmd_num = c(0, 6, 0, 17))
  expect_error(get_real_line_num("```{r test-0-en}", x),
               "There is no row that matches")

  # ---------------------------------------------------------------------------
  expect_error(get_real_line_num("```{r test-1-en}", x),
               "There is more than one row that matches")

  # ---------------------------------------------------------------------------
  actual <- get_real_line_num("```{r test-2-en}", x)
  expect_identical(actual, x[3, ]$chunk_ind + x[3, ]$pre_num + 1)

})
