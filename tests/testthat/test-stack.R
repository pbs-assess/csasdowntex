test_that("stack implementation works", {

  stack <- NULL
  expect_equal(stk_size(stack), 0)
  stack <- stk_push(stack, "a")
  stack <- stk_push(stack, "b")
  stack <- stk_push(stack, "c")
  stack <- stk_push(stack, "d")
  expect_equal(stk_size(stack), 4)
  expect_identical(stack, c("a", "b", "c", "d"))

  tmp <- stk_pop(stack)
  val <- tmp$val
  stack <- tmp$stack
  expect_equal(val, "d")
  expect_equal(stk_size(stack), 3)
  stack <- stk_push(stack, "e")
  expect_equal(stk_size(stack), 4)

  tmp <- stk_pop(stack)
  val <- tmp$val
  stack <- tmp$stack
  expect_equal(val, "e")
  expect_equal(stk_size(stack), 3)

  tmp <- stk_pop(stack)
  val <- tmp$val
  stack <- tmp$stack
  expect_equal(val, "c")
  expect_equal(stk_size(stack), 2)

  tmp <- stk_pop(stack)
  val <- tmp$val
  stack <- tmp$stack
  expect_equal(val, "b")
  expect_equal(stk_size(stack), 1)

  tmp <- stk_pop(stack)
  val <- tmp$val
  stack <- tmp$stack
  expect_equal(val, "a")
  expect_equal(stack, NULL)
  expect_equal(stk_size(stack), 0)

  tmp <- stk_pop(stack)
  val <- tmp$val
  stack <- tmp$stack
  expect_equal(val, NULL)
  expect_equal(stack, NULL)
  expect_equal(stk_size(stack), 0)
})
