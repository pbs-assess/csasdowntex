test_that("remove_pandoc_highlight_arg() removes old highlight style args", {
  args <- c("--to", "latex", "--highlight-style", "tango", "--wrap=none")

  expect_identical(
    remove_pandoc_highlight_arg(args),
    c("--to", "latex", "--wrap=none")
  )
})

test_that("remove_pandoc_highlight_arg() removes new syntax highlighting args", {
  args <- c("--to", "latex", "--syntax-highlighting", "tango", "--wrap=none")

  expect_identical(
    remove_pandoc_highlight_arg(args),
    c("--to", "latex", "--wrap=none")
  )
})

test_that("remove_pandoc_highlight_arg() removes equals-style args", {
  args <- c("--to", "latex", "--syntax-highlighting=tango", "--wrap=none")

  expect_identical(
    remove_pandoc_highlight_arg(args),
    c("--to", "latex", "--wrap=none")
  )
})

test_that("remove_pandoc_highlight_arg() leaves args unchanged if absent", {
  args <- c("--to", "latex", "--wrap=none")

  expect_identical(remove_pandoc_highlight_arg(args), args)
})
