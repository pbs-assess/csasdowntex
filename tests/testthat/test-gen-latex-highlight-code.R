test_that("gen_latex_highlight_code() works", {

  # Make sure decimal points in file don't get compared
  # with commas (if french)
  options(french = FALSE)
  options(OutDec = ".")

  all_themes = c("pygments", "tango", "espresso", "zenburn",
                 "kate", "monochrome", "breezedark", "haddock")
  json_fns <- system.file(file.path("themes", paste0(all_themes, ".json")),
                                    package = "csasdowntex",
                          mustWork = TRUE)

  latex_fns <- system.file(file.path("themes", paste0(all_themes, ".latex")),
                           package = "csasdowntex",
                           mustWork = TRUE)

  json_lst <- purrr::map(json_fns, ~{readLines(.x)})
  latex_lst <- purrr::map(latex_fns, ~{readLines(.x)})

  new_latex_lst <- gen_latex_highlight_code(json_lst)

  expect_error(gen_latex_highlight_code(NULL),
               "cannot be")

  expect_error(gen_latex_highlight_code(""),
               "element 1 does not start with an open curly brace")

  expect_identical(latex_lst, new_latex_lst)
})
