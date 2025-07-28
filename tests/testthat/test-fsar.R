test_that("FSAR builds", {
  testing_path <- file.path(tempdir(), "fsar")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    "fsar",
    create_dir = FALSE,
    edit = FALSE
  ))
  suppressWarnings(csasdown::render_sar())
  expect_true(file.exists("_book/fsar.docx"))
})