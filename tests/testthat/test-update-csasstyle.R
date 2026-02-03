test_that("update_csasstyle() works", {

  wd <- getwd()
  testing_path <- file.path(tempdir(), "test-update-csasstyle-resdoc")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # ---------------------------------------------------------------------------
  expect_error(update_csasstyle(copy = FALSE, line_nums = TRUE),
               paste0("YAML header. The permanent style file cannot be ",
                      "modified as needed to include line numbering"))

  # ---------------------------------------------------------------------------
  expect_error(update_csasstyle(copy = FALSE, line_nums = FALSE,
                                           lot_lof = TRUE),
               paste0("YAML header. The permanent style file cannot be ",
                      "modified as needed to include the lists of tables ",
                      "and figures"))

  # ---------------------------------------------------------------------------
  expect_error(update_csasstyle(copy = FALSE,
                                           line_nums = FALSE,
                                           draft_watermark = TRUE),
               paste0("YAML header. The permanent style file cannot be ",
                      "modified as needed to include the DRAFT watermark"))

  # ---------------------------------------------------------------------------
  # Set lot_lof (toggle show List of tables/List of Figures in doc)
  installed_3_1_2 <- pandoc::pandoc_available("3.1.2")
  if (installed_3_1_2) {
    # if (!installed_3_1_2){
    # pandoc::pandoc_install("3.1.2")
    # }
    # pandoc::pandoc_activate("3.1.2")
    rmd <- readLines("index.Rmd")
    ind <- grep("lot_lof:", rmd)
    rmd[ind] <- "   lot_lof: true"
    writeLines(rmd, "index.Rmd")
    render()
    expect_true(file.exists("_book/resdoc-english.pdf"))
    # if (!installed_3_1_2){
    # pandoc::pandoc_uninstall("3.1.2")
    # }
  }

  # ---------------------------------------------------------------------------
  # Set draft_watermark
  unlink("_book/resdoc-english.pdf", force = TRUE)
  unlink("_book/resdoc-english.tex", force = TRUE)
  rmd <- readLines("index.Rmd")
  ind <- grep("lot_lof:", rmd)
  rmd[ind] <- "   lot_lof: false"
  ind <- grep("draft_watermark:", rmd)
  rmd[ind] <- "   draft_watermark: true"
  writeLines(rmd, "index.Rmd")
  render()
  expect_true(file.exists("_book/resdoc-english.pdf"))

  setwd(wd)
})
