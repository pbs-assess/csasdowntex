test_that("fix_envs() works", {

  # ---------------------------------------------------------------------------
  # Place French region where English region goes and make sure it
  # is replaced with English in the tex file
  testing_path <- file.path(tempdir(), "test-fix-envs")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  rmd <- readLines("index.Rmd")
  fr_region <- grep("french_region:", rmd, value = TRUE)
  fr_region <- gsub("^\\s*french_region:\\s*(.*)$", "\\1", fr_region)
  en_region_ind <- grep("^\\s*region:", rmd)
  rmd[en_region_ind] <- paste0("region: ", fr_region)
  writeLines(rmd, "index.Rmd")
  render(suppress_warnings = TRUE)
  expect_true(file.exists("_book/sr-english.pdf"))
  expect_true(file.exists("_book/sr-english.tex"))
  tex <- readLines("_book/sr-english.tex")
  ind <- grep("% Region", tex) + 1
  region_name <- gsub("^.*\\{(.*)\\}$", "\\1", tex[ind])
  expect_identical(region_name, "Pacific Region")

  # ---------------------------------------------------------------------------
  # No abstract, should be missing from the tex file
  testing_path <- file.path(tempdir(), "test-no-abstract")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  rmd <- readLines("index.Rmd")
  abs_ind <- grep("^abstract:", rmd)
  french_abs_ind <- grep("^french_abstract:", rmd)
  rmd_prev <- rmd[1:(abs_ind - 1)]
  rmd_after <- rmd[(french_abs_ind - 1):length(rmd)]
  rmd <- c(rmd_prev, "abstract:", rmd_after)
  writeLines(rmd, "index.Rmd")
  render(suppress_warnings = TRUE)
  expect_true(file.exists("_book/resdoc-english.pdf"))
  expect_true(file.exists("_book/resdoc-english.tex"))

  tex <- readLines("_book/resdoc-english.tex")
  abs_beg <- grep("begin_abstract_csasdowntex", tex)
  abs_end <- grep("end_abstract_csasdowntex", tex)
  expect_identical(abs_beg, integer(0))
  expect_identical(abs_end, integer(0))

  # ---------------------------------------------------------------------------
  # test prepub section (English)
  testing_path <- file.path(tempdir(), "test-fix-envs-prepub-en")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  rmd <- readLines("index.Rmd")
  prepub_ind <- grep("prepub", rmd)
  rmd[prepub_ind] <- "   prepub: true"
  writeLines(rmd, "index.Rmd")
  render(suppress_warnings = TRUE)
  expect_true(file.exists("_book/sr-english.pdf"))
  expect_true(file.exists("_book/sr-english.tex"))

  # ---------------------------------------------------------------------------
  # test prepub section (French)
  # testing_path <- file.path(tempdir(), "test-fix-envs-prepub-fr")
  # unlink(testing_path, recursive = TRUE, force = TRUE)
  # dir.create(testing_path, showWarnings = FALSE)
  # setwd(testing_path)
  # suppressMessages(draft(
  #   system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
  #   create_dir = FALSE,
  #   edit = FALSE
  # ))
  #
  # rmd <- readLines("index.Rmd")
  # prepub_ind <- grep("prepub", rmd)
  # rmd[prepub_ind] <- "   prepub: true"
  # writeLines(rmd, "index.Rmd")
  # set_french(val = TRUE)
  # render() # FIXME!??
  # # Error in `ans[ypos] <- rep(yes, length.out = len)[ypos]`:
  # # ifelse(fr(), meta$french_title, meta$title)
  # expect_true(file.exists("_book/sr-french.pdf"))
  # expect_true(file.exists("_book/sr-french.tex"))

  # ---------------------------------------------------------------------------
  # Don't include section nums
  testing_path <- file.path(tempdir(), "test-fix-envs-section-nums")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  rmd <- readLines("index.Rmd")
  ind <- grep("include_section_nums:", rmd)
  rmd[ind] <- "   include_section_nums: false"
  writeLines(rmd, "index.Rmd")
  render(suppress_warnings = TRUE)
  expect_true(file.exists("_book/resdoc-english.pdf"))
  expect_true(file.exists("_book/resdoc-english.tex"))

  # # ---------------------------------------------------------------------------
  # # Test prepub French SR
  # testing_path <- file.path(tempdir(), "test-fix-envs-report-num")
  # unlink(testing_path, recursive = TRUE, force = TRUE)
  # dir.create(testing_path, showWarnings = FALSE)
  # setwd(testing_path)
  # suppressMessages(draft(
  #   system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
  #   create_dir = FALSE,
  #   edit = FALSE
  # ))
  #
  # set_french(val = TRUE)
  # set_yaml_tag("prepub", "true")
  # render() # FIXME!?
  # expect_true(file.exists("_book/sr-french.pdf"))
  # expect_true(file.exists("_book/sr-french.tex"))

})
