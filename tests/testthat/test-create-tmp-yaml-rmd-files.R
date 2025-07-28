test_that("create_tmp_yaml_rmd_files() throws errors", {
  testing_path <- file.path(tempdir(), "resdoc-create-ymp-yaml-rmd-files-errors")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_error(create_tmp_yaml_rmd_files("nonexistent-file.yml"),
               "The YAML file \\S+ does not exist")

  file.create("empty.yml")
  expect_error(create_tmp_yaml_rmd_files("empty.yml"),
               "YAML file \\S+ is empty")
  rmd <- readLines("_bookdown.yml")
  back_rmd <- rmd

  ind <- grep("\\[", rmd)
  tmp <- rmd[ind]
  rmd[ind] <- ""
  writeLines(rmd, "_bookdown.yml")
  expect_error(create_tmp_yaml_rmd_files(),
               "not found in \\S+. It must appear at the beginning")

  rmd[ind] <- tmp
  rmd[ind + 1] <- tmp
  writeLines(rmd, "_bookdown.yml")
  expect_error(create_tmp_yaml_rmd_files(),
               "More than one \\S+")

  rmd <- back_rmd
  writeLines(rmd, "_bookdown.yml")

  ind <- grep("\\]", rmd)
  tmp <- rmd[ind]
  rmd[ind] <- ""
  writeLines(rmd, "_bookdown.yml")
  expect_error(create_tmp_yaml_rmd_files(),
               "not found in \\S+. It must appear at the end")

  rmd[2:length(rmd)] <- ""
  rmd[2] <- "rmd_files: ["
  rmd[3] <- "]"
  writeLines(rmd, "_bookdown.yml")
  expect_error(create_tmp_yaml_rmd_files(),
               "No .Rmd filenames found")

  rmd <- back_rmd
  rmd <- rmd[-1]
  rmd <- rmd[-length(rmd)]
  writeLines(rmd, "_bookdown.yml")
  tmp <- create_tmp_yaml_rmd_files()
  expect_identical(tmp[[1]], c("tmp_bookdown.yml"))
  expect_identical(tmp[[2]], c("tmp-index.Rmd",
                               "tmp-01-chap1.Rmd",
                               "tmp-02-chap2.Rmd",
                               "tmp-03-chap3.Rmd",
                               "tmp-04-references.Rmd",
                               "tmp-05-appendix.Rmd"))
})

test_that("create_tmp_yaml_rmd_files() works", {
  testing_path <- file.path(tempdir(), "resdoc-create-ymp-yaml-rmd-files")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  create_tmp_yaml_rmd_files()
  expect_true(file.exists("tmp_bookdown.yml"))
  expect_true(file.exists("tmp-index.Rmd"))
  expect_true(file.exists("tmp-01-chap1.Rmd"))
  expect_true(file.exists("tmp-02-chap2.Rmd"))
  expect_true(file.exists("tmp-03-chap3.Rmd"))
  expect_true(file.exists("tmp-04-references.Rmd"))
  expect_true(file.exists("tmp-05-appendix.Rmd"))

})

test_that("create_tmp_yaml_rmd_files() properly deletes tmp files", {
  testing_path <- file.path(tempdir(), "resdoc-create-ymp-yaml-rmd-files-get-deleted")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE))

    writeLines("test", "tmp-index.Rmd")
    writeLines("test", "tmp-01-chap1.Rmd")
    tmp_yaml_rmd_fns <- create_tmp_yaml_rmd_files("_bookdown.yml")
    expect_false(file.exists("tmp-tmp-index.Rmd"))
    expect_false(file.exists("tmp-tmp-01-chap1.Rmd"))

    unlink("tmp-index.Rmd", force = TRUE)
    unlink("tmp-01-chap1.Rmd", force = TRUE)
    yml <- readLines("_bookdown.yml")
    yml_mod <- gsub("01-chap1.Rmd", "01_chap1.Rmd", yml)
    writeLines(yml_mod, "_bookdown.yml")
    writeLines("test", "01_chap1.Rmd")
    tmp_yaml_rmd_fns <- create_tmp_yaml_rmd_files("_bookdown.yml")
    expect_false(file.exists("tmp-tmp-01_chap1.Rmd"))

})