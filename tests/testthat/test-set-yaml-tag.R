test_that("set_yaml_tag() works", {
  wd <- getwd()
  testing_path <- file.path(tempdir(), "test-set-yaml-tags")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # ---------------------------------------------------------------------------
  # Test NULL filename
  expect_error(set_yaml_tag("french:", "true", NULL),
               "Filename \\S+ cannot be ")

  # ---------------------------------------------------------------------------
  # Test NULL tag
  expect_error(set_yaml_tag(NULL, "true"),
               "YAML tag \\S+ cannot be ")

  # ---------------------------------------------------------------------------
  # Test NULL tag value
  expect_error(set_yaml_tag("french:", NULL),
               "YAML tag value \\S+ cannot be")

  # ---------------------------------------------------------------------------
  # Test Empty file
  rmd <- character(0)
  writeLines(rmd, "empty.Rmd")
  expect_error(set_yaml_tag("french:", "tru", "empty.Rmd"),
               "File \\S+ does not contain anything")

  # ---------------------------------------------------------------------------
  # Test unbalanced YAML file
  rmd <- readLines("index.Rmd")
  rmd <- c(rmd, "", "---", "tag1: junk", "")
  writeLines(rmd, "unbalanced.Rmd")
  expect_error(set_yaml_tag("french:", "tru", "unbalanced.Rmd"),
               paste0("There are uneven sets of \\S+ lines meaning an ",
                      "unending YAML block in file"))

  # ---------------------------------------------------------------------------
  # Test no YAML blocks
  rmd <- readLines("index.Rmd")
  inds <- grep("---", rmd)
  rmd <- rmd[-inds]
  writeLines(rmd, "index.Rmd")
  expect_warning(set_yaml_tag("french:", "tru"),
                 paste0("There were no YAML blocks found in the file. ",
                        "Nothing was changed"))

  # ---------------------------------------------------------------------------
  # Test non existent
  testing_path <- file.path(tempdir(), "test-set-yaml-tags-2")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdowntex"),
    create_dir = FALSE,
    edit = FALSE
  ))
  expect_error(set_yaml_tag("prepub", "true", "no.Rmd"),
               "File \\S+ does not exist")

  # ---------------------------------------------------------------------------
  # Test trying to change
  # expect_error(set_yaml_tag("", "true"),
               # paste0("The \\S+ and \\S+ tags cannot ",
                      # "be changed with this function."))

  # ---------------------------------------------------------------------------
  # Test trying to change knit:
  expect_error(set_yaml_tag("knit", "true"),
               paste0("The \\S+ tag cannot be modified"))

  # ---------------------------------------------------------------------------
  # Test non existent
  expect_error(set_yaml_tag("nonexistent", "true"),
               paste0("The YAML tag \\S+ was not found in ",
                      "the file"))

  # ---------------------------------------------------------------------------
  # Test trying tag names not in a YAML block
  rmd <- readLines("index.Rmd")
  rmd[64] <- "fake_tag: true"
  writeLines(rmd, "index.Rmd")
  expect_error(set_yaml_tag("fake_chunk:", "false"),
               paste0("The YAML tag \\S+ was not found ",
                      "in the file"))

  # ---------------------------------------------------------------------------
  # Test tag names with and without leading spaces
  set_yaml_tag("title:", "New title")
  set_yaml_tag("approver", c("Bob Ross", "Regional Director"))
  rmd <- readLines("index.Rmd")
  expect_identical(rmd[2], "title: New title")
  expect_identical(rmd[10:12], c("approver: |", "  Bob Ross\\ ", "  Regional Director"))

  # ---------------------------------------------------------------------------
  # Test tag name at end of YAML block
  set_yaml_tag("lof", "true")

  # ---------------------------------------------------------------------------
  # Add more chunks and try
  rmd <- readLines("index.Rmd")
  rmd <- c(rmd, "", "", "---", "", "title: true",
           "another_newtag: Hello World!", "---", "",
           "---", "block3_tag1: Help", "block3_tag2: Help", "", "---", "", "")
  writeLines(rmd, "index.Rmd")
  set_yaml_tag("block3_tag2", "It's all good")
  set_yaml_tag("block3_tag1", "First tag of block")
  expect_error(set_yaml_tag("title", "X"),
               paste0("The YAML tag \\S+ was found more than ",
                      "once in the file"))

  setwd(wd)
})
