test_that("parse_pandoc_highlight_theme() works", {

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

  json <- json_lst[all_themes == "tango"]
  back_json <- json[[1]]
  j <- parse_pandoc_highlight_theme(json)
  expect_true(all(lengths(purrr::map(j[[3]], ~{as.logical(grep("text-color", .x))}))))
  expect_identical(names(j), c("text-color", "background-color", "sections-list"))
  expect_identical(class(j), "list")
  expect_true(is.na(j[[1]]))
  expect_identical(j[[2]], "#f8f8f8")
  expect_identical(names(j[[3]]), c("Other", "Attribute",
                                    "SpecialString", "Annotation",
                                    "Function", "String",
                                    "ControlFlow", "Operator",
                                    "Error", "BaseN",
                                    "Alert", "Variable",
                                    "Extension", "Preprocessor",
                                    "Information", "VerbatimString",
                                    "Warning", "Documentation",
                                    "Import", "Char",
                                    "DataType", "Float",
                                    "Comment", "CommentVar",
                                    "Constant", "SpecialChar",
                                    "DecVal", "Keyword"))
  # Select a few random Toks to check for tango
  op_list <- j[[3]]$Alert
  expect_identical(op_list[[1]], c("text-color", "#ef2929"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "false"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  op_list <- j[[3]]$Function
  expect_identical(op_list[[1]], c("text-color", "#000000"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "false"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  op_list <- j[[3]]$Keyword
  expect_identical(op_list[[1]], c("text-color", "#204a87"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "true"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  op_list <- j[[3]]$Operator
  expect_identical(op_list[[1]], c("text-color", "#ce5c00"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "true"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  # Remove all text-color and background-color items
  json <- gsub("text-color", "", json)
  json <- gsub("background-color", "", json)
  j <- parse_pandoc_highlight_theme(json)
  expect_false(any(lengths(purrr::map(j[[3]], ~{as.logical(grep("text-color", .x))}))))
  expect_true(is.na(j[[1]]))
  expect_true(is.na(j[[2]]))
})
