# testing_path <- tempdir()
#
# if(identical(.Platform$OS.type, "windows")){
#   x <- system("where xelatex", intern = TRUE)
# }else{
#   x <- system("which xelatex", intern = TRUE)
# }
#
# if(!grepl("latex", x)) {
#   if (!require(tinytex)){
#     install.packages("tinytex")
#   }
#   if(!tinytex:::is_tinytex()){
#     tinytex::install_tinytex(force = TRUE)
#   }
#   test_that("LaTeX is installed", {
#     expect_true(tinytex:::is_tinytex())
#   })
# }
#
# template_files <- list.files(system.file("rmarkdown",
#                                          package = "csasdowntex"),
#                              recursive = TRUE)
#
# test_that("csasdowntex template files are present", {
#   expect_true(length(template_files) > 10)
# })
