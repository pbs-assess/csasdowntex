#' Create the lists of source and destination filenames needed for copying
#' for the document testing
#'
#' @details
#' Meant to be called by the wrapper function [gotest()]
#' @param doc_dir If you keep your document project in a subdirectory
#' of the project root directory, then supply the name of that directory here.
#' This must be the full path of the directory.
#' @param bookdown_lst A list of values extracted from the bookdown file as
#' output by [read_bookdown_file()]
#' @param figures_dir The name of the directory in which premade figures
#' (typically png files) are kept
#' @param ignore_copy_errors If `TRUE` ignore errors from copying the required
#' input files and directories and continue to the testing directory anyway
#' @param ... Catch arguments meant for other functions
#'
#' @return A list of two vectors, `src_fns` which are the source filenames
#' as absolute filenames (full path) and `dest_fns` which are the destination
#' filenames as relative filenames
gotest_doc_get_src_dest_filenames <- function(doc_dir = NULL,
                                              bookdown_lst = NULL,
                                              figures_dir = NULL,
                                              ignore_copy_errors = FALSE,
                                              ...){

  if(is.null(bookdown_lst)){
    bail("`bookdown_lst` cannot be `NULL`")
  }

  figures_dir <- figures_dir %||% "figure"

  if(!is.null(doc_dir)){
    if(!dir.exists(doc_dir)){
      bail("Directory `", doc_dir, "` does not exist.")
    }
  }

  # Add the main figures (prebuilt figures and logos in files)
  main_figs_src_dir <- figures_dir
  main_figs_basename_fns <- list.files(main_figs_src_dir)
  main_figs_fns <- file.path(figures_dir, main_figs_basename_fns)

  # Set up to copy all Rmd files over to the tmp directory
  fns_rmd <- dir(doc_dir)
  fns_rmd <- grep(".*\\.[R|r]md$", fns_rmd, value = TRUE)
  # If test.Rmd or test.rmd exist in the main project, do not copy over
  ind_test_rmd <- grep("^test\\.[R|r]md$", fns_rmd)
  if(length(ind_test_rmd)){
    fns_rmd <- fns_rmd[-ind_test_rmd]
  }

  data_dir <- file.path(doc_dir, "data")
  fns_data <- file.path("data", dir(data_dir))

  fns <- c(file.path(doc_dir,
                     c(fns_rmd,
                       "bib/refs.bib",
                       "csl/csas.csl",
                       "csl/csas-french.csl",
                       fns_data)),
           main_figs_fns)

  fns_exists <- file.exists(fns)
  if(ignore_copy_errors){
    fns <- fns[fns_exists]
    fns_exists <- file.exists(fns)
  }
  if(!all(fns_exists)){
    bail("One or more files that are required to be copied for testing ",
         "do not exist in the directories provided. The file(s) that do ",
         "not exists are:\n\n",
         paste(fns[!fns_exists], collapse = "\n"),
         "\n\nCheck the `gotest()` function or use the `ignore_copy_errors` ",
         "argument.")
  }

  # Source filenames are absolute and destination filenames are relative
  src_fns <- fns
  # Remove full path for the `doc_dir` to get the relative destination filenames
  dest_fns <- gsub(doc_dir, "", fns)
  # Remove leading slashes
  dest_fns <- gsub("^\\/", "", dest_fns)

  list(src_fns = src_fns,
       dest_fns = dest_fns)
}
