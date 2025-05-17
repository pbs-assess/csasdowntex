#' Create and change to a temporary directory and copy a skeleton
#' version of the document files for testing
#'
#' @details
#' Extracts the source and destination filenames for copying into the
#' temporary directory with the function [gotest_doc_get_src_dest_filenames()].
#' See the *Debugging a figure or table, or any other Rmarkdown code*
#' section of the README.md file for detailed explanation.
#' Use the function [goback()] to return from the testing directory to the
#' directory you were in before calling [gotest()]
#'
#' @param doc_dir If you keep your document project in a subdirectory
#' of the project root directory, then supply the name of that directory here
#' @param config_fn The name of the bookdown YAML file. The default is
#' `_bookdown.yml`
#' @param figures_dir The name of the directory in which premade figures
#' (typically png files) are kept
#' @param knitr_figures_dir The name of the directory in which knitr saves
#' its copies of the figures it creates from knitr chunks
#'
#' @return Nothing, but the global variable `goback_dr` is set for the
#' function [goback()] to use to return to the original directory
#' @export
gotest <- function(doc_dir = NULL,
                   config_fn = "_bookdown.yml",
                   figures_dir = NULL,
                   knitr_figures_dir = NULL){

  if(is.null(doc_dir)){
    pre_path <- here()
  }else{
    pre_path <- here(doc_dir)
    if(!dir.exists(pre_path)){
      bail("Directory `", pre_path, "` does not exist.")
    }
  }

  config_fn <- file.path(pre_path, config_fn)
  figures_dir <- file.path(pre_path, figures_dir %||% "figure")
  knitr_figures_dir <- file.path(pre_path, knitr_figures_dir %||% "knitr-figs-pdf")
  if(!file.exists(config_fn)){
    bail("The `bookdown` config file `", config_fn, "` does not exist")
  }
  if(!dir.exists(figures_dir)){
    bail("The figures directory `", figures_dir, "` does not exist")
  }
  if(!dir.exists(knitr_figures_dir)){
    bail("The knitr figures directory `", knitr_figures_dir, "` does not exist")
  }

  curr_dir <- getwd()
  if(length(grep("Rtmp", curr_dir))){
    message("You appear to already be in a temporary directory. You ",
            "must `goback()` before trying to `gotest()` again or just ",
            "continue to test here")
    return(invisible())
  }
  # Set global directory name to return back to with `goback()`
  goback_dr <<- curr_dir

  # Read the bookdown file to determine which files need to be copied
  fns <- list.files(pre_path, full.names = TRUE)
  if(!config_fn %in% fns){
    bail("You can only call `gotest()` from a directory containing a ",
         "`bookdown` config file (`", config_fn, "`) as it is used to ",
         "determine which type of project you are wanting to test.")
  }

  # Search the index file (first RMD file in the bookdown config file ) to
  # find out which type of doc this is, beamer or main doc, and to get the
  # name of the index file
  bookdown_lst <- read_bookdown_file(config_fn)
  index_fn <- file.path(pre_path, bookdown_lst$rmd_fns[1])
  if(!file.exists(index_fn)){
    bail("The `bookdown` index file `", index_fn, "` does not exist")
  }

  fns_lst <- gotest_doc_get_src_dest_filenames(pre_path,
                                               bookdown_lst,
                                               figures_dir)

  src_fns <- fns_lst$src_fns
  dest_fns <- fns_lst$dest_fns

  work_dr <- tempdir()
  setwd(work_dr)
  unlink("*", recursive = TRUE, force = TRUE)
  dir.create(basename(pre_path))

  # Needed for `here::here()` to work right
  writeLines("", ".here")

  dir.create(file.path(basename(pre_path),
                       basename(figures_dir)))
  dir.create(file.path(basename(pre_path),
                       "bib"))
  dir.create(file.path(basename(pre_path),
                       "csl"))
  dir.create(file.path(basename(pre_path),
                       "data"))

  dest_fns <- file.path(basename(pre_path),
                        dest_fns)
  map2(src_fns, dest_fns, ~{
    file.copy(.x, .y, overwrite = TRUE, copy.mode = TRUE)
  })

  # Needed to set `here:here()` correctly
  i_am(paste0("./", basename(pre_path), "/", bookdown_lst$rmd_fns[1]))

  setwd(basename(pre_path))

  # Create the bookdown configuration file _bookdown.yml
  bd_lines <- c(
    'book_filename: "test"',
    'rmd_files: ["index.Rmd",',
    '            "00-test.Rmd"]',
    '',
    'delete_merged_file: false')
  writeLines(bd_lines, file.path(basename(config_fn)))
  writeLines(c("# Test Section", ""), "00-test.Rmd")

  message("\nAll necessary items copied into a temporary directory.",
          "\n\nTo build the document, run render().\nWhen finished, ",
          "run goback() to go back to the directory you came from.")

  message("Now in directory ", getwd())

  invisible()
}
