#' This function will clean the current working directory of remnants of
#' previous builds.
#'
#' @details
#' Clean (delete) all build output including knitr cache and knitr figures
#' directories from the current directory.
#'
#' @param ... Optional quoted or unquoted names of chunks that should be
#' deleted.The chunk name(s) will be used to delete only the objects
#' relating to them from the `knitr_figures_dir` and `knitr-cache`.as well
#' as all the LaTeX and intermediate RMD and MD files. For example if there
#' is a file called 'example-1.png' in the `knitr_figures_dir` you would run
#' `clean(example)` which will remove all outputs of the 'example' knitr chunk.
#' If this argument is not used, All files will be deleted from
#'  `knitr_figures_dir` and `knitr_cache_dir`
#'  `knitr_figures_dir` and `knitr_cache_dir`
#' @param book Logical. If `TRUE`, delete the '_book' directory recursively
#' @param book_dir The name of the directory in which the output '*.pdf' or
#' '*.docx' documents reside
#' @param knitr_figures_dir Directory where the knitr-generated
#' figures reside
#' @param knitr_cache_dir Directory where the knitr cached chunk
#' databases reside
#'
#' @return Nothing
#' @export
clean <- function(...,
                  book = FALSE,
                  book_dir = "_book",
                  knitr_figures_dir = "knitr-figs-pdf",
                  knitr_cache_dir = "knitr-cache-pdf"){

  ops_completed <- FALSE

  chunks <- enquos(...)
  if(length(chunks)){
    # Only delete files related to the chunks given
    figs <- list.files(knitr_figures_dir, full.names = TRUE)
    cache <- list.files(knitr_cache_dir, full.names = TRUE)
    walk(chunks, ~{
      nm <- gsub(" - ", "-", as_label(.x))
      nm <- gsub('"', "", nm)
      fns <- grep(nm, figs, value = TRUE)
      if(length(fns)){
        result <- unlink(fns, force = TRUE)
        if(!result){
          ops_completed <- TRUE
          message("\nDeleted file(s): ", paste(fns, collapse = ", "))
        }
      }
      fns <- grep(nm, cache, value = TRUE)
      if(length(fns)){
        result <- unlink(fns, force = TRUE)
        if(!result){
          ops_completed <- TRUE
          message("\nDeleted file(s): ", paste(fns, collapse = ", "))
        }
      }
    })
  }else{
    # Delete knitr directories recursively (all files)
    if(dir.exists(knitr_figures_dir)){
      unlink(knitr_figures_dir, recursive = TRUE, force = TRUE)
      ops_completed <- TRUE
      message("\nDeleted the `", knitr_figures_dir, "` directory recursively.")
    }
    if(dir.exists(knitr_cache_dir)){
      unlink(knitr_cache_dir, recursive = TRUE, force = TRUE)
      ops_completed <- TRUE
      message("\nDeleted the `", knitr_cache_dir, "` directory recursively.")
    }
  }

  # Possible names of the docs to delete without extensions
  docs_pat <- c("resdoc",
                "resdoc-english",
                "resdoc-french",
                "fmin")

  # The extensions of the above docs to delete
  extensions_pat <- paste0("(",
                           "aux|",
                           "bbl|",
                           "blg|",
                           "log|",
                           "lof|",
                           "lot|",
                           "md|",
                           "nav|",
                           "pdf|",
                           "ps|",
                           "Rmd|",
                           "snm|",
                           "tex|",
                           "toc|",
                           "txt|",
                           "upa|",
                           "upb",
                           ")")

  curr_dir <- getwd()

  # Delete files made up of files starting in any in `docs_pat` and ending in
  # any in `extensions_pat`
  for(fn_base in docs_pat){
    fns <- list.files(path = curr_dir,
                      pattern = paste0(fn_base,
                                       "\\.",
                                       extensions_pat),
                      full.names = TRUE)

    if(length(fns)){
    # Delete files that match above combinations
      unlink(fns, force = TRUE)
      ops_completed <- TRUE
      message("\nDeleted intermediate LaTeX files:\n",
              paste(fns, collapse = "\n"))
    }
  }

  # Delete files starting in "tmp"
  fns_start_tmp <- list.files(path = curr_dir,
                              pattern = "^tmp",
                              full.names = TRUE)
  if(length(fns_start_tmp)){
    unlink(fns_start_tmp, force = TRUE)
    ops_completed <- TRUE
    message("\nDeleted temporary build files:\n",
            paste(fns_start_tmp, collapse = "\n"))
  }

  if(book){
    if(dir.exists(book_dir)){
      unlink(book_dir, recursive = TRUE, force = TRUE)
      ops_completed <- TRUE
      message("\nDeleted the `", book_dir, "` directory recursively.")
    }
  }

  if(ops_completed){
    message("\nDone cleaning the `", curr_dir, "` directory\n")
  }else{
    message("\nDirectory already clean\n")
  }
}