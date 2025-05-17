#' Read a _bookdown.yml file into a reasonable format which is a list
#' containing the book filename, the RMD filenames, and the variable
#' `delete_merged_file`
#'
#' @param fn The YML filename
#'
#' @return The list created. Can be `NULL` if the regular expressions fail
#' to match
#' @export
read_bookdown_file <- function(fn = "_bookdown.yml"){

  bd <- readLines(fn)

  # Remove escaped quoted strings globally
  bd <- gsub('\"', "", bd)

  # Create output list
  x <- list()

  # Fetch the name of the book (PDF) file generated
  tmp <- grep("book_filename", bd, value = TRUE)
  if(length(tmp)){
    tmp <- gsub("^\\s*book_filename:\\s*(\\w+)", "\\1", tmp)
    x$book_fn <- paste0(tmp, ".pdf")
  }

  # Fetch the rest of the RMD files
  rmd_fn_pat <- "\\s+\\[?([0-9a-zA-Z\\.\\/\\-]+\\.[R|r]md)"
  tmp <- grep(rmd_fn_pat, bd, value = TRUE)
  if(length(tmp)){
    tmp <- gsub("rmd_files:\\s*\\[", "", tmp)
    tmp <- gsub("^\\s*", "", tmp)
    tmp <- gsub("\\]*", "", tmp)
    tmp <- gsub(",", "", tmp)
    x$rmd_fns <- tmp
  }

  dmf_pat <- "delete_merged_file:\\s*(true|false)"
  tmp <- grep(dmf_pat, bd, value = TRUE)
  if(length(tmp)){
    tmp <- gsub(dmf_pat, "\\1", tmp)
    if(tmp %in% c("true", "false")){
      x$delete_merged_file <- as.logical(tmp)
    }else{
      x$delete_merged_file <- FALSE
    }
  }

  x
}
