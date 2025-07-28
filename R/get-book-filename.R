#' Get the book filename from _bookdown.yml file
#'
#' @details
#' This function reads the _bookdown.yml file to extract the book_filename
#'
#' @keywords internal
#' @param fn The Bookdown YAML file name. '_bookdown.yml' by default
#'
#' @return A character string of the supplied book_filename
get_book_filename <- function(fn = "_bookdown.yml"){

  if(!file.exists(fn)){
    bail("The file ", fn_color(fn), " does not exist")
  }
  yml <- readLines(fn)
  trim_yml <- trimws(yml)

  # Get the document type from the `book_filename:` YAML tag
  filename_type_pat <- 'book_filename: \\"([^\\"]*)\\"'
  filename_ind <- grep(filename_type_pat, trim_yml)
  book_filename <- gsub(filename_type_pat, "\\1", trimws(yml[filename_ind]))

  book_filename

}