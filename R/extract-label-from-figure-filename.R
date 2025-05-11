#' Extract chunk label for the knitr chunk containing includegraphics and a
#' filename
#'
#' @details
#' 1. Starts by parsing the `_bookdown.yml` file to extract the names of all
#'    uncommented rmd files in the current build.
#' 2. Searches all those files for any lines starting with (ref:variable).
#'    Preceding spaces are ignored
#' 3. Extracts the `chunkname` given the filename loaded within the chunk
#'    that `chunkname` belongs to
#'
#' @param fn The figure file name to match (no extension)
#'
#' @return The chunk label found for the chunk which includes the figure
#' file `fn`
#' @export
extract_label_from_figure_filename <- function(fn){

  bd_lines <- readLines(here("doc/_bookdown.yml"))
  bd_rmd_raw <- grep("\\.rmd", bd_lines, value = TRUE)
  # Remove commented-out lines (for speed)
  bd <- gsub("^ *", "", bd_rmd_raw)
  if(length(grep("^#", bd))){
    bd <- bd[-grep("^#", bd)]
  }
  fns <- gsub(".*([0-9]{3}\\-[a-zA-Z\\-]+\\.rmd).*", "\\1", bd)
  fns <- here("doc", fns)

  k <- map(fns, ~{
    rmd <- readLines(.x)
    x <- grep(fn, rmd)
    if(length(x)){
      chunk_ind <- x
      repeat{
        if(length(grep("ref:[0-9a-zA-Z\\-]+", rmd[chunk_ind]))){
          alt_text_label <- gsub(".* +([0-9a-zA-Z\\-]+)\\-fig.*",
                                 "\\1",
                                 rmd[chunk_ind])
          return(alt_text_label)
        }
        chunk_ind <- chunk_ind - 1
      }
    }
  })
  # Remove all NULLs from the list
  k[sapply(k, is.null)] <- NULL

  k[[1]]
}
