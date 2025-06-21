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
extract_label_from_figure_filename <- function(fn,
                                               bd_fn = here("doc/_bookdown.yml")){

  if(!file.exists(bd_fn)){
    bail("File `", bd_fn, "` does not exist, in `extract_label_from_figure_filename()`")
  }

  bd_lines <- readLines(bd_fn)
  bd_rmd_raw <- grep("\\.[R|r]md", bd_lines, value = TRUE)

  source_dir <- dirname(bd_fn)

  # Remove leading and trailing whitespace
  bd <- trimws(bd_rmd_raw)
  # Remove commented-out lines (for speed)
  if(length(grep("^#", bd))){
    bd <- bd[-grep("^#", bd)]
  }
  bd <- bd |>
    # Remove escaped quotes
    stringr::str_remove_all('\"') %>%
    # Remove `rmd_files: [`
    gsub("^rmd_files: \\[", "", .) %>%
    # Remove closing `]` and commas
    gsub("\\]$", "", .) %>%
    gsub(",$", "", .)

  fns <- here(source_dir, bd)

  k <- map(fns, ~{

    rmd <- readLines(.x)
    inds <- grep(basename(fn), rmd)
    if(!length(inds)){
      return(NULL)
    }
    # Make sure that inds represents actual includegraphics calls. If this step
    # is omitted, then chunk labels and other text with the same name as the
    # file base will be matched and a bug will occur
    inds <- inds[grepl("include_graphics", rmd[inds])]

    # Find out which chunk the file was found included in
    j <- map_chr(inds, \(ind){
      repeat{
        ind <- ind - 1
        pat <- "```\\{r +([0-9A-Za-z_-]+).*$"
        label <- NULL
        if(length(grep(pat, rmd[ind]))){
          label <- gsub(pat, "\\1", rmd[ind])
          return(label)
        }
        if(ind == 1){
          return(NULL)
        }
      }
    })
    j
  })

  unlist(k)
}
