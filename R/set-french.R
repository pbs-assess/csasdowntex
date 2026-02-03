#' Set French in the index.Rmd file to 'true' or 'false'
#'
#' @description
#' Wrapper function to set the french: YAML tag
#'
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#' @param val `TRUE` or `FALSE`
#'
#' @return Nothing, write the file `fn`
#' @export
set_french <- function(fn = get_index_filename(
         system.file("rmarkdown",
                     "templates",
                     "resdoc", # All types have the same index filename
                     "skeleton",
                     "_bookdown.yml",
                     package = "csasdowntex")),
         val = TRUE){

  set_yaml_tag("french:", val = ifelse(val, "true", "false"), fn = fn)
}
