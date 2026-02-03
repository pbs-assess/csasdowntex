#' Parse the file which has YAML option 'french' in it and set option 'french'
#' accordingly in the current workspace
#'
#' @details
#' If the YAML option is not found or there is some other issue, the 'french'
#' option will be set to `FALSE`
#'
#' @keywords internal
#'
#' @param fn The bookdown index filename, typically index.Rmd. This file
#' must have a YAML option called 'french' set to either 'true' or 'false'
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return Nothing
set_language_option <- function(fn = get_index_filename(
         system.file("rmarkdown",
                     "templates",
                     "resdoc", # All types have the same index filename
                     "skeleton",
                     "_bookdown.yml",
                     package = "csasdowntex")),
         verbose = FALSE){

  if(verbose){
    notify("Checking file ", fn_color(fn), " for render language ...")
  }

  if(!file.exists(fn)){
    bail("File ", fn_color(fn), " does not exist")
  }

  trim_rmd <- trimws(readLines(fn))
  pat <- "^french:\\s*(\\S+)\\s*$"
  french_ind <- grep(pat, trim_rmd)

  if(!length(french_ind)){
    bail("No ", tag_color("french:"), " entry was found in the file ",
         fn_color(fn))
  }
  if(length(french_ind) > 1){
    bail("More than one ", tag_color("french:"), " entry was found in ",
         "the file ", fn_color(fn))
  }
  val <- gsub(pat, "\\1", trim_rmd[french_ind])
  if(val == "true"){
    options(french = TRUE)
  }else if(val == "false"){
    options(french = FALSE)
  }else{
    bail("Problem with regular expression in ",
         csas_color("set_language_option()"), ".\n",
         "Could not extract ", csas_color("true/false"),
         " for ", tag_color("french:"))
  }

  if(verbose){
    check_notify("Found the ", tag_color(trim_rmd[french_ind]),
                 " tag and set the language to ",
                 csas_color(ifelse(val == "true", "French", "English")), "\n")
  }
}
