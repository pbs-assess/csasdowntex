#' Extract the index filename from the YAML file (usually index.Rmd)
#'
#' @details
#' `index_file` is the code in the "skeleton.Rmd" file, which is called
#' 'index.Rmd' by default in a project. It is found in the bookdown YAML file,
#' `yaml_fn`
#'
#' @keywords internal
#'
#' @param yaml_fn The YAML file name. The default is '_bookdown.yml' for
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return The book filename (typically index.Rmd) for [bookdown::bookdown]
#' 
#' @importFrom stringr str_extract_all
get_index_filename <- function(yaml_fn = "_bookdown.yml",
                               verbose = FALSE){

  if(verbose){
    notify("Checking for Rmarkdown index filename in ",
           fn_color(yaml_fn), " file ...")
  }

  if(is.null(yaml_fn)){
    bail("The ", csas_color("yaml_fn"), " argument (filename) ",
         "cannot be ", csas_color("NULL"))
  }

  if(yaml_fn == ""){
    bail("The ", csas_color("yaml_fn"), " argument (filename) ",
         "cannot be an empty string")
  }

  if(!file.exists(yaml_fn)){
    bail("File ", fn_color(yaml_fn), " does not exist")
  }

  yaml <- readLines(yaml_fn)
  index_fn_ind <- grep("^rmd_files: \\[", yaml)
  if(!length(index_fn_ind)){
    bail("Index filename not found in ", fn_color(yaml_fn), ". ",
         "This is typically index.Rmd and should be the first entry after ",
         tag_color("rmd_files:["), " and on the same line as it")
  }
  index_fn <- str_extract_all(yaml[index_fn_ind], "[a-zA-Z0-9_\\-]+\\.(R|r)md")[[1]]

  if(verbose){
    check_notify("Rmarkdown index filename ", fn_color(index_fn[1]),
                 " found in ", fn_color(yaml_fn), " file\n")
  }

  # In case there were more than one Rmd files on the first line
  #
  index_fn[1]
}