#' Get the the type of document to render by extracting from the YAML tags in
#' the index file
#'
#' @details
#' Will overwrite the line in the file `fn` that contains
#' `csasdowntex::render_type` or `csasdowntex:::render_type` with
#' `csasdowntex:::render_type`. This is for backwards compatibility with versions
#' where those functions were exported.
#'
#' @keywords internal
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return A character string representing the render type. One of
#' `resdoc_pdf`, `resdoc_word`, `sr_pdf`, `sr_word`, `techreport_pdf`,
#' `techreport_word`, `manureport_pdf`,  or `manureport_word`
get_render_type <- function(fn = get_index_filename(
             system.file("rmarkdown",
                         "templates",
                         "resdoc", # All types have the same index filename
                         "skeleton",
                         "_bookdown.yml",
                         package = "csasdowntex")),
             verbose = FALSE){

  if(verbose){
    notify("Checking file ", fn_color(fn), " for document render type ...")
  }

  if(!file.exists(fn)){
    bail("The file ", fn_color(fn), " does not exist")
  }
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  # Get the document type from the `output:` YAML tag
  doc_type_pat <- "^csasdowntex::+(\\S+):\\s*$"
  doc_ind <- grep(doc_type_pat, trim_rmd)
  if(!length(doc_ind)){
    bail("Document type not found in file ", fn_color(fn), "\n",
         "The line ", tag_color("csasdowntex:::resdoc_pdf:"), " was not found")
  }
  if(length(doc_ind) > 1){
    alert("Document type defined more than once in file ", fn_color(fn), "\n",
          "A line like ", tag_color("csasdowntex:::resdoc_pdf:"),
          " is multiply defined.\n",
          "Using the first instance.")
    doc_ind <- doc_ind[1]
  }
  render_type <- gsub(doc_type_pat, "\\1", trimws(rmd[doc_ind]))

  if(verbose){
    check_notify("Found document render type: ",
                 csas_color(render_type), "\n")
  }

  render_type
}
