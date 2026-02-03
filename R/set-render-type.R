#' Set the the type of document to render by modifying the YAML tags in
#' the index file
#'
#' @keywords internal
#'
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#' @param doc_type The type of document to set for rendering. Either 'pdf',
#' 'word', or 'asis'. If `asis`, leave the render type as-is.
#'
#' @return Nothing
set_render_type <- function(fn = get_index_filename(
         system.file("rmarkdown",
                     "templates",
                     "resdoc", # All types have the same index filename
                     "skeleton",
                     "_bookdown.yml",
                     package = "csasdowntex")),
         doc_type = c("pdf", "word", "asis")){

  tryCatch({doc_type <- match.arg(doc_type)
  }, error = function(e){
    bail(csas_color("doc_type"), " must be one of ",
         csas_color("pdf"), ", or ", csas_color("word"), ".\n",
         "You tried: ", csas_color(doc_type))
  })

  full_doc_type <- get_render_type(fn)

  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  # Get the document type from the `output:` YAML tag
  doc_type_pat <- "^csasdowntex::+(\\S+):\\s*$"
  doc_ind <- grep(doc_type_pat, trim_rmd)
  if (grepl(":::", trim_rmd[doc_ind])) { # nocov
    bail("Found `csasdowntex:::` as document type. Please use `csasdowntex::`.") # nocov
  } # nocov

  csas_doc_type <- gsub("(.*)_\\S+$", "\\1", full_doc_type)
  format_type <- gsub("\\S+_(\\S+)$", "\\1", full_doc_type)
  leading_spaces <- gsub("^(\\s*)\\S+\\s*$", "\\1", rmd[doc_ind])
  if(doc_type == "asis"){
    full_type_line <- paste0(leading_spaces, "csasdowntex::",
                             csas_doc_type, "_", format_type, ":")
  }else{
    full_type_line <- paste0(leading_spaces, "csasdowntex::",
                             csas_doc_type, "_", doc_type, ":")
  }
  rmd[doc_ind] <- full_type_line
  unlink(fn, force = TRUE)
  writeLines(rmd, fn)
  invisible()
}
