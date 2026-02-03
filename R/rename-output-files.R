#' Rename the PDF, Word, and TEX files to include English or French in the name
#'
#' @keywords internal
#'
#' @description
#' Rename the PDF, Word, and TEX files to include English or French in the name
#' so that rendering in one language doesn't overwrite the other document
#'
#' @param index_fn The working copy of this file:
#' `csasdowntex/inst/rmarkdown/templates/resdoc-b/skeleton/skeleton.Rmd`
#' which is `index.Rmd` by default
#'
#' @importFrom purrr map2_lgl
#'
#' @return Nothing
rename_output_files <- function(index_fn, verbose = FALSE){

  if(verbose){
    notify("Renaming output files ...")
  }
  # This will be resdoc_pdf, sr_word, etc.
  csas_render_type <- get_render_type(index_fn)

  # This will be resdoc, sr, techreport, or manureport
  render_type <- gsub("(\\S+)_\\S+$", "\\1", csas_render_type)

  # This will be pdf or word
  format_type <- gsub("\\S+_(\\S+)$", "\\1", csas_render_type)

  # This will be pdf or docx to be used as a file extension
  pdf_or_docx <- ifelse(format_type == "pdf", "pdf", "docx")
  lang <- ifelse(fr(), "french", "english")

  new_doc_fn <- paste0(render_type, "-", lang)
  new_output_fn <- file.path("_book", paste0(new_doc_fn, ".", pdf_or_docx))
  if(pdf_or_docx == "pdf"){
    new_other_fn <- file.path("_book", paste0(new_doc_fn, ".tex"))
  }else if(pdf_or_docx == "docx"){
    new_other_fn <- file.path("_book",
                              paste0("reference-keys-docx-", lang, ".txt"))
  }
  new_fns <- c(new_other_fn, new_output_fn)

  old_output_fn <- file.path("_book", paste0(render_type, ".", pdf_or_docx))
  if(pdf_or_docx == "pdf"){
    old_other_fn <- file.path("_book", paste0(render_type, ".tex"))
  }else if(pdf_or_docx == "docx"){
    old_other_fn <- file.path("_book", "reference-keys.txt")
  }
  old_fns <- c(old_other_fn, old_output_fn)

  unlink(new_fns, force = TRUE)
  copy_success <- map2_lgl(old_fns, new_fns, ~{
    success <- file.copy(.x, .y, overwrite = TRUE)
    if(success){
      check_notify("File ", fn_color(.y), " created.")
      unlink(.x, force = TRUE)
      return(TRUE)
    }else{
      alert("Could not copy file from ", fn_color(.x), " to ", fn_color(.y))
      return(FALSE)
    }
  })

  if(verbose && all(copy_success)){
    check_notify("Renamed output files successfully")
  }
}
