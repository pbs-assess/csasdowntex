#' @rdname csas_docx
#' @export
manureport_word <- function(...) {
  file <- "manu-report.docx"
  base <- word_document2(...,
                         reference_docx = system.file("csas-docx",
                                                      file,
                                                      package = "csasdowntex"))

  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

