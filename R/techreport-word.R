#' @rdname csas_docx
#' @export
techreport_word <- function(...) {
  file <- "tech-report.docx"
  base <- word_document2(...,
                         reference_docx = system.file("csas-docx",
                                                      file,
                                                      package = "csasdowntex"))

  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

