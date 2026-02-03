#' Run latexmk/pdflatex on a csasdowntex report
#'
#' Run latexmk/pdflatex on a csasdowntex report with extra pdflatex runs.
#'
#' @details
#' This can be useful in edge cases where [tinytex::latexmk()] doesn't
#' run enough times on its own.
#' For example <https://github.com/pbs-assess/csasdowntex/issues/151>.
#'
#' @param extra_pdflatex Extra number of times to run [tinytex::pdflatex()].
#' @param ... Arguments to pass to [tinytex::latexmk()].
#'
#' @return
#' The PDF in the `_book` folder should be updated.
#' @importFrom tinytex latexmk pdflatex
#' @export
#'
#' @examples
#' \dontrun{
#' # After running `bookdown::render_book("index.Rmd")` or knitting, run:
#' run_pdflatex()
#' }
run_pdflatex <- function(extra_pdflatex = 1, ...) {

  book_tex_file <- list.files("_book", pattern = "tex", full.names = TRUE)
  if(length(book_tex_file)){
    book_tex_file <- book_tex_file[[1]]
  }
  book_pdf_file <- list.files("_book", pattern = "pdf", full.names = TRUE)
  if(length(book_pdf_file)){
    book_pdf_file <- book_pdf_file[[1]]
    if (file.exists(book_pdf_file)) {
      bail("The file ", fn_color(book_pdf_file), " exists. ",
           "Delete it before running this function.")
    }
  }
  tex_file <- list.files("_book", pattern = "tex", full.names = FALSE)[[1]]
  if (file.exists(book_tex_file)) {
    file.copy(book_tex_file, ".", overwrite = FALSE)
  }
  latexmk(tex_file, clean = FALSE, ...)
  # The point of this function is that latexmk sometimes misses a run:
  for (i in seq_len(extra_pdflatex - 1)) {
    pdflatex(tex_file, clean = FALSE)
  }
  pdflatex(tex_file, clean = TRUE)
  file.copy(gsub("\\.tex", "\\.pdf", tex_file), "_book", overwrite = TRUE)
  file.remove(tex_file)
  file.remove(gsub("\\.tex", "\\.pdf", tex_file))
}
