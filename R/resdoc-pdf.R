#' Creates an R Markdown PDF with CSAS formatting
#'
#' @description
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify using the CSAS LaTeX template and cls files.
#'
#' @param toc Logical. If `TRUE`, include a Table of Contents (TOC)
#' @param toc_depth A positive integer representing how many levels of section
#' headers to show in the TOC
#' @param highlight Syntax highlighting style. Supported styles include
#'  "tango", "pygments", "kate", "monochrome", "espresso",
#'  "zenburn", and "haddock". If not in this list of styles, the directory in
#'  which the build process was called will be searched for the given theme
#'  file. You can copy one from the csasdown library install on your machine
#'  and modify as necessary. Find them by looking here:
#'  file.path(.libPaths(), "csasdown", "themes")
#'  Pass `NULL` to prevent syntax highlighting (uses 'monochrome' theme) for
#'  slightly different text format but no highlighting
#' @param latex_engine LaTeX engine to render with. 'pdflatex' or 'xelatex'
#' @param prepub Logical. If `TRUE`, this is a pre-publication version.
#' Currently only implemented for SR
#' @param include_section_nums Logical. If `TRUE`, include the section and
#' subsection numbers in the body titles. The table of contents will still
#' show the numbers
#' @param copy_sty Logical. If `TRUE`, the '.sty' files will be copied from
#' the local library location for every render. If `FALSE`, the current '.sty'
#' file will be kept and any edits you have done to it will remain
#' @param line_nums Logical. If `TRUE`, include line numbering in the document
#' @param line_nums_mod Numerical. Which modulo line numbers to label,
#' e.g. 2 = every second line, 3 = every 3rd line, etc.
#' @param lot_lof Logical. If `TRUE`, include list of tables and list of
#' figures in the document. Implemented only for 'resdoc', 'techreport' and 'manureport'
#' @param draft_watermark Logical. If `TRUE`, show a DRAFT watermark on all
#' pages of the output document
#' @param french French?
#' @param pandoc_args Any other arguments to `pandoc`
#' @param ... other arguments to [bookdown::pdf_book()].
#' @return Output from [bookdown::pdf_book()], modified by the post-processor
#' [fix_envs()]
#' @import bookdown
#' @importFrom here here
#' @rdname csas_pdf
#' @export
resdoc_pdf <- function(toc = TRUE,
                       toc_depth = 3,
                       highlight = "tango",
                       latex_engine = "pdflatex",
                       prepub = FALSE,
                       copy_sty = TRUE,
                       line_nums = FALSE,
                       line_nums_mod = 1,
                       lot_lof = FALSE,
                       draft_watermark = FALSE,
                       french = FALSE,
                       include_section_nums = TRUE,
                       pandoc_args = c("--top-level-division=chapter",
                                       "--wrap=none",
                                       "--default-image-extension=png"),
                       ...) {

  themes <- c("pygments", "tango", "espresso",
              "zenburn", "kate", "monochrome",
              "breezedark", "haddock")

  fr <- function() if (french) TRUE else FALSE

  if(is.null(highlight)){
    highlight = "monochrome" # nocov
  }

  if((!highlight %in% themes) && !file.exists(here(highlight))){
    bail("in YAML, ", tag_color("csasdown:resdoc_pdf: highlight:"),
         " must be one of ",
         csas_color(paste(themes, collapse = ", ")),
         "\nor a filename for a custom latex theme file.",
         "\nSee pandoc documentation, ",
         csas_color("--highlight-style argument."))
  }

  if (fr()) {
    file <- system.file("csas-tex", "res-doc-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "res-doc.tex", package = "csasdown")
  }

  base <- pdf_book(
    template = file,
    toc = toc,
    toc_depth = toc_depth,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )
  tmp_hl <- grep("--highlight-style", base$pandoc$args)
  base$pandoc$args <- base$pandoc$args[-c(tmp_hl[1], tmp_hl[1] + 1)]

  if (!class(line_nums_mod) %in% c("integer", "numeric")) {
    bail(csas_color("line_nums_mod"), " must be a numeric or integer value.")
  }

  update_csasstyle(
    copy = copy_sty,
    line_nums = line_nums,
    line_nums_mod = line_nums_mod,
    draft_watermark = draft_watermark,
    lot_lof = lot_lof,
    which_sty = ifelse(fr(), "res-doc-french.sty", "res-doc.sty")
  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  # base$knitr$opts_chunk$fig.align <- "center"

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      prepub = prepub,
      highlight = highlight,
      include_section_nums = include_section_nums,
      include_abstract = TRUE,
      fix_ref_section_name = TRUE,
      ...
    )
  })
  on.exit(options(bookdown.post.latex = old_opt))

  base
}
