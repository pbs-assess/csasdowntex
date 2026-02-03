#' @rdname csas_pdf
#' @export
manureport_pdf <- function(latex_engine = "pdflatex",
                           copy_sty = TRUE,
                           line_nums = FALSE,
                           line_nums_mod = 1,
                           lot_lof = FALSE,
                           draft_watermark = FALSE,
                           highlight = "tango",
                           french = FALSE,
                           pandoc_args = c("--top-level-division=chapter",
                                           "--wrap=none",
                                           "--default-image-extension=png"),
                           ...) {

  themes <- c("pygments", "tango", "espresso",
              "zenburn", "kate", "monochrome",
              "breezedark", "haddock")

  if(is.null(highlight)){
    highlight = "monochrome" # nocov
  }

  fr <- function() if (french) TRUE else FALSE

  if((!highlight %in% themes) && !file.exists(here(highlight))){
    bail("in YAML, ", tag_color("csasdowntex:manureport_pdf: highlight"),
         "must be one of ",
         csas_color(paste(themes, collapse = ", ")),
         "\nor a filename for a custom latex theme file.",
         "\nSee pandoc documentation, ", csas_color("--highlight-style argument."))
  }

  if (fr()) {
    file <- system.file("csas-tex", "manu-report-french.tex", package = "csasdowntex") # nocov
  } else {
    file <- system.file("csas-tex", "manu-report.tex", package = "csasdowntex")
  }

  base <- pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )
  tmp_hl <- grep("--highlight-style", base$pandoc$args)
  base$pandoc$args <- base$pandoc$args[-c(tmp_hl[1], tmp_hl[1] + 1)]

  cover_file_pdf <- if (fr()) "manu-report-cover-french.pdf" else "manu-report-cover.pdf"
  cover_file_docx <- if (fr()) "manu-report-cover-french.docx" else "manu-report-cover.docx"
  if (!file.exists(cover_file_pdf)) {
    cover_docx <- system.file("rmarkdown", "templates", "manureport", "skeleton", cover_file_docx, package = "csasdowntex")
    cover_pdf <- system.file("rmarkdown", "templates", "manureport", "skeleton", cover_file_pdf, package = "csasdowntex")
    alert("Missing the Manuscript Report cover page. Copying in the files...")
    file.copy(cover_docx, ".", overwrite = FALSE)
    file.copy(cover_pdf, ".", overwrite = FALSE)
  }

  if (!class(line_nums_mod) %in% c("integer", "numeric")) {
    bail("line_nums_mod must be a numeric or integer value.")
  }

  update_csasstyle(
    copy = copy_sty,
    line_nums = line_nums,
    line_nums_mod = line_nums_mod,
    lot_lof = lot_lof,
    draft_watermark = draft_watermark,
    which_sty = ifelse(fr(), "manu-report-french.sty", "manu-report.sty")
  )

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      highlight = highlight
    )
  })
  on.exit(options(bookdown.post.late = old_opt))
  base
}

