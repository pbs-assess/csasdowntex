#' @rdname csas_pdf
#' @export
techreport_pdf <- function(copy_sty = TRUE,
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
    bail("in YAML, ", tag_color("csasdown:techreport_pdf: highlight"),
         "must be one of ",
         csas_color(paste(themes, collapse = ", ")),
         "\nor a filename for a custom latex theme file.",
         "\nSee pandoc documentation, ", csas_color("--highlight-style argument."))
  }

  if (fr()) {
    file <- system.file("csas-tex", "tech-report-french.tex", package = "csasdown") # nocov
  } else {
    file <- system.file("csas-tex", "tech-report.tex", package = "csasdown")
  }

  base <- pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = "lualatex",
    ...
  )
  tmp_hl <- grep("--highlight-style", base$pandoc$args)
  base$pandoc$args <- base$pandoc$args[-c(tmp_hl[1], tmp_hl[1] + 1)]

  cover_file_pdf <- if (fr()) "tech-report-cover-french.pdf" else "tech-report-cover.pdf"
  cover_file_docx <- if (fr()) "tech-report-cover-french.docx" else "tech-report-cover.docx"
  if (!file.exists(cover_file_pdf)) {
    cover_docx <- system.file("rmarkdown", "templates", "techreport", "skeleton", cover_file_docx, package = "csasdown")
    cover_pdf <- system.file("rmarkdown", "templates", "techreport", "skeleton", cover_file_pdf, package = "csasdown")
    alert("Missing the Tech Report cover page. Copying in the files...")
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
    which_sty = ifelse(fr(), "tech-report-french.sty", "tech-report.sty")
  )

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      highlight = highlight,
      ...
    )
  })
  on.exit(options(bookdown.post.late = old_opt))
  base
}

