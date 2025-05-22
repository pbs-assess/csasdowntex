#' Inject code that make bilingual features in a csasdown document possible
#'
#' @description
#' Inject code that make bilingual features in a csasdown document possible.
#' The main goal here is to hide this from authors as it is complex knitr
#' hook code combined with some of the YAML variable extraction code and if
#' modified, will crash the resdoc build process.
#'
#' @keywords internal
#'
#' @param fn Typically the working copy of this file:
#' `/inst/rmarkdown/templates/resdoc-b/skeleton/skeleton.Rmd`
#' which is `index.Rmd` by default
#' @param doc_type Document type
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return Lines for the file `fn`, but with the code injected in the right
#' place
inject_bilingual_code <- function(fn = get_index_filename(
                system.file("rmarkdown",
                            "templates",
                            "resdoc", # All types have the same index filename
                            "skeleton",
                            "_bookdown.yml",
                            package = "csasdown")),
                doc_type,
                verbose = FALSE){

  if(verbose){
    notify("Injecting bilingual code into ", fn_color(fn), " ...")
  }

  if(!file.exists(fn)){
    bail("File ", fn_color(fn), " does not exist") # nocov
  }
  lines <- readLines(fn)

  # Remove the _pdf or _word or _anything
  if(length(grep("_", doc_type))){
    doc_type <- gsub("^(\\S+)_.*$", "\\1", doc_type)
  }

  bi_code <- c(
    '```{r bilingual-code, cache=FALSE}',
    'options(',
    '  # Prevent xtable from adding a timestamp comment to the table code it produces',
    '  xtable.comment = FALSE,',
    '  # Do not allow kableExtra to load packages, we add them manually in csasdown',
    '  kableExtra.latex.load_packages = FALSE,',
    '  # Stop chunk output (echo) running into the margins',
    '  width = 80,',
    '  # Do not use scientific notation (stops tables from showing 1.2e3, etc.)',
    '  scipen = 999)',
    '',
    'meta <- rmarkdown::metadata',
    'meta_out <- rmarkdown::metadata$output',
    'csl <- "csl/csas.csl"',
    'options(OutDec = ".")',
    'if (is.null(getOption("french"))) {',
    '  stop("`french` was not set up correctly in YAML header in index.Rmd. ",',
    '       "It must be true or false",',
    '       call. = FALSE)',
    '}',
    'if (getOption("french")) {',
    '  csl <- "csl/csas-french.csl"',
    '  options(OutDec = ",")',
    '}',
    '',
    'french <- isTRUE(getOption("french")) # for backwards compatibility',
    '',
    '# This hook simplifies document translation for the author.',
    '# When building in French, it draws a box around paragraphs contained in chunks',
    '#  which have the option `needs_trans = TRUE`. It also labels',
    '#  the box with a "Needs translation" tag in red and the chunk label in blue.',
    '# You need to change the `needs_trans` option to `FALSE` for a chunk once you',
    '#  have inserted the translated text into it. You will get a utf-8 error if',
    '#  you leave it as `TRUE` and there is French in the chunk.',
    '#  This function assumes you are translating from English to French.',
    '#  If you wrote your document in French and want to translate to English,',
    '#  put a ! before `getOption("french")` below and add the `need_trans`',
    '#  chunk options to your English paragraph chunks instead of the French ones.',
    '#  ',
    '#  IMPORTANT NOTES',
    '#  - Use `csasdown::render()` to render the document. This runs a',
    '#    pre-processing step which ensures any inline R chunks',
    '#    `r print("Like this one")` are taken care of correctly and that all',
    '#    backslash variables (eg. \\pi, \\alpha, \\@ref, \\cite) are all processed',
    '#    correctly.',
    '#  - French latex places a space before the colon by default so if you need a colon',
    '#    with no space before it, use \\\\hc .',
    'knit_hooks$set(needs_trans = function(before, options){',
    '  if(getOption("french") && options$needs_trans){',
    '    if (before){',
    '      paste0("\\\\',
    '   \\\\begin{lrbox}{\\\\userinput}',
    '   \\\\begin{minipage}{\\\\textwidth}',
    '   \\\\textcolor{red}{\\\\textbf{Needs translation - \\\\textcolor{blue}{knitr chunk: ", options$label, "}}}',
    '   \\\\begin{lstlisting}',
    '  ")',
    '    } else {',
    '      "',
    '   \\\\end{lstlisting}',
    '   \\\\end{minipage}',
    '   \\\\end{lrbox}',
    '   \\\\noindent',
    '   \\\\fbox{\\\\usebox{\\\\userinput}}',
    '  "',
    '    }',
    '  }',
    '})',
    '```',
    ''
  )

  # Check that bilingual code has not been injected previously
  bi_code_ind <- which(bi_code[1] == lines)
  if(!length(bi_code_ind)){
    lines <- c(lines, bi_code)
    if(verbose){
      check_notify("Injected knitr hook and language extraction code")
    }
  }

  if(doc_type %in% c("techreport","manureport")){
    yaml_code <- c(
      '---',
      'title: `r ifelse(csasdown::fr(), meta$french_title, meta$title)`',
      'region: `r ifelse(csasdown::fr(), meta$french_region, meta$region)`',
      'csl: `r csl`',
      '---')
  } else if (doc_type == "sr") { # no title language flipping
    yaml_code <- c(
      '---',
      'month: `r ifelse(csasdown::fr(), meta$french_month, meta$month)`',
      'region: `r ifelse(csasdown::fr(), meta$french_region, meta$region)`',
      'csl: `r csl`',
      '---')
  } else {
    yaml_code <- c(
      '---',
      'title: `r ifelse(csasdown::fr(), meta$french_title, meta$title)`',
      'month: `r ifelse(csasdown::fr(), meta$french_month, meta$month)`',
      'region: `r ifelse(csasdown::fr(), meta$french_region, meta$region)`',
      'csl: `r csl`',
      '---')
  }

  # Check that bilingual YAML code has not been injected previously
  yaml_code_ind <- which(yaml_code[2] == lines)
  if(!length(yaml_code_ind)){
    lines <- c(lines, yaml_code)
    if(verbose){
      check_notify("Injected trailing YAML block")
    }
  }

  writeLines(lines, fn)

  if(verbose && (!length(bi_code_ind) || !length(yaml_code_ind))){
    check_notify("Injected bilingual code successfully\n")
  }
}