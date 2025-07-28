#' Creates an Microsoft Word CSAS-formatted document
#'
#' @description
#' This is a function called in output in the YAML of the `index.Rmd` file
#' to specify the creation of an FSAR word document.
#'
#' @param ... Other arguments to [officedown::rdocx_document()]
#' @rdname csas_docx
#' @return A Word Document in the `.docx` format based on the FSAR template.
#' @export
fsar_word <- function(...) {
  if (!requireNamespace("officedown", quietly = TRUE)) {
    stop(
      "Package \"officedown\" must be installed to use this function.",
      call. = FALSE
    )
  }
  ## Several modifications were made to the fsar template
  ## 1) bookmarks were added to headers and footers for officer replacement below
  ## 2) a border was added to the context style and the table was removed
  ## 3) ordered (ol style) and unordered list (ul style) styles manually added to template
  ## 4) created a Table Caption style since Caption - Table was not being applied. May be a bug (https://github.com/davidgohel/officedown/issues/112).
  ## 5) Added First Paragraph style to avoid issues with Body Text not being applied to the first paragraph of each section.
  file <- "fsar-template.docx"
  base <- officedown::rdocx_document(...,
                                     base_format = "bookdown::word_document2",
                                     number_sections = FALSE,
                                     tables = list(
                                       style = "Compact", layout = "autofit", width = 1,
                                       caption = list(
                                         style = "Table Caption",
                                         pre = "Table", sep = ". ",
                                         fp_text = officer::fp_text_lite(bold = FALSE)
                                         ),
                                       conditional = list(
                                         first_row = TRUE, first_column = FALSE, last_row = FALSE,
                                         last_column = FALSE, no_hband = FALSE, no_vband = TRUE
                                       )
                                     ),
                                     plots = list(
                                       style = "Figure",
                                       align = "center",
                                       caption = list(
                                         style = "Caption - Figure",
                                         pre = "Figure ", sep = ". ",
                                         fp_text = officer::fp_text_lite(bold = FALSE)
                                       )
                                     ),
                                     lists = list(
                                       ol.style = "ol style",
                                       ul.style = "ul style"
                                     ),
                                     mapstyles = list(
                                       "Body Text" = c("Normal", "First Paragraph")
                                     ),
                                     pandoc_args = "--no-highlight",
                                     reference_docx = system.file("csas-docx",
                                                                  file,
                                                                  package = "csasdown")

  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}


#' Render a SAR/FSAR
#'
#' @param config_file YAML configuration file.
#' @param ... Arguments to pass to [bookdown::render_book()].
#'
#' @export
#' @aliases render_fsar
render_sar <- function(config_file = "_bookdown.yml", ...) {

  cat("\n")

  # Make sure all YAML entries are present in `index.Rmd`
  render_type <- "fsar_word"
  check_yaml(type = render_type, verbose = TRUE)

  # Find out what language is set to and set the option 'french' here
  # so that it works on the first compilation in a workspace
  # It sets `options(french)` to the value in the file
  # set_language_option(index_fn, verbose)

  # Set up the console Render message
  cat("\n")

  # Create the temporary YAML and Rmd files and store their names
  tmp_yaml_rmd_fns <- create_tmp_yaml_rmd_files(yaml_fn = config_file, verbose = TRUE)
  tmp_yaml_fn <- tmp_yaml_rmd_fns[[1]]
  tmp_rmd_fns <- tmp_yaml_rmd_fns[[2]]
  on.exit(unlink(unlist(tmp_yaml_rmd_fns), force = TRUE))
  first_content_fn <- utils::head(tmp_rmd_fns[!grepl("index", tmp_rmd_fns)], 1)

  notify("Pre-processing Rmd files.")

  x <- rmarkdown::yaml_front_matter("index.Rmd")

  extra_context <- paste0('This Science Advisory Report is from the ', x$meeting_date, " ", x$report_title, ". ",
                          'Additional publications from this meeting will be posted on the [Fisheries and Oceans Canada (DFO) Science Advisory Schedule](http://www.isdm-gdsi.gc.ca/csas-sccs/applications/events-evenements/index-eng.asp) as they become available.')

  title_and_context <- c('::: {custom-style="Heading 1"}', x$report_title, ':::\n',
                         '::: {custom-style="Heading 2"}', "Context", ':::\n',
                         '::: {custom-style="Body Text"}', paste0(x$context, extra_context), ':::\n')

  content <- readLines(con = first_content_fn, warn = FALSE)

  sources <- c('\n## SOURCES OF INFORMATION {-}\n',
               '<div id="refs" custom-style = "citation"></div>\n',
               extra_context,
               '\n\\pagebreak\n')

  backmatter <- c('## THIS REPORT IS AVAILABLE FROM THE:{-}\n',
                  '::: {custom-style="Body Text + Centered"}',
                  x$csa_address,
                  'E-Mail:', x$email, "\\",
                  'Internet address: [www.dfo-mpo.gc.ca/csas-sccs/](www.dfo-mpo.gc.ca/csas-sccs/)\n',
                  'ISSN xxxx-xxxx\\',
                  paste0('ISBN 978-0-660-xxxxx-x&#9;Cat. No. Fs70-6/', x$report_year, '-nnnE-PDF\\'),
                  '\u00a9 His Majesty the King in Right of Canada, as represented by the Minister of the\\
                  Department of Fisheries and Oceans,', x$report_year, "\n",
                  'This report is published under the [Open Government Licence - Canada](https://open.canada.ca/en/open-government-licence-canada)\n',
                  'MOBIUS', # To be replaced below with image using officer
                  ':::\n',
                  "\nCorrect citation for this publication:\n",
                  '::: {custom-style="citation"}',
                  paste0("DFO. ", x$report_year, ". ", x$report_title, ". DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. ", x$report_year, "/", x$report_number, ". iv + xx p."),
                  ':::',
                  "\n*Aussi disponible en fran\u00e7ais:*\n",
                  '::: {custom-style="citation"}',
                  paste0("*MPO. ", x$report_year, ". ", x$report_title_french, ". Secr. can. des avis sci. du MPO. Avis sci. ", x$report_year, "/", x$report_number, ". iv + xx p.*"),
                  ':::',
                  "\nInuktitut Atuinnaummijuq:\n",
                  '::: {custom-style="citation"}',
                  x$inuktitut_citation,
                  ':::')

  writeLines(c(title_and_context, content, sources, backmatter), con = first_content_fn)

  notify(
    "Rendering the ", csas_color("FSAR"), " as a ",
    csas_color("Word"), " document in ",
    # csas_color(`if`(fr(), "French", "English")),
    " ...\n\n"
  )

  notify("Knitting Rmd files and running Pandoc to build the document ...")

  render_book("index.Rmd",
              config_file = tmp_yaml_fn,
              ...)

  ## officedown outputs to the root, not the _book folder like bookdown
  if (file.exists("fsar.docx")) {
    file.rename("fsar.docx", file.path("_book", "fsar.docx"))
  }

  fn <- "_book/fsar.docx"
  if (file.exists(fn)) {
    check_notify(
      "Knitting and Pandoc completed, document built ",
      "successfully\n"
    )
  } else {
    # nocov start
    bail(
      "The Knitting and Pandoc procedure did not produce ",
      fn_color(fn)
    )
    # nocov end
  }
  # }

  # Rename the output files to include 'english' or 'french' so that
  # rendering different language versions does not overwrite the other
  # language version.
  # rename_output_files(index_fn, verbose)
  # if (!verbose) {
  #   notify(
  #     "For help debugging your code, render your document like this:\n",
  #     csas_color("render(verbose = TRUE, keep_files = TRUE)"), "\n\n"
  #   )
  # }

  # Delete the temporary files
  # if (!keep_files) {
  #   map(fn_process, ~ {
  #     unlink(.x, force = TRUE)
  #   })
  #   unlink(tmp_yaml_fn)
  #   unlink(index_fn)
  #   unlink("*.log")
  #   unlink("*.upa")
  # }

  check_notify("Render completed")

  notify("Modifying headers and footers.")

  doc <- officer::read_docx("_book/fsar.docx")
  doc <- officer::headers_replace_text_at_bkm(doc, "region_name", x$region)
  doc <- officer::headers_replace_text_at_bkm(doc, "region_name_rest", x$region) # non-first page
  doc <- officer::headers_replace_text_at_bkm(doc, "short_title", x$report_title_short) # non-first page
  doc <- officer::headers_replace_text_at_bkm(doc, "report_year", x$report_year)
  doc <- officer::headers_replace_text_at_bkm(doc, "report_number", x$report_number)
  doc <- officer::footers_replace_text_at_bkm(doc, "release_month", x$release_month)
  doc <- officer::footers_replace_text_at_bkm(doc, "release_year", x$report_year)

  ## Insert mobius loop image using doc with just the image with the proper style and alt text
  doc <- officer::cursor_reach(doc, keyword = "MOBIUS") |>
    officer::body_remove() |>
    officer::cursor_backward() |>
    officer::body_add_docx(src = system.file("graphics", "mobius_loop.docx", package = "csasdown"))

  print(doc, target = "_book/fsar.docx")

  invisible()
}

render_fsar <- render_sar