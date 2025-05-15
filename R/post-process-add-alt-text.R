#' Add alternative text to figures in the document via the
#' `pdfmanagement-testphase` LaTeX package
#'
#' @details
#' Searches the given TEX file for the `includegraphics{}`,
#' finds the correct alternative text for those calls by searching through
#' the project RMD files for the matching `(ref:variable-alt)` definitions
#' and injects the text and the `pdfmanagement-testphase` structural
#' LaTeX lines for web accessibility
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param accessible_pdf Logical. If `TRUE`, inject the code needed to include
#' the `pdfmanagement-testphase`` package and add alternative text
#' @param figures_dir The subdirectory of the "doc" directory containing
#' images that have previously been made such as pictures and logos, and all
#' other figures made outside the scope of this project
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_add_alt_text <- function(x,
                                      accessible_pdf = NULL,
                                      title_page_image = NULL,
                                      title_page_image_width_cm = NULL,
                                      figures_dir = NULL,
                                      knitr_figures_dir = NULL,
                                      ...){

  figures_dir <- figures_dir %||% "figure"
  accessible_pdf <- accessible_pdf %||% FALSE
  knitr_figures_dir <- knitr_figures_dir %||% "knitr-figs-pdf"

  if(!accessible_pdf){
    return(x)
  }
browser()
  # Inject the headers needed for the pdfmanagement-testphase package
  x <- c(
    "%",
    "% The following code was injected by",
    "% csasdown::post_process_add_alt_text()",
    "%",
    #"\\RequirePackage{pdfmanagement-testphase}",
    paste0("\\DocumentMetadata{testphase={phase-II,table}, ",
           "pdfstandard=A-2U, lang=en-US}"),
    "%",
    "% End of injected code",
    "%",
    x)

  # Inject anti-table tagging code to stop errors on captions in longtables.
  # Captions in longtables will cause an error if the newest tagpdf
  # (pdfmanagement-testphase) is used as of 2025 version of the package
  lt_inds <- grep("\\\\begin\\{longtable\\}", x)
  lt_inds <- c(1, lt_inds, length(x))
  y <- NULL
  for(i in 2:length(lt_inds)){
    start <- lt_inds[i - 1]
    end <- lt_inds[i] - 1
    if(i != length(lt_inds)){
      y <- c(y, x[start:end], "\\tagpdfsetup{table/tagging=false}")
    }else{
      y <- c(y, x[start:(end + 1)])
    }
  }
  x <- y

  # Title page image is totally different in the way it is included so we
  # remove it from the process right at the start
  #pat_title_page_fig <- paste0("\\\\includegraphics\\[width\\s*=\\s*",
  #                             title_page_image_width_cm,
  #                             "\\s*cm\\]\\{",
  #                             title_page_image,
  #                             "\\}")
  #ind_title_page_fig <- grep(pat_title_page_fig, x)

  pat_all_figs <- paste0("\\\\includegraphics")
  inds_all_figs <- grep(pat_all_figs, x)

  # Remove all lines starting with comment character
  comment_lines <- grep("^%", trimws(x[inds_all_figs]))
  if(length(comment_lines)){
    inds_all_figs <- inds_all_figs[-comment_lines]
  }

  if(!length(inds_all_figs)){
    # No figures in document except possibly the title page image
    return(x)
  }

  # Get indices of file-based figures
  is_knitr <- map_lgl(x[inds_all_figs], ~{
    length(grep(knitr_figures_dir, .x))
  })
  inds_knitr <- inds_all_figs[is_knitr]
  inds_file <- inds_all_figs[!is_knitr]

  # Get chunk names for the knitr-based figures
  mid_pat <- ifelse(substr(knitr_figures_dir,
                           nchar(knitr_figures_dir),
                           nchar(knitr_figures_dir)) == "/",
                    "(.*?)-[0-9]+\\} *$",
                    "/(.*?)-[0-9]+\\} *$")
  knitr_labels <- gsub(paste0(".*\\{",
                              knitr_figures_dir,
                              mid_pat),
                       "\\1",
                       x[inds_knitr])

  pat_fns <- "^.*\\{([/0-9a-zA-Z_-]+)}\\s*$"
  file_fns_labels <- gsub(pat_fns, "\\1", x[inds_file], perl = F)

  file_based_labels <- map(unique(file_fns_labels), ~{
    extract_label_from_figure_filename(.x)
  }) |>
    unlist()

  alt_text_knitr <- map_chr(knitr_labels, ~{
    extract_alt_text(.x)
  })

  alt_text_files <- map_chr(file_based_labels, ~{
    extract_alt_text(.x)
  })

  # Put the alt text in the correct order
  file_which <- which(inds_all_figs %in% inds_file)
  knitr_which <- which(inds_all_figs %in% inds_knitr)

  all_alt_text <- NULL
  all_alt_text[knitr_which] <- alt_text_knitr
  all_alt_text[file_which] <- alt_text_files

  all_fig_labels <- NULL
  all_fig_labels[knitr_which] <- knitr_labels
  all_fig_labels[file_which] <- file_based_labels

  #all_fig_labels <- paste0(all_fig_labels, "-fig")

  # Inject the alt text and accompanying accessibility tags
  for(label in seq_along(all_fig_labels)){
    x <- alt_text_inject_tags(x,
                              all_fig_labels[label],
                              all_alt_text[label])
  }

  x
}
