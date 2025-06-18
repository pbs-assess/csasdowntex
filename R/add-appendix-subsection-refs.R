#' Post process appendix subsections in LaTeX code
#'
#' @description
#' A post-processor to modify the appendix subsection LaTeX code
#' after the document has been knit. The modifications ensure correct
#' referencing of the appendices throughout the document. References are
#' injected into the LaTeX code in `x`. The LaTeX macros injected prior to
#' each appendix/section/subsection/subsubsection are 'newcounter' and
#' 'refstepcounter'`.
#'
#' @keywords internal
#'
#' @param x The LaTeX code as a vector of character strings, one for each line
#'
#' @return The modified LaTeX code as a vector of character strings, one for
#' each line
add_appendix_subsection_refs <- function(x){

  # `app_pat` matches the title lines of each included appendix
  # These are lines starting with 1 hash symbol in rmarkdown
  # and after the \\Appendices line. Example line:
  # # THE FIRST APPENDIX {#app:a}
  app_pat <- "^\\\\starredchapter\\{"
  # `app_section_pat`matches each level 1 section in each appendix.
  # These are lines starting with 2 hash symbols in rmarkdown
  # and after the \\Appendices line. Example line:
  # ## a-1 {#app:a-1}
  app_section_pat <- "^\\\\appsection\\{"
  # `app_subsection_pat`matches each level 2 subsection in each section.
  # subsection. These are lines starting with 3 hash symbols in rmarkdown
  # and after the \\Appendices line. Example line:
  # ### a-1-1 {#app:a-1-1}
  app_subsection_pat <- "^\\\\subsection\\{"
  # `app_subsubsection_pat`matches each level 3 sub-subsection in each
  # subsection. These are lines starting with 4 hash symbols in rmarkdown
  # and after the \\Appendices line. Example line:
  # #### a-1-1-1 {#app:a-1-1-1}
  app_subsubsection_pat <- "^\\\\subsubsection\\{"

  app_start_ind <- grep("^\\\\Appendices", x)

  if(!length(app_start_ind)){
    message("'\\Appendices' text not found in the LaTeX code")
    return(x)
  }
  if(length(app_start_ind) > 1){
    warning("'\\Appendices' text found more than once in the LaTeX code. ",
            "Appendix/appendices referenced not added. Links to appendix ",
            "sections will fail")
    return(x)
  }
  main_doc_end_ind <- app_start_ind - 1

  # `app_inds` is a vector of all the locations of the appendix title lines
  # in the entire LaTeX code (`x`)
  app_inds <- grep(app_pat, x)
  if(!length(app_inds)){
    warning("No appendices found in '\\Appendices' section while attempting ",
            "to fix appendix subsection references")
    return(x)
  }

  # `main_doc_code` includes everything from the beginning of the file
  # up to but not including the line that is \\Appendices
  main_doc_code <- x[1:main_doc_end_ind]
  # `app_pre_code` includes everything from the line that is \\Appendices
  # up to but not including the first appendix title line
  app_pre_code <- x[app_start_ind:(app_inds[1] - 1)]

  # `app_code` includes everything from the line the first appendix title line
  #  to the end of the LaTeX code
  ap_code <- x[app_inds[1]:length(x)]

  # Get indices for appendix titles again since the vector is now shorter,
  # so we can reference `ap_code` instead of `x`, which makes it much easier
  # to follow while debugging since `ap_code` consists of ONLY appendices.
  # Also make counters for each appendix for referencing tags later
  app_inds <- grep(app_pat, ap_code)
  counters <- paste0("app_counter_", seq_along(app_inds))

  # Local function `split_at()`
  # Split the vector `x` into a list of chunks based on the vector of
  # indices given (`pos`). Edge cases are covered, e.g. if pos is `NA` or
  # `NULL`then the whole vector will be returned as a single element list.
  # If any of `pos` is out of range of the vector's dimensions, they will
  # be ignored.
  split_at <- \(x, pos){
    unname(split(x, cumsum(seq_along(x) %in% pos)))
  }

  # `apps` is a list of vectors of code lines for each appendix,
  # including the appendix title line (starredchapter line) for each
  apps <- split_at(ap_code, app_inds)

  # Parse each appendix for sections
  apps <- imap(apps, \(app, app_ind){

    sec_inds <- grep(app_section_pat, app)
    if(!length(sec_inds)){
      # Add referencing so the appendix can be referenced in the document
      # with a link
      counter_lines <- c(paste0("\\renewcommand{\\thechapter}{",
                                toupper(LETTERS[app_ind]),
                                "}"),
                         "\\refstepcounter{chapter}")
      app <- c(counter_lines, app)
      return(app)
    }

    # `sec_pre_code` includes everything from the title line of the appendix
    # up to but not including the first section title line
    sec_pre_code <- app[1:(sec_inds[1] - 1)]
    sec_code <- app[sec_inds[1]:length(app)]
    # Need to do the matching again because we stripped the title and header
    # code off (`sec_pre_code`)
    sec_inds <- grep(app_section_pat, sec_code)
    sections <- split_at(sec_code, sec_inds)

    sections <- imap(sections, \(sec, sec_ind){

      subsec_inds <- grep(app_subsection_pat, sec)
      if(!length(subsec_inds)){
        return(sec)
      }

      # `subsec_pre_code` includes everything from the title line of the
      # section up to but not including the first subsection title line
      subsec_pre_code <- sec[1:(subsec_inds[1] - 1)]
      subsec_code <- sec[subsec_inds[1]:length(sec)]
      # Need to do the matching again because we stripped the title and header
      # code off (`subsec_pre_code`)
      subsec_inds <- grep(app_subsection_pat, subsec_code)
      subsections <- split_at(subsec_code, subsec_inds)

      subsections <- imap(subsections, \(subsec, subsec_ind){

        subsubsec_inds <- grep(app_subsubsection_pat, subsec)
        if(!length(subsubsec_inds)){
          return(subsec)
        }

        # `subsubsec_pre_code` includes everything from the title line of
        # the subsection up to but not including the first subsubsubsection
        # title line
        subsubsec_pre_code <- subsec[1:(subsubsec_inds[1] - 1)]
        subsubsec_code <- subsec[subsubsec_inds[1]:length(subsec)]
        # Need to do the matching again because we stripped the title and
        # header code off (`subsubsec_pre_code`)
        subsubsec_inds <- grep(app_subsubsection_pat, subsubsec_code)
        subsubsections <- split_at(subsubsec_code, subsubsec_inds)

        subsubsections <- imap(subsubsections, \(subsubsec, subsubsec_ind){
          # Add referencing so the appendix subsubsections can be referenced
          # in the document with a link
          ref_string <- paste0("appendix_", app_ind,
                               "_appsection_", sec_ind,
                               "_subsection_", subsec_ind,
                               "_subsubsection_", subsubsec_ind,
                               "_counter")
          counter_lines <- c(paste0("\\newcounter{", ref_string, "}"),
                             paste0("\\refstepcounter{",ref_string,"}"))
          subsubsec <- c(counter_lines, subsubsec)
          subsubsec
        })
        # Add referencing so the appendix subsections can be referenced in the
        # document with a link
        ref_string <- paste0("appendix_", app_ind,
                             "_appsection_", sec_ind,
                             "_subsection_", subsec_ind,
                             "_counter")
        counter_lines <- c(paste0("\\newcounter{", ref_string, "}"),
                           paste0("\\refstepcounter{",ref_string,"}"))
        subsec <- c(counter_lines, subsec_pre_code, subsubsections)
        subsec
      })
      # Add referencing so the appendix sections can be referenced in the
      # document with a link
      ref_string <- paste0("appendix_", app_ind,
                           "_appsection_", sec_ind,
                           "_counter")
      counter_lines <- c(paste0("\\newcounter{", ref_string, "}"),
                         paste0("\\refstepcounter{",ref_string,"}"))
      sec <- c(counter_lines, sec_pre_code, subsections)
      sec
    })

    # Add referencing so the appendix can be referenced in the document
    # with a link
    counter_lines <- c(paste0("\\renewcommand{\\thechapter}{",
                              toupper(LETTERS[app_ind]),
                              "}"),
                       "\\refstepcounter{chapter}")
    # The first element in the `app` vector is a starredchapter.
    # To make linking work in lualatex, we must add a hypertarget to link to
    # for referenced using a hyperlink. This must come directly after the
    # starredchapter line. The reference is extracted from the label found on
    # starredchapter line
    star_chap_line <- app[1]
    the_rest <- app[2:length(app)]

    lab <- gsub("^.*label\\{(.*)}.*$", "\\1", star_chap_line)
    hyper_target_line <- paste0("\\hypertarget{", lab, "}{}")
    app <- c(star_chap_line,
             hyper_target_line,
             the_rest)

    app <- c(counter_lines, app)

    app
  })

  # Glue all the parts back together
  x <- c(main_doc_code,
         app_pre_code,
         unlist(apps))

  x
}
