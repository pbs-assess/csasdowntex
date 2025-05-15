#' Post-process landscape tables in TEX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_landscape_tables <- function(x, ...){

  # Add fancy landscape page type to landscape pages to remove the sideways
  # headers and footers, and add a page number at the bottom

  lscape_inds <- grep("^\\\\begin\\{landscape\\}", x)
  if(!length(lscape_inds)){
    return(x)
  }

  lst <- post_process_extract_chunks(x, lscape_inds, lscape_inds)

  # Replace the begin landscape line with the KOMA options lines
  lst$between <- map2(lst$between, lscape_inds, \(lscape_line, ind){
    # Extract font information
    adj_val <- "1in"
    adj_ind <- grep("ADJUST TABLE", x[(ind - 5):(ind - 1)])
    if(length(adj_ind)){
      adj_ind_in_x <- ind - 5 + adj_ind - 1
      adj_line <- x[adj_ind_in_x]
      adj_val <- gsub("ADJUST TABLE (-?[0-9]+)",
                      "\\1",
                      adj_line)
      adj_val <- as.numeric(adj_val)
      if(is.na(adj_val)){
        bail("ADJUST TABLE macro on line ", adj_ind_in_x, " of the ",
             "Tex file is not of the correct format.\nLine with error:\n",
             adj_line, "\n\n",
             "Format:\n",
             "ADJUST TABLE (-)[decimal number in inches]\n\n",
             "Examples:\n",
             "ADJUST TABLE 2\nADJUST TABLE -0.5\n")
      }
      adj_val <- paste0(adj_val, "in")
    }
    fnt_line <- gsub("^\\\\begin\\{landscape\\}\\\\begingroup(.*)$",
                     "\\1",
                     lscape_line)

    c("%",
      "% The following code was injected by",
      "% csasdown::post_process_landscape_tables()",
      "%",
      "\\KOMAoptions{paper = landscape, DIV = last}",
      paste0("\\newgeometry{",
             paste0("hmargin = ", adj_val, ", "),
             "top = 0.25in, ",
             "bottom = 1in, ",
             "height = 7in, ",
             "includehead}"),
      "\\fancyheadoffset{0pt}",
      fnt_line,
      "%",
      "% End of injected code",
      "%")
  })

  x <- post_process_interlace_chunks(lst)

  # Replace the \end{landscape] with the KOMA options lines
  # portrait mode
  # If end{longtable} and end{group} are part of the same line as  the table
  # itself, extract these out and create new lines for them after the table.
  # Heavy tex editing

  lscape_endgroup_inds <- grep("\\\\endgroup\\{\\}", x)
  lscape_end_inds <- grep("\\\\end\\{landscape\\}", x)
  wch <- which((lscape_endgroup_inds + 1) %in% lscape_end_inds)
  lscape_endgroup_inds <- lscape_endgroup_inds[wch]
  lscape_end_inds <- lscape_endgroup_inds + 1

  lst <- post_process_extract_chunks(x, lscape_endgroup_inds, lscape_end_inds)
  lst$between <- map(lst$between, \(lscape_line){
    c("%",
      "% The following code was injected by",
      "% csasdown::post_process_landscape_tables()",
      "%",
      "\\KOMAoptions{paper = portrait, DIV = last}",
      "\\restoregeometry",
      "\\fancyheadoffset{0pt}",
      "%",
      "% End of injected code",
      "%")
  })

  x <- post_process_interlace_chunks(lst)

  # Remove the ADJUST TABLE text from the doc
  inds <- grep("ADJUST TABLE", x)
  x[inds] <- ""

  x
}
