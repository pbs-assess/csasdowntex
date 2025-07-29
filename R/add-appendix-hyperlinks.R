#' Change appendix refs to hyperlinks and match the references with their
#' actual appendix letter, adding it into the text so that it appears
#' normally in the document text
#'
#' @details
#' With lualatex, starred chapters (appendices) cannot be linked to. It is
#' surprising that pdflatex allows this since it is in the latex spec to NOT
#' allow for it. This function uses a hack that inserts hypertargets and
#' hyperlinks for appendix references. The letters shown in the text are not
#' known at compile time so a data frame of letters and starred chapter labels
#' (key-value pair) is created and is in the order in which the appendices
#' are included in the document. The letter is extracted from this data frame
#' and inserted in the hyperlink for the appendix as the text portion to show
#' in the document. This should always work out correctly assuming appendices
#' are called A-Z inclusive. If there are more than 26 appendices this will
#' have to be rewritten in another way.
#'
#' @param x The LaTeX code as a vector of character strings, one for each line
#'
#' @return The modified LaTeX code as a vector of character strings, one for
#' each line
#' @importFrom tibble enframe
add_appendix_hyperlinks <- \(x){

  app_ref_inds <- grep("ref.*app:", x)
  if(!length(app_ref_inds)){
    return(x)
  }

  # Find Appendix hypertargets (Actual included appendices)
  app_inds <- grep("hypertarget.*app:", x)
  if(!length(app_inds)){
    alert("Appendix reference(s) found without any appendices in the document:\n",
         paste(x[app_ref_inds], collapse = "\n"))
  }

  labels <- gsub("^.*hypertarget\\{(.*?)\\}.*$", "\\1", x[app_inds])
  # Make a data frame with appendix letters matching up with the labels in
  # order found in the document. This should always be correct, but there is
  # no internal link so if you see a written appendix letter that does not
  # match where it takes you, this is probably to blame.
  labels_df <- enframe(labels, name = NULL) |>
    mutate(letter = LETTERS[1:length(labels)]) |>
    rename(label = value) |>
    select(letter, label)

  x[app_ref_inds] <- x[app_ref_inds] |>
  map_chr(~{

    k <- gsub("\\\\ref\\{app:", "\\\\protect\\\\hyperlink\\{app:", .x)
    lab <- gsub("^.*hyperlink\\{(.*?)\\}.*$", "\\1", k)
    # Find the appendix letter and make it the text that appears in the link
    row <- labels_df |>
      filter(label == lab)
    pat <- "(^.*hyperlink\\{.*?\\})(.*$)"
    pre_k <- gsub(pat, "\\1", k)
    post_k <- gsub(pat, "\\2", k)
    if(nrow(row) != 1){
      alert("Could not find an appendix label matching the appendix reference: ",
           .x)
      # References to appendices not included in the document will appear as
      # bold double question-marks, as they do for other latex refs that can't
      # be found
      k <- paste0(pre_k, "{\\textbf{??}}", post_k)
    }else{
      let <- row$letter
      k <- paste0(pre_k, "{", let, "}", post_k)
    }

    k
  })

  x
}