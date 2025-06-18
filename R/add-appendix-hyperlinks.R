#' Change appendix refs to hyperlinks and match the references with their
#' actual appendix letter, adding it into the text so that it appears
#' normally in the document text
#'
#' @param x The LaTeX code as a vector of character strings, one for each line
#'
#' @return The modified LaTeX code as a vector of character strings, one for
#' each line
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
  # order found in the document. This should always be correct
  labels_df <- enframe(labels, name = NULL) |>
    mutate(letter = LETTERS[1:length(labels)]) |>
    rename(label = value) |>
    select(letter, label)

  x[app_ref_inds] <- x[app_ref_inds] |>
  map_chr(~{

    k <- gsub("\\\\ref", "\\\\protect\\\\hyperlink", .x)
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
      k <- paste0(pre_k, "{\\textbf{NOT FOUND}}", post_k)
    }else{
      let <- row$letter
      k <- paste0(pre_k, "{", let, "}", post_k)
    }

    k
  })

  x
}