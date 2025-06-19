#' Add phantomsection latex macro after the references declaration so that
#' the TOC link for it works correctly
#'
#' @details
#' This is another item that failed when we changed to lualatex. Also see
#' [add_appendix_subsection_refs()] and [fix_table_refs()] which were required
#' to fix issues with lualatex.
#'
#' @param x The LaTeX code as a vector of character strings, one for each line
#'
#' @return The modified LaTeX code as a vector of character strings, one for
#' each line
add_references_phantom <- \(x){

  refs_ind <- grep("label\\{references\\}", x)
  if(!length(refs_ind)){
    return(x)
  }

  if(length(refs_ind) != 1){
    bail("More than one line containing the references label is present.\n",
         "There can only be one:\n",
         paste(x[refs_ind], collapse = "\n"))
  }

  pre_refs <- x[1:refs_ind]
  post_refs <- x[(refs_ind + 1):length(x)]

  x <- c(pre_refs,
         "\\phantomsection",
         post_refs)

  x
}