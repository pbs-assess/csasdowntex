#' Add extra code lines after the caption line for each table so that
#' the linking references work properly in the document
#'
#' @details
#' When using lualatex, standard table referencing fails. This hack must be
#' done to make the table references correct and the links, when clicked,
#' take you to the correct table. See comments in the code for more details.
#'
#' This is another item that failed when we changed to lualatex. Also see
#' [add_references_phantom()] and [add_appendix_subsection_refs()] which were
#' required to fix issues with lualatex.
#'
#' @param x The LaTeX code as a vector of character strings, one for each line
#'
#' @return The modified LaTeX code as a vector of character strings, one for
#' each line
fix_table_refs <- \(x){

  tab_caption_inds <- grep("\\\\caption\\{\\\\label\\{", x)
  if(!length(tab_caption_inds)){
    return(x)
  }

  # Regarding '.*?' below. The question mark means match non-greedy.
  # Prevents '.*' from matching the last end brace '}' and forces it
  # to match the first one it encounters
  pat <- "(^\\\\caption\\{)\\\\label\\{(.*?)\\}(.*$)"
  # Strip the label out from within the caption command
  labs <- gsub(pat,
               "\\2",
               trimws(x[tab_caption_inds]))
  # Re-create the caption command without the label present inside
  caps <- gsub(pat,
               "\\1\\3",
               trimws(x[tab_caption_inds]))

  caps_mod <- map2(caps, labs, \(str, lab){
    # This funny business took forever to figure out using trial and error.
    # For some reason the counter was not reference-able so all table refs
    # linked to the last page in the document. The solution was to subtract one
    # from the current table counter then add one again but use refstepcounter
    # to add it so that it could then be referenced properly and the link made.
    #
    # In addition, the label had to be stripped out of the caption (done above)
    # and added separately after the subtract/add counter commands.
    new_code_vec <- c(str,
                      paste0("\\addtocounter{table}{-1}",
                             "\\refstepcounter{table}",
                             "\\label{", lab, "}\\\\"))
    new_code_vec
  })

  if(length(caps_mod) != length(tab_caption_inds)){
    bail("There was a problem with the regular expressions in the function ",
         "`fix_table_refs()`. The number of modified table caption code chunks ",
         "does not match the number of table captions originally found")
  }

  # Need to offset the indices for insertion of code since we are modifying
  # the length of the latex code as we progress. There is 1 new line per
  # caption, which add up cumulatively as we progress
  num_additional_lines_vec <- rep(1, length(tab_caption_inds) - 1)
  lines_offset <- c(0, cumsum(num_additional_lines_vec))
  tab_caption_inds <- tab_caption_inds + lines_offset

  i <- 1
  # Inject the code lines that make the table referencing work
  for(cap_ind in tab_caption_inds){
    # The -1 and +1 below ensures the caption found at the cap_ind value is
    # not included (it is replaced) and not doubled when the new code is
    # injected. Technically this is a memory leak as one element is cast
    # into the ether at each iteration. Garbage collection should pick it up.
    chunk_end <- cap_ind - 1
    chunk_start_next <- cap_ind + 1

    pre_x <- x[1:chunk_end]
    post_x <- x[chunk_start_next:length(x)]

    x <- c(pre_x,
           caps_mod[[i]],
           post_x)

    i <- i + 1
  }

  x
}
