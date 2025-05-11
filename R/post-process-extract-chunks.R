#' Extract chunks of lines from a vector of lines
#'
#' @details
#' Creates two lists of slices of the input vector, the first list will
#' contain the slices between `beg_inds` and `end_inds`. The second
#' list will contain the slices NOT between `beg_inds` and `end_inds`.
#' If the first element of `beg_ind` is 1, then the return flag `first` will
#' be set to `TRUE`. If the first element of `beg_ind` is not a 1, the
#' return flag `first` will be set to `FALSE`. If the difference between
#' an `end_inds` value and the corresponding next `beg_inds` value, a
#' line with an empty string will be placed between them so that the
#' interlacing between the two lists remains perfect, so they 'zipper'
#' together 1 after the other.
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' @param beg_inds A vector of beginning indices for the chunks to
#' extract. Must be the same length as `end_inds`
#' @param end_inds A vector of ending indices for the chunks to
#' extract. Must be the same length as `beg_inds`
#'
#' @return A list of three elements, the first being the list of vectors
#' which are 'in-between' the `beg_inds` and `end_inds` values. The second
#' is a list of vectors which are 'between' the `beg_inds` and `end_inds`
#' values. the third is `first` which is explained above in `@details`
#' @export
#'
#' @examples
#' library(hake)
#' k <- extract_chunks(letters, c(2, 10), c(9, 12))
post_process_extract_chunks <- function(x, beg_inds, end_inds){

  if(!length(beg_inds)){
    stop("`beg_inds` is zero length. You must have at least one set of ",
         "indices to extract")
  }
  if(length(beg_inds) != length(end_inds)){
    stop("`beg_inds` and `end_inds` must be the same length")
  }
  out <- list()
  out$between <- list()
  out$inbetween <- list()
  out$first <-TRUE
  if(beg_inds[1] > 1){
    out$first <- FALSE
    # Extract the chunk before the first chunk defined by beg/end starts
    out$inbetween[[1]] <- x[1:(beg_inds[1] - 1)]
  }
  if(length(beg_inds) == 1){
    out$between[[1]] <- x[beg_inds[1]:end_inds[1]]
    out$inbetween[[1 + !out$first]] <-
      x[(end_inds[1] + 1):length(x)]
    return(out)
  }

  for(i in 1:(length(beg_inds) - 1)){
    out$between[[i]] <- x[beg_inds[i]:end_inds[i]]
    out$inbetween[[i + !out$first]] <-
      x[(end_inds[i] + 1):(beg_inds[i + 1] - 1)]
  }
  out$between[[length(beg_inds)]] <-
    x[beg_inds[length(beg_inds)]:end_inds[length(beg_inds)]]

  if(end_inds[length(end_inds)] < length(x)){
    out$inbetween[[length(end_inds) + !out$first]] <-
      x[(end_inds[length(beg_inds)] + 1):length(x)]
  }

  out
}
