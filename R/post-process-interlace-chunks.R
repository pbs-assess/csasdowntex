#' Interlace chunks of code that have been extracted by [extract_chunks()]
#'
#' @param lst A list, as returned by [extract_chunks()]
#'
#' @return A vector, with the `between` chunks and `inbetween` chunks
#' interlaced
#' @export
post_process_interlace_chunks <- function(lst){

  out <- NULL

  if(lst$first){
    out <- c(lst$between[[1]])
    lst$between <- lst$between[-1]
  }
  # Interlace the chunks, between followed by in-between, followed by
  # between, etc.
  for(i in seq_along(lst$between)){
    out <- c(out, lst$inbetween[[i]], lst$between[[i]])
  }
  # If there's one in-between chunk left at the end, add it here
  if(length(lst$inbetween) > length(lst$between)){
    out <- c(out, lst$inbetween[[length(lst$inbetween)]])
  }

  out
}