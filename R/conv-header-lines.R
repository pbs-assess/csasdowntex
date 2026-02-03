#' Convert header lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert header lines in Rmd code to WYSIWYG newlines. The provided chunk
#' will start with a header line and possibly more or return NULL as the
#' converted chunk, and the whole `chunk` as the rest. The series of header
#' lines will be converted into a mini-chunk, which will be returned
#' as the first element of a two-element list, the second element is the
#' rest of the Rmd.
#'
#' @keywords internal
#'
#' @family rmd_conversion_functions
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last blank line
#'
#' @examples
#' \dontrun{
#' library(csasdowntex)
#' chunk <- c("#Main", "##Introduction", "", "Some text..", "More Rmd text")
#' tmp <- conv_header_lines(chunk)
#' header_line_chunk <- tmp[[1]]
#' the_rest <- tmp[[2]]
#' }
conv_header_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(!is_rmd_header_line(chunk[1])){
    return(list(NULL, chunk))
  }

  if(length(chunk) == 1){
    return(list(chunk[1], NULL))
  }

  new_chunk <- NULL
  last_header_match <- 1
  i <- 1
  repeat{
    curr_is_header <- is_rmd_header_line(chunk[i])
    if(curr_is_header){
      new_chunk <- c(new_chunk, chunk[i])
      last_header_match <- i
    }else if(chunk[i] == ""){
      # Special case - ignore blank lines between headers
      repeat{
        curr_is_blank <- chunk[i] == ""
        if(!curr_is_blank){
          i <- i - 1
          break
        }
        if(i == length(chunk)){
          break
        }
        i <- i + 1
      }
    }else{
      # It's the start of a list or table header
      break
    }
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }

  new_chunk <- c(new_chunk, "")
  if(last_header_match == length(chunk)){
    list(new_chunk, NULL)
  }else{
    list(new_chunk, chunk[(last_header_match + 1):length(chunk)])
  }
}
