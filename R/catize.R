#' Convert a string including inline knitr code chunks to a cat-like string
#'
#' @description
#' Formats the supplied string so that inline knitr code chunks are replaced
#' with the format used in commands such as [cat()] and [paste()] which is a
#' series of comma-separated strings and R code. Use this function to format
#' strings from Rmarkdown to something that can be copy/pasted into a [cat()]
#' command inside a knitr chunk.
#'
#' @details
#' This is used primarily to convert sections of code that have been written
#' in Rmarkdown into simple strings with R code embedded between quoted,
#' comma-separated strings that can be passed to [cat()]. Here is a simple
#' example of the conversion this function does.
#'
#' Original rmarkdown string:
#'
#' ```
#' "The date is `r as.character(Sys.Date())` today. You are on
#' a `r Sys.info()["sysname"]` machine."
#' ```
#'
#' Modified cat-like string (can be passed to [cat()]):
#'
#' ```
#' "The date is ", as.character(Sys.Date()), " today. You are on a ",
#' Sys.info()["sysname"], " machine."
#' ```
#' @keywords internal
#'
#' @param str The string containing inline knitr-style R code
#' (backtick-enclosed)
#'
#' @return A non-quoted (see [noquote()]) string which can be
#' enclosed with double-quotes and copy/pasted into a [cat()]
#' or [paste()] command
#'
#' @importFrom stringr str_split str_extract_all str_replace
#' @importFrom knitr all_patterns
catize <- function(str){

  # `pattern` is close to the official knitr regexp,
  #  knitr::all_patterns$md$inline.code
  #
  # See this page:
  # https://rdrr.io/cran/knitr/man/knit_patterns.html
  #
  # Added lookahead and lookbehind to make sure the r inline code chunk is
  # not surrounded by double backticks. If it is, it is meant to be verbatim
  # in the output document instead of evaluated as code
  pattern <- "(?<!`` )`r[ #][^`]+\\s*`(?! ``.*$)"
  # knitr::all_patterns$md$inline.code
  txt <- str_split(str, pattern)[[1]]
  code <- str_extract_all(str, pattern)[[1]]
  if(!length(code)){
    return(str)
  }
  # Extract R code(s)
  code <- gsub("^`r ", "", code)
  code <- gsub("`$", "", code)
  if(length(txt) == 2 && txt[1] == "" && txt[2] == ""){
    return(paste('", ', code, ',"'))
  }
  # Paste the string back together
  out <- map2(txt[-length(txt)], code, ~{
    paste0(.x, '", ', .y, ', "')
  })
  out <- map_chr(out, ~{.x})
  out <- paste(out, collapse = "")
  out <- paste0(out, txt[length(txt)])
  noquote(out)
}
