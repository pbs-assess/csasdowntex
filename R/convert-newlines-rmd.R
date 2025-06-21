#' Remove all the constraints in Rmd involving newlines
#'
#' @description
#' Rmarkdown code requires several different entries to render newlines
#' in a final document. These are frustrating because the algorithm
#' is not clear on what you need for which case. For example, header lines
#' (lines starting with #) do not need a newline, but if you want 'N' blank
#' lines between the header and next text, you need 'N lines containing
#' only \\, without any empty lines following.
#' For other text, you need N + 1 lines containing only \\ with an empty line
#' following, before the next text occurs. List elements must not have newlines
#' between them.
#' This allows for 'What you see is what you get' in your Rmarkdown code.
#'
#' @keywords internal
#' @family rmd_conversion_functions
#'
#' @param chunk A vector of strings, one line of Rmarkdown per element
#' @param strip_blank_lines Strip all lines composed of whitespace.
#' Rstudio automatically indents several spaces to the start of lines when you
#' press 'Enter' and it is common to ignore those when editing
#' @param ... Absorb arguments intended for other functions
#'
#' @return A modified vector of strings
convert_newlines_rmd <- function(chunk,
                                 strip_blank_lines = FALSE,
                                 ...){

  if(is.null(chunk)){
    return(NULL)
  }

  if(strip_blank_lines){
    chunk <- map_chr(chunk, ~{
      `if`(grepl("^\\s+$", .x), "", .x)
    })
  }
  new_chunk <- NULL

  is_blank_line <- chunk[1] == ""
  is_header_line <- is_rmd_header_line(chunk[1])
  is_lst_line <- is_rmd_list_line(chunk[1])
  is_table_type <- is_rmd_table_line(chunk)

  if(is_blank_line){
    ret_lst <- conv_blank_lines(chunk)
  }else if(is_header_line){
    ret_lst <- conv_header_lines(chunk)
  }else if(is_lst_line){
    ret_lst <- conv_list_lines(chunk)
  }else if(is_table_type == "type1"){
    ret_lst <- conv_type_1_table_lines(chunk)
  }else if(is_table_type == "type2"){
    ret_lst <- conv_type_2_table_lines(chunk)
  }else{
    ret_lst <- conv_paragraph_lines(chunk)
  }

  c(new_chunk,
    ret_lst[[1]],
    convert_newlines_rmd(ret_lst[[2]]))
}