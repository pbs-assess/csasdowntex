#' Convert an Rmarkdown type 1 table into a simplified form with WYSIWYG
#' newlines
#'
#' @description
#' Convert an Rmarkdown type 1 table into a simplified form with WYSIWYG
#' newlines. If there is no table detected, NULL will be returned as the
#' converted chunk, or an error will be thrown depending on the situation.
#'
#' @details
#' A type 1 table is defined as a manually-entered Rmarkdown table with a
#' minimum of five lines with this format:
#' ```
#' ----- ----- -----
#'  a     b     c
#' ----- ----- -----
#' abc   def   ghi
#' ----- ----- -----
#' ```
#'
#' There is no whitespace in between any of these lines. The first text row is
#' the column headers for the table and the second text row is the table data.
#' The second row can be multiple rows separated by an arbitrary number of
#' blank lines, but there cannot be blank lines before the table data or after.
#' Here is an example with three table data rows in acceptable format, along
#' with the optional table caption text which must start with 'Table:'
#'
#' ```
#' ----- ----- -----
#'  a     b     c
#' ----- ----- -----
#' abc   def   ghi
#'
#' jkl   mno   pqr
#'
#'
#' stu   vwx   yza
#' ----- ----- -----
#' Table: Table caption (0 or more blank lines between table and this caption)
#' A second line of table caption here (no blank lines in between)
#' ```
#' @keywords internal
#' @family rmd_conversion_functions
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, which are the modified type 1 table and the
#' rest of the chunk starting with the line after the end of the table
#'
#' @examples
#' \dontrun{
#' chunk <- c("---------- -----------", "  Parameter   Value",
#'            "---------- -----------", "     x          1.0",
#'            "     y          2.2", "---------- -----------")
#' tmp <- csasdowntex:::conv_type_1_table_lines(chunk)
#' the_rest <- tmp[[2]]
#' }
conv_type_1_table_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(length(chunk) < 5){
    bail("A type 1 table must have at least 5 lines. Input chunk is:\n\n",
         csas_color(paste(chunk, collapse = "\n")),
         "\n\n")
  }

  if(is_rmd_table_line(chunk) != "type1"){
    bail("The following table is not a type 1 table based on the first four ",
         "rows:\n\n",
         csas_color(paste(chunk, collapse = "\n")),
         "\n\n",
         "They must start with:\n",
         "- a row of dashes\n",
         "- a row of text representing column headers\n",
         "- a row of dashes\n",
         "- a row of text representing the first row of data.")
  }

  start_tbl_ind <- 1
  end_tbl_ind <- NULL
  start_lbl_ind <- NULL
  end_lbl_ind <- NULL

  # Add the first 4 rows as they have been checked already in
  # `is_rmd_table_line()`
  tbl_chunk <- c(chunk[1:4], "")
  i <- 5
  repeat{
    end_tbl <- is_rmd_dashed_line(chunk[i])
    if(end_tbl){
      end_tbl_ind <- i
      # Remove previous row's extra blank line while adding ending row
      # of dashes
      tbl_chunk <- c(tbl_chunk[-length(tbl_chunk)], chunk[i])
      break
    }
    # Break out of the table if a list or header element is detected
    if(is_rmd_list_line(chunk[i]) ||
       is_rmd_header_line(chunk[i])){
      break
    }
    if(chunk[i] != ""){
      tbl_chunk <- c(tbl_chunk, chunk[i], "")
    }
    if(i == length(chunk)){
      break # nocov
    }
    i <- i + 1
  }
  if(end_tbl){
    # Basic table without a table caption string included
    if(end_tbl_ind == length(chunk)){
      return(list(tbl_chunk, NULL))
    }
  }else{
    bail("A table appears to have been started but not finished. ",
         "This can happen if a list element or header element was ",
         "misplaced inside the table code. Here is the offending ",
         "Rmarkdown text:\n\n",
         csas_color(paste(chunk, collapse = "\n")))
  }

  # Add label if it exists
  lbl <- extract_rmd_table_label(chunk[(end_tbl_ind + 1):length(chunk)])
  tbl_chunk <- c(tbl_chunk, lbl[[1]])
  post_chunk <- lbl[[2]]

  list(tbl_chunk, post_chunk)
}
