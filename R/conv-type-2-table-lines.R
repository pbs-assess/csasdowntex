#' Convert an Rmarkdown type 2 table into a simplified form with WYSIWYG
#' newlines
#'
#' @description
#' Convert an Rmarkdown type 2 table into a simplified form with WYSIWYG
#' newlines. If there is no table detected, NULL will be returned as the
#' converted chunk, or an error will be thrown depending on the situation.
#'
#' @details
#' A type 2 table is defined as a manually-entered Rmarkdown table with a
#' minimum of three lines with this format:
#'
#' ```
#'   a     b     c
#' ----- ----- -----
#'  abc   def   ghi
#' ```
#'
#' There is no whitespace in between any of these lines. The first text row is
#' the column headers for the table and the second text row is the table data.
#' The second row can be multiple rows which must not be separated by blank
#' lines. They all must be together without whitespace in between.
#' Here is an example with three table data rows in acceptable format, along
#' with the optional table caption text which must start with 'Table:'
#'
#' ```
#'   a     b     c
#' ----- ----- -----
#'  abc   def   ghi
#'  jkl   mno   pqr
#'  stu   vwx   yza
#'
#' Table: Table caption (0 or more blank lines between table and this caption)
#' A second line of table caption here (no blank lines in between)
#' ```
#'
#' @keywords internal
#'
#' @family rmd_conversion_functions
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, which are the modified type 1 table and the
#' rest of the chunk starting with the line after the end of the table
#'
#' @examples
#' \dontrun{
#' library(csasdowntex)
#' chunk <- c("  Parameter   Value", "---------- -----------",
#'            "     x          1.0", "     y          2.2")
#' tmp <- conv_type_2_table_lines(chunk)
#' the_rest <- tmp[[2]]
#' }
conv_type_2_table_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(length(chunk) < 3){
    bail("A type 2 table must have at least 3 lines. Input table is:\n\n",
         csas_color(paste(chunk, collapse = "\n")),
         "\n\n")
  }

  # ---------------------------------------------------------------------------
  # `dash_pat` matches any sequence of zero or more whitespace characters,
  #  followed by 1 or more dashes, followed by zero or more whitespace
  #  characters
  if(is_rmd_table_line(chunk) != "type2"){
    bail("The following table is not a type 2 table based on the first three ",
         "rows:\n\n",
         csas_color(paste(chunk, collapse = "\n")),
         "\n\n",
         "They must start with:\n",
         "- a row of text representing headers\n",
         "- a row of dashes\n",
         "- a row of text representing a row of table data.")
  }

  start_tbl_ind <- 1
  end_tbl_ind <- NULL
  start_lbl_ind <- NULL
  end_lbl_ind <- NULL
  # Add the first three rows as they have been checked already
  tbl_chunk <- chunk[1:3]
  if(length(chunk) == 3){
    tbl_chunk <- c(chunk, "")
    return(list(tbl_chunk, NULL))
  }
  i <- 4
  repeat{
    tn <- trimws(chunk[i])
    if(chunk[i] == "" ||
       is_rmd_list_line(chunk[i]) ||
       is_rmd_header_line(chunk[i]) ||
       grepl("^Table:.*$", tn)){
      end_tbl_ind <- i - 1
      break
    }
    tbl_chunk <- c(tbl_chunk, chunk[i])
    if(i == length(chunk)){
      tbl_chunk <- c(tbl_chunk, "")
      return(list(tbl_chunk, NULL))
    }
    i <- i + 1
  }
  # Add label if it exists
  lbl <- extract_rmd_table_label(chunk[(end_tbl_ind + 1):length(chunk)])
  if(!is.null(lbl[[1]])){
    # put a space between the table contents and the caption label
    tbl_chunk <- c(tbl_chunk, "")
  }
  # if(!is.null(lbl[[1]]) && lbl[[1]][1] != ""){
  #   lbl[[1]] <- c("", lbl[[1]])
  # }
  tbl_chunk <- c(tbl_chunk, lbl[[1]])
  post_chunk <- lbl[[2]]

  list(tbl_chunk, post_chunk)
}
