#' Search chunk to find an Rmarkdown table label
#'
#' @description
#' Search chunk to find an Rmarkdown table label. There are two possible
#' starting lines to the table caption
#'
#' @details
#' The two caption possibilities are of the forms:
#' 1. Table: Caption text
#'    Optional text..
#'    More optional text..
#'    ...
#' 2. Table: -Any amount of whitespace-
#'    Caption text..
#'    Optional text..
#'    More optional text..
#'    ...
#'
#' @keywords internal
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of length two. The elements are:
#' 1. A vector representing the lines of the label chunk or `NULL` if none found
#' 2. A vector representing the remainder of the chunk after the label or
#'    `NULL` if the label reached to the end of the chunk
extract_rmd_table_label <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  # Find start of label if it exists and if table has a caption label
  # (Table: Caption here)
  has_lbl <- FALSE
  start_lbl_ind <- NULL
  # `lbl_def_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more dashes, followed by zero or more whitespace
  # characters, preceded by "Table:" and stands for 'Label defined'
  # `lbl_undef_pat` matches any sequence of zero or more whitespace
  # characters, preceded by "Table:" and stands for 'Label undefined'
  lbl_def_pat <- "^Table:(\\s*\\S+\\s*)+$"
  lbl_undef_pat <- "^Table:\\s*$"
  # Skip blank lines looking for the table label caption
  i <- 1
  while(chunk[i] == "" && i < length(chunk)){
    i <- i + 1
  }
  # If the first text line found does not match either table label, return now
  line_mtch_lbl_style_1 <- length(grep(lbl_def_pat, trimws(chunk[i])))
  line_mtch_lbl_style_2 <- length(grep(lbl_undef_pat, trimws(chunk[i])))
  if(!line_mtch_lbl_style_1 && !line_mtch_lbl_style_2){
    return(list(NULL, chunk))
  }

  if(i == length(chunk)){
    # Whole chunk just had one line of text at the end that cannot match the
    # second type of label because it requires two lines minimum
    n_lead_spaces <- nchar(gsub("^(\\s*).*$", "\\1", chunk[i]))
    if(n_lead_spaces > 3){
      # Rmarkdown specs say a table caption line must be indented 3 or less
      # spaces. If more, it is just a regular text line
      alert("A line that looks like a table caption was found but it is ",
            "indented ", n_lead_spaces, " spaces. The Rmarkdown ",
            "specification says it must be 3 or less:\n\n",
            csas_color(chunk[i]),
            "\n\n")
      return(list(NULL, chunk))
    }
    return(list(chunk[i], NULL))
  }
  start_lbl_ind <- i
  # If here, the chunk is not all whitespace, `start_lbl_ind` is at a position
  # on a text line that is not the end of the chunk and is the first
  # non-whitespace line after any leading whitespace

  # Check to see if the line is the first type of label and process
  line_mtch_lbl_style_1 <- length(grep(lbl_def_pat,
                                       trimws(chunk[start_lbl_ind])))
  if(line_mtch_lbl_style_1){
    n_lead_spaces <- nchar(gsub("^(\\s*).*$", "\\1", chunk[i]))
    if(n_lead_spaces > 3){
      # Rmarkdown specs say a table caption line must be indented 3 or less
      # spaces. If more, it is just a regular text line
      alert("A line that looks like a table caption was found but it is ",
            "indented ", n_lead_spaces, " spaces. The Rmarkdown ",
            "specification says it must be 3 or less:\n\n",
            csas_color(chunk[i]),
            "\n\n")
      return(list(NULL, chunk))
    }
    text_follows <- FALSE
    repeat{
      # While there are successive lines of text, make them part of the label
      end_lbl_ind <- i
      if(i == length(chunk)){
        break
      }
      i <- i + 1
      if(chunk[i] == "" ||
         is_rmd_list_line(chunk[i]) ||
         is_rmd_header_line(chunk[i])){
        if(chunk[i] != ""){
          text_follows <- TRUE
        }
        break
      }
      if(i + 2 <= length(chunk) &&
         is_rmd_table_line(chunk[i:length(chunk)]) != "false"){
        if(chunk[i] != ""){
          text_follows <- TRUE
        }
        break
      }
    }
    if(end_lbl_ind == length(chunk)){
      return(list(chunk[start_lbl_ind:end_lbl_ind], NULL))
    }
    lbl_chunk <- chunk[start_lbl_ind:end_lbl_ind]
    if(text_follows){
      lbl_chunk <- c(lbl_chunk, "")
    }
    return(list(lbl_chunk,
                chunk[(end_lbl_ind + 1):length(chunk)]))
  }

  # Check to see if the line is the first type of label and process
  line_mtch_lbl_style_2 <- length(grep(lbl_undef_pat ,
                                       trimws(chunk[start_lbl_ind])))
  i <- start_lbl_ind
  if(line_mtch_lbl_style_2){
    n_lead_spaces <- nchar(gsub("^(\\s*).*$", "\\1", chunk[i]))
    if(n_lead_spaces > 3){
      # Rmarkdown specs say a table caption line must be indented 3 or less
      # spaces. If more, it is just a regular text line
      alert("A line that looks like a table caption was found but it is ",
            "indented ", n_lead_spaces, " spaces. The Rmarkdown ",
            "specification says it must be 3 or less:\n\n",
            csas_color(chunk[i]),
            "\n\n")
      return(list(NULL, chunk))
    }
    # The following line MUST be text or there is no label
    i <- i + 1
    if(chunk[i] == ""){
      alert("Blank ", tag_color("Table:"), " tag for table, this will ",
            "not produce a label.\nYou must not have blank lines between ",
            "an empty ",
            tag_color("Table:"), " tag and the caption that goes with it:\n\n",
            csas_color(paste(chunk, collapse = "\n")))
      return(list(NULL, chunk))
    }
    text_follows <- FALSE
    repeat{
      # While there are successive lines of text, make them part of the label
      end_lbl_ind <- i
      if(i == length(chunk)){
        break
      }
      i <- i + 1
      if(chunk[i] == "" ||
         is_rmd_list_line(chunk[i]) ||
         is_rmd_header_line(chunk[i])){
        if(chunk[i] != ""){
          text_follows <- TRUE
        }
        break
      }
      if(i + 2 < length(chunk) &&
         is_rmd_table_line(chunk[i:length(chunk)]) != "false"){
        if(chunk[i] != ""){ #nocov start
          text_follows <- TRUE
        }
        break #nocov end
      }
    }
    if(end_lbl_ind == length(chunk)){
      return(list(chunk[start_lbl_ind:end_lbl_ind], NULL))
    }
    lbl_chunk <- chunk[start_lbl_ind:end_lbl_ind]
    if(text_follows){
      lbl_chunk <- c(lbl_chunk, "")
    }
    return(list(lbl_chunk,
                chunk[(end_lbl_ind + 1):length(chunk)]))
  }
}