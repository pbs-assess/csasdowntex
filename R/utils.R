#' Convert a hex string into a vector of three decimal values (RGB)
#'
#' @keywords internal
#'
#' @param hex The hex string of 6 or 8 digits (if alpha included). May of may
#' not begin with  #
#' @param rel If `TRUE`, divide the RGB values by 255 for relative values
#' @param ret_alpha if `TRUE` alpha value will be included in the output
#' vector as the last item, so it will be length 4 instead of 3
#'
#' @return A vector of three RGB decimal values, or three relative values
hex2rgb <- function(hex, rel = FALSE, ret_alpha = FALSE){

  if(is.null(hex)){
    return(NULL)
  }
  if(is.na(hex)){
    return(NA_character_)
  }

  hex <- gsub("#", "", hex)
  if(nchar(hex) != 6 && nchar(hex) != 8){
    bail("hex must be a 6- or 8-digit number")
  }
  if(ret_alpha && nchar(hex) != 8){
    hex <- paste0(hex, "ff")
  }
  for(i in 1:nchar(hex)){
    if(!substr(hex, i, i) %in% c(0:9, letters[1:6], LETTERS[1:6])){
      bail("`hex` contains non-hexadecimal digits")
    }
  }
  if(ret_alpha){
    hex_vec <- substring(hex, seq(1, 7, 2), seq(2, 8, 2))
  }else{
    hex_vec <- substring(hex, seq(1, 5, 2), seq(2, 6, 2))
  }
  dec <- strtoi(hex_vec, 16)
  if(rel){
    dec <- dec / 255
  }
  dec
}

#' Supply the Rmarkdown newline code for a given number of newlines
#'
#' @keywords internal
#'
#' @param num_blank_lines A single value for the number of newlines,
#' or actual blank lines required
#'
#' @return A character vector containing the sequence of code necessary
#' to create the number of newlines required
rmd_nlines <- function(num_blank_lines){

  if(is.null(num_blank_lines)){
    bail("`num_blank_lines` must not be `NULL`")
  }
  if(num_blank_lines < 0){
    bail("`num_blank_lines` must be zero or greater")
  }
  if(num_blank_lines == 0){
    return("")
  }
  if(num_blank_lines == 1){
    #return(c("", "\\\\ \\\\", ""))
    return(c("\\n", ""))
  }
  if(num_blank_lines > 1){
    #return(c("", rep("\\\\", num_blank_lines - 1), ""))
    return(rep(c("\\n", ""), num_blank_lines - 1))
  }
}

#' Checks to see if character strings are Rmarkdown header lines
#'
#' @description
#' Checks to see if character strings are Rmarkdown header lines which are
#' text with at least one character, possibly containing whitespace. In other
#' words, typical paragraph or sentence text with punctuation.
#'
#' @details
#' The one thing that is not a text line is a header line (Starts with a #
#' followed by a space)
#'
#' @keywords internal
#'
#' @param lines The vector of character strings to check
#'
#' @return A logical vector representing whether or not the lines are
#' Rmarkdown header lines, which are normal text
is_rmd_text_line <- function(lines){
  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    bail("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n")
  }

  map_lgl(lines, ~{
    (!is_rmd_header_line(.x) &&
      grepl("^(\\s*\\S+\\s*)+$", trimws(.x)))
  })
}

#' Checks to see if character strings are dashed lines
#'
#' @description
#' Checks to see if character strings are dashed lines of any length,
#' including length 1
#'
#' @keywords internal
#'
#' @param lines The vector of character strings to check
#'
#' @return A logical vector representing whether or not the lines are
#' dashed lines
is_rmd_dashed_line <- function(lines){

  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    bail("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n")
  }

  map_lgl(lines, ~{
    grepl("^(\\s*-+\\s*)+$", trimws(.x))
  })

}

#' Checks to see if character strings are Rmarkdown header lines
#'
#' @param lines The vector of character strings to check
#'
#' @keywords internal
#'
#' @details
#' A header line must be indented less than 4 spaces and start with a #
#' followed by one or more spaces, and the n text
#'
#' @return A logical vector representing whether or not the lines are
#' Rmarkdown header lines
is_rmd_header_line <- function(lines){
  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    bail("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n")
  }

  map_lgl(lines, ~{
    leading_spaces <- nchar(gsub("^(\\s*)#+.*$", "\\1", .x))
    if(leading_spaces > 3){
      return(FALSE)
    }
    grepl("^#+\\s+.*$", trimws(.x))
  })
}

#' Checks to see if character strings are Rmarkdown list lines
#'
#' @keywords internal
#'
#' @param lines The vector of character strings to check
#'
#' @return A logical vector representing whether or not the lines are
#' Rmarkdown list lines
is_rmd_list_line <- function(lines){
  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    bail("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n")
  }
  map_lgl(lines, function(.x) {
    substr(trimws(.x), 2, 3) == ". " ||
      substr(trimws(.x), 1, 2) == "* " ||
      substr(trimws(.x), 1, 2) == "+ " ||
      substr(trimws(.x), 1, 2) == "- "
  })
}

#' Checks to see if character strings represent the start of a Rmarkdown
#' tables
#'
#' @keywords internal
#'
#' @param lines_lst A list of character strings vectors of at least length 5
#' for a type 1 table and 3 for a type 2 table
#'
#' @details
#' Three lines from the beginning of the table are required to determine if a
#' table is possibly valid
#'
#' @return A character vector representing which Rmarkdown table type each
#' element in `lines_lst` is
is_rmd_table_line <- function(lines_lst){

  if(is.null(lines_lst)){
    return(NULL)
  }
  if(!is.list(lines_lst)){
    lines_lst <- list(lines_lst)
  }
  if(any(is.na(lines_lst))){
    bail("An NA is present in the vector of strings:\n\n",
         paste(lines_lst, collapse = "\n"),
         "\n\n")
  }

  map_chr(lines_lst, ~{
    is_type_1 <- FALSE
    is_type_2 <- FALSE
    if(length(.x) >= 5){
      is_type_1 <- is_rmd_dashed_line(.x[1]) &&
                   is_rmd_text_line(.x[2]) &&
                   !is_rmd_header_line(.x[2]) &&
                   !is_rmd_list_line(.x[2]) &&
                   is_rmd_dashed_line(.x[3]) &&
                   is_rmd_text_line(.x[4]) &&
                   !is_rmd_header_line(.x[4]) &&
                   !is_rmd_list_line(.x[4])
    }
    if(length(.x) >= 3){
      is_type_2 <- is_rmd_text_line(.x[1]) &&
                   !is_rmd_header_line(.x[1]) &&
                   !is_rmd_list_line(.x[1]) &&
                   is_rmd_dashed_line(.x[2]) &&
                   is_rmd_text_line(.x[3]) &&
                   !is_rmd_header_line(.x[3]) &&
                   !is_rmd_list_line(.x[3])
    }
    if(is_type_1){
      "type1"
    }else if(is_type_2){
      "type2"
    }else{
      "false"
    }
  })
}

#' Detect which columns are year columns based on the range and type
#'
#' @keywords internal
#'
#' @param df A data frame with column names
#' @param year_range The range to use for year column acceptance. All values
#' in the column must be in this range
#'
#' @return A vector of column names, or NULL if no year columns were found
year_cols <- function(df, year_range = 1800:4000){

  if("matrix" %in% class(df)){
    df <- df |>
      as.data.frame()
  }

  col_is_year <- map2(df, names(df), ~{
    if(is.numeric(.x)){
      # Check that all values are in the year range and that they are integers
      # even if the type has not been set to integer, i.e. `is.integer(.x)` is FALSE
      # but `is.numeric(.x)` is TRUE.
      if(all(.x %in% year_range) && all(sapply(.x, `%%`, 1) == 0)){
        .y
      }
    }
  })
  # Remove all NULLs from the list and make the list a character vector
  col_is_year[sapply(col_is_year, is.null)] <- NULL
  col_is_year <- map_chr(col_is_year, ~{.x})
  if(!length(col_is_year)){
    return(NULL)
  }
  # Remove names because testing is easier to code, and they are the same
  # as the values anyway
  names(col_is_year) <- NULL
  col_is_year
}

#' Return value of the `french` Option in the current environment
#'
#' @description
#' Used to retrieve `TRUE` or `FALSE` for whether
#' or not to render the document in French
#'
#' @return The `french` option value. If the `french` option is `NULL`,
#' `FALSE` will be returned
#' @export
fr <- function(){

  getOption("french", default = FALSE)
}

#' Translate from English to French or vice-versa
#'
#' @details
#' If `fr()` is `TRUE` and `x` is English (is in the translation database)
#' then it will be translated to French
#' If `fr()` is `TRUE` and `x` is French (is not in the translation database)
#' then it will not be translated and will be returned as it was received
#'
#' If `fr()` is `FALSE` and `x` is French (is in the translation database)
#' then it will be translated to English
#' If `fr()` is `TRUE` and `x` is English (is not in the translation database)
#' then it will not be translated and will be returned as it was received
#'
#' Reasoning for this function: This was needed for switching back and forth
#' from building an English document and a French one. Some things such as
#' Gear names can simply be wrapped in `tr()` and will be translated properly
#' no matter the document language. This greatly simplifies code.
#'
#' CAVEAT: If you attempt to translate a word and it is legitimately not
#' found in the translation database, this function will simply return
#' the word you supplied, be it French or English, without warning. It is up
#' to you to test that the words are found in the translation database for
#' both English and French.
#'
#' @param vector_of_terms A vector of terms to translate based on the
#' return value of `fr()`
#'
#' @return The possibly translated character vector (see **Details** above)
#' @importFrom rosettafish en2fr fr2en trans
#' @export
tr <- function(vector_of_terms){

  vector_of_terms |>
    map_chr(\(term){
      tryCatch({
        return(trans(term,
                     from = ifelse(fr(),
                                   "english",
                                   "french"),
                     to =  ifelse(fr(),
                                  "french",
                                  "english")))
      }, error = \(e){
        # If the term is not in the database, we assume it is already in the
        # target language, so we just return it as-is
        return(term)
      })
    })
}

# nocov start
#' Add a Res Doc titlepage to a docx file
#'
#' Add a Res Doc titlepage. Must hand edit `templates/RES2024-eng-titlepage.docx`
#' to have your desired title and authors etc.
#'
#' @param titlepage Filename
#' @param resdoc Filename
#'
#' @return A merged .docx
#' @importFrom officer read_docx body_add_docx cursor_reach body_add_toc
#' @export
add_resdoc_docx_titlepage <- function(titlepage = "templates/RES2024-eng-titlepage.docx",
                                      resdoc = "_book/resdoc-english.docx") {
  title_doc <- read_docx(titlepage)
  x <- body_add_docx(title_doc, resdoc, pos = "before")
  print(x, target = resdoc)
}

#' Add front matter to Res Doc docx file
#'
#' Add title page and table of contents to a Res Doc. Must hand edit
#' `templates/RES2024-eng-frontmatter.docx`to have your desired title and authors etc.
#'
#' @param frontmatter  Path to title page file included with resdoc template
#' @param resdoc       Path to content generated using resdoc_word
#'
#' @return A merged .docx
#' @export
add_resdoc_docx_frontmatter <- function(frontmatter = "templates/RES2024-eng-frontmatter.docx",
                                        resdoc = "_book/resdoc-english.docx") {
  frontmatter_doc <- read_docx(frontmatter)
  x <- body_add_docx(frontmatter_doc, resdoc, pos = "before")
  x <- cursor_reach(x, keyword = "TABLE OF CONTENTS")
  x <- body_add_toc(x)
  print(x, target = resdoc)
}

#' Add a titlepage to a Tech report docx file
#'
#' Must hand edit the first two pages of your file afterwards to have your desired title and authors.
#'
#' @param titlepage Filename
#' @param doc Filename
#'
#' @return A merged .docx
#' @export
add_techreport_docx_titlepage <- function(titlepage = ifelse(fr(),
                                                             "templates/tech-report-cover-fra.docx",
                                                             "templates/tech-report-cover-eng.docx"),
                                          doc = "_book/techreport.docx") {
  title_doc <- read_docx(titlepage)
  x <- body_add_docx(title_doc, doc, pos = "before")
  print(x, target = doc)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Add a titlepage to a Manu report docx file
#'
#' Must hand edit the first two pages of your file afterwards to have your desired title and authors.
#'
#' @param titlepage Filename
#' @param doc Filename
#'
#' @return A merged .docx
#' @export
add_manureport_docx_titlepage <- function(titlepage = ifelse(fr(),
                                                             "templates/manu-report-cover-fra.docx",
                                                             "templates/manu-report-cover-eng.docx"),
                                          doc = "_book/manureport.docx") {
  title_doc <- read_docx(titlepage)
  x <- body_add_docx(title_doc, doc, pos = "before")
  print(x, target = doc)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Redefinition of [cat()] with separator set to empty string
#'
#' @keywords internal
#'
#' @param ... `R` objects (see `Details` for the types of objects allowed)
#' @param file A connection, or a character string naming the file to print
#' to. If "" (the default), cat prints to the standard output connection
#' the console unless redirected by sink
#' @param sep A character vector of strings to append after each element.
#' @param fill A logical or (positive) numeric controlling how the output is
#' broken into successive lines. If `FALSE` (default), only newlines created
#' explicitly by ‘⁠\\n"⁠’ are printed. Otherwise, the output is broken in
#' to lines with print width equal to the option width if fill is `TRUE`,
#' or the value of `fill` if this is numeric. Linefeeds are only inserted
#' between elements, strings wider than `fill` are not wrapped. Non-positive
#' `fill` values are ignored, with a warning
#' @param labels A character vector of labels for the lines printed. Ignored
#' if `fill` is `FALSE`
#' @param append Logical. Only used if the argument file is the name of file
#' (and not a connection or "`|cmd`"). If `TRUE` output will be appended to file;
#' otherwise, it will overwrite the contents of file.
#'
#' @inherit base::cat details note references seealso examples
cat <- function(...,
                file = "",
                sep = " ",
                fill = FALSE,
                labels = NULL,
                append = FALSE){

  base::cat(unlist(list(...)),
            file = file,
            sep = "",
            fill = fill,
            labels = labels,
            append = append)
}

#' Capture multiple log messages from a function call
#'
#' @details
#' Capture all messages from a function.
#' Use like this:
#' `` x <- capture_log(function_call(args)) ``
#' `` j <- x(1) ``
#' `` j <- j$logs ``
#' `` messages <- map_chr(j, ~{.x$message}) ``
#'
#' @keywords internal
#' @param f The function call
#'
#' @return A function which you call like this (if you name sit `x`): `x(1)`
#' to extract the messages
capture_log <- function(f) {

  function(...) {
    logs <- list()
    add_log <- function(type, message) {
      new_l <- logs
      new_log <- list(timestamp = format(Sys.time(),
                                         tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'),
                      type = type,
                      message = message)
      new_l[[length(new_l) + 1]]  <- new_log
      logs <<- new_l
    }
    res <- withCallingHandlers(
      tryCatch(f(...), error = function(e) {
        add_log("error", conditionMessage(e))
        NULL
      }), warning = function(w) {
        add_log("warning", conditionMessage(w))
        invokeRestart("muffleWarning")
      }, message = function(m) {
        add_log("message", conditionMessage(m))
        invokeRestart("muffleMessage")
      })
    list(res, logs = logs)
  }
}
# nocov end

#' Create functions for coloring various keyword families
#'
#' @param message The message to color
#' @param r_color The R color string which will be accepted by
#' [crayon::make_style()]
#'
#' @importFrom crayon make_style combine_styles
#' @return A function to custom color text
color_factory <- function(message, r_color){
# nocov start
  function(message){
    # How to use both background and foreground (needs arguments)
    # bg <- make_style("snow", bg = TRUE)
    # fg <- make_style("red4")
    # x <- combine_styles(fg, bg)
    x <- make_style(r_color)
    x(message)
  }
# nocov end
}

#' Overall error color throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
error_color <- color_factory(message, "orangered3")

#' Overall warning color throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
warning_color <- color_factory(message, "yellow")

#' Overall message color throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
message_color <- color_factory(message, "dodgerblue3")

#' Overall question color throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
question_color <- color_factory(message, "firebrick1")

#' Overall message color throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
completed_message_color <- color_factory(message, "green")

#' Filename color for messages throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
fn_color <- color_factory(message, "skyblue")

#' YAML tag color for messages throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
tag_color <- color_factory(message, "limegreen")

#' csasdown items (render type, format, chunk names, etc) color
#' for messages throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
csas_color <- color_factory(message, "yellowgreen")

#' modified tag color for messages throughout the project
#'
#' @rdname color_factory
#' @return The customized message modified by the [crayon::make_style()]
#' color function
mod_color <- color_factory(message, "orangered")

#' Abort code execution
#'
#' @details
#' Uses [cli::symbol] and [crayon::red()]
#'
#' @keywords internal
#'
#' @param ... Arguments that make up the message
#' @importFrom cli symbol
bail <- function(...){
  msg <- paste0(unlist(list(...)), collapse = "")
  stop(error_color(paste(symbol$cross, msg)), call. = FALSE)
}

#' Issue a warning
#'
#' @details
#' Uses [cli::symbol] and [crayon::yellow()]
#'
#' @keywords internal
#'
#' @param ... Arguments that make up the message
alert <- function(...){
  msg <- paste0(unlist(list(...)), collapse = "")
  warning(warning_color(paste(symbol$warning, msg)),
          call. = FALSE)
}

#' Issue a notification
#'
#' @details
#' Uses [cli::symbol] and [crayon::green()]
#'
#' @keywords internal
#'
#' @param ... Arguments that make up the message
notify <- function(...){
  msg <- paste0(unlist(list(...)), collapse = "")
  message(message_color(paste(symbol$info, msg)))
}

#' Issue a check-mark notification
#'
#' @details
#' Uses [cli::symbol] and [crayon::green()]
#'
#' @keywords internal
#'
#' @param ... Arguments that make up the message
check_notify <- function(...){
  msg <- paste0(unlist(list(...)), collapse = "")
  message(completed_message_color(paste(symbol$tick, msg)))
}

# nocov start
#' Issue a question notification
#'
#' @details
#' Uses [cli::symbol] and [crayon::green()]
#'
#' @keywords internal
#'
#' @param ... Arguments that make up the message
question <- function(...){
  msg <- paste0(unlist(list(...)), collapse = "")
  message(question_color(paste(symbol$fancy_question_mark, msg)))
}
# nocov end

