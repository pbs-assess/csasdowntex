#' Adds an extra header to the top of a [csas_table()]. Works for longtables.
#'
#' @param kable_input An R object, typically a matrix or data frame.
#' @param header a vector of character strings to use for the extra header names
#' @param bold See kableExtra:::pdfTable_add_header_above()
#' @param italic See kableExtra:::pdfTable_add_header_above()
#' @param monospace See kableExtra:::pdfTable_add_header_above()
#' @param underline See kableExtra:::pdfTable_add_header_above()
#' @param strikeout See kableExtra:::pdfTable_add_header_above()
#' @param align See kableExtra:::pdfTable_add_header_above()
#' @param color See kableExtra:::pdfTable_add_header_above()
#' @param background See kableExtra:::pdfTable_add_header_above()
#' @param font_size See kableExtra:::pdfTable_add_header_above()
#' @param angle See kableExtra:::pdfTable_add_header_above()
#' @param escape See kableExtra:::pdfTable_add_header_above()
#' @param line See kableExtra:::pdfTable_add_header_above()
#' @param line_sep See kableExtra:::pdfTable_add_header_above()
#'
#' @importFrom kableExtra magic_mirror
#' @return See kableExtra:::pdfTable_add_header_above()
add_extra_header <- function(kable_input,
                             header = NULL,
                             bold = FALSE,
                             italic = FALSE,
                             monospace = FALSE,
                             underline = FALSE,
                             strikeout = FALSE,
                             align = c("c", "l", "r"),
                             color = NULL,
                             background,
                             font_size,
                             angle,
                             escape,
                             line = TRUE,
                             line_sep = 3) {

  tryCatch({align <- match.arg(align)
  }, error = function(e){
    bail(csas_color("align"), " must be one of ",
         csas_color("c"), ", ", csas_color("l"), ", or ",
         csas_color("r"), ".\n",
         "You tried: ", csas_color(align))
  })

  table_info <- magic_mirror(kable_input)
  header <- standardize_header_input(header)
  if (length(table_info$colnames) != nrow(header)) {
    # nocov start
    bail("The number of extra headers supplied is not the same as the ",
         "number of columns in the table")
    # nocov end
  }
  if (escape) {
    header$header <- input_escape(header$header, align)
  }

  hline_type <- switch(table_info$booktabs + 1,
                       "\\\\hline",
                       "\\\\toprule"
  )
  new_header_split <-
    pdfTable_new_header_generator(header,
                                  table_info$booktabs,
                                  bold,
                                  italic,
                                  monospace,
                                  underline,
                                  strikeout,
                                  align,
                                  color,
                                  background,
                                  font_size,
                                  angle,
                                  line_sep,
                                  border_left = FALSE,
                                  border_right = FALSE
    )
  if (line) {
    new_header <- paste0(
      new_header_split[1], "\n",
      new_header_split[2]
    )
  } else {
    new_header <- new_header_split[1] # nocov
  }

  j <- utf8_inp <- solve_enc(kable_input)
  out <- stringr::str_replace_all(
    utf8_inp,
    hline_type,
    paste0(hline_type, "\n", new_header)
  )
  out <- structure(out,
                   format = "latex",
                   class = "knitr_kable"
  )

  if (is.null(table_info$new_header_row)) {
    table_info$new_header_row <- new_header_split[1]
    table_info$header_df <- list(header)
  } else {
    # nocov start
    table_info$new_header_row <- c(
      table_info$new_header_row,
      new_header_split[1])
    table_info$header_df[[length(table_info$header_df) + 1]] <- header
    # nocov end
  }
  attr(out, "kable_meta") <- table_info
  out
}
