# nocov start

#' input_escape is a [kableExtra] non-exported function
#'
#' @description
#' Included to pass R CMD check cleanly
#' @keywords internal
#' @param x x
#' @param latex_align latex_align
#' @references \url{https://github.com/haozhu233/kableExtra}
input_escape <- function (x, latex_align){

  x <- escape_latex2(x)
  x <- linebreak(x, align = latex_align, double_escape = TRUE)
}

#' escape_latex2 is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param x x
#' @references \url{https://github.com/haozhu233/kableExtra}
escape_latex2 <- function (x){

  x = gsub("\\\\", "\\\\\\\\textbackslash", x)
  x = gsub("([#$%&_{}])", "\\\\\\\\\\1", x)
  x = gsub("\\\\textbackslash", "\\\\\\\\textbackslash{}",
           x)
  x = gsub("~", "\\\\\\\\textasciitilde{}", x)
  x = gsub("\\^", "\\\\\\\\textasciicircum{}", x)
  x
}

#' pdfTable_new_header_generator is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param header_df header_df
#' @param booktabs booktabs
#' @param bold bold
#' @param italic italic
#' @param monospace monospace
#' @param underline underline
#' @param strikeout strikeout
#' @param align align
#' @param color color
#' @param background background
#' @param font_size font_size
#' @param angle angle
#' @param line_sep line_sep
#' @param border_left border_left
#' @param border_right border_right
#' @references \url{https://github.com/haozhu233/kableExtra}
pdfTable_new_header_generator <- function (header_df,
                                           booktabs = FALSE,
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
                                           border_left,
                                           border_right) {

  n <- nrow(header_df)
  bold <- ez_rep(bold, n)
  italic <- ez_rep(italic, n)
  monospace <- ez_rep(monospace, n)
  underline <- ez_rep(underline, n)
  strikeout <- ez_rep(strikeout, n)
  align <- ez_rep(align, n)
  color <- ez_rep(color, n)
  background <- ez_rep(background, n)
  font_size <- ez_rep(font_size, n)
  angle <- ez_rep(angle, n)
  if (!booktabs & n != 1) {
    align[1:(n - 1)] <- paste0(align[1:(n - 1)], "|")
  }
  if (border_left) {
    align[1] <- paste0("|", align[1])
  }
  if (border_right) {
    align[n] <- paste0(align[n], "|")
  }
  header <- header_df$header
  colspan <- header_df$colspan
  header <- ifelse(bold, paste0("\\\\textbf\\{", header, "\\}"),
                   header)
  header <- ifelse(italic, paste0("\\\\em\\{", header, "\\}"),
                   header)
  header <- ifelse(monospace, paste0("\\\\ttfamily\\{", header,
                                     "\\}"), header)
  header <- ifelse(underline, paste0("\\\\underline\\{", header,
                                     "\\}"), header)
  header <- ifelse(strikeout, paste0("\\\\sout\\{", header,
                                     "\\}"), header)
  if (!is.null(color)) {
    color <- latex_color(color)
    header <- paste0("\\\\textcolor", color, "\\{", header,
                     "\\}")
  }
  if (!is.null(background)) {
    background <- latex_color(background)
    header <- paste0("\\\\cellcolor", background, "\\{",
                     header, "\\}")
  }
  if (!is.null(font_size)) {
    header <- paste0("\\\\bgroup\\\\fontsize\\{", font_size,
                     "\\}\\{", as.numeric(font_size) + 2, "\\}\\\\selectfont ",
                     header, "\\\\egroup\\{\\}")
  }
  if (!is.null(angle)) {
    header <- paste0("\\\\rotatebox\\{", angle, "\\}\\{",
                     header, "\\}")
  }
  header_items <- paste0("\\\\multicolumn\\{", colspan, "\\}\\{",
                         align, "\\}\\{", header, "\\}")
  header_text <- paste(paste(header_items, collapse = " & "),
                       "\\\\\\\\")
  cline <- cline_gen(header_df, booktabs, line_sep)
  return(c(header_text, cline))
}

#' ez_rep is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param x x
#' @param n n
#' @references \url{https://github.com/haozhu233/kableExtra}
ez_rep <- function (x, n) {

  if (is.null(x))
    return(NULL)
  if (length(x) == 1)
    return(rep(x, n))
  return(x)
}

#' latex_color is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param colors colors
#' @param escape escape
#' @references \url{https://github.com/haozhu233/kableExtra}
latex_color <- function (colors, escape = TRUE) {

  colors <- as.character(colors)
  if (escape) {
    return(sapply(colors, latex_color_))
  }
  else {
    return(sapply(colors, latex_color__))
  }
}

#' latex_color_ is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param color color
#' @references \url{https://github.com/haozhu233/kableExtra}
latex_color_ <- function (color) {

  if (substr(color, 1, 1) != "#") {
    return(paste0("\\{", color, "\\}"))
  }
  else {
    color <- sub("#", "", color)
    if (nchar(color) == 8)
      color <- substr(color, 1, 6)
    return(paste0("\\[HTML\\]\\{", color, "\\}"))
  }
}

#' latex_color__ is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param color color
#' @references \url{https://github.com/haozhu233/kableExtra}
latex_color__ <- function (color) {

  if (substr(color, 1, 1) != "#") {
    return(paste0("{", color, "}"))
  }
  else {
    color <- sub("#", "", color)
    if (nchar(color) == 8)
      color <- substr(color, 1, 6)
    return(paste0("[HTML]{", color, "}"))
  }
}
#' solve_enc s a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param x x
#' @references \url{https://github.com/haozhu233/kableExtra}
solve_enc <- function (x){

  if (Encoding(x) == "UTF-8") {
    out <- x
  }
  else {
    out <- enc2utf8(as.character(base::format(x, trim = TRUE,
                                              justify = "none")))
  }
  mostattributes(out) <- attributes(x)
  return(out)
}

#' standardize_header_input is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param header header
#' @references \url{https://github.com/haozhu233/kableExtra}
standardize_header_input <- function (header) {

  header_names <- names(header)
  if (is.null(header_names)) {
    return(data.frame(header = header, colspan = 1, row.names = NULL))
  }
  names(header)[header_names == ""] <- header[header_names == ""]
  header[header_names == ""] <- 1
  header_names <- names(header)
  header <- as.numeric(header)
  names(header) <- header_names
  return(data.frame(header = names(header), colspan = header,
                    row.names = NULL, stringsAsFactors = F))
}

#' standardize_header_input is a [kableExtra] non-exported function
#'
#' @keywords internal
#' @description
#' Included to pass R CMD check cleanly
#' @param header_df header_df
#' @param booktabs booktabs
#' @param line_sep line_sep
#' @importFrom glue glue
#' @references \url{https://github.com/haozhu233/kableExtra}
cline_gen <- function (header_df, booktabs, line_sep) {

  cline_end <- cumsum(header_df$colspan)
  cline_start <- c(0, cline_end) + 1
  cline_start <- cline_start[-length(cline_start)]
  cline_type <- switch(booktabs + 1, "\\\\cline{",
                       glue("\\\\cmidrule(l{[line_sep]pt}r{[line_sep]pt}){",
                            .open = "[", .close = "]"))
  cline <- paste0(cline_type, cline_start, "-", cline_end,
                  "}")
  cline <- cline[trimws(header_df$header) != ""]
  cline <- paste(cline, collapse = " ")
  return(cline)
}

# nocov end
