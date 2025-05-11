#' Inject the `tagpdf` code around the figure chunk given by the knitr
#' chunk `label`
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param label The label used in the knitr chunk
#' @param alt_text The alternative text to inject
#'
#' @return The modified Tex code, as a vector
#' @export
alt_text_inject_tags <- function(x, label, alt_text){

  caption_ind <- grep(paste0("\\\\label\\{fig:", label, "\\}"), x)
  if(!length(caption_ind)){
    stop("Could not find \\label{fig:", label, "} in the LaTeX file. Make ",
         "Sure you have the caption set up properly for this figure")
  }
  if(length(caption_ind) > 1){
    stop("There was more than one \\label{fig:", label, "} in the LaTeX ",
         "file. Make sure you have the caption set up properly for this ",
         "figure")
  }
  includegraphics_ind <- caption_ind
  repeat{
    if(length(grep("includegraphics", x[includegraphics_ind]))){
      break
    }
    includegraphics_ind <- includegraphics_ind - 1
  }

  pre <- x[1:(includegraphics_ind - 1)]
  post <- x[(caption_ind + 1):length(x)]
  chunk <- x[includegraphics_ind:(caption_ind - 1)]

  ret <- c(pre,
           "%",
           "% The following code was injected by",
           "% csasdown::alt_text_inject_tags()",
           "%",
           paste0("\\tagstructbegin{tag=Figure,alttext=",
                  alt_text,
                  "}"),
           "\\tagmcbegin{tag=Figure}",
           chunk,
           "\\tagmcend",
           "\\tagstructend",
           "\\tagpdfparaOn",
           "\\tagstructbegin{tag=Caption}",
           "\\tagmcbegin{tag=Caption}",
           x[caption_ind],
           "\\tagmcend",
           "\\tagstructend",
           "%",
           "% End of injected code",
           "%",
           post)

  ret
}
