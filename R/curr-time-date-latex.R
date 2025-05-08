#' Create a time and date string for the title page of the doc
#'
#' @keywords internal
#' @param testday A test string in the format e.g. "May 02" or "July 1"
#'
#' @return A string with the nice date and time in LaTeX form
curr_time_date <- function(testday = NULL){

  if(is.null(testday)){
    day <- format(Sys.time(), '%B %d')
  }else{
    day <- testday
  }

  last_char <- substr(day, nchar(day), nchar(day))
  penult_char <- substr(day, nchar(day) - 1, nchar(day) - 1)
  if(penult_char == "0"){
    day <- paste0(substr(day, 1, nchar(day) - 2),
                  last_char)
    penult_char <- substr(day, nchar(day) - 1, nchar(day) - 1)
  }

  if(last_char == "1" &&
     penult_char %in% c(" ", "2", "3")){
    day <- paste0(day, "\\\\textsuperscript{st}")
  }else if(last_char == "2" &&
           penult_char %in% c(" ", "2")){
    day <- paste0(day, "\\\\textsuperscript{nd}")
  }else if(last_char == "3" &&
           penult_char %in% c(" ", "2")){
    day <- paste0(day, "\\\\textsuperscript{rd}")
  }else{
    day <- paste0(day, "\\\\textsuperscript{th}")
  }

  year <- format(Sys.time(), '%Y')

  paste0(day, ", ", year)

}