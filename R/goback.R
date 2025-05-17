#' Navigate back to the directory you were in before calling [gotest()]
#'
#' @details
#' After running [gotest()], you will be in a temporary directory,
#' and you'll be able to build a test document with arbitrary
#' knitr code chunks. Once you're done testing and checking your output,
#' run this function to return to the real document directory, and
#' to reset the [here::here()] command so that it references the real
#' project again.
#'
#' Assumes you're going back to a directory in the hake repository and that
#' the file `.here` exists in the hake repository root directory.
#'
#' @return Nothing
goback <- function(){

  curr_dir <- getwd()
  if(length(grep("hake", curr_dir)) && !length(grep("tmp", curr_dir))){
    message("You appear to already be in the hake repository. You ",
            "must `gotest()` before trying to `goback()`")
    return(invisible())
  }

  if(!exists("goback_dr")){
    stop("The variable `goback_dr` does not exist. You need to set ",
         "this to the directory name you want to go back to, then ",
         "run goback() again. In future, make sure you enter a temporary ",
         "directory for testing using the `hake::gotest()` function, which ",
         "automatically sets `goback_dr` for you")
  }

  setwd(goback_dr)
  #set_here(".")
  i_am(".here")
  message("Now in directory ", getwd())
}