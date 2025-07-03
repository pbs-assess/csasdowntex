#' Check to make sure `index.Rmd` contains all current YAML options
#'
#' @description
#' As the csasdown package is updated, sometimes new mandatory YAML options are added
#' to the `index.Rmd` file. Running this function will compare your file to the
#' version built into the currently installed version of csasdown and issue
#' am error message telling you what doesn't match if needed.
#'
#' @param index_fn Filename foir the index RMD file
#' @param type Type of document
#' @param tag_exceptions A vector of YAML tag names that if missing, will not
#' cause an error
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @importFrom rmarkdown yaml_front_matter
#' @export
check_yaml <- function(index_fn = "index.Rmd",
                       type = c("resdoc", "resdoc_pdf", "resdoc_word",
                                "sr", "sr_pdf", "sr_word",
                                "manureport", "manureport_pdf",
                                "manureport_word",
                                "techreport", "techreport_pdf",
                                "techreport_word", "fsar_word"),
                       tag_exceptions = c("show_continued_text"),
                       verbose = FALSE) {

  if(!file.exists(index_fn)){
    bail("The file `", index_fn, "` does not exist")
  }

  tryCatch({type <- match.arg(type)
  }, error = function(e){
    bail(csas_color("type"), " must be one of ",
         csas_color("resdoc"), ", ", csas_color("resdoc_pdf"), ", ",
         csas_color("resdoc_word"), ", ", csas_color("sr"), ", ",
         csas_color("sr_pdf"), ", ", csas_color("sr_word"), ", ",
         csas_color("manureport"), ", ", csas_color("manureport_pdf"),
         csas_color("manureport_word"), ", ",
         csas_color("techreport"), ", ", csas_color("techreport_pdf"),
         ", ", csas_color("techreport_pdf"),
         ", or ", csas_color("techreport_word"), ".\n",
         "You tried: ", csas_color(type))
  })

  if(verbose){
    notify("Checking that YAML options are all present for document type ",
           csas_color(type), " ...")
  }

  if(type %in% c("resdoc", "resdoc_pdf", "resdoc_word")){
    type <- "resdoc"
  }else if(type %in% c("sr", "sr_pdf", "sr_word")){
    type <- "sr"
  }else if(type %in% c("techreport", "techreport_pdf", "techreport_word")){
    type <- "techreport"
  }else if(type %in% c("fsar_word")) {
    type <- "fsar"
  }else if(type %in% c("manureport", "manureport_pdf", "manureport_word")){
    type <- "manureport"
  }

  x_skeleton <- names(yaml_front_matter(
    system.file("rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
                package = "csasdown"
    )
  ))
  x_index <- names(yaml_front_matter(index_fn))
  .diff <- setdiff(x_skeleton, x_index)
  .diff <- .diff[!.diff %in% tag_exceptions]
  if (length(.diff) > 0L) {
    bail("Your ", fn_color(index_fn), " file is missing the YAML ",
         "tag(s):\n\n", tag_color(paste(.diff, collapse = "\n")))
  } else {
    if(verbose){
      check_notify("Your ", fn_color(index_fn), " file contains all ",
                   "necessary YAML options\n")
    }
  }
}

