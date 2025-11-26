#' Create a new CSAS document based on a template
#'
#' Create a draft of an R Markdown CSAS document
#'
#' @param type The type of document to draft. Must be one of `resdoc`,
#' `resdoc-b`, `sr`, `techreport`, `manureport`, or `fsar`
#' @param directory The directory to place the draft document files.
#' Is the current directory by default
#' @param fn Bookdown starting file, `index.Rmd` by default
#' @param edit Logical. If `TRUE`, edit the template file immediately
#' (See `edit` in [rmarkdown::draft()])
#' @param create_rstudio_file Logical. Create RStudio project file? Set to
#' `FALSE` if in a subdirectory or a folder that already has an RStudio file.
#' @param verbose Logical. If `TRUE`, print messages
#' @param testing Logical. If `TRUE`, the question about overwriting
#' files which already exist will skipped to ensure unit tests work
#' correctly
#' @param testing_affirm_ovr Logical. If `TRUE` and `testing` is `TRUE`,
#' set overwrite files to `TRUE`. If `FALSE` and `testing` is `TRUE`,
#' set overwrite to `FALSE`. If `testing` is `FALSE`, this argument is ignored
#' @param ... Other arguments to pass to [rmarkdown::draft()].
#'
#' @details This is a light wrapper around [rmarkdown::draft()]. Consult that
#' function for further details.
#'
#' @family drafting
#' @examples
#' \dontrun{
#' csasdown::draft("resdoc")
#' csasdown::draft("resdoc-b")
#' csasdown::draft("sr")
#' csasdown::draft("techreport")
#' csasdown::draft("manureport")
#' csasdown::draft("fsar")
#' }
#' @export
draft <- function(type = c("resdoc", "resdoc-b", "sr", "techreport", "manureport", "fsar"),
                  directory = getwd(),
                  fn = "index.Rmd",
                  edit = FALSE,
                  create_rstudio_file = TRUE,
                  verbose = TRUE,
                  testing = FALSE,
                  testing_affirm_ovr = FALSE,
                  ...) {


  # nocov start
  if(!grepl("\\/rmarkdown\\/templates", type)){
    # This is necessary so this function also works with unit testing
    if(!type %in% c("resdoc", "resdoc-b", "sr", "techreport", "manureport", "fsar")){
      alert(csas_color("type"), " must be one of ",
            csas_color("resdoc"), ", ", csas_color("resdoc-b"), ", ",
            csas_color("sr"), ", ",
            csas_color("techreport"), ", ",
            csas_color("manureport"), ", or", csas_color("fsar"))
    }
    package <- "csasdown"
  }else{
    package <- NULL
  }
  # nocov end
  # type <- match.arg(type)

  if(!dir.exists(directory)){
    bail("The directory ", fn_color(directory), " does not exist so ",
         "the csasdown project cannot be created there")
  }
  wd <- getwd()
  on.exit(setwd(wd))
  setwd(directory)

  if(verbose){
    notify("Drafting a new ", csas_color(type), " project ...")
    # Show PDF warning for resdoc and sr types
    if(type %in% c("resdoc", "resdoc-b", "sr")){
      message("\n", csas_pdf_message, "\n")
    }
  }

  # Get files and directories present in the directory ------------------------
  # `all_files` is files ONLY, no directories
  all_files <- dir()
  # Keep RStudio project files during deletion and don't include them
  # in the check to see if the directory is empty
  fns <- all_files[!grepl("^.*(RProj|Rproj|rproj)$", all_files)]

  # `dirs` is directories ONLY, no files
  dirs <- list.dirs()
  # Remove 'hidden' directories that start with './.'
  dirs <- dirs[-grep("^(\\.\\/\\.\\S+|\\.)$", dirs)]
  # Removed leading './' from directory names
  dirs <- gsub("^\\.\\/(\\S+)$", "\\1", dirs)

  if(length(fns) || length(dirs)){
    # There is at least one file or directory present
    bail("New document not drafted because there are existing files ",
         "and/or directories in the directory ", fn_color(directory), ".",
         "Please delete these files and/or directories manually or set ",
         "your working directory to a new folder.")
  }

  rmarkdown::draft(fn,
                   template = type,
                   package = package,
                   edit = edit,
                   ...)

  # rmarkdown::draft() does not copy files that begin with a dot (on Windows)
  # so we just rename the git ignore file
  if (file.exists("_gitignore")) {
    file.rename("_gitignore", ".gitignore")
  }
  # if (file.exists("_here")) {
  #   file.rename("_here", ".here")
  # }
  if (create_rstudio_file) create_rstudio_project_file()

  if(verbose){
    # `dirs` is directories ONLY, no files
    dirs <- list.dirs()
    # Remove 'hidden' directories that start with './.'
    dirs <- dirs[-grep("^(\\.\\/\\.\\S+|\\.)$", dirs)]
    # Removed leading './' from directory names
    dirs <- gsub("^\\.\\/(\\S+)$", "\\1", dirs)
    # Add angle brackets around them
    dirs <- paste0("<", dirs, ">")

    check_notify("Directories copied:\n",
                 paste(dirs, collapse = "\n"), "\n")
    files <- setdiff(list.files(), list.dirs(recursive = FALSE,
                                             full.names = FALSE))
    check_notify("Files copied:\n",
                 paste(files, collapse = "\n"), "\n")
    check_notify("Successfully drafted a new ", csas_color(type),
                 " project in ", fn_color(directory), "\n")
    notify("To render it into a document, run ",
           csas_color("csasdown::render()"))
    notify("Change render options in the file ",
           fn_color(fn))
  }
}
