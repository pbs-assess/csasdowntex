#' Create a copy of a bookdown YAML file containing modified Rmd File names
#' Create a copy of the RMd files listed, with the names listed in the new
#' YAML file
#'
#' @description
#' Creates a copy of a bookdown YAML file with "tmp_" prepended to the name.
#' Inside the new file, all listed Rmd files will have their names prepended
#' with "tmp_". Those files will also be created by copying the ones listed
#' in the original YAML file.
#'
#' @details
#' The file must have an entry like this:
#'
#' `` rmd_files: ["index.Rmd", ``
#' ``             "01-chap1.Rmd"] ``
#' ``            #"02-chap2.Rmd", ``
#' ``            #"03-chap3.Rmd", ``
#' ``            #"04-references.Rmd", ``
#' ``            #"05-appendix.Rmd"] ``
#'
#' This will appear in the new file as:
#'
#' `` rmd_files: ["index.Rmd", ``
#' ``             "tmp_01-chap1.Rmd"] ``
#'
#' The file must also have a line like this before the above list, which is
#' used as an index for where to insert the modified file list after:
#' book_filename: "resdoc"
#'
#' There are only two requirements to how the files are listed in the
#' `rmd_files: [` section: 1) they must begin right after the `[`
#' and 2) the closing `]` must be at the end of its line with no more code
#' following. They look nice as listed above but could be more than one to
#' a line, or all on one line.
#'
#' @keywords internal
#'
#' @param yaml_fn The [bookdown::bookdown] YAML file name, by default is "_bookdown.yml"
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @importFrom purrr walk2
#'
#' @return A list of two, 1) The name of the temporary YAML file created
#' 2) A vector of the temporary Rmd files created
create_tmp_yaml_rmd_files <- function(yaml_fn = "_bookdown.yml",
                                      verbose = FALSE){

  if(verbose){
    notify("Creating temporary YAML and Rmarkdown files ...")
  }

  # In case render() was stopped and tmp- files are still present:
  tmp_files <- list.files(pattern = "^tmp-*")
  unlink(tmp_files)

  if(!file.exists(yaml_fn)){
    bail("The YAML file ", fn_color(yaml_fn), " does not exist")
  }
  yaml <- readLines(yaml_fn)
  if(!length(yaml)){
   bail("The YAML file ", fn_color(yaml_fn), " is empty")
  }
  # Remove surrounding whitespace
  yaml <- trimws(yaml)
  # Remove commented lines and extra surrounding quotes
  yaml <- noquote(yaml[!grepl("^#", yaml)])

  # Store indices where the open square bracket is
  brac_open_ind <- grep("^rmd_files: \\[", yaml)
  if(!length(brac_open_ind)){
    bail(tag_color("rmd_files: ["), " not found in ", fn_color(yaml_fn), ". ",
         "It must appear at the beginning of a line")
  }
  if(length(brac_open_ind) > 1){
    bail("More than one ", tag_color("rmd_files: ["), " found in ",
         fn_color(yaml_fn))
  }
  if(brac_open_ind > 1){
    pre_rmd_fns <- yaml[1:(brac_open_ind - 1)]
  }else{
    pre_rmd_fns <- NULL
  }

  # Store indices where the close square bracket is
  brac_close_ind <- grep("\\]$", yaml)
  if(!length(brac_close_ind)){
    bail(tag_color("]"), " not found in ", fn_color(yaml_fn), ". ",
         "It must appear at the end of a line")
  }

  # Choose first `]` index. This will be the non-greedy match
  brac_close_ind <- brac_close_ind[1]
  if(brac_close_ind < length(yaml)){
    post_rmd_fns <- yaml[(brac_close_ind + 1):length(yaml)]
  }else{
    post_rmd_fns <- NULL
  }

  # Extract all Rmd filenames
  rmd_fns <- unlist(str_extract_all(yaml, "[a-zA-Z0-9_\\-]+\\.(R|r)md"))
  if(!length(rmd_fns)){
    bail("No .Rmd filenames found in ", fn_color(yaml_fn))
  }
  # In case render() was stopped and tmp- files are still listed in yaml:
  rmd_fns <- gsub("^tmp\\-", "", rmd_fns)

  # Copy the files into the new files
  orig_rmd_fns <- rmd_fns
  rmd_fns <- paste0("tmp-", rmd_fns)
  if(interactive() && any(grepl("^tmp-tmp", rmd_fns))){
    repeat{
      ques <- readline(
        question("csasdowntex detected that some of the Rmd ",
                 "files listed in ", fn_color(yaml_fn),
                 " begin with two or more ", csas_color("tmp-") ,
                 " prefixes.\nContinuing will result in temporary ",
                 "files beginning with ", csas_color("tmp-tmp-"), ".\n\n",
                 "Are you sure you want to render this? (y/n)"))
      if(tolower(ques) != "n" &&
         tolower(ques) != "no" &&
         tolower(ques) != "y" &&
         tolower(ques) != "yes"){
        notify("Please answer yes or no.")
      }else{
        break
      }
    }
    if(tolower(ques) == "n" || tolower(ques) == "no"){
      bail("Stopped rendering due to extra ", csas_color("tmp-"),
           " prefixes being found ",
           "in Rmd filenames listed in ", fn_color(yaml_fn))
    }
  }
  unlink(rmd_fns, force = TRUE)

  walk2(orig_rmd_fns, rmd_fns, ~{
    success <- file.copy(.x, .y, overwrite = TRUE)
    if(success){
      if(verbose){
        check_notify("File copied from ", fn_color(.x), " to ", fn_color(.y))
      }
    }else{
      # nocov start
      bail("Could not copy file from ", fn_color(.x), " to ", fn_color(.y), "\n",
           "Check that file exists in the current directory")
      # nocov end
    }
  })

  # Create the text listing the new Rmd files for inside the new YAML file
  rmd_fns_listing <- imap_chr(rmd_fns, ~{
    if(.y == 1){
      paste0('rmd_files: ["', .x, '",')
    }else if(.y == length(rmd_fns)){
      paste0('            "', .x, '"]')
    }else{
      paste0('            "', .x, '",')
    }
  })

  # Create the new YAML file
  yaml <- c(pre_rmd_fns, rmd_fns_listing, post_rmd_fns)
  tmp_fn <- paste0("tmp", yaml_fn)
  writeLines(yaml, tmp_fn)

  if(file.exists(tmp_fn)){
    if(verbose){
      check_notify("File ", fn_color(tmp_fn), " created")
    }
  }else{
    # nocov start
    bail("Could not create file ", fn_color(tmp_fn))
    # nocov end
  }

  if(verbose){
    check_notify("Temporary YAML and Rmarkdown files created ",
                 "successfully\n")
  }

  list(tmp_fn, rmd_fns)
}
