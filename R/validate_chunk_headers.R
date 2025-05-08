#' Validate that chunk headers contain only certain combinations of settings
#'
#' @description
#' Validate that chunk headers contain only certain combinations of settings.
#' Check that chunks with `eval = fr()` or `eval = !fr()`, have the correct
#' `needs_trans` setting and a correct chunk name format if `chunk_regex`
#' is `TRUE`. Verify that these chunks also contain the `results = 'asis'`
#' header option. Without that, `cat()` produces non-Rmarkdown output
#' in the document.
#'
#' @details
#' Inserts `results = 'asis'` if necessary for chunks that have the
#' `eval = fr()` or `eval = !fr()` header options.
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#' @param en_chunk_regex A regular expression to match for the chunk
#' name for English chunks. Default is 'ends in -en'. The `$` means anchor
#' to the end, so '-en' must be at the end. `\\S+` means match one or more
#' any other non-whitespace characters
#' @param fr_chunk_regex A regular expression to match for the chunk
#' name for French chunks. Default is 'ends in -fr'
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return A vector of error messages, one item for each chunk problem
#' @keywords internal
validate_chunk_headers <- function(rmd_files,
                                   en_chunk_regex = "^\\S+-en$",
                                   fr_chunk_regex = "^\\S+-fr$",
                                   verbose = FALSE){

  if(verbose){
    notify("Validating knitr chunk headers ...")
  }

  if(!length(rmd_files)){
    return(invisible())
  }
  map(rmd_files, function(fn){

    if(!file.exists(fn)){
      bail("File ", fn_color(fn), " does not exist") # nocov
    }
    suppressWarnings(rmd <- readLines(fn))

    file_has_errors <- FALSE
    modified_rmd_code <- FALSE
    chunk_head_inds <- grep(all_patterns$md$chunk.begin, rmd)
    if(!length(chunk_head_inds)){
      return(NULL)
    }

    chunk_name_pat <- "^```\\s*\\{\\s*r,?\\s*(\\s*\\S+\\s*)+?\\s*(,|\\}$)\\s*.*"
    map(chunk_head_inds, function(chunk_head_ind){

      curr_chunk_modified <- FALSE
      chunk_head <- trimws(rmd[chunk_head_ind])
      chunk_name <- gsub(chunk_name_pat, "\\1", chunk_head)
      fr_opt <- chunk_header_contains(chunk_head, "eval = fr()")
      en_opt <- chunk_header_contains(chunk_head, "eval = !fr()")
      needs_trans_true <- chunk_header_contains(chunk_head,
                                                "needs_trans = TRUE")
      needs_trans_false <- chunk_header_contains(chunk_head,
                                                 "needs_trans = FALSE")
      results_asis <- chunk_header_contains(chunk_head, "results = 'asis'")
      # Verify that the chunk name fulfills English format requirement
      if(en_opt){
        if(!is.null(en_chunk_regex)){
          if(!grepl(en_chunk_regex, chunk_name)){
            bail("Chunk name on line ", tag_color(chunk_head_ind), " in file ",
                 fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)), " is not of ",
                 "correct format for English chunks (because it contains ",
                 csas_color("`eval = !fr()`"), "). The name does not match ",
                 "the regular expression which is ",
                 csas_color(en_chunk_regex), ":\n",
                 csas_color(chunk_head))
          }
        }
        # Verify that the English chunk does not have `needs_trans = TRUE` or
        # `needs_trans = FALSE`
        if(needs_trans_true || needs_trans_false){
          prev_chunk_head <- rmd[chunk_head_ind]
          rmd[chunk_head_ind] <<- chunk_header_remove(rmd[chunk_head_ind],
                                                      "needs_trans")
          curr_chunk_modified <- TRUE
          modified_rmd_code <<- TRUE
          alert(csas_color("`needs_trans`"), " on line ",
                tag_color(chunk_head_ind), " in file ",
                fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)), " is not allowed ",
                "for English chunks and has been removed automatically:\n",
                csas_color(prev_chunk_head), "  ", tag_color("-->"), "\n",
                csas_color(rmd[chunk_head_ind]), mod_color(" (code modified)"))
        }
      }
      if(fr_opt){
        # Verify that the chunk name fulfills French format requirement
        if(!is.null(fr_chunk_regex)){
          if(!grepl(fr_chunk_regex, chunk_name)){
            file_has_errors <<- TRUE
            bail("Chunk name on line ", tag_color(chunk_head_ind),
                 " in file ",
                 fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)), " is not of ",
                 "correct format for French chunks (because it contains ",
                 csas_color("`eval = fr()`"), " ). The name does not match ",
                 "the regular expression which is ",
                 csas_color(fr_chunk_regex), ":\n",
                 csas_color(chunk_head))
          }
        }
        # Verify that the French chunk has `needs_trans = TRUE` or
        # `needs_trans = FALSE`
        if(!chunk_header_contains(chunk_head, "needs_trans")){
          prev_chunk_head <- rmd[chunk_head_ind]
          rmd[chunk_head_ind] <<- chunk_header_add(rmd[chunk_head_ind],
                                                   "needs_trans = TRUE")
          curr_chunk_modified <- TRUE
          modified_rmd_code <<- TRUE
          alert(csas_color("`needs_trans = TRUE`"), " on line ",
                tag_color(chunk_head_ind), " in file ",
                fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)), " is missing ",
                "for this French chunk and has been added automatically:\n",
                csas_color(prev_chunk_head), "  ", tag_color("-->"), "\n",
                csas_color(rmd[chunk_head_ind]), mod_color(" (code modified)"))
        }
      }
      if(!en_opt && !fr_opt){
        # Ensure that neutral chunks do not have `need_trans` set
        if(needs_trans_true || needs_trans_false){
          prev_chunk_head <- rmd[chunk_head_ind]
          rmd[chunk_head_ind] <<- chunk_header_remove(rmd[chunk_head_ind],
                                                      "needs_trans")
          curr_chunk_modified <- TRUE
          modified_rmd_code <<- TRUE
          alert("Chunk name on line ", tag_color(chunk_head_ind), " in file ",
                fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)), " is not of ",
                "correct format for neutral chunks (chunk headers without ",
                "`fr()` or `!fr()`). It has been removed automatically:\n",
                csas_color(prev_chunk_head), "  ", tag_color("-->"), "\n",
                csas_color(rmd[chunk_head_ind]), mod_color(" (code modified)"))
        }
      }
      if(en_opt || fr_opt){
        # Verify presence of "results = 'asis'" and add if necessary
        if(!results_asis){
          prev_chunk_head <- rmd[chunk_head_ind]
          rmd[chunk_head_ind] <<- chunk_header_add(rmd[chunk_head_ind],
                                                   "results = 'asis'")
          curr_chunk_modified <- TRUE
          modified_rmd_code <<- TRUE
          alert(csas_color("`results = 'asis'`"), " on line ",
                tag_color(chunk_head_ind), " in file ",
                fn_color(fn), " is missing and has been added ",
                "automatically.\n",
                "Any chunk header that contains ",
                csas_color("`eval = fr()`"), " or ",
                csas_color("`eval = !fr()`"), " requires it:\n",
                csas_color(prev_chunk_head), "  ", tag_color("-->"), "\n",
                csas_color(rmd[chunk_head_ind]), mod_color(" (code modified)"))
        }
      }
      if(!is.null(en_chunk_regex)){
        if(!en_opt && grepl(en_chunk_regex, chunk_name)){
          alert("Found a chunk on line ",
                tag_color(chunk_head_ind), " in file ",
                fn_color(fn), " with a name that follows the format for an ",
                "English chunk but is missing ", csas_color("`eval = !fr()`"),
                ". Check this chunk to verify your intentions:\n",
                csas_color(rmd[chunk_head_ind]))
        }
      }
      if(!is.null(fr_chunk_regex)){
        if(!fr_opt && grepl(fr_chunk_regex, chunk_name)){
          alert("Found a chunk on line ",
                tag_color(chunk_head_ind), " in file ",
                fn_color(fn), " with a name that follows the format for a ",
                "French chunk but is missing ", csas_color("`eval = fr()`"),
                ". Check this chunk to verify your intentions:\n",
                csas_color(rmd[chunk_head_ind]))
        }
      }
      if(verbose){
        notify(csas_color(rmd[chunk_head_ind]),
               ifelse(curr_chunk_modified, mod_color(" (code modified)"), ""))
      }
    })
    if(modified_rmd_code){
      writeLines(rmd, fn)
    }
  })

  if(verbose){
    check_notify("Knitr chunk headers are all acceptable\n")
  }
}