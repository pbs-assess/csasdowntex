#' Remove all comment lines from knitr chunks in the Rmd code for the files
#' provided
#'
#' @details
#' Overwrites the files `rmd_files` with a modified version that has the
#' comments removed from chunks which contain a call to `cat()`,
#' `rmd_file()`, or a mirror chunk `<<chunk-name>>`
#'
#' @keywords internal
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return A [tibble::tibble()] containing the Rmd filenames (column `fn`)
#' and all the full chunk headers (column `chunk_header`) which are
#' unique document-wide. The chunk line number (column `chunk_ind`) is
#' also included so that headers can be searched for later, matched,
#' and correct line numbers from unmodified Rmd lines can be reported.
#' Columns containing the number of comment lines and blank lines removed
#' from each chunk before (`pre_num`) and after (`post_num`) a call to
#' `cat()`, `rmd_file()`, or a mirror chunk `<<chunk-name>>` are included.
#' Addition of `chunk_ind` and `pre_num` allow exact line numbers to be
#' quoted in messages for the original files.
#'
#' @importFrom dplyr summarize enquos group_by mutate
#' @importFrom knitr all_patterns
#' @importFrom purrr map_dbl pwalk
remove_comments_from_chunks <- function(rmd_files,
                                        verbose = FALSE){

  if(verbose){
    notify("Removing comments from knitr code chunks containing:\n",
           csas_color("cat()"), ", ", csas_color("<<chunk-name>>"),
           ", or ", csas_color("rmd_file()"), " ...")
  }

  offsets <- map(rmd_files, function(fn){
    suppressWarnings(rmd <- readLines(fn))
    # Extract the knitr code chunks
    #pat_code_begin <- all_patterns$md$chunk.begin
    pat_code_begin <- "^[\t >]*```+\\s*\\{(=latex|([a-zA-Z0-9_]+( *[ ,].*)?))\\}\\s*$"
    # The following pat code matched chunks with four backticks, which are
    # verbatim code chunks and not meant to be processed
    # pat_code_end <- all_patterns$md$chunk.end
    pat_code_end <- "^[\t >]*```\\s*$"
    start_inds <- grep(pat_code_begin, rmd)
    end_inds <- grep(pat_code_end, rmd)
    chunk_headers <- rmd[start_inds]

    if(length(start_inds) != length(!end_inds)){
      bail("The number of knitr starting code chunk header lines does not ",
           "equal the number of ending code chunk lines ",
           "(triple backtick-lines)")
    }

    if(!length(start_inds)){
      # No code chunks found
      return(NULL)
    }
    code_chunks <- map2(start_inds, end_inds, function(.x, .y) {
      rmd[seq(.x, .y)]
    })
    # Process code chunks here (remove comment lines and blank lines).
    # Unfortunately, if an Rmarkdown header is inside a `cat()` statement,
    # starting on its own line, it will appear exactly the same as a
    # comment inside the code chunk. To fix this, look for `cat()` statements
    # inside code chunks, and ignore lines that start with # inside the
    # `cat()` statements.
    com_pat <- "(^#.*$|^$)"
    # code_chunks is a list of three items for each chunk:
    # 1. chunk contents (without any comments)
    # 2. how many comment lines came before the `cat()` or `rmd_lines()`
    # 3. how many comment lines came after the `cat()` or `rmd_lines()`
    code_chunks <- imap(code_chunks, function(chunk, chunk_ind){
      trim_chunk <- trimws(chunk)
      cat_ind <- grep("^cat\\(.*", trim_chunk)
      mirror_ind <- grep("^<<\\S+>>$", trim_chunk)
      rmd_file_ind <- grep("^rmd_file\\(\\S+\\)$", trim_chunk)
      has_cat <- as.logical(length(cat_ind))
      has_mirror <- as.logical(length(mirror_ind))
      has_rmd_file <- as.logical(length(rmd_file_ind))
      if(has_cat || has_mirror || has_rmd_file){
        if(length(cat_ind) > 1 ||
           length(rmd_file_ind) > 1){
          bail("Can only have one ", csas_color("cat() call"), " or ",
               csas_color("rmd_file() call"), " inside each code chunk:\n\n",
               csas_color(paste(chunk, collapse = "\n")),
               "\n\n")
        }
        if(has_cat){
          # Get the indices of the chunk which are inside the `cat()` call
          k <- parse_cat_text(chunk[cat_ind:length(chunk)], ret_inds = TRUE)
          k <- cat_ind + k - 1
        }else if(has_mirror){
          k <- mirror_ind
        }else if(has_rmd_file){
          k <- rmd_file_ind
        }else{
          # no else needed since we can't get in here unless one of
          # the 3 is TRUE
        }

        # Need to loop through the lines of the chunk
        new_chunk <- NULL
        pre <- TRUE
        num_com_lines_pre <- 0
        num_com_lines_post <- 0
        i <- 1
        repeat{
          if(i %in% k){
            pre <- FALSE
            new_chunk <- c(new_chunk, chunk[i])
          }else{
            trim_line <- trimws(chunk[i])
            is_com <- as.logical(length(grep(com_pat, trim_line)))
            is_header <- as.logical(length(grep(pat_code_begin, trim_line)))
            if(is_com){
              if(pre){
                num_com_lines_pre <- num_com_lines_pre + 1
              }else{
                num_com_lines_post <- num_com_lines_post + 1
              }
            }else{
              new_chunk <- c(new_chunk, chunk[i])
            }
          }
          if(i == length(chunk)){
            break
          }
          i <- i + 1
        }
        ln <- start_inds[chunk_ind]
        if(verbose){
          num_comments_removed <- num_com_lines_pre + num_com_lines_post
          if(num_comments_removed > 0){
            comm <- ifelse(num_comments_removed == 1,
                           "comment line/blank line",
                           "comment lines/blank lines")
            notify("Removing ",
                   tag_color(num_comments_removed),
                   " ", comm, " from chunk on line ", tag_color(ln), " of ",
                   fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)))
          }
        }
        return(list(new_chunk, num_com_lines_pre, num_com_lines_post))
      }else{
        # not a 'cat()', '<<chunk-name>>' or 'rmd_file()' chunk, leave
        # comments in
        return(list(chunk, 0, 0))
      }
    })

    num_comments_before <- map_dbl(code_chunks, ~{.x[[2]]})
    num_comments_after <- map_dbl(code_chunks, ~{.x[[3]]})
    offset_df <- tibble(fn = rep(fn, length(num_comments_before)),
                        chunk_header = chunk_headers,
                        chunk_ind = start_inds,
                        pre_num = num_comments_before,
                        post_num = num_comments_after)
    code_chunks <- map(code_chunks, ~{.x[[1]]})

    out_rmd <- NULL
    if(start_inds[1] > 1){
      out_rmd <- c(out_rmd, rmd[1:(start_inds[1] - 1)])
    }
    imap(start_inds, ~{
      out_rmd <<- c(out_rmd, code_chunks[[.y]])
      if(.y < length(start_inds)){
        if(start_inds[.y + 1] - end_inds[.y] > 1){
          out_rmd <<- c(out_rmd,
                        rmd[(end_inds[.y] + 1):(start_inds[.y + 1] - 1)])
        }
      }
    })
    if(end_inds[length(end_inds)] < length(rmd)){
      out_rmd <- c(out_rmd,
                   rmd[(end_inds[length(end_inds)] + 1):length(rmd)])
    }

    writeLines(out_rmd, fn)
    offset_df
  }) |>
    map_df(~{.x})

  if(verbose && nrow(offsets)){
    tot_comments_removed <- sum(offsets$pre_num + offsets$post_num)
    if(as.logical(sum(tot_comments_removed))){
      comm <- ifelse(sum(tot_comments_removed) == 1,
                     "comment line/blank line",
                     "comment lines/blank lines")
      check_notify(tag_color(tot_comments_removed),
                   " ", comm, " successfully removed from chunks containing:\n",
                   csas_color("cat()"), ", ", csas_color("<<chunk-name>>"),
                   ", or ", csas_color("rmd_file()"))

      tot_removed_by_file <- offsets |>
        group_by(fn) |>
        summarize(removed = sum(pre_num + post_num))

      pwalk(tot_removed_by_file, ~{
        check_notify(paste0("Removed ", tag_color(..2),
                            " comment lines/blank lines from file ",
                            fn_color(..1)))
      })
      if(verbose){
        check_notify("Comment lines and blank lines removed successfuly\n")
      }
    }else{
      check_notify("There were no comments in chunks found to remove\n")
    }
  }

  offsets
}