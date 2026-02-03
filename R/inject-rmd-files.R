#' Inject verbatim text from Rmd files into Rmd code
#'
#' @description
#' Inject verbatim text from Rmd files into Rmd code wrapped in
#' [cat()] so that the bilingual features of csasdowntex can be used properly
#' but the author can write paragraphs or sections in pure Rmarkdown.
#' Uses [read_rmd_file()] to read in the file and place `cat()` around
#' the contents before injecting it
#'
#' @keywords internal
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#' @param line_offsets A [data.frame()] or [tibble::tibble()] containing the
#' line offsets to be added to line numbers in messages in order to give the
#' correct line numbers for the original files. Has columns `fn` (chr),
#' `chunk_header` (chr)  `chunk_ind` (int), `pre_num` (dbl), `post_num` (dbl),
#' and `rmd_num` (dbl). See `return` values for [inject_rmd_files()] and
#' [remove_comments_from_chunks()]
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return Nothing
inject_rmd_files <- function(rmd_files,
                             line_offsets = NULL,
                             verbose = FALSE){

  if(verbose){
    notify("Injecting external Rmarkdown files included with ",
           csas_color("rmd_file('filename')"), " ...")
  }

  offsets <- imap(rmd_files, function(fn, fn_ind){
    rmd <- readLines(fn)
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
           "equal the number of ending code chunk lines (triple backtick-lines)")
    }
    if(!length(start_inds)){
      # No code chunks found
      return(NULL)
    }
    code_chunks <- map2(start_inds, end_inds, function(.x, .y) {
      rmd[seq(.x, .y)]
    })

    # Process code chunks here (replace rmd_files('file') with the 'file'
    # contents).
    # Single or double quotes around filename
    code_chunks <- imap(code_chunks, function(chunk, chunk_ind){
      trim_chunk <- trimws(chunk)
      rmd_file_ind <- grep("^rmd_file\\([\"|\'].*[\"|\']\\)$", trim_chunk)
      rmd_file_name <- trimws(chunk[rmd_file_ind])
      rmd_file_name <- gsub("^rmd_file\\([\"|\'](.*)[\"|\']\\)$", "\\1",
                            rmd_file_name)

      if(!length(rmd_file_name)){
        return(list(chunk, 0))
      }
      rmd <- read_rmd_file(rmd_file_name, fn)
      # Strip any HTML comments out of the file
      rmd <- remove_html_comments(rmd, rmd_file_name)
      # Need to remove the NA's from the chunk that was returned from `remove_html_comments()`
      rmd <- rmd[!is.na(rmd)]
      # Number of lines actually injected
      num_lines <- length(rmd)
      # Check the rmd code to make sure there are no triple-tick code chunks in it
      backtick_inds <- grep("^```", trimws(rmd))
      if(length(backtick_inds)){
        bail("Triple-backticks found in file ",
             fn_color(basename(rmd_file_name)),
             " on line(s) ",
             tag_color(paste(backtick_inds, collapse = ", ")), "\n",
             "Triple- or Quadruple-backtick code chunks are not allowed in ",
             "external RMD files which have been injected using ",
             csas_color("rmd_file()"), ". The code is going to be imported ",
             "into a chunk and embedding chunks into other chunks is not ",
             "possible in knitr. ",
             "Delete them or use Markdown comments to comment those chunks out ",
             "<!-- -->.")
      }
      if(verbose){
        if(is.null(line_offsets)){
          notify("Injecting ", fn_color(rmd_file_name), " into ",
                 fn_color(fn))
        }else{
          ln <- get_real_line_num(chunk_headers[chunk_ind], line_offsets)
          notify("Injecting ", fn_color(rmd_file_name), " at ",
                 "line ", ln, " of ",
                 fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)))
        }
      }
      # Insert the code into the chunk
      chunk <- c(chunk[1:(rmd_file_ind - 1)],
                 rmd,
                 chunk[(rmd_file_ind + 1):length(chunk)])
      list(chunk, num_lines)
    })

    # Number of lines in each file injected
    num_lines <- map_dbl(code_chunks, ~{.x[[2]]})
    offset_df <- tibble(fn = rep(fn, length(num_lines)),
                        chunk_header = chunk_headers,
                        chunk_ind = start_inds,
                        post_num = num_lines)


    code_chunks <- map(code_chunks, ~{.x[[1]]})

    # Rebuild the Rmd file by alternating code chunks and non-code chunks
    new_rmd <- NULL
    if(start_inds[1] > 1){
      new_rmd <- rmd[1:(start_inds[1] - 1)]
    }

    # Note that `start_inds` and `end_inds` have the same length as code_chunks.
    # This is guaranteed from error checks above
    for(i in seq_along(code_chunks)){
      # Append the next code chunk
      new_rmd <- c(new_rmd, code_chunks[[i]])
      # Append the lines between this code chunks and the next one
      # (possibly zero lines)
      if(i == length(code_chunks)){
        # In case the last code chunk was also the last line in the file
        if((end_inds[i] + 1) <= length(rmd)){
          new_rmd <- c(new_rmd, rmd[(end_inds[i] + 1):length(rmd)])
        }
      }else{
        # In case the code chunk is bumped up to the next code chunk,
        # so no blank lines or other lines in between
        if((end_inds[i] + 1) <= (start_inds[i + 1] - 1)){
          new_rmd <- c(new_rmd, rmd[(end_inds[i] + 1):(start_inds[i + 1] - 1)])
        }
      }
    }

    writeLines(new_rmd, fn)
    offset_df
  }) |>
    map_df(~{.x})

  if(verbose){
    tot_lines_added <- sum(offsets$post_num)
    if(as.logical(tot_lines_added)){
      check_notify(tag_color(tot_lines_added),
                   " lines successfully injected into chunks:")

      tot_added_by_file <- offsets |>
        group_by(fn) |>
        summarize(added = sum(post_num))

      pwalk(tot_added_by_file, ~{
        check_notify(paste0("Injected ", tag_color(..2),
                            " Rmarkdown lines to file ", fn_color(..1)))
      })
    }else{
      check_notify("There were no ", fn_color("rmd_file()"),
                   " calls, therefore no Rmarkdown code to inject\n")
    }

    check_notify("Rmd files injected successfuly\n")
  }

  offsets
}
