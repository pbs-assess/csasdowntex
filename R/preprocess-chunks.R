#' Run the pre-processor on the chunks in the Rmd files
#'
#' @keywords internal
#'
#' @param fns A vector of character strings holding the Rmd filenames that
#' are to be included
#' @param yaml_fn The Bookdown YAML filename
#' @param line_offsets A [data.frame()] or [tibble::tibble()] containing the
#' line offsets to be added to line numbers in messages in order to give the
#' correct line numbers for the original files. Has columns `fn` (chr),
#' `chunk_header` (chr)  `chunk_ind` (int), `pre_num` (dbl), `post_num` (dbl),
#' and `rmd_num` (dbl). See `return` values for [inject_rmd_files()] and
#' @param verbose Logical. If `TRUE`, print messages
#' @param ... Absorb arguments intended for other functions
#'
#' @importFrom purrr map2_chr
#'
#' @return Nothing
preprocess_chunks <- function(fns,
                              yaml_fn = "_bookdown.yml",
                              line_offsets = NULL,
                              verbose = FALSE,
                              ...){

  if(verbose){
    notify("Running preprocessor on knitr chunks ...")
  }

  map(fns, ~{
    fn <- .x
    if(!file.exists(.x)){
      bail("The file ", fn_color(.x), " does not exist. Check the YAML ",
           "file entry in file ", fn_color(yaml_fn))
    }
    rmd <- readLines(.x)

    cat_inds <- grep("^cat\\(.*", trimws(rmd))
    # Find `needs_trans = TRUE` chunks. Those will not have newlines converted
    # Need to make sure they are in chunk headers and not in text
    # (inside `cat()` calls) so have to pre-match the chunk header lines
    # then match from those
    chunk_head_inds <- grep(all_patterns$md$chunk.begin, rmd)
    if(!length(chunk_head_inds)){
      # No chunks in the file, return the file
      return(NULL)
    }
    if(verbose){
      purrr::walk(chunk_head_inds, ~{
        notify(csas_color(rmd[.x]))
      })
    }
    nt_inds <- chunk_head_inds[grep("needs_trans\ *=\ *TRUE",
                                    rmd[chunk_head_inds])]
    nt_chunks <- NULL
    if(length(nt_inds)){
      if(!all((nt_inds + 1) %in% cat_inds)){
        # Name the chunks which are missing `cat()`
        bad_chunk_headers <- map(nt_inds, ~{
          x <- .x + 1
          if(!x %in% cat_inds){
            chunk_head <- grep(all_patterns$md$chunk.begin, rmd[.x], value = TRUE)
            return(chunk_head)
          }
        })
        bad_chunk_headers <- bad_chunk_headers[lengths(bad_chunk_headers) > 0]
        bad_chunk_names <- map(bad_chunk_headers, ~{
          pat <- "r\\s*(\\s*\\S+\\s*)+?\\s*,\\s*.*$"
          chunk_head_contents <- gsub(all_patterns$md$chunk.begin, "\\1", .x)
          gsub(pat, "\\1", chunk_head_contents)
        })
        bad_chunk_line_nums <- map_dbl(bad_chunk_headers, ~{
          get_real_line_num(.x, line_offsets)
        })
        bail("The following chunk name(s) are missing ",
             csas_color("cat()"), " or ", csas_color("rmd_file('filename')"),
             " as their first lines of code (not including blank lines and ",
             "comment lines):\n\n",
             paste(map2_chr(bad_chunk_names, bad_chunk_line_nums, ~{
               paste0("In file ", fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)),
                      ", line ", tag_color(.y), ": <<", .x, ">>")
             }), collapse = "\n"),
             "\n\nIf those chunks contain ", csas_color("<<chunk-name>>"),
             ", check the source chunks for those by searching the project ",
             "for the chunk name and make sure they have ",
             csas_color("cat()"), " or ", csas_color("rmd_file('filename')"),
             " as their first lines of code")
      }
      cat_inds <- cat_inds[!(cat_inds %in% (nt_inds + 1))]

      # Deal with needs-trans chunks
      nt_inds <- nt_inds + 1
      chunk_end_inds <- NULL
      nt_chunks <- imap(nt_inds, ~{
        if(.y == length(nt_inds)){
          text_chunk <- parse_cat_text(rmd[.x:length(rmd)])
        }else{
          text_chunk <- parse_cat_text(rmd[.x:(nt_inds[.y + 1] - 1)])
        }
        if(any(grepl("[^ -~]", text_chunk))){
          bail("You have set ", message_color("`needs_trans = TRUE` "),
               "on a chunk with UTF-8 text (likely French text). You most ",
               "likely forgot to set ",
               message_color("`needs_trans = FALSE` "), " in the header ",
               "after pasting the translated French into the chunk:\n",
               message_color(rmd[.x - 1]))
        }
        chunk_end_inds <<- c(chunk_end_inds, .x + length(text_chunk) - 1)
        # Convert any single backslashes to double. All the extra ones are needed here
        # because of escaping and because we are inside a cat() layer so it is a double-
        # double situation
        text_chunk <- gsub("\\\\", "\\\\\\\\", text_chunk)
        # If single quotes were used to surround the text in [cat()], make them double
        # so that they match the quotes used to make the embedded code parts
        if(substr(text_chunk[1], 1, 1) == "'"){
          substr(text_chunk[1], 1, 1) <- "\""
        }
        if(substr(text_chunk[length(text_chunk)],
                  nchar(text_chunk[length(text_chunk)]),
                  nchar(text_chunk[length(text_chunk)])) == "'"){
          substr(text_chunk[length(text_chunk)],
                 nchar(text_chunk[length(text_chunk)]),
                 nchar(text_chunk[length(text_chunk)])) <- "\""
        }
        text_chunk <- map_chr(text_chunk, ~{
          catize(.x)
        })
        text_chunk[1] <- paste0("cat(", text_chunk[1])
        text_chunk[length(text_chunk)] <- paste0(text_chunk[length(text_chunk)], ")")
        text_chunk
      })
    }
    if(length(nt_chunks)){
      out_rmd <- NULL
      out_rmd <- rmd[1:(nt_inds[1] - 1)]
      # extract chunks not part of chunks with `needs_trans = TRUE`
      nt_start_inds <- nt_inds[-1]
      nt_start_inds <- c(nt_start_inds, length(rmd))
      nonnt_chunks <- imap(seq_along(nt_start_inds), ~{
        end <- ifelse(.x == length(nt_start_inds),
                      nt_start_inds[.x],
                      (nt_start_inds[.x] - 1))
        rmd[(chunk_end_inds[.x] + 1):end]
      })
      # Interlace the two, nt_chunks and nonnt_chunks
      map(seq_along(nt_chunks), ~{
        out_rmd <<- c(out_rmd, nt_chunks[[.x]], nonnt_chunks[[.x]])
      })
    }else{
      out_rmd <- rmd
    }
    rmd <- out_rmd

    # Deal with non-needs_trans chunks, the same way as we dealt with the
    # need_trans chunks
    chunk_end_inds <- NULL
    cat_chunks <- NULL

    if(length(cat_inds)){
      cat_chunks <- imap(cat_inds, ~{
        if(.y == length(cat_inds)){
          text_chunk <- parse_cat_text(rmd[.x:length(rmd)])
        }else{
          text_chunk <- parse_cat_text(rmd[.x:(cat_inds[.y + 1] - 1)])
        }
        chunk_end_inds <<- c(chunk_end_inds, .x + length(text_chunk) - 1)

        # Convert any single backslashes to double. All the extra ones are needed here
        # because of escaping and because we are inside a cat() layer so it is a double-
        # double situation
        text_chunk <- gsub("\\\\", "\\\\\\\\", text_chunk)

        # Check for equations
        start_eq <- grep("\\\\begin\\{(equation|flalign|align)\\*?\\}", text_chunk)
        end_eq <- grep("\\\\end\\{(equation|flalign|align)\\*?\\}", text_chunk)
        chunk_has_eq <- FALSE
        if(length(start_eq)){
          if(length(start_eq) != length(end_eq)){
            bail("Non-equal numbers of ", csas_color("\\begin{(equation|flalign|align)}"),
                 " and ", csas_color("\\end{(equation|flalign|align)}"), " detected")
          }
          chunk_has_eq <- TRUE

          eqs <- map2(start_eq, end_eq, ~{text_chunk[seq(.x, .y)]})
          # Make sure the first equation does not have a quote preceding it
          # on the begin equation label on its first line. If it does, remove it
          eqs[[1]][1] <-
            gsub("(\\s*'|\")",
                 "",
                 eqs[[1]][1])
          # Make sure the final equation does not have a quote following after
          # the end equation label on its last line. If it does, remove it
          eqs[[length(eqs)]][length(eqs[[length(eqs)]])] <-
             gsub("(\\\\end\\{(equation|flalign|align)\\*?\\})(\\s*'|\")$",
                  "\\1",
                  eqs[[length(eqs)]][length(eqs[[length(eqs)]])])
        }

        if(!(length(text_chunk) == 1 && text_chunk[1] == "\"\"")){
          # There will be a leading quote and ending quote, but they may not be on
          # their own line. Remove them and keep track if they shared a line with other
          # values.
          if(text_chunk[1] == "\"" || text_chunk[1] == "'"){
            text_chunk <- text_chunk[-1]
            # Toggle to paste the quote to the beginning of the first element
            # If FALSE, add a new element to the beginning of the vector with
            # the quote character only after calling convert_newlines_rmd()
            paste_beg <- FALSE
          }
          if(text_chunk[length(text_chunk)] == "\"" || text_chunk[length(text_chunk)] == "'"){
            text_chunk <- text_chunk[-length(text_chunk)]
            # Toggle to paste the quote to the end of the last element
            # If FALSE, add a new element to the end of the vector with
            # the quote character only after calling convert_newlines_rmd()
            paste_end <- FALSE
          }
          if(length(grep("^(\"|').+$", text_chunk[1]))){
            text_chunk[1] <- gsub("(\"|')(.*)", "\\2", text_chunk[1])
            paste_beg <- TRUE
          }
          if(length(grep("^.+(\"|')$", text_chunk[length(text_chunk)]))){
            text_chunk[length(text_chunk)] <- gsub("(.*)(\"|')", "\\1", text_chunk[length(text_chunk)])
            paste_end <- TRUE
          }

          text_chunk <- convert_newlines_rmd(text_chunk)

          if(paste_beg){
            text_chunk[1] <- paste0("\"", text_chunk[1])
          }else{
            text_chunk <- c("\"", text_chunk)
          }
          if(paste_end){
            text_chunk[length(text_chunk)] <- paste0(text_chunk[length(text_chunk)], "\"")
          }else{
            text_chunk <- c(text_chunk, "\"")
          }
        }

        text_chunk <- map_chr(text_chunk, ~{
          catize(.x)
        })

        if(chunk_has_eq){
          # Replace the modified equation lines with the unmodified ones
          start_eq <- grep("\\\\begin\\{(equation|flalign|align)\\*?\\}", text_chunk)
          end_eq <- grep("\\\\end\\{(equation|flalign|align)\\*?\\}", text_chunk)
          text_chunk <- replace_vecs_with_vecs(text_chunk, eqs, start_eq, end_eq)
        }

        # If the first line is the beginning of an equation and paste_beg
        # of the line is TRUE, add a quote. Quote is added for other cases
        # earlier
        if(chunk_has_eq &&
           paste_beg &&
           length(grep("\\\\begin\\{(equation|flalign|align)\\*?\\}", text_chunk[1]))){
          text_chunk[1] <- paste0("cat(\"", text_chunk[1])
        }else{
          text_chunk[1] <- paste0("cat(", text_chunk[1])
        }
        # If the last line is the end of an equation and paste_end of the
        # line is TRUE, add a quote. Quote is added for other cases earlier
        if(chunk_has_eq &&
           paste_end &&
           length(grep("\\\\end\\{(equation|flalign|align)\\*?\\}", text_chunk[length(text_chunk)]))){
          text_chunk[length(text_chunk)] <- paste0(text_chunk[length(text_chunk)], "\")")
        }else{
          text_chunk[length(text_chunk)] <- paste0(text_chunk[length(text_chunk)], ")")
        }
        text_chunk
      })
    }

    if(length(cat_chunks)){
      out_rmd <- NULL
      out_rmd <- rmd[1:(cat_inds[1] - 1)]
      # extract chunks not part of 'cat()'s
      cat_start_inds <- cat_inds[-1]
      cat_start_inds <- c(cat_start_inds, length(rmd))
      noncat_chunks <- imap(seq_along(cat_start_inds), ~{
        end <- ifelse(.x == length(cat_start_inds),
                      cat_start_inds[.x],
                      (cat_start_inds[.x] - 1))
        rmd[(chunk_end_inds[.x] + 1):end]
      })
      # Interlace the two, cat_chunks and noncat_chunks
      map(seq_along(cat_chunks), ~{
        out_rmd <<- c(out_rmd, cat_chunks[[.x]], noncat_chunks[[.x]])
      })
    }else{
      out_rmd <- rmd
    }

    if(length(out_rmd)){
      # Make references work, they were modified above in the code that changes
      # the backslashes
      out_rmd <- gsub("\\\\\\\\\\\\\\\\@ref", "\\\\\\\\@ref", out_rmd)

      # If any chunks do not have a blank line between them, make it so
      # The lack of blank line between them messes up the typesetting in a bad
      # way in some cases
      bt_inds <- grep("```", out_rmd)
      spaced_out_rmd <- NULL
      i <- 1
      while(i < length(out_rmd)){
        if(substr(trimws(out_rmd[i]), 1, 3) == "```" &&
           substr(trimws(out_rmd[i + 1]), 1, 3) == "```"){
          spaced_out_rmd <- c(spaced_out_rmd, out_rmd[i], "", out_rmd[i + 1])
          i <- i + 2
        }else{
          spaced_out_rmd <- c(spaced_out_rmd, out_rmd[i])
          i <- i + 1
        }
      }
      if(i <= length(out_rmd)){
        spaced_out_rmd <- c(spaced_out_rmd, out_rmd[i])
      }
    }else{
      spaced_out_rmd <- "" # nocov
    }
    unlink(.x, force = TRUE)
    writeLines(spaced_out_rmd, .x)
  })

  if(verbose){
    check_notify("Chunks preprocessed successfully\n")
  }

  invisible()
}